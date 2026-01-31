# Load the library packages
library(sf)
library(sp)
library(terra)
library(raster)
library(rangeBuilder)
library(tidyverse)
library(fasterize)
library(CCAMLRGIS)
library(prioritizr)
library(epm)
library(vegan)
library(cluster)
library(geojsonio)
library(tmap)
library(mapview)
library(stars)
library(tabula)
library(lwgeom)
library(exactextractr)
library(quantarcticR)
library(dbplyr)
library(spdep)
library(alphahull)

# Define the base directory for output
base_dir <- "Output/speciesranges/"

# Create the base directory if it doesn't exist
dir.create(base_dir, showWarnings = FALSE)

wdpa_crs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# By default, terra() will create a 1° resolution map in the *WGS 84* coordinate system (lon/lat).
r_1deg <- terra::rast()

# Reading Icefree Habitats
icefree <- st_read("data/outcrop_rock_medium_poly_v73.shp")
icefree.crs <- sf::st_transform(icefree, crs = 3031)
summary(icefree.crs)
plot(icefree.crs)

# Reading ASPA raster Data
aspa <- terra::rast("data/aspa_1km_raster_2024.tif")
aspa
aspa <- project(aspa, "EPSG:3031")
plot(aspa)

# Reading ASPA polygon Data
aspaVector <- vect("data/ASPA-ASMA-Polygons-2024/ASPAs_polygons_v5_2024.shp")
aspaVector <- project(aspaVector, "EPSG:3031")
plot(aspaVector)

# Reading Species Raw Data
bio <- st_read("data/bio_v2.shp")
bio <- st_drop_geometry(bio)
head(bio)

bio_v2 <- terra::vect("data/bio_v2.shp")

# Filter the data for the species
dc_cl <- bio %>%
  dplyr::select("PTM_ACBR2", "decimalLon", "decimalLat")
summary(dc_cl)

# Remove records without coordinates
dc_cl <- dc_cl %>%
  filter(!is.na(decimalLon)) %>%
  filter(!is.na(decimalLat)) %>%
  filter(!is.na(PTM_ACBR2))
summary(dc_cl)

# Remove duplicated records
dc_cl <- dc_cl[!duplicated(dc_cl), ]
summary(dc_cl)

# Remove species with low occurrence records
dc_cl <- dc_cl %>%
  group_by(PTM_ACBR2) %>%
  filter(n() > 3) %>%
  ungroup()
summary(dc_cl)

species <- unique(dc_cl$PTM_ACBR2)
species <- unique(species$PTM_ACBR2)

# Loop through each species to create Alpha Hulls and save rasters
for (i in species) {
  print(i)

  # Filter the data for the species
  dc <- dc_cl %>% filter(PTM_ACBR2 %in% i)

  # Replace spaces with underscores in the species name
  speciesname <- gsub(" ", "_", i)
  x <- dc %>%
    dplyr::select("decimalLon", "decimalLat")

  # Alpha Hull
  range <- getDynamicAlphaHull(x,
    coordHeaders = c("decimalLon", "decimalLat"), initialAlpha = 2,
    clipToCoast = "no", buff = 0
  )
  a.range <- range[[1]]
  alpha_range <- st_as_sf(a.range)
  sf_use_s2(TRUE)
  alpha_range <- st_make_valid(alpha_range)
  # all(st_is_valid(alpha_range))
  alpha_range <- st_transform(alpha_range, crs = 3031)

  # Rasterize AH
  a.hull <- rasterStackFromPolyList(alpha_range, resolution = 1000)

  # Mask AH
  a.hull <- crop(a.hull, icefree.crs, mask = TRUE)

  # Check if the directory for the species exists
  species_dir <- file.path(base_dir, speciesname)
  if (!dir.exists(species_dir)) {
    # Create a directory for the species within the base directory
    dir.create(species_dir, showWarnings = FALSE)
  }

  # Define the output directory
  output_dir <- paste0(species_dir, "/")

  # Save the AH raster
  writeRaster(a.hull, paste0(output_dir, "", speciesname, "_AH.tif"), overwrite = TRUE)
}

# ASPA overlap (AH)
ah.area <- global(a.hull, c("sum", "mean", "sd"), na.rm = TRUE)
aspa <- terra::rast("data/aspa_1km_raster_2024.tif")
aspa <- terra::resample(aspa, a.hull)
ov.ah <- terra::crop(a.hull, aspa, mask = TRUE)
ov.ah.pa <- global(which.lyr(ov.ah > 0), "sum", na.rm = TRUE)

# Saving Model Output
Output <- tibble::tibble(
  species = speciesname,
  total_area = ah.area,
  relative_held = ov.ah.pa,
  relative_gap_percent = (ov.ah.pa / ah.area[[1]]) * 100,
)

write.csv(Output, file = paste0(output_dir, speciesname, "_AH.csv"), row.names = FALSE, quote = TRUE)
