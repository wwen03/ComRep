# Load the library packages
library(terra)
library(sf)

# Read research stations
research.stations <- st_read("Data/stations_comnap.shp")
acbr <- vect("data/ACBRs_v2_2016/ACBRs_v2_2016.shp")

# Define your study area extent and resolution
# Adjust these parameters based on your data
study_extent <- ext(acbr)
resolution <- 1000 # meters, adjust as needed

# Create a base raster for the study area
base_raster <- rast(study_extent, resolution = resolution)
crs(base_raster) <- crs(acbr)

# Convert stations to SpatVector for terra
stations_vect <- vect(research.stations)

# Rasterize stations - these will be the target cells
stations_raster <- rasterize(stations_vect, base_raster, field = 1)

# ============================================================================
# ACCESS COST CALCULATION METHODS
# ============================================================================

# 1. EUCLIDEAN DISTANCE (d_E)
# For a cell at position (x, y) to nearest station at (x_s, y_s):
# d_E(x,y) = min_{s ∈ S} √[(x - x_s)² + (y - y_s)²]
# where S is the set of all research station locations

dist_to_stations <- terra::distance(stations_raster)

# 2. FRICTION SURFACE (F)
# Friction represents the cost per unit distance of movement through a cell
# We define three friction models based on distance to nearest station:

max_dist <- global(dist_to_stations, "max", na.rm = TRUE)[1, 1]

# LINEAR FRICTION MODEL
# F(x,y) = F_min + (d_E(x,y) / d_max) × (F_max - F_min)
# where F_min = 1, F_max = 100, d_max = maximum distance in study area
# This creates uniform cost increase with distance
friction_surface <- 1 + (dist_to_stations / max_dist) * 99

# Set station cells to target value (0)
# This designates them as origins for access cost calculation
friction_surface[!is.na(stations_raster)] <- 0

# Plot: Friction surface
plot(friction_surface, main = "Friction (Cost) Surface\n(Low near stations)")
plot(stations_vect, add = TRUE, col = "red", pch = 16)
