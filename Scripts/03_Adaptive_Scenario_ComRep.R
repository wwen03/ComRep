# Load the library packages
library(prioritizr)
library(prioritizrdata)
library(terra)
library(sf)
library(ggplot2)
library(scales)
library(sp)
library(raster)
library(tibble)
library(topsis)
library(withr)
library(dplyr)
library(tidyverse)
library(viridisLite)
library(gridExtra)
library(units)
library(slam)
library(gurobi)
library(broom)
library(AICcmodavg)
library(ggpubr)

################################################################################
## Scenario 3 - Adaptive scenario

# Load the data
print(planning_units.1.resize)
print(species)
print(aspa.r)

# Budget threshold 17% of the total cost of planning units
management.cost_3 <- round(0.17 * length(cells(planning_units.2.resize)))
print(management.cost_3)

#Basic problem formulation (equal targets + adaptive research stations)
p3 <-
  problem(planning_units.2.resize, species) %>%
  add_min_shortfall_objective(budget = management.cost_3) %>%
  add_relative_targets(targets = 0.3) %>%
  add_locked_in_constraints(aspa.r) %>%
  add_binary_decisions() %>%
  add_default_solver()

print(p3)

# solve problem
s3 <- solve(p3, force = TRUE)

print(s3)

# Convert the solution to a data frame
s3_df <- as.data.frame(s3, xy = TRUE) %>%
  na.omit()

# Evaluate the number of selected planning units
n <- eval_n_summary(p3, s3)
n
n$n / length(cells(planning_units.2.resize)) * 100

# Evaluate the selected planning units cost
eval_cost_summary(p3, s3)

# Evaluate boundary length
eval_boundary_summary(p3, s3)

# Evaluate the representativeness of the solution
s3p3_target_spp_rep <- eval_feature_representation_summary(p3, s3)

# print the representativeness of the solution
print(s3p3_target_spp_rep, n = Inf)
write.csv(s3p3_target_spp_rep, "Output/Analysis/s3p3_target_spp_rep_min_shortfall_sensitivity_analysis.csv")
s3p3_target_spp_rep_fix <- read.csv("Output/Analysis/s3p3_target_spp_rep_min_shortfall_sensitivity_analysis.csv")

# Calculate the percentage of rows where 'absolute_held' is more than 0
# Filter the data to include only rows where 'absolute_held' is 0
absolute_held_zero_s3 <- s3p3_target_spp_rep %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s3 <- nrow(absolute_held_zero_s3)

# Calculate the total number of rows in the data frame
total_rows_s3 <- nrow(s3p3_target_spp_rep)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s3 <- (count_absolute_held_zero_s3 / total_rows_s3) * 100

# Print the percentage
print(percentage_absolute_held_zero_s3)

## Evaluate the ACBR representativeness of the solution
rast.p3 <- terra::rasterize(acbr, s3, field="ACBR_Name") ## convert acbr to raster format

x.s3 <- terra::zonal(s3.p, rast.p3, fun="sum", na.rm=TRUE) ## sum of raster values within the polygon
print(x.s3)

r.p <- terra::zonal(aspa.r, rast.p3, fun="sum", na.rm=TRUE) ## sum of raster values within the polygon
print(r.p)

# Create a bar plot using ggplot2
sp3 <- ggplot(x.s3, aes(x = reorder(ACBR_Name, cost), y = cost, fill = as.factor(ACBR_Name))) +
  geom_bar(stat = "identity") +
  labs(title = "(c) Adaptive - ACBR Comprehensiveness",
       x = "ACBR Name",
       y = "Sum",
       fill = "ACBR Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(sp3)

# calculate target coverage for the solution
s3p3_target_coverage <- eval_target_coverage_summary(p3, s3)
print(s3p3_target_coverage, n = Inf)
summary(s3p3_target_coverage$relative_target)

# comprehensive target coverage
# check percentage of the features that have their target met given the solution
print(mean(s3p3_target_coverage$met) * 100) 

# representativeness target coverage
absolute_held_zero_s3 <- s3p3_target_coverage %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s3 <- nrow(absolute_held_zero_s3)

# Calculate the total number of rows in the data frame
total_rows_s3 <- nrow(s3p3_target_coverage)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s3 <- (count_absolute_held_zero_s3 / total_rows_s3) * 100

# Print the percentage
print(percentage_absolute_held_zero_s3)

# Save the plots
ggsave("Output/Figures/scenario1_acbr_comprehensiveness.png", plot = sp3, width = 8, height = 6)
################################################################################