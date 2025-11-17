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
## Scenario 1 - Basic scenario

# Load the data
print(planning_units.1.resize)
print(species)
print(aspa.r)

# Budget threshold 17% of the total cost of planning units
management.cost_1 <- round(0.17 * length(cells(planning_units.1.resize)))
print(management.cost_1)

# Basic problem formulation (equal targets + near research stations)
p1 <-
  problem(planning_units.1.resize, species) %>%
  add_min_shortfall_objective(budget = management.cost_1) %>%
  add_relative_targets(targets = 0.3) %>%
  add_locked_in_constraints(aspa.r) %>%
  add_binary_decisions() %>%
  add_default_solver()

print(p1)

# solve problem
s1 <- solve(p1, force = TRUE)

print(s1)

# Convert the solution to a data frame
s1_df <- as.data.frame(s1, xy = TRUE) %>%
  na.omit()

# Evaluate the number of selected planning units
n <- eval_n_summary(p1, s1)
print(n)
(n$n / length(cells(planning_units.1.resize))) * 100

# Evaluate the selected planning units cost
eval_cost_summary(p1, s1)

# Evaluate boundary length
eval_boundary_summary(p1, s1)

# Evaluate the targets of the solution
s1p1_target_spp_rep <- eval_feature_representation_summary(p1, s1)
s1p1_target_spp_rep_pa2 <- eval_feature_representation_summary(p1, PA2$cost)

print(s1p1_target_spp_rep_pa2, n = Inf)
write.csv(s1p1_target_spp_rep_pa2, "Output/Analysis/s0p0_target_spp_rep_baseline_min_shortfall_sensitivity_analysis.csv")

# print the final solution
print(s1p1_target_spp_rep, n = Inf)
write.csv(s1p1_target_spp_rep, "Output/Analysis/s1p1_target_spp_rep_min_shortfall.csv")
s1234p1234_target_spp_rep_final <- read.csv("Output/Analysis/s1234p1234_target_spp_rep_fix_final.csv")

# split the 'feature' column into separate columns based on the '_' separator
sp1_rep_final <- separate(s1p1_target_spp_rep_fix, feature, into = c("species_group", "acbr"), sep = "_")

# Remove rows where the 'acbr' column has the value "No ACBR"
sp1_rep_final <- sp1_rep_final %>% filter(acbr != "No ACBR")

# Calculate the percentage of rows where 'absolute_held' is more than 0
# Filter the data to include only rows where 'absolute_held' is 0
absolute_held_zero_s1 <- s1p1_target_spp_rep %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s1 <- nrow(absolute_held_zero_s1)

# Calculate the total number of rows in the data frame
total_rows_s1 <- nrow(s1p1_target_spp_rep)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s1 <- (count_absolute_held_zero_s1 / total_rows_s1) * 100

# Print the percentage
print(percentage_absolute_held_zero_s1)

## Evaluate the ACBR representativeness of the solution
rast.p1 <- terra::rasterize(acbr, s1, field = "ACBR_Name") ## convert acbr to raster format

x.s1 <- terra::zonal(s1, rast.p1, fun = "sum", na.rm = TRUE) ## sum of raster values within the polygon
print(x.s1)

# Create a bar plot using ggplot2
sp1 <- ggplot(x.s1, aes(x = reorder(ACBR_Name, cost), y = cost, fill = as.factor(ACBR_Name))) +
  geom_bar(stat = "identity") +
  labs(
    title = "(a) Basic - ACBR Comprehensiveness",
    x = "ACBR Name",
    y = "Sum",
    fill = "ACBR Name"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(sp1)

# calculate target coverage for the solution
s1p1_target_coverage <- eval_target_coverage_summary(p1, s1)
print(s1p1_target_coverage, n = Inf)
summary(s1p1_target_coverage$relative_target)

# comprehensive evaluation of the solution
# check percentage of the features that have their target met given the solution
print(mean(s1p1_target_coverage$met) * 100)

# representativeness target coverage
absolute_held_zero_s1 <- s1p1_target_coverage %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s1 <- nrow(absolute_held_zero_s1)

# Calculate the total number of rows in the data frame
total_rows_s1 <- nrow(s1p1_target_coverage)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s1 <- (count_absolute_held_zero_s1 / total_rows_s1) * 100

# Print the percentage
print(percentage_absolute_held_zero_s1)

# Save the plots
ggsave("Output/Figures/scenario1_acbr_comprehensiveness.png", plot = sp1, width = 8, height = 6)
################################################################################
