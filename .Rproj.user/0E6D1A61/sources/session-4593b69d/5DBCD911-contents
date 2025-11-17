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
## Scenario 2 - Species range scenario

# Load the data
print(planning_units.1.resize)
print(species)
print(aspa.r)

# Budget threshold 17% of the total cost of planning units
management.cost_2 <- round(0.17 * length(cells(planning_units.1.resize)))
print(management.cost_2)

#Basic problem formulation (log-linear targets + near research stations)
p2 <-
  problem(planning_units.1.resize, species) %>%
  add_min_shortfall_objective(budget = management.cost_2) %>%
  add_loglinear_targets(10, 1, 10^4, 0.3) %>%
  add_locked_in_constraints(aspa.r) %>%
  add_binary_decisions() %>%
  add_default_solver()

print(p2)

# solve problem
s2 <- solve(p2, force = TRUE)

print(s2)

# Convert the solution to a data frame
s2_df <- as.data.frame(s2, xy = TRUE) %>%
  na.omit()

# Evaluate the number of selected planning units
n <- eval_n_summary(p2, s2)
print(n)
n$n / length(cells(planning_units.1.resize)) * 100

# Evaluate the selected planning units cost
eval_cost_summary(p2, s2)

# Evaluate boundary length
eval_boundary_summary(p2, s2)

# Evaluate the representativeness of the solution
s2p2_target_spp_rep <- eval_feature_representation_summary(p2, s2)

# print the representativeness of the solution
print(s2p2_target_spp_rep, n = Inf)
write.csv(s2p2_target_spp_rep, "Output/Analysis/s2p2_target_spp_rep_min_shortfall_sensitivity_analysis.csv")
s2p2_target_spp_rep_fix <- read.csv("Output/Analysis/s2p2_target_spp_rep_min_shortfall_sensitivity_analysis.csv")

# Calculate the percentage of rows where 'absolute_held' is more than 0
# Filter the data to include only rows where 'absolute_held' is 0
absolute_held_zero_s2 <- s2p2_target_spp_rep %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s2 <- nrow(absolute_held_zero_s2)

# Calculate the total number of rows in the data frame
total_rows_s2 <- nrow(s2p2_target_spp_rep)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s2 <- (count_absolute_held_zero_s2 / total_rows_s2) * 100

# Print the percentage
print(percentage_absolute_held_zero_s2)

## Evaluate the ACBR representativeness of the solution
rast.p2 <- terra::rasterize(acbr, s2, field="ACBR_Name") ## convert acbr to raster format

x.s2 <- terra::zonal(s2, rast.p2, fun="sum", na.rm=TRUE) ## sum of raster values within the polygon
print(x.s2)

# Create a bar plot using ggplot2
sp2 <-  ggplot(x.s2, aes(x = reorder(ACBR_Name, cost), y = cost, fill = as.factor(ACBR_Name))) +
  geom_bar(stat = "identity") +
  labs(title = "(b) Species range - ACBR Comprehensiveness",
       x = "ACBR Name",
       y = "Sum",
       fill = "ACBR Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(sp2)

# calculate target coverage for the solution
s2p2_target_coverage <- eval_target_coverage_summary(p2, s2)
print(s2p2_target_coverage, n = Inf)
summary(s2p2_target_coverage$relative_target)

# comprehensive evaluation of the solution
# check percentage of the features that have their target met given the solution
print(mean(s2p2_target_coverage$met) * 100)

# representativeness target coverage
absolute_held_zero_s2 <- s2p2_target_coverage %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s2 <- nrow(absolute_held_zero_s2)

# Calculate the total number of rows in the data frame
total_rows_s2 <- nrow(s2p2_target_coverage)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s2 <- (count_absolute_held_zero_s2 / total_rows_s2) * 100

# Print the percentage
print(percentage_absolute_held_zero_s2)

# Save the plots
ggsave("Output/Figures/scenario1_acbr_comprehensiveness.png", plot = sp2, width = 8, height = 6)
################################################################################
