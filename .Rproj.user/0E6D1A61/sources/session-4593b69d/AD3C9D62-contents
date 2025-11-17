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
## Scenario 4 - Integrated scenario

# Load the data
print(planning_units.2.resize)
print(species)
print(aspa.r)

# Budget threshold 17% of the total cost of planning units
management.cost_4 <- round(0.17 * length(cells(planning_units.2.resize)))
print(management.cost_4)


#Basic problem formulation (log-linear targets + adaptive research stations)
p4 <-
  problem(planning_units.2.resize, species) %>%
  add_min_shortfall_objective(budget = management.cost_4) %>%
  add_loglinear_targets(10, 1, 10^4, 0.3) %>%
  add_locked_in_constraints(aspa.r) %>%
  add_binary_decisions() %>%
  add_default_solver()

print(p4)

# solve problem
s4 <- solve(p4, force = TRUE)

print(s4)

# Convert the solution to a data frame
s4_df <- as.data.frame(s4, xy = TRUE) %>%
  na.omit()

# Evaluate the number of selected planning units
n <- eval_n_summary(p4, s4)
n
n$n / length(cells(planning_units.2.inverts)) * 100

# Evaluate the selected planning units cost
eval_cost_summary(p4, s4)

# Evaluate boundary length
eval_boundary_summary(p4, s4)

# Evaluate the representativeness of the solution
s4p4_target_spp_rep <- eval_feature_representation_summary(p4, s4)

# print the representativeness of the solution
print(s4p4_target_spp_rep, n = Inf)
write.csv(s4p4_target_spp_rep, "Output/Analysis/s4p4_target_spp_rep_min_shortfall_sensitivity_analysis.csv")
s4p4_target_spp_rep_fix <- read.csv("Output/Analysis/s4p4_target_spp_rep_min_shortfall_sensitivity_analysis.csv")

# Calculate the percentage of rows where 'absolute_held' is more than 0
# Filter the data to include only rows where 'absolute_held' is 0
absolute_held_zero_s4 <- s4p4_target_spp_rep %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s4 <- nrow(absolute_held_zero_s4)

# Calculate the total number of rows in the data frame
total_rows_s4 <- nrow(s4p4_target_spp_rep)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s4 <- (count_absolute_held_zero_s4 / total_rows_s4) * 100

# Print the percentage
print(percentage_absolute_held_zero_s4)

## Evaluate the ACBR representativeness of the solution
rast.p4 <- terra::rasterize(acbr, s4, field="ACBR_Name") ## convert acbr to raster format

x.s4 <- terra::zonal(s4, rast.p4, fun="sum", na.rm=TRUE) ## sum of raster values within the polygon
print(x.s4)

# Create a bar plot using ggplot2
sp4 <- ggplot(x.s4, aes(x = reorder(ACBR_Name, cost), y = cost, fill = as.factor(ACBR_Name))) +
  geom_bar(stat = "identity") +
  labs(title = "(d) Integrated - ACBR Comprehensiveness",
       x = "ACBR Name",
       y = "Sum",
       fill = "ACBR Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(sp4)

# calculate target coverage for the solution
s4p4_target_coverage <- eval_target_coverage_summary(p4, s4)
print(s4p4_target_coverage, n = Inf)
summary(s4p4_target_coverage$relative_target)

# comprehensiveness target coverage
# check percentage of the features that have their target met given the solution
print(mean(s4p4_target_coverage$met) * 100)

# representativeness target coverage
absolute_held_zero_s4 <- s4p4_target_coverage %>% filter(absolute_held > 0)

# Count the number of such rows
count_absolute_held_zero_s4 <- nrow(absolute_held_zero_s4)

# Calculate the total number of rows in the data frame
total_rows_s4 <- nrow(s4p4_target_coverage)

# Calculate the percentage of rows where 'absolute_held' is 0
percentage_absolute_held_zero_s4 <- (count_absolute_held_zero_s4 / total_rows_s4) * 100

# Print the percentage
print(percentage_absolute_held_zero_s4)

# Save the plots
ggsave("Output/Figures/scenario1_acbr_comprehensiveness.png", plot = sp4, width = 8, height = 6)
################################################################################