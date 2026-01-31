# Load the library packages
library(prioritizr)
library(terra)
library(sf)
library(dplyr)
library(gurobi)
library(ggplot2)
library(patchwork)
library(tidyverse)

# Reading the planning unit data
ant_pu <- st_read("data/Final_PU_10km.shp")

# format columns in planning unit data
ant_pu$locked_in <- as.logical(ant_pu$locked_in)
ant_pu$locked_out <- as.logical(ant_pu$locked_out)

# Reading Species Richness dataset
species <- terra::rast("data/species.tif")
print(species)

# Calculate total planning units
total_planning_units <- nrow(ant_pu)
print(total_planning_units)

# Define a range of different target values for sensitivity analysis
## These represent different conservation scenario levels
budget_proportions <- 0.17

# Calculate actual budget values
threshold <- round(budget_proportions * total_planning_units)

# Set fixed target level (30% as recommended)
fixed_target <- 0.30

################################################################################
# 30% Target for each species group
################################################################################
# Formulate problem
p1 <-
    problem(ant_pu, species, cost_column = "cost") %>%
    add_min_shortfall_objective(budget = threshold) %>%
    add_relative_targets(targets = fixed_target) %>%
    add_locked_in_constraints("locked_in") %>%
    add_binary_decisions() %>%
    add_gurobi_solver()

print(p1)

# solve problem
s1 <- solve(p1)
st_write(s1, "Outputs/s1_output_30target.shp", overwrite = TRUE)
################################################################################
# Log-linear targets
################################################################################
# Formulate problem
p2 <-
    problem(ant_pu, species, cost_column = "cost") %>%
    add_min_shortfall_objective(budget = threshold) %>%
    add_loglinear_targets(10, 1, 10^4, 0.3) %>%
    add_locked_in_constraints("locked_in") %>%
    add_binary_decisions() %>%
    add_gurobi_solver()

print(p2)

# solve problem
s2 <- solve(p2)
st_write(s2, "Outputs/s2_output_log_linear.shp", overwrite = TRUE)
################################################################################
# Save results for all scenarios
scenario_list <- list(
    S1 = list(problem = p1, solution = s1[, "solution_1"]),
    S2 = list(problem = p2, solution = s2[, "solution_1"])
)

results_summary <- list()

for (scenario_name in names(scenario_list)) {
    scenario <- scenario_list[[scenario_name]]
    p <- scenario$problem
    s <- scenario$solution

    n_summary <- eval_n_summary(p, s)
    cost_summary <- eval_cost_summary(p, s)
    boundary_summary <- eval_boundary_summary(p, s)
    target_summary <- eval_target_coverage_summary(p, s)

    results_summary[[scenario_name]] <- list(
        n_summary = n_summary,
        cost_summary = cost_summary,
        boundary_summary = boundary_summary,
        target_summary = target_summary
    )
}

# Save results summary to an csv file
for (scenario_name in names(results_summary)) {
    summary <- results_summary[[scenario_name]]
    write.csv(summary$n_summary, paste0("Outputs/", scenario_name, "_n_summary.csv"), row.names = FALSE)
    write.csv(summary$cost_summary, paste0("Outputs/", scenario_name, "_cost_summary.csv"), row.names = FALSE)
    write.csv(summary$boundary_summary, paste0("Outputs/", scenario_name, "_boundary_summary.csv"), row.names = FALSE)
    write.csv(summary$target_summary, paste0("Outputs/", scenario_name, "_target_summary.csv"), row.names = FALSE)
}

# Save feature representation summary for each scenario
for (scenario_name in names(scenario_list)) {
    scenario <- scenario_list[[scenario_name]]
    p <- scenario$problem
    s <- scenario$solution

    feature_rep_summary <- eval_feature_representation_summary(p, s)
    write.csv(feature_rep_summary, paste0("Outputs/", scenario_name, "_feature_representation_summary.csv"), row.names = FALSE)
}

################################################################################
