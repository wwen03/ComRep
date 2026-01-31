# Load the library packages
library(prioritizr)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)
library(grid)

################################################################################
## Target Calibration for Sensitivity Analysis
## Testing different target levels to evaluate their impact on solutions
################################################################################

ant_pu <- st_read("Data/Final_PU_10km.shp")
print(ant_pu)

# format columns in planning unit data
ant_pu$locked_in <- as.logical(ant_pu$locked_in)
ant_pu$locked_out <- as.logical(ant_pu$locked_out)

print(ant_pu)

species <- terra::rast("Data/species.tif")

print(species)

# Define a range of different target values for sensitivity analysis
## These represent different resource availability levels

# Calculate total planning units
total_planning_units <- nrow(ant_pu)
print(total_planning_units)

budget_proportions <- 0.17 # Fixed budget proportion for this analysis

# Calculate actual budget values
threshold <- round(budget_proportions * total_planning_units)
print(threshold)

# Define a range of different target values for sensitivity analysis
## These represent different conservation scenario levels
prelim_targets <- seq(0.10, 1, by = 0.1)

# Print target values
print(prelim_targets)

# Create base problem with fixed budget threshold
p0 <-
  problem(ant_pu, species, cost_column = "cost_equal") %>%
  add_min_shortfall_objective(budget = threshold) %>%
  add_locked_in_constraints("locked_in") %>%
  add_binary_decisions()

# generate prioritizations based on each target level
## note that the prioritizations are solved to within 10% of optimality
## (the default gap) because the gap is not specified
hierarchical_results_full <- lapply(prelim_targets, function(x) {
  ## Create problem with specific target
  p <-
    p0 %>%
    add_relative_targets(targets = x)

  ## Solve the problem
  s <- solve(p)

  ## Return both problem and solution (keep only solution column)
  list(
    problem = p,
    solution = s[, "solution_1", drop = FALSE], # Keep as single-column sf object
    target = x
  )
})

# Extract solutions for spatial object
hierarchical_results <- lapply(hierarchical_results_full, function(x) {
  s <- data.frame(s = x$solution$solution_1)
  names(s) <- paste0("prelim_targets_", x$target)
  s
})

# format results as a single spatial object
hierarchical_results <- cbind(ant_pu, do.call(bind_cols, hierarchical_results))
print(hierarchical_results)

################################################################################
# calculate metrics for prioritizations using stored problems
hierarchical_metrics <- lapply(seq_along(hierarchical_results_full), function(i) {
  p <- hierarchical_results_full[[i]]$problem
  s <- hierarchical_results_full[[i]]$solution # Single-column sf object

  data.frame(
    total_cost = eval_cost_summary(p, s)$cost,
    total_boundary_length = eval_boundary_summary(p, s)$boundary,
    total_met = mean(eval_target_coverage_summary(p, s)$met) * 100,
    total_relative = mean(eval_feature_representation_summary(p, s)$relative_held) * 100,
    total_selected = eval_n_summary(p, s)$n
  )
})

hierarchical_metrics <- do.call(bind_rows, hierarchical_metrics)
hierarchical_metrics$prelim_targets <- prelim_targets
hierarchical_metrics <- as_tibble(hierarchical_metrics)

# preview metrics
print(hierarchical_metrics)
write.csv(
  hierarchical_metrics,
  "Outputs/hierarchical_metrics_target_cost_equal.csv",
  row.names = FALSE
)

################################################################################
