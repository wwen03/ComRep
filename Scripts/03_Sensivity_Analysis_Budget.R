# Load the library packages
library(prioritizr)
library(terra)
library(sf)
library(dplyr)
library(gurobi)

################################################################################
## Budget Calibration for Sensitivity Analysis
## Testing different budget levels to evaluate their impact on solutions
################################################################################

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

# Define budget proportions with scientific justification
budget_proportions <- c(
    0.05,
    0.10,
    0.15,
    0.20,
    0.25,
    0.30,
    0.35,
    0.40,
    0.45,
    0.50
)

# Define a range of different target values for sensitivity analysis
## These represent different conservation scenario levels
budget_proportions <- seq(0.05, 0.50, by = 0.05)

# Calculate actual budget values
threshold <- round(budget_proportions * total_planning_units)

budget_scenarios <- data.frame(
    scenario = c(
        "5%",
        "10%",
        "15%",
        "20%",
        "25%",
        "30%",
        "35%",
        "40%",
        "45%",
        "50%"
    ),
    proportion = budget_proportions,
    budget_units = threshold,
    justification = c(
        "5%",
        "10%",
        "15%",
        "20%",
        "25%",
        "30%",
        "35%",
        "40%",
        "45%",
        "50%"
    )
)

# Set fixed target level (30% as recommended)
fixed_target <- 0.30

# Create base problem with fixed target
p0 <-
    problem(ant_pu, species, cost_column = "cost_equal") %>%
    add_relative_targets(targets = fixed_target) %>%
    add_locked_in_constraints("locked_in") %>%
    add_binary_decisions() %>%
    add_gurobi_solver()

print(p0)

# generate prioritizations based on each threshold
## note that the prioritizations are solved to within 10% of optimality
## (the default gap) because the gap is not specified
hierarchical_results_full <- lapply(threshold, function(x) {
    ## Create problem with specific target
    p <-
        p0 %>%
        add_min_shortfall_objective(budget = x)

    ## Solve the problem
    s <- solve(p)

    ## Return both problem and solution (keep only solution column)
    list(
        problem = p,
        solution = s[, "solution_1", drop = FALSE], # Keep as single-column sf object
        budget = x
    )
})

# Extract solutions for spatial object
hierarchical_results <- lapply(hierarchical_results_full, function(x) {
    s <- data.frame(s = x$solution$solution_1)
    names(s) <- paste0("threshold_", x$budget)
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
        total_selected = eval_n_summary(p, s)$n
    )
})

hierarchical_metrics <- do.call(bind_rows, hierarchical_metrics)
hierarchical_metrics$threshold <- threshold
hierarchical_metrics <- as_tibble(hierarchical_metrics)

# preview metrics
print(hierarchical_metrics)
write.csv(
    hierarchical_metrics,
    "Outputs/hierarchical_metrics_budget_equal_cost.csv",
    row.names = FALSE
)
