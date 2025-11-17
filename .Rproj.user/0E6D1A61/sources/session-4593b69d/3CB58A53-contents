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
## Budget Calibration for Sensitivity Analysis
## Testing different budget levels to evaluate their impact on solutions
################################################################################

# Define a range of different budget values for sensitivity analysis
## These represent different resource availability levels
## Based on scientific justification for Antarctic conservation

# Calculate total planning units
total_planning_units <- length(cells(planning_units.1.resize))

# Define budget proportions with scientific justification
budget_proportions <- c(
    0.05, # 5% - Minimal expansion (current ~1.5% + small increase)
    0.10, # 10% - Current scenario (incremental expansion)
    0.17, # 17% - CBD Aichi Target 11 (international commitment)
    0.30, # 30% - Post-2020 Framework (30x30 target - RECOMMENDED)
    0.40, # 40% - Enhanced protection
    0.50 # 50% - Ambitious protection (science-based maximum)
)

# Calculate actual budget values
prelim_budgets <- round(budget_proportions * total_planning_units)

# Create budget scenarios table with justifications
budget_scenarios <- data.frame(
    scenario = c(
        "Minimal",
        "Current",
        "CBD_Aichi",
        "Post2020_30x30",
        "Enhanced",
        "Ambitious"
    ),
    proportion = budget_proportions,
    budget_units = prelim_budgets,
    justification = c(
        "Minimal expansion from current 1.5% ASPA coverage",
        "Incremental expansion, current management capacity",
        "CBD Aichi Target 11 (17% terrestrial protection)",
        "Post-2020 Global Biodiversity Framework (30x30) - RECOMMENDED",
        "Enhanced protection for climate refugia",
        "Science-based maximum for ecosystem resilience (30-50%)"
    )
)

# Print budget scenarios
print(budget_scenarios)
cat("\n")
cat("Total planning units:", total_planning_units, "\n")
cat("Budget range:", min(prelim_budgets), "-", max(prelim_budgets), "units\n")
cat("\n")

# Save budget justification table
write.csv(budget_scenarios,
    "Output/Analysis/budget_scenarios_justification.csv",
    row.names = FALSE
)

# Generate boundary length data for the planning units
antarctic_bd <- boundary_matrix(planning_units.1.resize)

# Manually re-scale the boundary length values
antarctic_bd <- rescale_matrix(antarctic_bd)

################################################################################
## Generate solutions for each budget level
################################################################################

# Set fixed target level (10% as recommended)
fixed_target <- 0.10

cat("Running budget sensitivity analysis with fixed target:", fixed_target, "\n")
cat("Testing", length(prelim_budgets), "budget scenarios...\n\n")

# Create base problem with fixed target
p_base_budget <-
    problem(planning_units.1.resize, species) %>%
    add_min_shortfall_objective(budget = max(prelim_budgets)) %>% # Will be updated
    add_relative_targets(targets = fixed_target) %>%
    add_locked_in_constraints(aspa.r) %>%
    add_binary_decisions()

# Generate prioritizations based on different budget levels
budget_results <- lapply(seq_along(prelim_budgets), function(i) {
    cat(
        "Solving scenario", i, "of", length(prelim_budgets),
        "- Budget:", prelim_budgets[i], "units (",
        round(budget_proportions[i] * 100, 1), "%)\n"
    )

    ## Generate solution with specific budget level
    s <-
        problem(planning_units.1.resize, species) %>%
        add_min_shortfall_objective(budget = prelim_budgets[i]) %>%
        add_relative_targets(targets = fixed_target) %>%
        add_locked_in_constraints(aspa.r) %>%
        add_binary_decisions() %>%
        add_default_solver(gap = 0.1, time_limit = 10 * 60) %>%
        solve(force = TRUE)

    cat("  Solution found. Objective:", attr(s, "objective"), "\n\n")

    ## Return solution
    s
})

################################################################################
## Calculate performance metrics for each budget scenario
################################################################################

scenarios_performance_budgets <- data.frame(
    solution = character(),
    scenario = character(),
    budget_proportion = numeric(),
    budget_units = numeric(),
    objective = numeric(),
    n_selected = numeric(),
    cost = numeric(),
    boundary = numeric(),
    mean_representation = numeric(),
    targets_met = numeric(),
    species_represented = numeric(),
    shortfall = numeric()
)

# Calculate metrics for each prioritization
for (i in seq_along(budget_results)) {
    cat("Evaluating scenario:", budget_scenarios$scenario[i], "\n")

    # Create problem with current budget for evaluation
    p_eval <-
        problem(planning_units.1.resize, species) %>%
        add_min_shortfall_objective(budget = prelim_budgets[i]) %>%
        add_relative_targets(targets = fixed_target) %>%
        add_locked_in_constraints(aspa.r) %>%
        add_binary_decisions()

    # Calculate feature representation
    feature_rep <- eval_feature_representation_summary(p_eval, budget_results[[i]])

    # Calculate target coverage
    target_coverage <- eval_target_coverage_summary(p_eval, budget_results[[i]])

    # Count species with any representation
    species_represented <- sum(feature_rep$absolute_held > 0)

    # Calculate shortfall (objective value)
    shortfall_value <- attr(budget_results[[i]], "objective")

    cat(
        "  Budget:", prelim_budgets[i], "| Cost:",
        eval_cost_summary(p_eval, budget_results[[i]])$cost, "\n"
    )
    cat("  Mean representation:", round(mean(feature_rep$relative_held) * 100, 2), "%\n")
    cat("  Targets met:", round(mean(target_coverage$met) * 100, 2), "%\n\n")

    scenarios_performance_budgets <- rbind(
        scenarios_performance_budgets,
        data.frame(
            solution = "budget_calibration",
            scenario = budget_scenarios$scenario[i],
            budget_proportion = budget_proportions[i],
            budget_units = prelim_budgets[i],
            objective = shortfall_value,
            n_selected = eval_n_summary(p_eval, budget_results[[i]])$n,
            cost = eval_cost_summary(p_eval, budget_results[[i]])$cost,
            boundary = eval_boundary_summary(p_eval, budget_results[[i]])$boundary,
            mean_representation = mean(feature_rep$relative_held),
            targets_met = mean(target_coverage$met) * 100,
            species_represented = species_represented,
            shortfall = shortfall_value
        )
    )
}

# Print summary table
cat("\n=== BUDGET SENSITIVITY ANALYSIS RESULTS ===\n")
print(scenarios_performance_budgets)

# Save results
write.csv(scenarios_performance_budgets,
    "Output/Analysis/budget_calibration_metrics.csv",
    row.names = FALSE
)

################################################################################
## Visualization: Sensitivity analysis between budgets and performance metrics
################################################################################

# Plot 1: Budget vs Shortfall (Objective)
plot_budget_shortfall <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = shortfall,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "darkred") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    xlab("Budget Level (% of planning units)") +
    ylab("Total Shortfall (objective value)") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Shortfall")

print(plot_budget_shortfall)
ggsave("Output/Analysis/budget_vs_shortfall.png", plot_budget_shortfall,
    width = 10, height = 6, dpi = 300
)

# Plot 2: Budget vs Mean Representation
plot_budget_representation <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = mean_representation * 100,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "darkgreen") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    # geom_hline(
    #    yintercept = fixed_target * 100, linetype = "dashed",
    #    color = "red", linewidth = 0.8
    # ) +
    # annotate("text",
    #    x = max(budget_proportions * 100) * 0.5,
    #    y = fixed_target * 100 + 5,
    #   label = paste0("Target: ", fixed_target * 100, "%"),
    #    color = "red", fontface = "bold"
    # ) +
    xlab("Budget Level (% of planning units)") +
    ylab("Avg Species Representation (%)") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Species Representation")

print(plot_budget_representation)
ggsave("Output/Analysis/budget_vs_representation.png", plot_budget_representation,
    width = 10, height = 6, dpi = 300
)

# Plot 3: Budget vs Targets Met (%)
plot_budget_met <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = targets_met,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "red") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    xlab("Budget Level (% of planning units)") +
    ylab("Percentage of Targets Met (%)") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Targets Met (%)")

print(plot_budget_met)
ggsave("Output/Analysis/budget_vs_met.png", plot_budget_met,
    width = 10, height = 6, dpi = 300
)

# Plot 4: Budget vs Number of Planning Units Selected
plot_budget_units <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = n_selected,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "purple") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    xlab("Budget Level (% of planning units)") +
    ylab("Planning Units Selected") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Planning Units")

print(plot_budget_units)
ggsave("Output/Analysis/budget_vs_units.png", plot_budget_units,
    width = 10, height = 6, dpi = 300
)

# Plot 5: Budget vs Boundary Length
plot_budget_boundary <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = boundary,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "orange") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    xlab("Budget Level (% of planning units)") +
    ylab("Total Boundary Length") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Boundary Length")

print(plot_budget_boundary)
ggsave("Output/Analysis/budget_vs_boundary.png", plot_budget_boundary,
    width = 10, height = 6, dpi = 300
)

# Plot 6: Budget vs Cost Efficiency (representation per unit cost)
scenarios_performance_budgets <- scenarios_performance_budgets %>%
    mutate(
        cost_efficiency = (mean_representation * 100) / budget_proportion
    )

plot_budget_efficiency <- ggplot(
    data = scenarios_performance_budgets,
    aes(
        x = budget_proportion * 100, y = cost_efficiency,
        label = paste0(round(budget_proportion * 100, 0), "%")
    )
) +
    geom_line(color = "darkgray", linewidth = 1) +
    geom_point(size = 3, color = "steelblue") +
    geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
    xlab("Budget Level (% of planning units)") +
    ylab("Cost Efficiency (representation / budget)") +
    scale_x_continuous(
        breaks = budget_proportions * 100,
        expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 15) +
    theme(
        legend.title = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
    ) +
    ggtitle("Sensitivity Analysis: Budget Level vs Cost Efficiency")

print(plot_budget_efficiency)
ggsave("Output/Analysis/budget_vs_efficiency.png", plot_budget_efficiency,
    width = 10, height = 6, dpi = 300
)

################################################################################
## Multi-panel comparison plot
################################################################################

combined_plot_budget <- grid.arrange(
    plot_budget_shortfall + ggtitle("(a) Shortfall (objective)"),
    plot_budget_met + ggtitle("(b) Targets Met (%)"),
    plot_budget_representation + ggtitle("(c) Species Representation (%)"),
    plot_budget_boundary + ggtitle("(d) Boundary Length"),
    ncol = 2,
    nrow = 2,
    top = textGrob("Sensitivity Analysis of Budget Levels",
        gp = gpar(fontsize = 20, fontface = "bold")
    )
)

ggsave("Output/Analysis/budget_calibration_combined.png", combined_plot_budget,
    width = 14, height = 14, dpi = 300
)

################################################################################
