# Load the library packages
library(prioritizr)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(tibble)

################################################################################
## Target Calibration for Sensitivity Analysis
## Testing different target levels to evaluate their impact on solutions
################################################################################

# Define a range of different target values for sensitivity analysis
## These represent different conservation ambition levels
prelim_targets <- c(0.10, 0.30, 0.50, 0.70, 1.0)

# Print target values
print(prelim_targets)

# Set budget constraint (using same as your Scenario 1)
management.cost_1 <- round(0.1 * length(cells(planning_units.1.resize)))
print(management.cost_1)

# Generate boundary length data for the planning units
antarctic_bd <- boundary_matrix(planning_units.1.resize)

# Manually re-scale the boundary length values
antarctic_bd <- rescale_matrix(antarctic_bd)

################################################################################
## Generate solutions for each target level
################################################################################

# Create base problem without targets (to add them iteratively)
p_base <-
  problem(planning_units.1.resize, species) %>%
  add_min_shortfall_objective(budget = management.cost_1) %>%
  add_locked_in_constraints(aspa.r) %>%
  add_binary_decisions()

# Generate prioritizations based on different target levels
target_results <- lapply(prelim_targets, function(x) {
  ## Generate solution with specific target level
  s <-
    p_base %>%
    add_relative_targets(targets = x) %>%
    add_default_solver(gap = 0.1, time_limit = 10 * 60) %>%
    solve(force = TRUE)

  ## Return solution
  s
})

################################################################################
## Calculate performance metrics for each target scenario
################################################################################

scenarios_performance_targets <- data.frame(
  solution = character(),
  target = numeric(),
  objective = numeric(),
  n_selected = numeric(),
  cost = numeric(),
  boundary = numeric(),
  mean_representation = numeric(),
  targets_met = numeric(),
  species_represented = numeric()
)

# Calculate metrics for each prioritization
for (i in seq_along(target_results)) {
  # Create problem with current target for evaluation
  p_eval <-
    problem(planning_units.1.resize, species) %>%
    add_min_shortfall_objective(budget = management.cost_1) %>%
    add_relative_targets(targets = prelim_targets[i]) %>%
    add_locked_in_constraints(aspa.r) %>%
    add_binary_decisions()

  # Calculate feature representation
  feature_rep <- eval_feature_representation_summary(p_eval, target_results[[i]])

  # Calculate target coverage
  target_coverage <- eval_target_coverage_summary(p_eval, target_results[[i]])

  # Count species with any representation
  species_represented <- sum(feature_rep$absolute_held > 0)

  print(paste("Target:", prelim_targets[i]))
  print(eval_cost_summary(p_eval, target_results[[i]]))
  print(eval_boundary_summary(p_eval, target_results[[i]]))
  print(paste("Mean representation:", mean(feature_rep$relative_held)))
  print(paste("Targets met:", mean(target_coverage$met) * 100, "%"))

  scenarios_performance_targets <- rbind(
    scenarios_performance_targets,
    data.frame(
      solution = "target_calibration",
      target = prelim_targets[i],
      objective = attr(target_results[[i]], "objective"),
      n_selected = eval_n_summary(p_eval, target_results[[i]])$n,
      cost = eval_cost_summary(p_eval, target_results[[i]])$cost,
      boundary = eval_boundary_summary(p_eval, target_results[[i]])$boundary,
      mean_representation = mean(feature_rep$relative_held),
      targets_met = mean(target_coverage$met) * 100,
      species_represented = species_represented
    )
  )
}

# Print summary table
print(scenarios_performance_targets)

# Save results
write.csv(scenarios_performance_targets,
  "Output/Analysis/target_calibration_metrics.csv",
  row.names = FALSE
)

################################################################################
## Visualization: Sensitivity analysis between targets and performance metrics
################################################################################

# Plot 1: Target vs Cost
plot_target_cost <- ggplot(
  data = scenarios_performance_targets,
  aes(x = target, y = cost, label = round(target, 2))
) +
  geom_line(color = "darkgray", linewidth = 1) +
  geom_point(size = 3, color = "blue") +
  geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
  xlab("Target Level (proportion)") +
  ylab("Total Cost") +
  scale_x_continuous(breaks = prelim_targets, expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Sensitivity Analysis: Target Level vs Cost")

print(plot_target_cost)
ggsave("Output/Analysis/target_vs_cost.png", plot_target_cost,
  width = 10, height = 6, dpi = 300
)

# Plot 2: Target vs Mean Representation
plot_target_representation <- ggplot(
  data = scenarios_performance_targets,
  aes(x = target, y = mean_representation * 100, label = round(target, 2))
) +
  geom_line(color = "darkgray", linewidth = 1) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
  xlab("Target Level (proportion)") +
  ylab("Avg Species Representation (%)") +
  scale_x_continuous(breaks = prelim_targets, expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Sensitivity Analysis: Target Level vs Species Representation")

print(plot_target_representation)
ggsave("Output/Analysis/target_vs_representation.png", plot_target_representation,
  width = 10, height = 6, dpi = 300
)

# Plot 3: Target vs Targets Met (%)
plot_target_met <- ggplot(
  data = scenarios_performance_targets,
  aes(x = target, y = targets_met, label = round(target, 2))
) +
  geom_line(color = "darkgray", linewidth = 1) +
  geom_point(size = 3, color = "red") +
  geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
  xlab("Target Level (proportion)") +
  ylab("Percentage of Targets Met (%)") +
  scale_x_continuous(breaks = prelim_targets, expand = expansion(mult = c(0.05, 0.15))) +
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
  ggtitle("Sensitivity Analysis: Target Level vs Targets Met (%)")

print(plot_target_met)
ggsave("Output/Analysis/target_vs_met.png", plot_target_met,
  width = 10, height = 6, dpi = 300
)

# Plot 4: Target vs Number of Planning Units Selected
plot_target_units <- ggplot(
  data = scenarios_performance_targets,
  aes(x = target, y = n_selected, label = round(target, 2))
) +
  geom_line(color = "darkgray", linewidth = 1) +
  geom_point(size = 3, color = "purple") +
  geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
  xlab("Target Level (proportion)") +
  ylab("Planning Units Selected") +
  scale_x_continuous(breaks = prelim_targets, expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Sensitivity Analysis: Target Level vs Planning Units")

print(plot_target_units)
ggsave("Output/Analysis/target_vs_units.png", plot_target_units,
  width = 10, height = 6, dpi = 300
)

# Plot 5: Target vs Boundary Length
plot_target_boundary <- ggplot(
  data = scenarios_performance_targets,
  aes(x = target, y = boundary, label = round(target, 2))
) +
  geom_line(color = "darkgray", linewidth = 1) +
  geom_point(size = 3, color = "orange") +
  geom_text(hjust = -0.2, vjust = 0, size = 3.5, color = "black") +
  xlab("Target Level (proportion)") +
  ylab("Total Boundary Length") +
  scale_x_continuous(breaks = prelim_targets, expand = expansion(mult = c(0.05, 0.15))) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Sensitivity Analysis: Target Level vs Boundary Length")

print(plot_target_boundary)
ggsave("Output/Analysis/target_vs_boundary.png", plot_target_boundary,
  width = 10, height = 6, dpi = 300
)

################################################################################
## Multi-panel comparison plot
################################################################################

library(gridExtra)
library(grid)

combined_plot <- grid.arrange(
  plot_target_cost + ggtitle("(a) Cost (sum of distance)"),
  plot_target_met + ggtitle("(b) Targets Met (%)"),
  plot_target_units + ggtitle("(c) Planning Units"),
  plot_target_boundary + ggtitle("(d) Boundary Length (perimeter)"),
  ncol = 2,
  nrow = 2,
  top = textGrob("Sensitivity Analysis of Target Levels",
    gp = gpar(fontsize = 20, fontface = "bold")
  )
)

ggsave("Output/Analysis/target_calibration_combined.png", combined_plot,
  width = 14, height = 16, dpi = 300
)

################################################################################
