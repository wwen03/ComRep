################################################################################
# Compare 30% target vs. log-linear target performance
################################################################################

# Create lists of problems and solutions for comparison
target_problems <- list(
  "30% Target" = p1,
  "Log-linear Target" = p2
)

target_solutions <- list(
  "30% Target" = s1,
  "Log-linear Target" = s2
)

# Initialize performance comparison dataframe
target_comparison <- data.frame(
  solution = character(),
  no_selected = numeric(),
  total_cost = numeric(),
  total_boundaries = numeric(),
  target_met = numeric(),
  stringsAsFactors = FALSE
)

# Evaluate performance metrics for each scenario
for (scenario_name in names(target_problems)) {
  cat(paste0("Evaluating: ", scenario_name, "\n"))

  # Extract problem and solution
  prob <- target_problems[[scenario_name]]
  sol <- target_solutions[[scenario_name]][, "solution_1"]

  # Validate solution
  if (is.null(sol) || all(is.na(sol))) {
    warning(paste("Solution for", scenario_name, "is invalid. Skipping."))
    next
  }

  # Calculate performance metrics
  n_selected <- eval_n_summary(prob, sol)$n
  total_cost <- eval_cost_summary(prob, sol)$cost
  total_boundaries <- eval_boundary_summary(prob, sol)$boundary
  target_met <- mean(eval_target_coverage_summary(prob, sol)$met * 100)

  # Add to comparison dataframe
  target_comparison <- rbind(
    target_comparison,
    data.frame(
      solution = scenario_name,
      no_selected = n_selected,
      total_cost = total_cost,
      total_boundaries = total_boundaries,
      target_met = target_met,
      stringsAsFactors = FALSE
    )
  )
}

# Visualization: Compare target scenarios
a1 <- ggplot(target_comparison, aes(x = solution, y = no_selected)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(a) Number of planning units selected",
    x = "Target Scenario",
    y = "Count"
  )

b1 <- ggplot(target_comparison, aes(x = solution, y = total_cost)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(b) Total cost",
    x = "Target Scenario",
    y = "Cost"
  )

c1 <- ggplot(target_comparison, aes(x = solution, y = total_boundaries)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(c) Total boundary length",
    x = "Target Scenario",
    y = "Perimeter"
  )

d1 <- ggplot(target_comparison, aes(x = solution, y = target_met)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(d) Target achievement",
    x = "Target Scenario",
    y = "% Targets Met"
  )

# Combine plots and save
combined_targets <- a1 + d1 + b1 + c1 & theme(legend.position = "none")
combined_targets + plot_layout(guides = "collect")
ggsave("Outputs/targets_comparison.png", combined_targets + plot_layout(guides = "collect"), width = 10, height = 8, dpi = 400)

################################################################################
# Compare access cost (S1) vs. equal cost (S3) performance
################################################################################

# Create lists of problems and solutions for cost comparison
cost_problems <- list(
  "Access Cost" = p1,
  "Equal Cost" = p3
)

cost_solutions <- list(
  "Access Cost" = s1,
  "Equal Cost" = s3
)

# Initialize performance comparison dataframe
cost_comparison <- data.frame(
  solution = character(),
  no_selected = numeric(),
  total_cost = numeric(),
  total_boundaries = numeric(),
  target_met = numeric(),
  stringsAsFactors = FALSE
)

# Evaluate performance metrics for each cost scenario
for (scenario_name in names(cost_problems)) {
  cat(paste0("Evaluating: ", scenario_name, "\n"))

  # Extract problem and solution
  prob <- cost_problems[[scenario_name]]
  sol <- cost_solutions[[scenario_name]][, "solution_1"]

  # Validate solution
  if (is.null(sol) || all(is.na(sol))) {
    warning(paste("Solution for", scenario_name, "is invalid. Skipping."))
    next
  }

  # Calculate performance metrics
  n_selected <- eval_n_summary(prob, sol)$n
  total_cost <- eval_cost_summary(prob, sol)$cost
  total_boundaries <- eval_boundary_summary(prob, sol)$boundary
  target_met <- mean(eval_target_coverage_summary(prob, sol)$met * 100)

  # Add to comparison dataframe
  cost_comparison <- rbind(
    cost_comparison,
    data.frame(
      solution = scenario_name,
      no_selected = n_selected,
      total_cost = total_cost,
      total_boundaries = total_boundaries,
      target_met = target_met,
      stringsAsFactors = FALSE
    )
  )
}

# Visualization: Compare cost scenarios
a1 <- ggplot(cost_comparison, aes(x = solution, y = no_selected)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(a) Number of planning units selected",
    x = "Cost Scenario",
    y = "Count"
  )

b1 <- ggplot(cost_comparison, aes(x = solution, y = total_cost)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(b) Total cost",
    x = "Cost Scenario",
    y = "Cost"
  )

c1 <- ggplot(cost_comparison, aes(x = solution, y = total_boundaries)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(c) Total boundary length",
    x = "Cost Scenario",
    y = "Perimeter"
  )

d1 <- ggplot(cost_comparison, aes(x = solution, y = target_met)) +
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),
    axis.line = element_line(color = "black"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  labs(
    title = "(d) Target achievement",
    x = "Cost Scenario",
    y = "% Targets Met"
  )

# Combine plots and save
combined_costs <- a1 + d1 + b1 + c1 & theme(legend.position = "none")
combined_costs + plot_layout(guides = "collect")
ggsave("Outputs/costs_comparison.png", combined_costs + plot_layout(guides = "collect"), width = 10, height = 8, dpi = 400)


################################################################################
# Species representation analysis across cost scenarios
################################################################################

# Analyze representation for individual species across cost scenarios
species_representation_costs <- data.frame(
  solution = character(),
  feature = character(),
  relative_held = numeric(),
  relative_shortfall = numeric(),
  stringsAsFactors = FALSE
)

for (scenario_name in names(cost_problems)) {
  cat(paste0("Processing species representation for: ", scenario_name, "\n"))

  prob <- cost_problems[[scenario_name]]
  sol <- cost_solutions[[scenario_name]][, "solution_1"]

  # Calculate target coverage for each species
  coverage <- eval_target_coverage_summary(prob, sol)
  coverage$solution <- scenario_name

  species_representation_costs <- rbind(
    species_representation_costs,
    coverage[, c("solution", "feature", "relative_held", "relative_shortfall")]
  )
}

# Boxplot of species representation across cost scenarios
ggplot(species_representation_costs, aes(x = solution, y = relative_held)) +
  geom_boxplot(alpha = 0.3) +
  geom_point(aes(x = solution, y = relative_held, fill = feature), show.legend = FALSE, position = position_jitter(width = 0.2)) +
  theme_bw() +
  labs(
    title = "Species Representation by Cost Scenario",
    fill = "Feature",
    y = "Proportion of Species Target Met",
    x = "Cost Scenario"
  )
ggsave("Outputs/species_representation_costs.png", width = 8, height = 6, dpi = 400)

################################################################################
# Species group category analysis
################################################################################
# Note: This section requires species category data generated from previous analyses
# Expected data structure:
# - scenario: numeric scenario identifier
# - category: broader species group category
# - relative_held: proportion of species target met
# This data is typically output from earlier conservation prioritization analyses

# Define data file path (adjust if your data has a different name)
species_data_file <- "Outputs/species_category_data.csv"

# Check if the data file exists
if (file.exists(species_data_file)) {
  # Read species category data from previous analysis
  species_category_data <- read.csv(species_data_file)

  # Filter for specific scenario (adjust scenario number based on your analysis)
  filtered_data <- species_category_data %>% filter(scenario == 3)

  # Calculate mean representation by category
  aggregated_data <- filtered_data %>%
    group_by(category) %>%
    summarise(mean_relative_held = mean(relative_held, na.rm = TRUE))

  # Visualization: Species representation by category
  ggplot(aggregated_data, aes(x = category, y = mean_relative_held)) +
    geom_bar(stat = "identity", fill = "#ccd0d3") +
    geom_hline(yintercept = 0.3, linetype = "dashed", color = "black", size = 1.5) +
    labs(
      x = "Species group category",
      y = "Average proportion represented"
    ) +
    scale_y_continuous(breaks = seq(0, 0.5, by = 0.1)) +
    theme_classic(base_family = "sans") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  ggsave("Outputs/species_group_category_representation.png", width = 8, height = 6, dpi = 400)

  cat("Species category analysis completed.\n")
} else {
  cat("Species category data file not found. Skipping this analysis.\n")
  cat("Generate this file from your conservation prioritization results first.\n")
}

################################################################################
# Matrix illustrating protection of 69 species groups across 16 ACBRs
################################################################################
# Note: This section requires detailed species representation data from previous analyses
ggplot(targets_specs, aes(
  x = acbr, y = species_group, # Reverted to 'species_group' on y-axis
  color = as.factor(ifelse(relative_held == 0, "Not represented", "Represented")),
  shape = as.factor(scenario)
)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Not represented" = "red", "Represented" = "black")) +
  scale_shape_manual(values = c(15, 15, 15, 15)) +
  labs(
    x = "ACBR",
    y = "69 Species Groups",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  guides(shape = "none") +
  facet_wrap(~scenario, ncol = 4)
ggsave("Outputs/species_group_acbr_matrix.png", width = 12, height = 8, dpi = 400)

################################################################################
# Correlation and statistical analysis across solutions
################################################################################
# Note: This section requires detailed species representation data from previous analyses
# Expected data structure:
# - solution: numeric solution identifier (1, 2, 3, etc.)
# - species_group: species group identifier
# - area: area or planning unit identifier
# - relative_held: proportion of species target met

# Define data file path
species_solution_file <- "Outputs/species_solution_representation.csv"

# Check if the data file exists
if (file.exists(species_solution_file)) {
  # Load the species representation data
  species_representation_data <- read.csv(species_solution_file)

  # Reshape to wide format: create columns for relative_held in each solution
  solution_wide <- species_representation_data %>%
    select(solution, species_group, area, relative_held) %>%
    pivot_wider(
      names_from = solution,
      values_from = relative_held,
      names_prefix = "Solution_"
    ) %>%
    na.omit()

  # Preview the reshaped data
  head(solution_wide)

  # Select only the Solution columns for correlation
  solution_data <- solution_wide[, grep("^Solution_", colnames(solution_wide))]

  # Compute pairwise Pearson correlations
  cor_matrix <- cor(solution_data, use = "complete.obs", method = "pearson")
  print(cor_matrix)

  # Compute p-values for each pair
  solution_pairs <- combn(colnames(solution_data), 2, simplify = FALSE)
  cor_p_values <- lapply(solution_pairs, function(pair) {
    test <- cor.test(solution_data[[pair[1]]], solution_data[[pair[2]]], method = "pearson")
    c(pair = paste(pair, collapse = " vs "), r = test$estimate, p = test$p.value)
  })
  cor_p_df <- as.data.frame(do.call(rbind, cor_p_values))
  print(cor_p_df)

  # Visualize with pairs plot
  pairs(solution_data, main = "Pairwise Correlations Across Solutions")

  # Save correlation results
  write.csv(cor_matrix, "Outputs/solution_correlations.csv")
  write.csv(cor_p_df, "Outputs/solution_correlation_p_values.csv")

  # Paired t-tests for each pair
  t_test_results <- lapply(solution_pairs, function(pair) {
    test <- t.test(solution_data[[pair[1]]], solution_data[[pair[2]]], paired = TRUE)
    data.frame(
      pair = paste(pair, collapse = " vs "),
      mean_diff = test$estimate,
      conf_low = test$conf.int[1],
      conf_high = test$conf.int[2],
      t_stat = test$statistic,
      df = test$parameter,
      p_value = test$p.value
    )
  })
  t_test_df <- do.call(rbind, t_test_results)
  print(t_test_df)

  # Save t-test results
  write.csv(t_test_df, "Outputs/solution_t_tests.csv")

  # Detailed comparison: Solution 1 vs Solution 2 (e.g., 30% target vs log-linear)
  if ("Solution_1" %in% colnames(solution_data) && "Solution_2" %in% colnames(solution_data)) {
    solution_1_2 <- solution_data[, c("Solution_1", "Solution_2")]

    # Correlation test
    cor_test_1_2 <- cor.test(solution_1_2$Solution_1, solution_1_2$Solution_2, method = "pearson")
    print("Correlation Test (Solution 1 vs Solution 2):")
    print(cor_test_1_2)

    # Paired t-test
    t_test_1_2 <- t.test(solution_1_2$Solution_1, solution_1_2$Solution_2, paired = TRUE)
    print("Paired T-Test (Solution 1 vs Solution 2):")
    print(t_test_1_2)

    # Save results
    results_df_1_2 <- data.frame(
      Test = c("Correlation", "T-Test"),
      Statistic = c(cor_test_1_2$estimate, t_test_1_2$statistic),
      P_Value = c(cor_test_1_2$p.value, t_test_1_2$p.value),
      CI_Lower = c(cor_test_1_2$conf.int[1], t_test_1_2$conf.int[1]),
      CI_Upper = c(cor_test_1_2$conf.int[2], t_test_1_2$conf.int[2])
    )
    write.csv(results_df_1_2, "Outputs/solution_1_vs_2_stats.csv", row.names = FALSE)

    # Visualization
    library(ggpubr)
    plot_1_2 <- ggscatter(solution_1_2,
      x = "Solution_1", y = "Solution_2",
      add = "reg.line", conf.int = TRUE,
      cor.coef = TRUE, cor.method = "pearson",
      xlab = "30% target representation",
      ylab = "Log-linear target representation",
      title = "30% Target vs Log-linear Target"
    )
    ggsave("Outputs/solution_1_vs_2_scatter.pdf", plot_1_2, width = 8, height = 6, dpi = 300)
  }

  # Detailed comparison: Solution 1 vs Solution 3 (e.g., access cost vs equal cost)
  if ("Solution_1" %in% colnames(solution_data) && "Solution_3" %in% colnames(solution_data)) {
    solution_1_3 <- solution_data[, c("Solution_1", "Solution_3")]

    # Correlation test
    cor_test_1_3 <- cor.test(solution_1_3$Solution_1, solution_1_3$Solution_3, method = "pearson")
    print("Correlation Test (Solution 1 vs Solution 3):")
    print(cor_test_1_3)

    # Paired t-test
    t_test_1_3 <- t.test(solution_1_3$Solution_1, solution_1_3$Solution_3, paired = TRUE)
    print("Paired T-Test (Solution 1 vs Solution 3):")
    print(t_test_1_3)

    # Save results
    results_df_1_3 <- data.frame(
      Test = c("Correlation", "T-Test"),
      Statistic = c(cor_test_1_3$estimate, t_test_1_3$statistic),
      P_Value = c(cor_test_1_3$p.value, t_test_1_3$p.value),
      CI_Lower = c(cor_test_1_3$conf.int[1], t_test_1_3$conf.int[1]),
      CI_Upper = c(cor_test_1_3$conf.int[2], t_test_1_3$conf.int[2])
    )
    write.csv(results_df_1_3, "Outputs/solution_1_vs_3_stats.csv", row.names = FALSE)

    # Visualization
    plot_1_3 <- ggscatter(solution_1_3,
      x = "Solution_1", y = "Solution_3",
      add = "reg.line", conf.int = TRUE,
      cor.coef = TRUE, cor.method = "pearson",
      xlab = "Access cost representation",
      ylab = "Equal cost representation",
      title = "Access Cost vs Equal Cost"
    )
    ggsave("Outputs/solution_1_vs_3_scatter.pdf", plot_1_3, width = 8, height = 6, dpi = 300)
  }

  # Combined visualization
  if (exists("plot_1_2") && exists("plot_1_3")) {
    library(patchwork)
    combined_plot <- plot_1_2 + plot_1_3 +
      plot_annotation(tag_levels = "A")
    print(combined_plot)
    ggsave("Outputs/solution_comparisons_combined.pdf", combined_plot, width = 12, height = 6, dpi = 300)
  }

  cat("Statistical analysis completed.\n")
} else {
  cat("Species solution representation data file not found. Skipping statistical analysis.\n")
  cat("Expected file: ", species_solution_file, "\n")
}

################################################################################
