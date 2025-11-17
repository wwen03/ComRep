#################################################################################################################
# Compare the different solutions in terms of number of planning units selected, cost, boundaries, and target met
#################################################################################################################

problems <- c(p1, p3, p4)
names(problems) <- c("problem-1", "problem-3", "problem-4")

solutions <- c(s1, s3, s4)
names(solutions) <- c("scenario-1", "scenario-3", "scenario-4")


## analyse in the different solutions with species targets and research stations

scenarios_performance_models <- data.frame(solution = character(),
                                           no_selected = numeric(),
                                           total_cost = numeric(),  
                                           total_boundaries = numeric(),
                                           target_met = numeric())

## loop across solutions to extract all information
for (i in seq_along(problems)) for (j in 1:1:nlyr(solutions)){
  if (i == j) {
    cat(paste0(i, " ", j, " \n"))  # keep track
    psr.s_i <- eval_n_summary(problems[[i]], solutions[[j]])
    psr.s_i$solution <- names(solutions)[j]
    psr.s_i$no_selected <- (eval_n_summary(problems[[i]], solutions[[j]]))
    psr.s_i$total_cost <- (eval_cost_summary(problems[[i]], solutions[[j]]))
    psr.s_i$total_boundaries <- (eval_boundary_summary(problems[[i]], solutions[[j]]))
    psr.s_i$target_met <- (mean(eval_target_coverage_summary(problems[[i]], solutions[[j]])$met * 100))
    psr_i <- as.data.frame(psr.s_i)
    scenarios_performance_models <- rbind(scenarios_performance_models,
                                          psr_i[, c("solution", "no_selected", "total_cost", "total_boundaries", "target_met")]
    )
  } else
  {
    NULL
  }
}

a <- ggplot(scenarios_performance_models, aes(x=solution, y=no_selected$n)) + 
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(element_text(family = 'Helvetica'),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(
    title = "(a) Number of planning units selected",
    fill = "Solution",
    x = "Solution",
    y = ""
  )

b <- ggplot(scenarios_performance_models, aes(x=solution, y=total_cost$cost)) + 
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(element_text(family = 'Helvetica'),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(
    title = "(c) Total cost (sum of distance)",
    fill = "Solution",
    x = "Solution",
    y = ""
  )

c <- ggplot(scenarios_performance_models, aes(x=solution, y=total_boundaries$boundary)) + 
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(element_text(family = 'Helvetica'),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(
    title = "(d) Total boundary length (perimeter)",
    fill = "Solution",
    x = "Solution",
    y = ""
  )

d <- ggplot(scenarios_performance_models, aes(x=solution, y=target_met)) + 
  geom_bar(stat = "identity", fill = "grey") +
  theme_minimal() +
  theme(element_text(family = 'Helvetica'),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  labs(
    title = "(b) Sum of species group captured (%)",
    fill = "Solution",
    x = "Solution",
    y = ""
  )

# Figure 5a, 5b, 5c, 5d Bar charts of the number of planning units selected, total cost, total boundary length, and target met in the different solutions
combined <- a + d + b + c & theme(legend.position = "none")
combined + plot_layout(guides = "collect")

## analyse representation gains in the different solutions with a given budget
## for individual species
problems <- c(p1, p2, p3, p4)
names(problems) <- c("problem-1", "problem-2", "problem-3", "problem-4")

solutions <- c(s1, s2, s3, s4)
names(solutions) <- c("1 - Basic", "2 - Species range", "3 - Adaptive", "4 - Integrated")

scenarios_performance_species <- data.frame(solution = character(),
                                            feature = character(),
                                            relative_held = numeric(),  ## representation: percentage of distribution held in the solution
                                            relative_shortfall = numeric()) ## shortfall to target: how far from the area target for each species

## loop across solutions to extract all information
for (i in seq_along(problems)) for (j in 1:1:nlyr(solutions)){
  if (i == j) {
    cat(paste0(i, " ", j, " \n"))  # keep track
    rpz.s_i <- eval_target_coverage_summary(problems[[j]], solutions[[i]]) ## for each species. Note that here, we assess the target shortfall based on the targets defined in p1, i.e. log linear targets.
    rpz.s_i$solution <- names(solutions)[i]
    rpz_i <- as.data.frame(rpz.s_i)
    scenarios_performance_species <- rbind(scenarios_performance_species,
                                           rpz_i[, c("solution", "feature", "relative_held", "relative_shortfall")]
    )
  } else
  {
    NULL
  }
}


## Figure 7: Boxplot of representation gains in the different scenarios
## add jitter points to see individual species representations
ggplot(scenarios_performance_species, aes(x = solution, y = relative_held)) +
  geom_boxplot(alpha = 0.3)+
  geom_point(aes(x = solution, y = relative_held, fill = feature), show.legend = FALSE, position = position_jitterdodge())+
  theme_bw() +
  labs(
    title = "",
    fill = "Scenario",
    y = "The proportion of Selected Species Groups",
    x = "Scenario"
  )

# Boxplot statistic
bp <- boxplot(scenarios_performance_species$relative_held ~ scenarios_performance_species$solution, data = scenarios_performance_species)
bp <- boxplot(relative_held ~ solution,
              data = scenarios_performance_species,
              plot = FALSE)

# bp$names = scenario names
# bp$stats is a 5 x G matrix: rows = (lower whisker, Q1, median, Q3, upper whisker)
medians <- bp$stats[3, ]      # Q2 = medians
Q3s     <- bp$stats[4, ]      # 75th hinge (box top)
upper_whisker <- bp$stats[5,] # whisker (may be less than true max if outliers exist)

data.frame(
  solution = bp$names,
  median = medians,
  Q3 = Q3s,
  upper_whisker = upper_whisker
)

print(bp)

# Test the correlation between the scenarios
corr.test <- c(s1p1_target_spp_rep, s2p2_target_spp_rep, s3p3_target_spp_rep, s4p4_target_spp_rep)
corr.test <- as.data.frame(corr.test)

ggscatter(corr.test, x = "relative_held.1", y = "relative_held.3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Scenario-2", ylab = "Scenario-4")

pairs(~relative_held + relative_held.1 + relative_held.2 + relative_held.3, data = corr.test)

################################################################################
# calculate the patches size Figure 6
y1 <- patches(s1, 8, zeroAsNA = TRUE)
f1 <- freq(y1)
head(f1)
z1 <- cellSize(y1, unit = "km") |> zonal(y1, sum)
head(z1) |> round(2)

summary(z1)

# ggplot histogram  Basic ASPA size distribution
pz1 <- ggplot(z1, aes(x = area)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(
    title = "(a) Basic",
    x = "ASPA Size Distribution (km2)",
    y = "Frequency"
  ) +
  theme_minimal()

plot(pz1)

# Filter the data to exclude outliers (e.g., area > 50)
z1_filtered <- z1[z1$area <= 50, ] # Keep only rows where area <= 50

# ggplot histogram for adaptive ASPA size distribution (filtered)
pz1 <- ggplot(z1_filtered, aes(x = area)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") + # Adjust binwidth for larger range
  labs(
    title = "(a) Basic",
    x = "ASPA Size Distribution (km2)",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot the histogram
plot(pz1)

# calculate the patches size
y2 <- patches(s2, 8, zeroAsNA=TRUE)
f2 <- freq(y2)
head(f2)
z2 = cellSize(y2,unit="km") |> zonal(y2, sum)
head(z2) |> round(2)

summary(z2)

# ggplot histogram  Weighted ASPA size distribution
pz2 <- ggplot(z2, aes(x = area)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "(b) Weighted",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

plot(pz2)

# Filter the data to exclude outliers (e.g., area > 50)
z2_filtered <- z2[z2$area <= 50, ]  # Keep only rows where area <= 50

# ggplot histogram for adaptive ASPA size distribution (filtered)
pz2 <- ggplot(z2_filtered, aes(x = area)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +  # Adjust binwidth for larger range
  labs(title = "(b) Weighted",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

# Plot the histogram
plot(pz2)

# calculate the patches size
y3 <- patches(s3, 8, zeroAsNA=TRUE)
f3 <- freq(y3)
head(f3)
z3 = cellSize(y3,unit="km") |> zonal(y3, sum)
head(z3) |> round(2)

summary(z3)

# ggplot histogram adaptive ASPA size distribution
pz3 <- ggplot(z3, aes(x = area)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "(c) Adaptive",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

plot(pz3)

# Filter the data to exclude outliers (e.g., area > 50)
z3_filtered <- z3[z3$area <= 50, ]  # Keep only rows where area <= 50

# ggplot histogram for adaptive ASPA size distribution (filtered)
pz3 <- ggplot(z3_filtered, aes(x = area)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +  # Adjust binwidth for larger range
  labs(title = "(c) Adaptive",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

# Plot the histogram
plot(pz3)

# calculate the patches size
y4 <- patches(s4, 8, zeroAsNA=TRUE)
f4 <- freq(y4)
head(f4)
z4 = cellSize(y4,unit="km") |> zonal(y4, sum)
head(z4) |> round(2)

summary(z4)

# ggplot histogram adaptive ASPA size distribution
pz4 <- ggplot(z4, aes(x = area)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "(d) Integrated",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

plot(pz4)

# Filter the data to exclude outliers (e.g., area > 50)
z4_filtered <- z4[z4$area <= 50, ]  # Keep only rows where area <= 50

# ggplot histogram for adaptive ASPA size distribution (filtered)
pz4 <- ggplot(z4_filtered, aes(x = area)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +  # Adjust binwidth for larger range
  labs(title = "(d) Integrated",
       x = "ASPA Size Distribution (km2)",
       y = "Frequency") +
  theme_minimal()

# Plot the histogram
plot(pz4)

# Figure 6 of histogram ASPA size distribution
pz5 <- pz1 + pz2 + pz3 + pz4 & theme(legend.position = "none")
pz5 + plot_layout(guides = "collect")

################################################################################
# Figure 8 Proportional representation (%) of ACBRs under four scenarios

s01234p01234_target_spp_rep_final <- read.csv("Output/Analysis/s01234p01234_target_spp_rep_fix_final.csv")

# Combine 'acbr' and 'category' into a single column
data <- s01234p01234_target_spp_rep_final %>%
  unite("scenario_acbr_category", c(scenario, acbr, category), sep = " - ", remove = FALSE)

# Filter data for each scenario
data1 <- s01234p01234_target_spp_rep_final %>% filter(scenario == 1)
data2 <- s01234p01234_target_spp_rep_final %>% filter(scenario == 2)
data3 <- s01234p01234_target_spp_rep_final %>% filter(scenario == 3)
data4 <- s01234p01234_target_spp_rep_final %>% filter(scenario == 4)

# Reshape the data into long format
data_long1 <- data1 %>%
  pivot_longer(
    cols = c(existing_pa_relative, proposed_pa_relative),  # Columns to reshape
    names_to = "metric",  # New column to indicate scenario
    values_to = "relative_value"  # New column for the values
  )

# Reshape the data into long format
data_long2 <- data2 %>%
  pivot_longer(
    cols = c(existing_pa_relative, proposed_pa_relative),  # Columns to reshape
    names_to = "metric",  # New column to indicate scenario
    values_to = "relative_value"  # New column for the values
  )

# Reshape the data into long format
data_long3 <- data3 %>%
  pivot_longer(
    cols = c(existing_pa_relative, proposed_pa_relative),  # Columns to reshape
    names_to = "metric",  # New column to indicate scenario
    values_to = "relative_value"  # New column for the values
  )

# Reshape the data into long format
data_long4 <- data4 %>%
  pivot_longer(
    cols = c(existing_pa_relative, proposed_pa_relative),  # Columns to reshape
    names_to = "metric",  # New column to indicate scenario
    values_to = "relative_value"  # New column for the values
  )

plot1 <- ggplot(data_long1, aes(x = acbr, y = relative_value, fill = metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("existing_pa_relative" = "#0072B2", "proposed_pa_relative" = "grey"),
    labels = c("Existing", "Candidate")
  ) +
  labs(
    title = "(a) Basic",
    x = "ACBR",
    y = "Proportion (%)",
    fill = "Status"
  ) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +  # Set y-axis scale from 0 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  coord_flip()

plot1

plot2 <- ggplot(data_long2, aes(x = acbr, y = relative_value, fill = metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("existing_pa_relative" = "#0072B2", "proposed_pa_relative" = "grey"),
    labels = c("Existing", "Candidate")
  ) +
  labs(
    title = "(b) Weighted",
    x = "ACBR",
    y = "Proportion (%)",
    fill = "Status"
  ) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +  # Set y-axis scale from 0 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  coord_flip()

plot2

plot3 <- ggplot(data_long3, aes(x = acbr, y = relative_value, fill = metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("existing_pa_relative" = "#0072B2", "proposed_pa_relative" = "grey"),
    labels = c("Existing", "Candidate")
  ) +
  labs(
    title = "(c) Adaptive",
    x = "ACBR",
    y = "Proportion (%)",
    fill = "Status"
  ) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +  # Set y-axis scale from 0 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  coord_flip()

plot3

plot4 <- ggplot(data_long4, aes(x = acbr, y = relative_value, fill = metric)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = c("existing_pa_relative" = "#0072B2", "proposed_pa_relative" = "grey"),
    labels = c("Existing", "Candidate")
  ) +
  labs(
    title = "(d) Integrated",
    x = "ACBR",
    y = "Proportion (%)",
    fill = "Status"
  ) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +  # Set y-axis scale from 0 to 1
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  coord_flip()

plot4

# Combine all figure panels for figure 8
plot5 <- plot1 + plot2 + plot3 + plot4 & theme(legend.position = "bottom")
plot5 + plot_layout(guides = "collect", nrow = 4)

# Final Figure 4 Matrix illustrating protection of 69 species groups across 16 ACBRs
ggplot(s1234p1234_target_spp_rep_final, aes(
  x = acbr, y = species_group, 
  color = as.factor(ifelse(relative_held == 0, "Not represented", "Represented")),
  shape = as.factor(label)
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
  facet_wrap(~label, ncol = 4)

################################################################################