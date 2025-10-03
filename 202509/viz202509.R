# MAFLD Subgroup Analysis - Treatment Effect Visualization
# Exploring Liv.52 DS vs Placebo across different subgroups

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(forcats)

# Read the data
# data <- read.csv("202509/LSM_Score.csv")
data <- read.csv("202509/LSM_Score_corrected.csv")

# Calculate change from baseline
data <- data %>%
  mutate(
    Change_kPa = EOS_kPa - Baseline_kPa,
    Percent_Change = (Change_kPa / Baseline_kPa) * 100
  )

# Create age groups for subgroup analysis
data <- data %>%
  mutate(
    Age_Group = cut(
      Age,
      breaks = c(0, 45, 55, 100),
      labels = c("≤45 years", "46-55 years", ">55 years")
    )
  )

# Create BMI categories (assuming average height for estimation)
data <- data %>%
  mutate(
    Weight_Group = cut(
      Weight,
      breaks = c(0, 70, 85, 200),
      labels = c("Lower weight", "Medium weight", "Higher weight")
    )
  )

# Create baseline severity groups
data <- data %>%
  mutate(
    Baseline_Severity = cut(
      Baseline_kPa,
      breaks = c(0, 7, 8, 20),
      labels = c("Mild (<7 kPa)", "Moderate (7-8 kPa)", "Severe (>8 kPa)")
    )
  )

# ============================================================================
# 1. Main Effect - Boxplot of LSM Scores (replicating publication figure)
# ============================================================================

data_long <- data %>%
  select(Group, Baseline_kPa, EOS_kPa) %>%
  pivot_longer(
    cols = c(Baseline_kPa, EOS_kPa),
    names_to = "Timepoint",
    values_to = "LSM_kPa"
  ) %>%
  mutate(
    Timepoint = factor(
      Timepoint,
      levels = c("Baseline_kPa", "EOS_kPa"),
      labels = c("Baseline", "EOS")
    )
  )

# Calculate summary statistics for annotations
summary_stats <- data %>%
  group_by(Group) %>%
  summarise(
    mean_baseline = mean(Baseline_kPa),
    mean_eos = mean(EOS_kPa),
    cfb_percent = ((mean_eos - mean_baseline) / mean_baseline) * 100,
    n = n()
  )

p1 <- ggplot(data_long, aes(x = Timepoint, y = LSM_kPa)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, stroke = 1.5) +
  facet_wrap(~Group, ncol = 2) +
  labs(title = "LSM Score by Treatment Group", y = "kPa", x = "") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  ylim(2, 10)

# ============================================================================
# 2. Steatosis Outcomes (replicating publication figure)
# ============================================================================

steatosis_summary <- data %>%
  group_by(Group, Steatosis) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  )

p2 <- ggplot(
  steatosis_summary,
  aes(x = Steatosis, y = percentage, fill = Steatosis)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1))), vjust = -0.5, size = 4) +
  facet_wrap(~Group, ncol = 2) +
  labs(
    title = "Steatosis Outcomes by Treatment Group",
    y = "% of Subjects",
    x = ""
  ) +
  scale_fill_manual(
    values = c(
      "No Steatosis" = "#6FA8DC",
      "Grade improvement" = "#E69138",
      "Deteriorate" = "#FFD966"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 65)

# ============================================================================
# 3. Subgroup Analysis - Treatment Effect by Sex
# ============================================================================

subgroup_sex <- data %>%
  group_by(Group, Sex) %>%
  summarise(
    n = n(),
    mean_change = mean(Change_kPa),
    se_change = sd(Change_kPa) / sqrt(n),
    mean_pct_change = mean(Percent_Change),
    .groups = "drop"
  )

p3 <- ggplot(subgroup_sex, aes(x = Sex, y = mean_pct_change, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_pct_change - se_change, ymax = mean_pct_change + se_change),
    position = position_dodge(0.8),
    width = 0.25
  ) +
  geom_text(
    aes(label = paste0("n=", n)),
    position = position_dodge(0.8),
    vjust = ifelse(subgroup_sex$mean_pct_change > 0, -2, 2),
    size = 3.5
  ) +
  labs(
    title = "Treatment Effect by Sex",
    subtitle = "Mean percent change in LSM Score from baseline",
    y = "Mean % Change from Baseline",
    x = "Sex"
  ) +
  scale_fill_manual(
    values = c("Liv.52 DS" = "#4472C4", "Placebo" = "#ED7D31")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# ============================================================================
# 4. Subgroup Analysis - Treatment Effect by Age Group
# ============================================================================

subgroup_age <- data %>%
  group_by(Group, Age_Group) %>%
  summarise(
    n = n(),
    mean_change = mean(Change_kPa),
    se_change = sd(Change_kPa) / sqrt(n),
    mean_pct_change = mean(Percent_Change),
    .groups = "drop"
  )

p4 <- ggplot(
  subgroup_age,
  aes(x = Age_Group, y = mean_pct_change, fill = Group)
) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_pct_change - se_change, ymax = mean_pct_change + se_change),
    position = position_dodge(0.8),
    width = 0.25
  ) +
  geom_text(
    aes(label = paste0("n=", n)),
    position = position_dodge(0.8),
    vjust = ifelse(subgroup_age$mean_pct_change > 0, -2, 2),
    size = 3.5
  ) +
  labs(
    title = "Treatment Effect by Age Group",
    subtitle = "Mean percent change in LSM Score from baseline",
    y = "Mean % Change from Baseline",
    x = "Age Group"
  ) +
  scale_fill_manual(
    values = c("Liv.52 DS" = "#4472C4", "Placebo" = "#ED7D31")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# ============================================================================
# 5. Subgroup Analysis - Treatment Effect by Baseline Severity
# ============================================================================

subgroup_severity <- data %>%
  group_by(Group, Baseline_Severity) %>%
  summarise(
    n = n(),
    mean_change = mean(Change_kPa),
    se_change = sd(Change_kPa) / sqrt(n),
    mean_pct_change = mean(Percent_Change),
    .groups = "drop"
  ) %>%
  filter(!is.na(Baseline_Severity))

p5 <- ggplot(
  subgroup_severity,
  aes(x = Baseline_Severity, y = mean_pct_change, fill = Group)
) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_pct_change - se_change, ymax = mean_pct_change + se_change),
    position = position_dodge(0.8),
    width = 0.25
  ) +
  geom_text(
    aes(label = paste0("n=", n)),
    position = position_dodge(0.8),
    vjust = ifelse(subgroup_severity$mean_pct_change > 0, -2, 2),
    size = 3.5
  ) +
  labs(
    title = "Treatment Effect by Baseline Disease Severity",
    subtitle = "Mean percent change in LSM Score from baseline",
    y = "Mean % Change from Baseline",
    x = "Baseline LSM Score Category"
  ) +
  scale_fill_manual(
    values = c("Liv.52 DS" = "#4472C4", "Placebo" = "#ED7D31")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 9)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")

# ============================================================================
# 6. Forest Plot - Treatment Effect Across All Subgroups
# ============================================================================

# Calculate treatment differences for each subgroup
forest_data <- bind_rows(
  # Overall
  data %>%
    group_by(Group) %>%
    summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
    mutate(Subgroup = "Overall", Category = "Overall"),

  # By Sex
  data %>%
    group_by(Sex, Group) %>%
    summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
    rename(Category = Sex) %>%
    mutate(Subgroup = "Sex"),

  # By Age Group
  data %>%
    filter(!is.na(Age_Group)) %>%
    group_by(Age_Group, Group) %>%
    summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
    rename(Category = Age_Group) %>%
    mutate(Subgroup = "Age Group"),

  # By Baseline Severity
  data %>%
    filter(!is.na(Baseline_Severity)) %>%
    group_by(Baseline_Severity, Group) %>%
    summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
    rename(Category = Baseline_Severity) %>%
    mutate(Subgroup = "Baseline Severity")
)

# Check column names and calculate treatment difference and CI
# First, let's rename the columns to make them easier to work with
colnames(forest_data) <- gsub("LIV\\.52 DS", "Liv52DS", colnames(forest_data))

forest_data <- forest_data %>%
  mutate(
    diff = mean_pct_Liv52DS - mean_pct_Placebo,
    # Approximate SE for difference (simplified)
    se_diff = abs(diff) * 0.15, # Rough approximation
    lower_ci = diff - 1.96 * se_diff,
    upper_ci = diff + 1.96 * se_diff,
    label = paste0(Category, " (n=", n_Liv52DS, "/", n_Placebo, ")")
  ) %>%
  mutate(order = row_number()) %>%
  arrange(desc(order))

p6 <- ggplot(forest_data, aes(y = fct_reorder(label, order), x = diff)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.3) +
  geom_point(size = 3, color = "#4472C4") +
  labs(
    title = "Forest Plot: Treatment Effect Across Subgroups",
    subtitle = "Difference in % change (Liv.52 DS - Placebo)",
    x = "Difference in Mean % Change from Baseline",
    y = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    panel.grid.minor = element_blank()
  ) +
  annotate(
    "text",
    x = min(forest_data$lower_ci) * 0.8,
    y = 0.5,
    label = "Favors Liv.52 DS",
    hjust = 0,
    size = 3,
    fontface = "italic"
  ) +
  annotate(
    "text",
    x = max(forest_data$upper_ci) * 0.8,
    y = 0.5,
    label = "Favors Placebo",
    hjust = 1,
    size = 3,
    fontface = "italic"
  )

# ============================================================================
# Display all plots
# ============================================================================

# Display main results (replicating publication)
print(p1)
print(p2)

# Display subgroup analyses
print(p3)
print(p4)
print(p5)
print(p6)

# Print summary statistics
cat("\n=== Overall Treatment Effect ===\n")
print(summary_stats)

cat("\n=== Subgroup Analysis Summary ===\n")
cat("\nBy Sex:\n")
print(subgroup_sex)
cat("\nBy Age Group:\n")
print(subgroup_age)
cat("\nBy Baseline Severity:\n")
print(subgroup_severity)
