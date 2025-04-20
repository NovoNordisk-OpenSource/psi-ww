# Install and load necessary libraries
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(scales)){install.packages("scales")}
if(!require(ggtext)){install.packages("ggtext")}

library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(ggtext)

# Read the CSV file into a DataFrame
df <- read.csv("202505/WW_GCdose.csv") |>
  filter(grepl("Baseline|Week", AVISIT)) |>
  mutate(
    AVISITW = AVISIT,
    AVISITWN = AVISITN,
    AVISIT = ifelse(ASTDY == 0, AVISIT, paste0("Week ", AVISITN, "(Day ",ASTDY, ")")),
    AVISITN = ASTDY
  ) |>
 mutate(AVISIT = fct_reorder(factor(AVISIT), AVISITN))


# Patient Profiles --------------------------------------------------------

# PP1 - Daily glucocorticoid dose (mg/kg/day)

# Create a vector of unique subjects (panels)
subjects <- unique(df$SUBJID)

panel_data <- expand.grid(panel = subjects, dummy = 1) |>
    inner_join(df |> mutate(dummy = 1), by = "dummy") |>
    select(-dummy) |>
    mutate(highlight = ifelse(SUBJID == panel, "current", "other"))

# Create the plot:
(
  pp1 <- ggplot() +
    # Draw the other subjects first in gray
    geom_line(
        data = panel_data |> filter(highlight == "other"),
        aes(x = ASTDY, y = AVAL1, group = SUBJID),
        color = "gray", alpha = 1
    ) +
    # Draw the current subject on top in blue
    geom_line(
        data = panel_data |> filter(highlight == "current"),
        aes(x = ASTDY, y = AVAL1, group = SUBJID),
        color = "blue", size = 1
    ) +
    facet_wrap(~panel, ncol = 3) +
    labs(
        title = "Line Plot per Subject",
        x = "Study Day", y = "Daily glucocorticoid dose (mg/kg/day)"
    ) +
    theme_minimal()
)

## PP2 - Weekly average glucocorticoid dose

# Create a vector of unique subjects (panels)

panel_data <- expand.grid(panel = subjects, dummy = 1) |>
    inner_join(df |> mutate(dummy = 1), by = "dummy") |>
    select(-dummy) |>
    mutate(highlight = ifelse(SUBJID == panel, "current", "other"))

# Create the plot:
(
  pp2 <- 
  ggplot() +
    # Draw the other subjects first in gray
    geom_line(
        data = panel_data |> filter(highlight == "other"),
        aes(x = AVISITN, y = AVAL2, group = SUBJID),
        color = "gray", alpha = 1
    ) +
    # Draw the current subject on top in blue
    geom_line(
        data = panel_data |> filter(highlight == "current"),
        aes(x = AVISITN, y = AVAL2, group = SUBJID),
        color = "blue", size = 1
    ) +
    facet_wrap(~panel, ncol = 3) +
    labs(
        title = "Line Plot per Subject",
        x = "Study Day", y = "Weekly average glucocorticoid dose"
    ) +
    theme_minimal()
  )


# Summary Charts ----------------------------------------------------------


# --- Analysis 1: Mean GC Dose Over 8 Weeks ---
mmean_data <- df |>
  group_by(AVISITN, AVISIT) |>
  summarise(mean_dose = mean(AVAL1))

# Define colors explicitly with factor levels to force all categories into the legend
mmean_data <- mmean_data %>%
  mutate(threshold_color = case_when(
    mean_dose < 0.2 ~ "<0.2",
    mean_dose >= 0.2 & mean_dose < 0.5 ~ "0.2–0.5",
    mean_dose >= 0.5 & mean_dose < 1.0 ~ "0.5–1.0",
    mean_dose >= 1.0 ~ "≥1.0"
  )) %>%
  mutate(threshold_color = factor(threshold_color, 
                                  levels = c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0")))

# Dummy points explicitly for legend
dummy_points <- data.frame(
  AVISIT = rep(unique(mmean_data$AVISIT)[1], 4),
  mean_dose = rep(NA_real_, 4),
  threshold_color = factor(c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0"), 
                           levels = levels(mmean_data$threshold_color))
)

# Plot with threshold-specific colors
(
  mean_gc_dose_plot <- ggplot(mmean_data, aes(x = AVISIT, y = mean_dose, group = 1)) +
    # Clinical threshold lines
    geom_hline(yintercept = 0.2, linetype = "dashed", color = "green", linewidth = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "orange", linewidth = 0.8) +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.8) +
    # Lines connecting points
    geom_line(color = "grey50", linewidth = 1.2) +
    # Real points
    geom_point(aes(color = threshold_color), size = 3) +
    # Dummy points just for legend
    geom_point(data = dummy_points, aes(color = threshold_color), size = 3, shape = 16) +
    scale_color_manual(
      values = c("<0.2" = "green", 
                 "0.2–0.5" = "yellow", 
                 "0.5–1.0" = "orange", 
                 "≥1.0" = "red"),
      name = "GC Dose (mg/kg/day)",
      drop = FALSE,
      na.translate = TRUE
    ) +
    scale_y_continuous(
      breaks = seq(0, max(mmean_data$mean_dose, na.rm = TRUE) + 1, by = 1)
    ) +
    labs(
      title = "Mean GC Dose",
      subtitle = "Clinically important thresholds: <span style='color:red'><b>1.0</b></span>, <span style='color:orange'><b>0.5</b></span> and <span style='color:green'><b>0.2</b></span> mg/kg/day",
      x = "Week",
      y = "Mean GC Dose (mg/kg/day)"
    ) +
    coord_cartesian(ylim = c(0, max(mmean_data$mean_dose, na.rm = TRUE) + 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = ggtext::element_markdown(),
      legend.position = "right"
    )
)
#TODO: reverse the legend order

# --- Analysis 2: Proportion of Subjects by Dose Threshold Over Time (using clinically important thresholds) ---

# Define the clinically important dose thresholds
bins_clinical <- c(0, 0.2, 0.5, 1.0, Inf)
labels_clinical <- c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0")

# Create the `dose_threshold_mg_kg` column based on `AVAL1`
filtered_df <- df |>
  mutate(dose_threshold_mg_kg = cut(AVAL1, breaks = bins_clinical, labels = labels_clinical, right = FALSE))

# Group by `AVISIT` and `dose_threshold_mg_kg`, count subjects
grouped_df_clinical <- filtered_df |>
  group_by(AVISIT, dose_threshold_mg_kg) |>
  summarise(subject_count = n(), .groups = 'drop')

# Calculate total subjects per visit
total_subjects_by_visit_clinical <- grouped_df_clinical |>
  group_by(AVISIT) |>
  summarise(total_count = sum(subject_count), .groups = 'drop')

# Calculate percentages
grouped_df_clinical <- grouped_df_clinical |>
  left_join(total_subjects_by_visit_clinical, by = "AVISIT") |>
  mutate(percentage = (subject_count / total_count) * 100)

grouped_df_clinical <- grouped_df_clinical %>%
  mutate(dose_threshold_mg_kg = factor(dose_threshold_mg_kg, levels = rev(c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0"))))


# Create the stacked bar chart for clinical thresholds
(
  stacked_bar_chart_clinical <- ggplot(
    grouped_df_clinical,
    aes(x = AVISIT, y = percentage, fill = dose_threshold_mg_kg)
  ) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
      values = c(
        "<0.2" = "green",
        "0.2–0.5" = "yellow",
        "0.5–1.0" = "orange",
        "≥1.0" = "red"
      ),
      name = "GC Dose (mg/kg/day)",
      drop = FALSE,
      na.translate = TRUE
    ) +
    labs(
      title = "Proportion of Subjects by Clinically-Relevant Dose Threshold Over Time",
      x = "Week",
      y = "Percentage of Subjects",
      fill = "Dose Threshold"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)


# --- Analysis 3: Individual Subject Shifts Between Threshold Levels ---

# Calculate the mean AVAL1 for each SUBJID at each AVISIT
filtered_df <- df |>
  select(SUBJID, AVISITW, AVISITWN, AVAL2) |>
  unique() |>
  mutate(AVISITW = fct_reorder(factor(AVISITW), AVISITWN))

# Define the clinically important dose thresholds
bins_clinical <- c(0, 0.2, 0.5, 1.0, Inf)
labels_clinical <- c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0")

# Create the `dose_threshold_mg_kg` column based on `AVAL2`
filtered_df <- filtered_df |>
  mutate(dose_threshold_mg_kg = cut(AVAL2, breaks = bins_clinical, labels = labels_clinical, right = FALSE))

# Pivot the dataframe to have `AVISITW` as columns and `dose_threshold` as values
pivot_df <- filtered_df |>
  distinct(SUBJID, AVISITW, dose_threshold_mg_kg) |>  # Ensure distinct combinations
  pivot_wider(id_cols = SUBJID, names_from = AVISITW, values_from = dose_threshold_mg_kg)

# Define the sort order for dose threshold levels (≥1.0est to <0.2est)
RESPONSE_LEVELS <- c("≥1.0", "0.5–1.0", "0.2–0.5", "<0.2", NA)
SORT_ORDER_MAP <- setNames(1:5, c(RESPONSE_LEVELS))

# Function to create sort keys
create_sort_key <- function(df, visit_cols) {
  # Handle NA values by assigning them to the last position (5)
  df <- df |> mutate(across(all_of(visit_cols), ~ifelse(is.na(.), NA, as.character(.))))
  
  sapply(df[visit_cols], function(col) {
    # Map values to numeric values for sorting
    mapped_values <- sapply(col, function(x) {
      if(is.na(x)) return("05")  # NA values get <0.2est priority
      return(sprintf("%02d", SORT_ORDER_MAP[x]))
    })
    return(mapped_values)
  }) |>
    apply(1, paste, collapse = "")
}

# Get visit columns in the correct order
visit_cols <- setdiff(names(pivot_df), "SUBJID")
visit_cols <- visit_cols[order(match(visit_cols, unique(filtered_df$AVISITW)))]

# Create sort keys for each subject and sort
pivot_df <- pivot_df %>%
  mutate(sort_key = create_sort_key(., visit_cols))

sorted_subjects <- pivot_df |>
  arrange(sort_key) |>
  pull(SUBJID)

# Convert the pivot table to long format for plotting
plot_df <- pivot_df |>
  pivot_longer(
    cols = -c(SUBJID, sort_key),
    names_to = "AVISITW",
    values_to = "dose_threshold_mg_kg"
  )

# Set the SUBJID order (≥1.0er values at top)
plot_df <- plot_df |>
  mutate(
    SUBJID = factor(SUBJID, levels = rev(sorted_subjects)),
    dose_threshold_mg_kg = factor(dose_threshold_mg_kg,
                                  levels = rev(c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0")))
  )

# Create the heatmap
(
  heatmap_plot <- ggplot(plot_df, aes(x = AVISITW, y = SUBJID, fill = dose_threshold_mg_kg)) +
  geom_tile() +
  scale_fill_manual(values = c("<0.2" = "green", "0.2–0.5" = "yellow", 
                               "0.5–1.0" = "orange", "≥1.0" = "red"),
                    na.value = "grey") +
  labs(title = "Subject Dose Threshold Shifts Over Time", 
       x = "Week", y = "Subject ID", fill = "Dose Threshold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# TODO: Replace NA with missing. Think about modifying the x-axis labels