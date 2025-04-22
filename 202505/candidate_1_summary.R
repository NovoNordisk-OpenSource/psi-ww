# ---- Load Libraries ----
packages <- c("tidyverse", "ggplot2", "dplyr", "scales", "ggtext")
installed <- packages %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# ---- Load and Preprocess Data ----
df <- read.csv("202505/WW_GCdose.csv") %>%
  filter(grepl("Baseline|Week", AVISIT)) %>%
  mutate(
    AVISITW = AVISIT,
    AVISITWN = AVISITN,
    AVISIT = ifelse(ASTDY == 0, AVISIT, paste0("Week ", AVISITN, "(Day ", ASTDY, ")")),
    AVISITN = ASTDY,
    AVISIT = fct_reorder(factor(AVISIT), AVISITN)
  )

# ---- Compute Mean Dose ----
mmean_data <- df %>%
  group_by(AVISITN, AVISIT) %>%
  summarise(mean_dose = mean(AVAL1), .groups = "drop") %>%
  mutate(
    threshold_color = case_when(
      mean_dose < 0.2 ~ "<0.2",
      mean_dose < 0.5 ~ "0.2–0.5",
      mean_dose < 1.0 ~ "0.5–1.0",
      TRUE ~ "≥1.0"
    ),
    threshold_color = factor(threshold_color, levels = c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0")),
    axis_label = ifelse(AVISITN == 0, "BL", as.character(AVISITN))
  )

mmean_data$axis_label <- factor(mmean_data$axis_label, levels = unique(mmean_data$axis_label))

# ---- Dummy Points for Legend ----
dummy_points <- data.frame(
  axis_label = "BL",
  mean_dose = NA_real_,
  threshold_color = factor(c("<0.2", "0.2–0.5", "0.5–1.0", "≥1.0"),
                           levels = levels(mmean_data$threshold_color))
)

# ---- Week Labels & Positions ----
week_labels <- mmean_data %>%
  filter(AVISITN != 0) %>%
  mutate(week = ceiling(AVISITN / 7)) %>%
  group_by(week) %>%
  summarise(
    week_label = paste0("Week ", unique(week)),
    start_day = min(AVISITN),
    end_day = max(AVISITN),
    .groups = "drop"
  ) %>%
  mutate(
    start_label = as.character(start_day),
    end_label = as.character(end_day),
    start_pos = match(start_label, levels(mmean_data$axis_label)),
    end_pos = match(end_label, levels(mmean_data$axis_label)),
    center_pos = (start_pos + end_pos) / 2
  )

# ---- Week Shading Rectangles ----
week_shading <- week_labels %>%
  mutate(
    xmin = start_pos - 0.5,
    xmax = end_pos + 0.5,
    fill_color = rep(c("gray95", "white"), length.out = n())
  )

# ---- Plot ----
mean_gc_dose_plot <- ggplot(mmean_data, aes(x = axis_label, y = mean_dose, group = 1)) +
  
  # Background shading
  geom_rect(
    data = week_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = week_shading$fill_color,
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  
  # Threshold lines
  geom_hline(yintercept = c(0.2, 0.5, 1.0),
             linetype = "dashed",
             color = c("green", "orange", "red"),
             linewidth = 0.8) +
  
  # Dose line and points
  geom_line(color = "grey50", linewidth = 1.2) +
  geom_point(aes(color = threshold_color), size = 3) +
  geom_point(
    data = dummy_points,
    aes(x = axis_label, y = mean_dose, color = threshold_color),
    size = 3, shape = 16
  ) +
  
  # Week labels
  geom_text(
    data = week_labels,
    aes(x = center_pos, y = max(mmean_data$mean_dose, na.rm = TRUE) + 1, label = week_label),
    vjust = 0,
    size = 3.5,
    fontface = "bold"
  ) +
  
  # Colors for thresholds
  scale_color_manual(
    values = c("<0.2" = "green", "0.2–0.5" = "yellow", "0.5–1.0" = "orange", "≥1.0" = "red"),
    name = "GC Dose (mg/kg/day)",
    guide = guide_legend(reverse = TRUE),
    drop = FALSE,
    na.translate = TRUE
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(0, max(mmean_data$mean_dose, na.rm = TRUE) + 1, by = 1)
  ) +
  
  # Labels and layout
  labs(
    title = "Mean GC Dose",
    subtitle = "Clinically important thresholds: <span style='color:red'><b>1.0</b></span>, <span style='color:orange'><b>0.5</b></span>, and <span style='color:green'><b>0.2</b></span> mg/kg/day",
    x = "Day",
    y = "Mean GC Dose (mg/kg/day)"
  ) +
  coord_cartesian(ylim = c(0, max(mmean_data$mean_dose, na.rm = TRUE) + 1.5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.subtitle = element_markdown(),
    legend.position = "right",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

# ---- Display Plot ----
mean_gc_dose_plot
