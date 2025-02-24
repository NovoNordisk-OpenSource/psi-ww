# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(RColorBrewer)

# Define color palettes
benefit_colors <-   c("#FF7F0E", "#1F77B4")  # Orange and Blue (complementary) for main benefit plot
active_color <- "#2CA02C"  # Darker Green for Active Treatment (arrows and annotations)
placebo_color <- "#6A3D9A"  # Darker Purple for Placebo (arrows and annotations)
risk_color <- "#D62728"  # Red for risk plot

# Function to create benefit plot
create_benefit_plot <- function(data) {
  ggplot(data, aes(x = RelRisk, y = Drug, color = Outcome)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbarh(aes(xmin = RelRisk_LowerCI, xmax = RelRisk_UpperCI),
                   position = position_dodge(width = 0.5), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    scale_x_log10(limits = c(0.5, 20), breaks = c(0.5, 1, 2, 5, 10, 20)) +
    scale_color_manual(values = benefit_colors) +
    labs(title = "Benefits: Comparative Efficacy vs Placebo", x = "Relative Risk (log scale)", y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom", plot.margin = margin(b = 20)) +
    coord_cartesian(clip = "off")
}

# Function to create risk plot
create_risk_plot <- function(data) {
  ggplot(data, aes(y = Drug)) +
    geom_point(aes(x = Diff_IncRate), size = 3, color = risk_color) +
    geom_errorbarh(aes(xmin = Diff_IncRate_LowerCI, xmax = Diff_IncRate_UpperCI),
                   height = 0.1, linewidth = 0.5, color = risk_color) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_continuous(limits = c(min(data$Diff_IncRate_LowerCI, -20), max(data$Diff_IncRate_UpperCI) * 1.1),
                       expand = expansion(mult = c(0.1, 0.1))) +
    labs(title = "Risks: Drug Class Toxicity", x = "Difference in Incidence Rate per 100 Patient-Years", y = NULL) +
    theme_minimal() +
    theme(legend.position = "none", plot.margin = margin(b = 20)) +
    coord_cartesian(clip = "off")
}

# Function to create count plot
create_count_plot <- function(data) {
  count_data <- data %>%
    select(Drug, nSub1, N1, Prop1, nSub2, N2, Prop2) %>%
    pivot_longer(cols = c(Prop1, Prop2), names_to = "Group", values_to = "Proportion") %>%
    mutate(Group = ifelse(Group == "Prop1", "Active Treatment", "Placebo"),
           Count = ifelse(Group == "Active Treatment", paste(nSub1, "/", N1), paste(nSub2, "/", N2)))
  
  ggplot(count_data, aes(x = Proportion, y = Drug, color = Group)) +
    geom_point(size = 3) +
    geom_text(aes(label = Count), hjust = -0.3, vjust = 0.5, size = 3, show.legend = FALSE) +
    scale_x_continuous(limits = c(0, max(count_data$Proportion) * 1.2), labels = scales::percent) +
    scale_color_manual(values = setNames(c(active_color, placebo_color), c("Active Treatment", "Placebo"))) +
    labs(title = "Event Counts and Proportions for Drug Class Toxicity", x = "Proportion of Events", y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = NULL))
}

# Function to remove y-axis
remove_y_axis <- function(p) {
  p + theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
}

# Function to adjust y-axis
adjust_y_axis <- function(p, data) {
  p + scale_y_discrete(limits = unique(data$Drug))
}

# Function to add arrows and labels
add_arrows_and_labels <- function(p, x_left, x_right, y_pos, label_left, label_right, break_point, is_log_scale = FALSE, transparent = FALSE, reverse_colors = FALSE) {
  text_offset <- if(is_log_scale) 0.1 else 0.05 * (x_right - x_left)
  
  if (is_log_scale) {
    gap <- 0.01  # Fixed small gap for log scale
    left_end <- break_point / (1 + gap)
    right_start <- break_point * (1 + gap)
  } else {
    gap <- 0.005 * (x_right - x_left)
    left_end <- break_point - gap
    right_start <- break_point + gap
  }
  
  if (reverse_colors) {
    arrow_color_left <- if(transparent) alpha(active_color, 0) else active_color
    arrow_color_right <- if(transparent) alpha(placebo_color, 0) else placebo_color
    text_color_left <- active_color
    text_color_right <- placebo_color
  } else {
    arrow_color_left <- if(transparent) alpha(placebo_color, 0) else placebo_color
    arrow_color_right <- if(transparent) alpha(active_color, 0) else active_color
    text_color_left <- placebo_color
    text_color_right <- active_color
  }
  
  p +
    # Left arrow
    annotate("segment", x = x_left, xend = left_end, y = y_pos, yend = y_pos,
             arrow = arrow(length = unit(0.3, "cm"), ends = "first"), color = arrow_color_left) +
    # Right arrow
    annotate("segment", x = right_start, xend = x_right, y = y_pos, yend = y_pos,
             arrow = arrow(length = unit(0.3, "cm"), ends = "last"), color = arrow_color_right) +
    # Labels
    annotate("text", x = x_left * (if(is_log_scale) 1.1 else 1), y = y_pos + 0.1, label = label_left, hjust = 0, color = text_color_left) +
    annotate("text", x = x_right / (if(is_log_scale) 1.1 else 1), y = y_pos + 0.1, label = label_right, hjust = 1, color = text_color_right)
}

# Reverse the order of drugs in the data
benefit_data <-  benefit_data %>% mutate(Drug = factor(Drug, levels = rev(unique(Drug))))
risk_data <-  risk_data %>% mutate(Drug = factor(Drug, levels = rev(unique(Drug))))

# Create and modify plots
benefit_plot <-   create_benefit_plot(benefit_data) %>%
  add_arrows_and_labels(0.5, 20, 0.2, "Favors Placebo", "Favors Treatment", break_point = 1, is_log_scale = TRUE)

risk_plot <-   create_risk_plot(risk_data) %>%
  remove_y_axis() %>%
  add_arrows_and_labels(min(risk_data$Diff_IncRate_LowerCI, -20), 
                        max(risk_data$Diff_IncRate_UpperCI) * 1.1, 
                        0.2, "Favors Treatment", "Favors Placebo", break_point = 0, is_log_scale = FALSE, reverse_colors = TRUE)

count_plot <-   create_count_plot(risk_data) %>% 
  remove_y_axis() %>%
  # Add phantom line and annotation for alignment (now transparent)
  add_arrows_and_labels(0, 0, 0.2, "", "", break_point = 0.5, is_log_scale = FALSE, transparent = TRUE) +
  theme(
    plot.margin = margin(t = 20, r = 5.5, b = 5.5, l = 5.5, unit = "pt")
  )

# Combine the plots
plots <-   list(benefit_plot, risk_plot, count_plot) %>%
  map(~ adjust_y_axis(., benefit_data))

combined_plot <-   plots[[1]] + plots[[2]] + plots[[3]] +
  plot_layout(ncol = 3, widths = c(3, 2, 3)) +
  plot_annotation(title = "Benefit-Risk Profile of Drugs",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save the plot
ggsave("202503/benefit_risk_plot_NN1.png", combined_plot, width = 20, height = 8, dpi = 300)

# Print the combined plot
print(combined_plot)