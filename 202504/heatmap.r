# File: 202504/heatmap_comparison.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)

sort_order_map <- c("No Response" = 1, "Low Response" = 2, "Response" = 3, "High Response" = 4, "Missing" = 5)
response_levels <- names(sort_order_map)

create_heatmap <- function(df, treatment_name) {
  heatmap_data <- df %>%
    select(USUBJID, AVISITN, AVALC) %>%
    mutate(AVALC = factor(ifelse(is.na(AVALC), "Missing", AVALC), levels = response_levels)) %>%
    arrange(USUBJID)
  
  visit_levels <- sort(unique(heatmap_data$AVISITN))
  
  subject_wide <- heatmap_data %>%
    pivot_wider(names_from = AVISITN, values_from = AVALC, values_fill = "Missing")
  
  subject_wide$sort_key <- ""
  for (v in visit_levels) {
    col <- as.character(v)
    if (!col %in% colnames(subject_wide)) {
      subject_wide[[col]] <- "Missing"
    }
    subject_wide$sort_key <- paste0(subject_wide$sort_key, sprintf("%02d", sort_order_map[subject_wide[[col]]]))
  }
  
  subject_wide <- subject_wide %>% arrange(desc(sort_key))
  sorted_subjects <- subject_wide$USUBJID
  
  plot_data <- heatmap_data %>%
    mutate(AVISITN = factor(AVISITN, levels = visit_levels)) %>%
    mutate(AVALC = factor(AVALC, levels = response_levels)) %>%
    mutate(USUBJID = factor(USUBJID, levels = rev(sorted_subjects)))
  
  p <- ggplot(plot_data, aes(x = AVISITN, y = USUBJID, fill = AVALC)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c(
        "High Response" = "#00AA00",
        "Response" = "#AAAA00",
        "Low Response" = "#FFAA00",
        "No Response" = "#FF0000",
        "Missing" = "#888888"
      ),
      breaks = c("High Response", "Response", "Low Response", "No Response", "Missing"),
      drop = FALSE
    ) +
    labs(
      x = "Visit",
      y = "Subject ID",
      fill = "Response",
      title = treatment_name
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.y = element_blank(),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.title = element_text(size = 12),
      panel.grid = element_blank()
    )
  
  return(p)
}

data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)
treatment_a_data <- data %>% filter(TRT == "Drug A")
treatment_b_data <- data %>% filter(TRT == "Drug B")

heatmap_a <- create_heatmap(treatment_a_data, "Drug A")
heatmap_b <- create_heatmap(treatment_b_data, "Drug B") + theme(axis.title.y = element_blank())

# Save the combined heatmap side-by-side
if (!is.null(heatmap_a) && !is.null(heatmap_b)) {
  library(cowplot)
  library(grid)
  
  legend <- cowplot::get_legend(
    heatmap_b +
      guides(fill = guide_legend(ncol = 1, reverse = TRUE, override.aes = list(colour = "black"))) +
      theme(
        legend.position = "right",
        legend.spacing.y = unit(0.5, "lines"),
        legend.key = element_rect(colour = "black", fill = NA)
      )
  )
  
  plots <- plot_grid(heatmap_a + theme(legend.position = "none"),
                     heatmap_b + theme(legend.position = "none"),
                     ncol = 2, align = "v")
  
  combined <- plot_grid(
    plot_grid(NULL,
              ggdraw() + draw_label("Patient Response Over Time by Treatment", fontface = "bold", size = 16, hjust = 0.5),
              NULL,
              ncol = 3, rel_widths = c(0.1, 0.8, 0.1)),
    plot_grid(heatmap_a + theme(legend.position = "none"),
              plot_grid(heatmap_b + theme(legend.position = "none"), legend, ncol = 2, rel_widths = c(1, 0.2)),
              ncol = 2, align = "v"),
    ncol = 1,
    rel_heights = c(0.1, 1)
  )
  
  ggsave("202504/heatmap_drug_comparison.png", combined, width = 20, height = 8, bg = "white")
  cat("Saved combined heatmap: heatmap_drug_comparison.png\n")
} else if (!is.null(heatmap_a)) {
  ggsave("202504/heatmap_drug_a.png", heatmap_a, width = 10, height = 8, bg = "white")
  cat("Saved Drug A heatmap: heatmap_drug_a.png\n")
} else if (!is.null(heatmap_b)) {
  ggsave("202504/heatmap_drug_b.png", heatmap_b, width = 10, height = 8, bg = "white")
  cat("Saved Drug B heatmap: heatmap_drug_b.png\n")
} else {
  cat("No data available to create heatmaps\n")
}
