library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)

# Constants
RESPONSE_LEVELS <-  c("No Response", "Low Response", "Response", "High Response", "Missing")
SORT_ORDER_MAP <- setNames(1:5, RESPONSE_LEVELS)
COLOR_VALUES <- c(
  "High Response" = "#00AA00",
  "Response" = "#AAAA00",
  "Low Response" = "#FFAA00",
  "No Response" = "#FF0000",
  "Missing" = "#888888"
)

create_sort_key <- function(df, visit_levels) {
  sapply(df[as.character(visit_levels)], function(col) sprintf("%02d", SORT_ORDER_MAP[col])) %>%
    apply(1, paste, collapse = "")
}

prepare_plot_data <-  function(heatmap_data, visit_levels, sorted_subjects) {
  heatmap_data %>%
    mutate(
      AVISITN = factor(AVISITN, levels = visit_levels),
      AVALC = factor(AVALC, levels = RESPONSE_LEVELS),
      USUBJID = factor(USUBJID, levels = rev(sorted_subjects))
    )
}

create_ggplot <-  function(plot_data, treatment_name) {
  existing_levels <- unique(plot_data$AVALC)
  color_values <- COLOR_VALUES[names(COLOR_VALUES) %in% existing_levels]
  legend_order <- rev(RESPONSE_LEVELS[RESPONSE_LEVELS %in% existing_levels])
  
  ggplot(plot_data, aes(x = AVISITN, y = USUBJID, fill = AVALC)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = color_values, breaks = legend_order, drop = FALSE) +
    labs(x = "Visit", y = "Subjects", fill = NULL, title = treatment_name) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.y = element_blank(),
      axis.title = element_text(size = 14, face = "bold"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank()
    )
}

create_heatmap <- function(df, treatment_name) {
  heatmap_data <- df %>%
    select(USUBJID, AVISITN, AVALC) %>%
    mutate(AVALC = factor(ifelse(is.na(AVALC), "Missing", AVALC), levels = RESPONSE_LEVELS)) %>%
    arrange(USUBJID)
  
  visit_levels <-  sort(unique(heatmap_data$AVISITN))
  
  subject_wide <- heatmap_data %>%
    pivot_wider(names_from = AVISITN, values_from = AVALC, values_fill = "Missing") %>%
    mutate(sort_key = create_sort_key(., visit_levels)) %>%
    arrange(desc(sort_key))
  
  sorted_subjects <- subject_wide$USUBJID
  
  plot_data <- prepare_plot_data(heatmap_data, visit_levels, sorted_subjects)
  
  create_ggplot(plot_data, treatment_name)
}

save_heatmaps <- function(heatmap_a, heatmap_b) {
  if (!is.null(heatmap_a) && !is.null(heatmap_b)) {
    # Remove legends and adjust y-axis title
    heatmap_a_no_legend <- heatmap_a + theme(legend.position = "none")
    heatmap_b_no_legend <- heatmap_b + theme(legend.position = "none", axis.title.y = element_blank())
    
    # Extract legend
    legend <- ggplotGrob(heatmap_b + 
                           theme(legend.position = "right",
                                 legend.box.margin = margin(0, 0, 0, 12),
                                 legend.key.size = unit(0.5, "cm")))$grobs
    legend <- legend[[which(sapply(legend, function(x) x$name) == "guide-box")]]
    
    # Arrange plots
    combined <- grid.arrange(
      grob = textGrob("Patient Response Over Time by Treatment", gp = gpar(fontsize = 16, fontface = "bold")),
      arrangeGrob(
        arrangeGrob(heatmap_a_no_legend, heatmap_b_no_legend, ncol = 2, widths = c(1, 1)),
        legend,
        ncol = 2,
        widths = c(0.9, 0.1)
      ),
      ncol = 1,
      heights = c(0.1, 1)
    )
    
    # Save the plot
    ggsave("202504/heatmap_drug_comparison.png", combined, width = 20, height = 10, bg = "white")
    cat("Saved combined heatmap: heatmap_drug_comparison.png\n")
  } else {
    if (!is.null(heatmap_a)) {
      ggsave("202504/heatmap_drug_a.png", heatmap_a, width = 10, height = 8, bg = "white")
      cat("Saved Drug A heatmap: heatmap_drug_a.png\n")
    }
    if (!is.null(heatmap_b)) {
      ggsave("202504/heatmap_drug_b.png", heatmap_b, width = 10, height = 8, bg = "white")
      cat("Saved Drug B heatmap: heatmap_drug_b.png\n")
    }
  }
}

# Main execution
data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)
treatment_a_data <- filter(data, TRT == "Drug A")
treatment_b_data <- filter(data, TRT == "Drug B")

heatmap_a <- create_heatmap(treatment_a_data, "Drug A")
heatmap_b <- create_heatmap(treatment_b_data, "Drug B")

save_heatmaps(heatmap_a, heatmap_b)