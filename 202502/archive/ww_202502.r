library(tidyverse)
library(readr)
library(RColorBrewer)
library(plotly)
library(htmlwidgets)

# Read the CSV file
read_ae_data <-  function(file_path) {
  read_csv(file_path)
}

# Count the occurrences of each adverse event for each subject
count_ae_occurrences <- function(adae) {
  adae %>%
    count(USUBJID, AEDECOD)
}

calculate_ae_cooccurrence <-  function(ae_data) {
  required_cols <- c("USUBJID", "AEDECOD")
  if (!all(required_cols %in% names(ae_data))) {
    stop("Data must contain USUBJID and AEDECOD columns")
  }
  
  ae_terms <- unique(ae_data$AEDECOD)
  n_terms <- length(ae_terms)
  
  result <- expand.grid(AEDECOD1 = ae_terms, AEDECOD2 = ae_terms, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(n = map2_dbl(AEDECOD1, AEDECOD2, ~{
      if (.x == .y) return(0)
      subjects_x <-  ae_data %>% filter(AEDECOD == .x) %>% pull(USUBJID)
      subjects_y <-  ae_data %>% filter(AEDECOD == .y) %>% pull(USUBJID)
      length(intersect(subjects_x, subjects_y))
    }))
  
  result %>%
    arrange(AEDECOD1, AEDECOD2)
}

create_color_palette <-  function(n = 9) {
  colorRampPalette(c(
    "#FFFFFF", "#FFF7BC", "#FEE391", "#FEC44F",
    "#FE9929", "#EC7014", "#CC4C02", "#993404"
  ))(n)
}

plot_ae_cooccurrence_ggplot <- function(ae_cooccurrence_data) {
  max_cooccurrence <- max(ae_cooccurrence_data$n, na.rm = TRUE)
  colors <- create_color_palette()
  
  ggplot(ae_cooccurrence_data, aes(x = AEDECOD1, y = AEDECOD2, fill = n)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradientn(
      colors = colors,
      limits = c(0, max_cooccurrence),
      breaks = pretty(c(0, max_cooccurrence), n = 5),
      guide = guide_colorbar(
        title = "Co-occurrence\nCount",
        barwidth = 1,
        barheight = 10
      )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = "Co-occurrence of Adverse Events",
      x = "Adverse Event 1",
      y = "Adverse Event 2"
    ) +
    coord_fixed()
}

create_plotly_colorscale <- function() {
  ylord_colors <- brewer.pal(9, "YlOrRd")
  n <- length(ylord_colors)
  data.frame(
    z = seq(0, 1, length.out = n),
    col = ylord_colors
  )
}

plot_ae_cooccurrence_plotly <- function(ae_cooccurrence_data) {
  ae_wide <- ae_cooccurrence_data %>%
    mutate(n = ifelse(AEDECOD1 == AEDECOD2, NA, n)) %>%
    pivot_wider(names_from = AEDECOD2, values_from = n, values_fill = list(n = NA))
  
  labels <-  ae_wide$AEDECOD1
  ae_matrix <- as.matrix(ae_wide[, -1])
  
  plot_ly(
    x = labels, 
    y = labels, 
    z = ae_matrix, 
    type = "heatmap",
    colorscale = create_plotly_colorscale(),
    colorbar = list(title = "Co-occurrence Count"),
    showscale = TRUE
  ) %>%
    layout(
      title = "Co-occurrence of Adverse Events",
      xaxis = list(title = "Adverse Event 1"),
      yaxis = list(title = "Adverse Event 2"),
      margin = list(l = 100, b = 100)
    )
}

# Main execution
adae <- read_ae_data("202502/DummyAEData.csv")
ae_counts <- count_ae_occurrences(adae)
ae_cooccurrence <- calculate_ae_cooccurrence(ae_counts)

# Generate and save ggplot visualization
(ggplot_viz <- plot_ae_cooccurrence_ggplot(ae_cooccurrence))
ggsave("202502/ae_cooccurrence_ggplot.png", ggplot_viz, width = 12, height = 10)

# Generate and save plotly visualization
(plotly_viz <- plot_ae_cooccurrence_plotly(ae_cooccurrence))
saveWidget(plotly_viz, "202502/ae_cooccurrence_plotly.html")

