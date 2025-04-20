# Load required libraries
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Read the data
data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)

# Function to create a Sankey diagram
create_sankey <-  function(df, treatment_name) {
  df <- df %>% arrange(USUBJID, AVISITN)
  visits <-  sort(unique(df$AVISITN))
  
  response_order <- c("High Response", "Response", "Low Response", "No Response")
  df$AVALC <- factor(df$AVALC, levels = response_order)
  
  # Create deterministic all_nodes
  all_nodes_df <- expand.grid(
    visit = visits,
    response = response_order,
    stringsAsFactors = FALSE
  ) %>%
    mutate(label = paste0("Visit ", visit, ": ", response),
           x = (visit - 1)/(max(visits) - 1))
  
  all_nodes <-  all_nodes_df$label
  
  wide_data <- df %>%
    select(USUBJID, AVISITN, AVALC) %>%
    pivot_wider(
      id_cols = USUBJID,
      names_from = AVISITN,
      values_from = AVALC,
      names_prefix = "Visit_"
    )
  
  source_labels <-  c()
  target_labels <- c()
  values <- c()
  hover_texts <- c()
  
  for (i in 1:(length(visits)-1)) {
    v1 <- visits[i]
    v2 <- visits[i+1]
    v1_col <- paste0("Visit_", v1)
    v2_col <- paste0("Visit_", v2)
    
    transitions <- wide_data %>%
      filter(!is.na(!!sym(v1_col)) & !is.na(!!sym(v2_col))) %>%
      group_by(!!sym(v1_col), !!sym(v2_col)) %>%
      summarise(count = n(), .groups = "drop")
    
    if (nrow(transitions) == 0) next
    
    for (j in 1:nrow(transitions)) {
      from_resp <- as.character(transitions[[v1_col]][j])
      to_resp <- as.character(transitions[[v2_col]][j])
      source_label <- paste0("Visit ", v1, ": ", from_resp)
      target_label <- paste0("Visit ", v2, ": ", to_resp)
      source_labels <- c(source_labels, source_label)
      target_labels <- c(target_labels, target_label)
      values <- c(values, transitions$count[j])
      hover_texts <- c(hover_texts, paste0("Visit ", v1, " (", from_resp, ") → Visit ", v2, " (", to_resp, "): ", transitions$count[j], " patients"))
    }
  }
  
  if (length(source_labels) == 0) {
    cat("No transitions found for treatment:", treatment_name, "\n")
    return(NULL)
  }
  
  source_indices <- match(source_labels, all_nodes) - 1
  target_indices <- match(target_labels, all_nodes) - 1
  
  response_colors <- c(
    "No Response" = "#FF9999",
    "Low Response" = "#FFCC99",
    "Response" = "#99CC99",
    "High Response" = "#66CC66"
  )
  
  node_colors <- sapply(all_nodes_df$response, function(response) response_colors[response])
  node_labels <- rep("", length(all_nodes))
  x_positions <- all_nodes_df$x
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = node_labels,
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      x = x_positions
    ),
    link = list(
      source = source_indices,
      target = target_indices,
      value = values,
      label = hover_texts,
      hoverinfo = "text",
      text = hover_texts,
      color = sapply(source_indices, function(i) {
        response <- sub(".*: ", "", all_nodes[i+1])
        color <- response_colors[response]
        r <- strtoi(substr(color, 2, 3), 16)
        g <- strtoi(substr(color, 4, 5), 16)
        b <- strtoi(substr(color, 6, 7), 16)
        paste0("rgba(", r, ",", g, ",", b, ",0.5)")
      })
    )
  )
  
  x_axis_annotations <- lapply(visits, function(visit) {
    list(
      x = (visit - 1)/(max(visits) - 1),
      y = -0.15,
      text = paste("Visit", visit),
      showarrow = FALSE,
      xanchor = 'center',
      yanchor = 'top',
      font = list(size = 12)
    )
  })
  
  legend_items <-  names(response_colors)
  legend_annotations <- list()
  spacing <- 0.2
  
  for(i in 1:length(legend_items)) {
    legend_annotations[[length(legend_annotations) + 1]] <- list(
      x = 0.25 + (i-1)*spacing,
      y = -0.31,
      text = legend_items[i],
      showarrow = FALSE,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'center',
      yanchor = 'bottom',
      font = list(size = 12, color = response_colors[legend_items[i]])
    )
    
    legend_annotations[[length(legend_annotations) + 1]] <- list(
      type = "rect",
      x0 = 0.25 + (i-1)*spacing - 0.01,
      x1 = 0.25 + (i-1)*spacing + 0.01,
      y0 = -0.35,
      y1 = -0.33,
      xref = 'paper',
      yref = 'paper',
      fillcolor = response_colors[legend_items[i]],
      line = list(color = response_colors[legend_items[i]]),
      opacity = 0.8
    )
  }
  
  fig <- fig %>% layout(
    title = paste("Response Over Time -", treatment_name),
    font = list(size = 12),
    annotations = c(x_axis_annotations, legend_annotations[seq(1, length(legend_annotations), by=2)]),
    shapes = legend_annotations[seq(2, length(legend_annotations), by=2)],
    margin = list(l = 50, r = 50, b = 160, t = 50),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE),
    autosize = TRUE,
    height = 600
  )
  
  return(fig)
}

# Split data by treatment
treatment_groups <- split(data, data$TRT)

# Create and save Sankey diagrams
sankey_plots <- list()
for (trt in names(treatment_groups)) {
  cat("Processing treatment:", trt, "\n")
  sankey <- create_sankey(treatment_groups[[trt]], trt)
  
  if (!is.null(sankey)) {
    sankey_plots[[trt]] <- sankey
    filename <- paste0("202504/sankey_", gsub(" ", "_", trt), ".html")
    saveWidget(sankey, file = filename, selfcontained = TRUE)
    cat("  Saved diagram to:", filename, "\n")
  }
}

# Create a combined HTML file with all treatments
if (length(sankey_plots) > 0) {
  combined_html <-  '
  <!DOCTYPE html>
  <html>
  <head>
    <title>Response Over Time by Treatment</title>
    <style>
      body { font-family: Arial, sans-serif; margin: 20px; }
      .container { display: flex; flex-direction: column; }
      .plot-container { margin-bottom: 30px; border: 1px solid #ddd; padding: 10px; }
      h1 { text-align: center; }
      iframe { width: 100%; height: 600px; border: none; }
    </style>
  </head>
  <body>
    <h1>Patient Response Over Time by Treatment</h1>
    <div  class="container">
  '
  
  for (trt in names(sankey_plots)) {
    filename <-  paste0("sankey_", gsub(" ", "_", trt), ".html")
    combined_html <- paste0(combined_html, '
      <div class="plot-container">
        <h2>', trt, '</h2>
        <iframe  src="', filename, '" frameborder="0" scrolling="no" onload="resizeIframe(this)"></iframe>
      </div>
    ')
  }
  
  combined_html <- paste0(combined_html, '
    </div>
    <script>
      function resizeIframe(obj) {
        obj.style.height = obj.contentWindow.document.documentElement.scrollHeight + "px";
      }
    </script>
  </body>
  </html>
  ')
  
  writeLines(combined_html, "202504/sankey_all_treatments.html")
  cat("Created combined HTML file: 202504/sankey_all_treatments.html\n")
}