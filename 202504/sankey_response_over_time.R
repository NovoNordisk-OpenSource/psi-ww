# Load required libraries
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Read the data
data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)

# Function to create a Sankey diagram
create_sankey <- function(df, treatment_name) {
  df <- df %>% arrange(USUBJID, AVISITN)
  visits <- sort(unique(df$AVISITN))
  
  wide_data <- df %>%
    select(USUBJID, AVISITN, AVALC) %>%
    pivot_wider(
      id_cols = USUBJID,
      names_from = AVISITN,
      values_from = AVALC,
      names_prefix = "Visit_"
    )
  
  source_labels <- c()
  target_labels <- c()
  values <- c()
  
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
      source_label <- paste0("Visit ", v1, ": ", transitions[[v1_col]][j])
      target_label <- paste0("Visit ", v2, ": ", transitions[[v2_col]][j])
      source_labels <- c(source_labels, source_label)
      target_labels <- c(target_labels, target_label)
      values <- c(values, transitions$count[j])
    }
  }
  
  if (length(source_labels) == 0) {
    cat("No transitions found for treatment:", treatment_name, "\n")
    return(NULL)
  }
  
  all_nodes <- unique(c(source_labels, target_labels))
  source_indices <- match(source_labels, all_nodes) - 1
  target_indices <- match(target_labels, all_nodes) - 1
  
  response_colors <- c(
    "High Response" = "#66CC66",
    "Response" = "#99CC99",
    "Low Response" = "#FFCC99",
    "No Response" = "#FF9999"
  )
  
  node_colors <- sapply(all_nodes, function(node) {
    response <- sub(".*: ", "", node)
    return(response_colors[response])
  })
  
  # Create empty labels for nodes
  node_labels <- rep("", length(all_nodes))
  
  # Create x-axis positions for nodes (visits)
  x_positions <- numeric(length(all_nodes))
  for(i in 1:length(all_nodes)) {
    visit_num <- as.numeric(sub(":.*$", "", sub("^Visit ", "", all_nodes[i])))
    x_positions[i] <- (visit_num - 1)/(max(visits) - 1)
  }
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = node_labels,  # Empty labels
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5),
      x = x_positions  # Set x positions for proper visit spacing
    ),
    link = list(
      source = source_indices,
      target = target_indices,
      value = values,
      color = sapply(source_indices, function(i) {
        response <- sub(".*: ", "", all_nodes[i+1])
        color <- response_colors[response]
        r <- strtoi(substr(color, 2, 3), 16)
        g <- strtoi(substr(color, 4, 5), 16)
        b <- strtoi(substr(color, 6, 7), 16)
        return(paste0("rgba(", r, ",", g, ",", b, ",0.5)"))
      })
    )
  )
  
  # Create visit labels at the bottom
  x_axis_annotations <- list()
  for(visit in visits) {
    x_axis_annotations[[length(x_axis_annotations) + 1]] <- list(
      x = (visit - 1)/(max(visits) - 1),
      y = -0.15,  # Position for visit labels
      text = paste("Visit", visit),
      showarrow = FALSE,
      xanchor = 'center',
      yanchor = 'top',
      font = list(size = 10)
    )
  }
  
  # Create horizontal legend at the bottom with colored rectangles
  legend_items <- names(response_colors)
  legend_annotations <- list()
  legend_width <- 1 / length(legend_items)
  
  # Add colored rectangles for legend
  for(i in 1:length(legend_items)) {
    # Add rectangle shape
    legend_annotations[[length(legend_annotations) + 1]] <- list(
      type = "rect",
      x0 = (i-1)*legend_width,
      x1 = (i-1)*legend_width + 0.02,  # Small rectangle width
      y0 = -0.3,
      y1 = -0.28,
      xref = 'paper',
      yref = 'paper',
      fillcolor = response_colors[legend_items[i]],
      line = list(color = response_colors[legend_items[i]]),
      opacity = 0.8
    )
    
    # Add text label
    legend_annotations[[length(legend_annotations) + 1]] <- list(
      x = (i-1)*legend_width + 0.04,  # Position text after rectangle
      y = -0.29,  # Position below visit labels
      text = legend_items[i],
      showarrow = FALSE,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'left',
      font = list(
        size = 10,
        color = 'black'
      )
    )
  }
  
  # Single layout call with all annotations and shapes
  fig <- fig %>% layout(
    title = paste("Response Over Time -", treatment_name),
    font = list(size = 12),
    annotations = c(x_axis_annotations, 
                    legend_annotations[seq(2, length(legend_annotations), by=2)]),  # Text annotations only
    shapes = legend_annotations[seq(1, length(legend_annotations), by=2)],  # Rectangle shapes only
    margin = list(
      l = 50,
      r = 50,
      b = 120,  # Increased bottom margin to accommodate labels and legend
      t = 50
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE)
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
  combined_html <- '
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
    <div class="container">
  '
  
  for (trt in names(sankey_plots)) {
    filename <- paste0("sankey_", gsub(" ", "_", trt), ".html")
    combined_html <- paste0(combined_html, '
      <div class="plot-container">
        <h2>', trt, '</h2>
        <iframe src="', filename, '"></iframe>
      </div>
    ')
  }
  
  combined_html <- paste0(combined_html, '
    </div>
  </body>
  </html>
  ')
  
  writeLines(combined_html, "202504/sankey_all_treatments.html")
  cat("Created combined HTML file: 202504/sankey_all_treatments.html\n")
}