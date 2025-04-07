# Load required libraries
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

# Read the data
data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)

# Function to create a heatmap
create_heatmap <- function(df, treatment_name) {
    # Prepare data for the heatmap
    heatmap_data <- df %>%
        select(USUBJID, AVISITN, AVALC) %>%
        mutate(AVALC = factor(AVALC, levels = c("No Response", "Low Response", "Response", "High Response"))) %>%
        pivot_wider(names_from = AVISITN, values_from = AVALC, values_fill = NA) %>%
        arrange(USUBJID)

    # Get column names for visit numbers
    visit_cols <- setdiff(colnames(heatmap_data), "USUBJID")

    # Check if we have any data
    if (nrow(heatmap_data) == 0 || length(visit_cols) == 0) {
        cat("No data available for", treatment_name, "\n")
        return(NULL)
    }

    # Create a response to numeric mapping
    response_levels <- c("No Response" = 1, "Low Response" = 2, "Response" = 3, "High Response" = 4)

    # Create a matrix for the heatmap values
    heatmap_matrix <- matrix(NA, nrow = nrow(heatmap_data), ncol = length(visit_cols))

    # Fill the matrix with numeric values
    for (i in 1:length(visit_cols)) {
        col <- visit_cols[i]
        heatmap_matrix[, i] <- sapply(heatmap_data[[col]], function(x) {
            if (is.na(x)) {
                return(0)
            } else {
                return(response_levels[as.character(x)])
            }
        })
    }

    # Sort subjects based on their responses across visits
    sort_order <- do.call(order, as.data.frame(heatmap_matrix))
    heatmap_matrix <- heatmap_matrix[sort_order, , drop = FALSE]
    sorted_ids <- heatmap_data$USUBJID[sort_order]

    # Convert column names to numeric visit labels
    visit_labels <- seq_along(visit_cols)

    # Debugging
    cat("Matrix dimensions for", treatment_name, ":", nrow(heatmap_matrix), "x", ncol(heatmap_matrix), "\n")

    # Create heatmap
    fig <- plot_ly(
        x = visit_labels,
        y = sorted_ids,
        z = heatmap_matrix,
        type = "heatmap",
        colorscale = list(
            c(0, "#FFFFFF"), # 0 (missing) - white
            c(0.2, "#FF9999"), # 1 - No Response
            c(0.4, "#FFCC99"), # 2 - Low Response
            c(0.6, "#99CC99"), # 3 - Response
            c(0.8, "#66CC66") # 4 - High Response
        ),
        colorbar = list(
            title = "Response",
            tickvals = c(0, 1, 2, 3, 4),
            ticktext = c("Missing", "No Response", "Low Response", "Response", "High Response")
        )
    ) %>%
        layout(
            title = paste("Heatmap of Responses for", treatment_name),
            xaxis = list(title = "Visit", tickvals = visit_labels, ticktext = visit_labels),
            yaxis = list(title = "Subjects", autorange = "reversed", showticklabels = FALSE) # Label Y-axis as "Subjects"
        )

    return(fig)
}

# Filter data for two treatments
treatment_a_data <- data %>% filter(TRT == "Drug A")
treatment_b_data <- data %>% filter(TRT == "Drug B")

# Create heatmaps for the two treatments
heatmap_a <- create_heatmap(treatment_a_data, "Drug A")
heatmap_b <- create_heatmap(treatment_b_data, "Drug B")

# Combine the two heatmaps side by side if both exist
if (!is.null(heatmap_a) && !is.null(heatmap_b)) {
    combined_heatmap <- subplot(heatmap_a, heatmap_b, nrows = 1, shareY = FALSE) %>%
        layout(
            title = "Heatmaps for Drug A and Drug B",
            yaxis = list(title = "Subjects"), # Add Y-axis label for the combined plot
            showlegend = TRUE # Ensure only one legend is displayed
        )

    # Save the combined heatmap as an HTML file
    htmlwidgets::saveWidget(combined_heatmap, file = "202504/heatmap_drug_comparison.html", selfcontained = TRUE)
    cat("Saved combined heatmap: 202504/heatmap_drug_comparison.html\n")
} else if (!is.null(heatmap_a)) {
    # Save just Drug A heatmap
    htmlwidgets::saveWidget(heatmap_a, file = "202504/heatmap_drug_a.html", selfcontained = TRUE)
    cat("Saved Drug A heatmap: 202504/heatmap_drug_a.html\n")
} else if (!is.null(heatmap_b)) {
    # Save just Drug B heatmap
    htmlwidgets::saveWidget(heatmap_b, file = "202504/heatmap_drug_b.html", selfcontained = TRUE)
    cat("Saved Drug B heatmap: 202504/heatmap_drug_b.html\n")
} else {
    cat("No data available to create heatmaps\n")
}
