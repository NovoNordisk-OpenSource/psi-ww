library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(RColorBrewer)

# Read the CSV file
read_ae_data <-   function(file_path) {
  read_csv(file_path)
}

count_ae_occurrences <- function(adae) {
  adae %>%
    count(USUBJID, AEDECOD)
}

calculate_ae_cooccurrence <-   function(ae_data) {
  required_cols <- c("USUBJID", "AEDECOD")
  if (!all(required_cols %in% names(ae_data))) {
    stop("Data must contain USUBJID and AEDECOD columns")
  }
  
  ae_terms <- unique(ae_data$AEDECOD)
  n_terms <- length(ae_terms)
  
  result <- expand.grid(AEDECOD1 = ae_terms, AEDECOD2 = ae_terms, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(USUBJID = map2(AEDECOD1, AEDECOD2, ~{
      if (.x == .y) return(character(0))
      subjects_x <-  ae_data %>% filter(AEDECOD == .x) %>% pull(USUBJID)
      subjects_y <-  ae_data %>% filter(AEDECOD == .y) %>% pull(USUBJID)
      intersect(subjects_x, subjects_y)
    }),
    n = map_dbl(USUBJID, length)) # Calculate n after getting USUBJID
  
  result %>%
    arrange(AEDECOD1, AEDECOD2)
}

create_plotly_colorscale <-  function() {
  ylord_colors <- brewer.pal(9, "YlOrRd")
  n <- length(ylord_colors)
  data.frame(
    z = seq(0, 1, length.out = n),
    col = ylord_colors
  )
}

create_patient_profile_plot <- function(data, selected_aes) {
  if (nrow(data) == 0) {
    return(plot_ly() %>% layout(title = "No data available"))
  }
  
  data_grouped <-  data %>%
    group_by(USUBJID, AEDECOD) %>%
    summarise(
      ASTDY = min(ASTDY),
      AENDY = max(AENDY),
      AESEV = paste(unique(AESEV), collapse = ", "),
      AESER = paste(unique(AESER), collapse = ", "),
      AEACN = paste(unique(AEACN), collapse = ", "),
      AEREL = paste(unique(AEREL), collapse = ", "),
      AEOUT = paste(unique(AEOUT), collapse = ", ")
    ) %>%
    ungroup()
  
  hover_text <-  with(data_grouped, paste(
    "Subject ID: ", USUBJID,
    "<br>AE: ", AEDECOD,
    "<br>Start Day: ", ASTDY,
    "<br>End Day: ", AENDY,
    "<br>Severity: ", AESEV,
    "<br>Serious: ", AESER,
    "<br>Action Taken: ", AEACN,
    "<br>Relationship: ", AEREL,
    "<br>Outcome: ", AEOUT
  ))
  
  plot <-  plot_ly() %>%
    add_segments(
      data = data_grouped,
      x = ~ASTDY,
      xend = ~AENDY,
      y = ~USUBJID,
      yend = ~USUBJID,
      color = ~AEDECOD,
      hoverinfo = "text",
      text = hover_text,
      showlegend = FALSE
    ) %>%
    add_markers(
      data = data_grouped,
      x = ~ASTDY,
      y = ~USUBJID,
      color = ~AEDECOD,
      marker = list(size = 8),
      hoverinfo = "text",
      text = hover_text,
      name = ~AEDECOD,
      legendgroup = ~AEDECOD
    ) %>%
    add_markers(
      data = data_grouped,
      x = ~AENDY,
      y = ~USUBJID,
      color = ~AEDECOD,
      marker = list(size = 8),
      hoverinfo = "text",
      text = hover_text,
      showlegend = FALSE,
      legendgroup = ~AEDECOD
    ) %>%
    layout(
      title = "Patient Profile: Adverse Event Timeline",
      xaxis = list(title = "Study Day"),
      yaxis = list(title = "Subject ID"),
      showlegend = TRUE,
      legend = list(
        title = list(text = if (!is.null(selected_aes)) "Selected AEs" else "Adverse Events"),
        traceorder = "normal"
      )
    )
  
  return(plot)
}

# Load raw AE dataset
adae <- read_ae_data("202502/DummyAEData.csv")
# adae <-  read_ae_data("DummyAEData.csv")

# Ensure AEDECOD is character to avoid factor mismatches
adae <- adae %>% mutate(AEDECOD = as.character(AEDECOD))

# Compute AE counts and co-occurrence matrix
ae_counts <-  count_ae_occurrences(adae)
ae_cooccurrence <- calculate_ae_cooccurrence(ae_counts)

# Ensure AEDECOD columns in ae_cooccurrence are also character
ae_cooccurrence <- ae_cooccurrence %>%
  mutate(AEDECOD1 = as.character(AEDECOD1),
         AEDECOD2 = as.character(AEDECOD2))

ui <-  page_fillable(
  title="AE Co-occurrence Analysis",
  card(
    plotlyOutput(outputId = "heatmap"),
    plotlyOutput(outputId = "patient_profile_plot")
  )
)

server <-  function(input, output, session) {
  selected_aes <- reactiveVal(NULL)
  
  output$heatmap <- renderPlotly({
    data <- ae_cooccurrence |> select(-USUBJID)
    
    data <-  data %>%
      mutate(n = ifelse(AEDECOD1 == AEDECOD2, NA, n))
    
    ae_wide <-  data %>%
      pivot_wider(names_from = AEDECOD2, values_from = n, values_fill = list(n = NA))
    
    labels <-  ae_wide$AEDECOD1
    ae_matrix <- as.matrix(ae_wide[, -1])
    
    selected <- selected_aes()
    
    hover_text <- matrix(
      paste0(
        "AE1: ", rep(labels, each = length(labels)), "<br>",
        "AE2: ", rep(labels, times = length(labels)), "<br>",
        "Count: ", ae_matrix
      ),
      nrow = length(labels)
    )
    
    # Create a matrix for cell borders
    border_matrix <-  matrix("", nrow = nrow(ae_matrix), ncol = ncol(ae_matrix))
    if (!is.null(selected) && length(selected) == 2) {
      row_index <- which(labels == selected[1])
      col_index <- which(labels == selected[2])
      border_matrix[row_index, col_index] <- "black"
      border_matrix[col_index, row_index] <- "black"
    }
    
    plot_ly(
      x = labels, 
      y = labels, 
      z = ae_matrix, 
      type = "heatmap",
      colorscale = create_plotly_colorscale(),
      colorbar = list(title = "Co-occurrence Count"),
      source = "heatmap_click",
      hoverinfo = "text",
      text = hover_text
    ) %>%
      layout(
        title = "AE Co-occurrence Heatmap",
        xaxis = list(title = "Adverse Event 1"),
        yaxis = list(title = "Adverse Event 2")
      ) %>%
      event_register("plotly_click") %>%
      add_annotations(
        x = rep(labels, each = length(labels)),
        y = rep(labels, times = length(labels)),
        text = "",
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        bordercolor = as.vector(border_matrix),
        borderwidth = 2
      )
  })
  
  observeEvent(event_data("plotly_click", source = "heatmap_click"), {
    event <-  event_data("plotly_click", source = "heatmap_click")
    
    if (!is.null(event) && !is.null(event$x) && !is.null(event$y)) {
      ae1 <- as.character(event$x)
      ae2 <- as.character(event$y)
      
      selected_aes(c(ae1, ae2))
    }
  })
  
  filtered_data <- reactive({
    selected <- selected_aes()

    if (is.null(selected)) {
      return(adae)
    } else {
      subjects_with_both_aes <- ae_cooccurrence %>%
        dplyr::filter(AEDECOD1 == selected[1] & AEDECOD2 == selected[2]) %>%
        pull(USUBJID) %>%
        unlist()

      return(adae %>% 
               filter(USUBJID %in% subjects_with_both_aes & AEDECOD %in% selected) %>%
               arrange(USUBJID, AEDECOD) %>%
               mutate(ASTDY = as.numeric(ASTDY),
                      AENDY = as.numeric(AENDY)))
    }
  })

  output$patient_profile_plot <- renderPlotly({
    create_patient_profile_plot(filtered_data(), selected_aes())
  })
}

shinyApp(ui, server)