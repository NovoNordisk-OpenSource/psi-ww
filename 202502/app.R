library(shiny)
library(bslib)
library(ggplot2)
library(tools)
library(plotly)
library(dplyr)
library(purrr)
library(DT)
library(readr)
library(tidyr)

# Read the CSV file
read_ae_data <-  function(file_path) {
  read_csv(file_path)
}

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
    mutate(USUBJID = map2(AEDECOD1, AEDECOD2, ~{
      if (.x == .y) return(character(0))
      subjects_x <- ae_data %>% filter(AEDECOD == .x) %>% pull(USUBJID)
      subjects_y <- ae_data %>% filter(AEDECOD == .y) %>% pull(USUBJID)
      intersect(subjects_x, subjects_y)
    }),
    n = map_dbl(USUBJID, length)) # Calculate n after getting USUBJID
  
  result %>%
    arrange(AEDECOD1, AEDECOD2)
}

# Load raw AE dataset
# adae <- read_ae_data("202502/DummyAEData.csv")
adae <- read_ae_data("DummyAEData.csv")

# Ensure AEDECOD is character to avoid factor mismatches
adae <- adae %>% mutate(AEDECOD = as.character(AEDECOD))

# Compute AE counts and co-occurrence matrix
ae_counts <- count_ae_occurrences(adae)
ae_cooccurrence <- calculate_ae_cooccurrence(ae_counts)

# Ensure AEDECOD columns in ae_cooccurrence are also character
ae_cooccurrence <- ae_cooccurrence %>%
  mutate(AEDECOD1 = as.character(AEDECOD1),
         AEDECOD2 = as.character(AEDECOD2))

ui <- page_fillable(
  title="AE Co-occurrence Analysis",
  card(
    plotlyOutput(outputId = "heatmap"),
    dataTableOutput(outputId = "details_table")
  )
)

server <- function(input, output, session) {
  # Reactive value to store selected AEs
  selected_aes <- reactiveVal(NULL)
  
  # Render Plotly heatmap
  output$heatmap <- renderPlotly({
    data <- ae_cooccurrence |> select(-USUBJID)
    
    # Set diagonal to NA for clarity
    data <- data %>%
      mutate(n = ifelse(AEDECOD1 == AEDECOD2, NA, n))
    
    # Reshape for heatmap
    ae_wide <- data %>%
      pivot_wider(names_from = AEDECOD2, values_from = n, values_fill = list(n = NA))
    
    labels <- ae_wide$AEDECOD1
    ae_matrix <- as.matrix(ae_wide[, -1])
    
    plot_ly(
      x = labels, 
      y = labels, 
      z = ae_matrix, 
      type = "heatmap",
      colorscale = "YlOrRd",
      colorbar = list(title = "Co-occurrence Count"),
      source = "heatmap_click"
    ) %>%
      layout(
        title = "AE Co-occurrence Heatmap",
        xaxis = list(title = "Adverse Event 1"),
        yaxis = list(title = "Adverse Event 2")
      )
  })
  
  # Observe click event on heatmap
  observeEvent(event_data("plotly_click", source = "heatmap_click"), {
    event <- event_data("plotly_click", source = "heatmap_click")
    
    if (!is.null(event) && !is.null(event$x) && !is.null(event$y)) {
      ae1 <- as.character(event$x)
      ae2 <- as.character(event$y)
      
      # Update selected AEs
      selected_aes(c(ae1, ae2))
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    selected <- selected_aes()

    if (is.null(selected)) {
      return(adae)
    } else {
      # Find subjects who experienced both AEs
      subjects_with_both_aes <- ae_cooccurrence %>%
        dplyr::filter(AEDECOD1 == selected[1] & AEDECOD2 == selected[2]) %>%
        pull(USUBJID) %>%
        unlist()

      # Return all AE records for these subjects
      return(adae %>% 
               filter(USUBJID %in% subjects_with_both_aes & AEDECOD %in% selected) %>%
               arrange(USUBJID, AEDECOD)) 
    }
  })
  
  # Render filtered table
  output$details_table <- renderDataTable({
    datatable(
      filtered_data()%>%
        dplyr::select(USUBJID, AETERM, AELLT, AEDECOD, AEHLT, AEHLGT, AEBODSYS, AESOC, AESEV, AESER, AEACN, AEREL, AEOUT, LDRELTM),
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        scrollY = "400px"
      ),
      filter = "top"
    )
  })
}

shinyApp(ui, server)