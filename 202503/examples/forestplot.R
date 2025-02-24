# Load required libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(patchwork)
library(stringr)

# Create sample datasets with updated treatment names
axcont <-  tibble(
  BLWEEK = rep(0, 9),
  LMWEEK = rep(68, 9),
  MGROUP = rep("Treatment A", 9),
  CGROUP = rep(c("Treatment B", "Treatment C", "Placebo"), each = 3),
  POPVAR = rep("FASFL", 9),
  ESTIMAND = rep("TREATMENT POLICY", 9),
  CHGTYPE = rep("PCHG", 9),
  TESTHYP = rep("SUPERIORITY", 9),
  PARAMCD = rep("WEIGHT", 9),
  MODEL = rep(c("ANCOVA - RD-MI*", "ANCOVA - J2R-MI**", "MMRM**"), 3),
  CONTR = c(-2.5, -2.3, -2.1, -3.2, -3.0, -2.8, -10.5, -10.0, -9.5),
  CONTRLL = c(-3.5, -3.3, -3.1, -4.2, -4.0, -3.8, -11.5, -11.0, -10.5),
  CONTRUL = c(-1.5, -1.3, -1.1, -2.2, -2.0, -1.8, -9.5, -9.0, -8.5)
)

mdparam <- tibble(
  PARAMCD = "WEIGHT",
  PARAM = "Body weight",
  AVALLB = "Body weight (%)",
  PCHGLB = "Body weight (%) change",
  STUDYID = "EXAMPLE",
  FAVCAGRI = "NEGATIVE"
)

# Update mdsymbol to match the new contrast_name levels
mdsymbol <- tibble(
  PLOTID = rep("TRTPF", 3),
  VALUE = c("Treatment A-Treatment B", 
            "Treatment A-Treatment C", 
            "Treatment A-Placebo"),
  color = c("#00857C", "#6ECEB2", "#4E84C4"),
  shape = c(16, 17, 15),
  size = c(2, 2, 2),
  linetype = c(1, 1, 1)
)

# Helper function to build parameter annotations
build_parameter_annotations <- function(mdparam) {
  param_data <- mdparam 
  
  all_negative <- all(param_data$FAVCAGRI == "NEGATIVE")
  all_positive <- all(param_data$FAVCAGRI == "POSITIVE")
  
  if (all_negative || all_positive) {
    return(NULL)
  } else {
    param_annotations <- list()
    
    for (param in param_data$AVALLB) {
      param_info <- param_data %>% filter(AVALLB == param)
      
      if (nrow(param_info) > 0) {
        if (param_info$FAVCAGRI == "NEGATIVE") {
          param_annotations[[param]] <- list(
            left = "Favours \nTreatment A",
            right = "Favours \nComparator"
          )
        } else if (param_info$FAVCAGRI == "POSITIVE") {
          param_annotations[[param]] <- list(
            left = "Favours \nComparator",
            right = "Favours \nTreatment A"
          )
        }
      }
    }
    
    return(param_annotations)
  }
}

# Main function to create forest plot
create_forest_plot <- function(
    outname, 
    in_data, 
    values = c("CONTR", "CONTRLL", "CONTRUL"), 
    infilter, 
    popfilter, 
    paramfilter, 
    in_mdparam = mdparam, 
    in_mdsymbol = mdsymbol, 
    is_ratio = FALSE, 
    include_model = FALSE,
    plot_widths = c(1, 0.5, 2.5, 0.5, 1),
    Fg.Width = NULL,
    Fg.Height = NULL,
    debug = FALSE 
) {
  # Set defaults
  if (is_ratio) {
    sp <- "/"
    x_label_end <-  "ratio"
    table_label <- "ETR [95% CI]"
    intercept <- 1
  } else {
    sp <- "-"
    x_label_end <- "difference"
    table_label <- "ETD [95% CI]"
    intercept <- 0
  }
  
  trt <- "Treatment A"
  
  # Handle metadata
  mdsymbol <- in_mdsymbol
  
  mdparam <- in_mdparam %>% 
    filter({{paramfilter}})
  
  # Filter and setup data
  processed_data <-  in_data %>%
    filter(
      BLWEEK == 0,
      LMWEEK == 68,
      MGROUP == trt,
      {{popfilter}},
      {{infilter}},
      {{paramfilter}}
    ) %>% 
    mutate(
      contrast_name = factor(paste0(MGROUP, sp, CGROUP),
                             levels = c(paste0(trt, sp, "Treatment B"),
                                        paste0(trt, sp, "Treatment C"),
                                        paste0(trt, sp, "Placebo")))
    ) %>%
    left_join(mdparam, by = "PARAMCD") %>%
    mutate(
      AVALLB = case_when(
        CHGTYPE == "PCHG" ~ gsub("\\(\\w+\\)","\\(\\%\\)", AVALLB),
        TRUE ~ AVALLB),
      alabel = case_when(
        include_model ~ paste0(AVALLB,", \n", MODEL),
        TRUE ~ AVALLB)
    ) %>% 
    select(alabel, contrast_name, all_of(values))
  
  # Order by placebo contrast
  param_order <-  processed_data %>%
    filter(grepl("placebo", contrast_name, ignore.case = TRUE)) %>%
    arrange(desc(!!sym(values[1]))) %>%
    pull(alabel)
  
  # Convert alabel to an ordered factor in processed_data
  processed_data <-  processed_data %>%
    mutate(alabel = factor(alabel, levels = param_order, ordered = TRUE))
  
  # Calculate y positions for parameters and groups
  data <-  processed_data %>%
    group_by(alabel) %>%
    mutate(param_y = as.numeric(alabel) * 4 - 2) %>%
    ungroup() %>%
    group_by(alabel, contrast_name) %>%
    mutate(group_y = cur_group_id() + param_y - 2)
  
  # Create a separate dataframe for parameter labels
  param_labels <-  data %>%
    group_by(alabel) %>%
    summarise(param_y = mean(group_y))
  
  # Determine x-axis limits
  x_min <-  pmin(min(data[[values[2]]]) * 1.05, min(data[[values[2]]]) * 0.95)
  x_max <- pmax(max(data[[values[3]]]) * 1.1, 0.1)
  
  # Determine y-axis limits with a small buffer
  y_min <- min(data$group_y) - 1
  y_max <- max(data$group_y) + 2 # Added 1 extra buffer for table label
  
  # Define debug theme
  debug_theme <- if(debug) {
    theme(plot.background = element_rect(colour = "red", fill = NA, size = 1))
  } else {
    theme()
  }
  
  # Create the forest plot
  p_forest <- ggplot(data, aes(y = group_y, color = contrast_name, shape = contrast_name)) +
    geom_vline(xintercept = intercept, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(x = !!sym(values[1]), xmin = !!sym(values[2]), xmax = !!sym(values[3])),
                    size = 1, fatten = 2) +
    geom_errorbar(aes(xmin = !!sym(values[2]), xmax = !!sym(values[3])), width = 0.2) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                         ends = "both")),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    labs(x = paste("Estimated treatment", x_label_end)) +
    scale_color_manual(values = setNames(mdsymbol$color, mdsymbol$VALUE)) +
    scale_shape_manual(values = setNames(mdsymbol$shape, mdsymbol$VALUE)) +
    debug_theme
  
  # Build parameter annotations
  parameter_annotations <- build_parameter_annotations(mdparam)
  
  # Determine if we should use bottom annotations or parameter-specific annotations
  use_bottom_annotations <- is.null(parameter_annotations)
  
  # Create the "Favours" annotation plots
  p_favours_left <- ggplot() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    debug_theme
  
  p_favours_right <- ggplot() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    debug_theme
  
  if (use_bottom_annotations) {
    # Add global annotations
    p_favours_left <- p_favours_left +
      annotate("text", x = 0.5, y = y_min, label = "Favours \nTreatment A", 
               hjust = 0.5, vjust = 0, size = 3)
    
    p_favours_right <- p_favours_right +
      annotate("text", x = 0.5, y = y_min, label = "Favours \nComparator", 
               hjust = 0.5, vjust = 0, size = 3)
  } else {
    # Add parameter-specific annotations
    for (param in names(parameter_annotations)) {
      anno <- parameter_annotations[[param]]
      y_pos <- param_labels$param_y[param_labels$alabel == param]
      
      if (!is.null(anno$left)) {
        p_favours_left <- p_favours_left +
          annotate("text", x = 0.5, y = y_pos, label = anno$left, 
                   hjust = 0.5, vjust = 0.5, size = 3)
      }
      if (!is.null(anno$right)) {
        p_favours_right <- p_favours_right +
          annotate("text", x = 0.5, y = y_pos, label = anno$right, 
                   hjust = 0.5, vjust = 0.5, size = 3)
      }
    }
  }
  
  # Dynamically set the fontsize based on the number of parameters
  fontsize <- if (nrow(param_labels) >= 7) 2 else 3
  
  # Create the table plot
  p_table <- ggplot(data, aes(y = group_y, color = contrast_name)) +
    geom_text(aes(x = 0.5, label = sprintf("%.2f [%.2f ; %.2f]", !!sym(values[1]), !!sym(values[2]), !!sym(values[3]))),
              hjust = 0.5, size = fontsize) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_void() +
    theme(plot.margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
          legend.position = "none") +
    annotate("text", x = 0.5, y = y_max, label = table_label, 
             hjust = 0.5, vjust = 1, size = 2, fontface = "bold") +
    scale_color_manual(values = setNames(mdsymbol$color, mdsymbol$VALUE)) +
    debug_theme
  
  # Function to wrap text
  wrap_text <- function(text, width = 20) {
    wrapped <- str_wrap(text, width = width)
  }
  
  # Create the parameter labels plot with wrapped text
  param_labels$wrapped_label <- sapply(param_labels$alabel, wrap_text)
  
  p_labels <- ggplot(param_labels, aes(y = param_y, x = 0.1)) +
    geom_text(aes(label = wrapped_label), hjust = 0, lineheight = 0.8, size = fontsize) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    debug_theme
  
  # Combine plots using patchwork
  p_combined <- p_labels + p_favours_left + p_forest + p_favours_right + p_table +
    plot_layout(widths = plot_widths)
  
  # Print the combined plot
  print(p_combined)
  
  # Uncomment these lines if you want to save the plot and data
  # ggsave(paste0(outname, ".png"), p_combined, width = Fg.Width, height = Fg.Height)
  # saveRDS(p_combined[[1]]$data, paste0(outname, ".rds"))
  # writexl::write_xlsx(p_combined[[1]]$data, paste0(outname, ".xlsx"))
}

# Call the function to create the forest plot
create_forest_plot(
  outname = "fforestbwit_example",
  in_data = axcont,
  popfilter = POPVAR == "FASFL",
  infilter = ESTIMAND == "TREATMENT POLICY" &
    CHGTYPE == "PCHG" &
    TESTHYP == "SUPERIORITY",
  paramfilter = PARAMCD == "WEIGHT",
  in_mdparam = mdparam,
  in_mdsymbol = mdsymbol,
  include_model = TRUE
)