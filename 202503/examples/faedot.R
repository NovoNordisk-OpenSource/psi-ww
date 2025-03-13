# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# Create sample dataset
set.seed(123)

# Function to generate random AE data
generate_ae_data <-  function(n_subjects, treatment) {
  ae_list <- c("Nausea", "Vomiting", "Headache", "Dizziness", "Fatigue", "Diarrhea", "Constipation", "Insomnia")
  
  data.frame(
    USUBJID = paste0(treatment, "_", 1:n_subjects),
    TRTAF = treatment,
    AEDECOD = sample(ae_list, n_subjects, replace = TRUE, 
                     prob = c(0.3, 0.2, 0.15, 0.1, 0.1, 0.05, 0.05, 0.05)),
    SAFFL = "Y",
    ANL02FL = "Y",
    ONTRDSFY = sample(30:365, n_subjects, replace = TRUE)
  )
}

# Generate data for each treatment group
adae_a <- generate_ae_data(100, "Treatment A")
adae_b <- generate_ae_data(100, "Treatment B")
adae_p <- generate_ae_data(100, "Placebo")

# Combine all treatment groups
adae <- rbind(adae_a, adae_b, adae_p)

# Create ADSL dataset
adsl <- adae %>%
  group_by(USUBJID, TRTAF) %>%
  summarise(SAFFL = first(SAFFL),
            ONTRDSFY = first(ONTRDSFY),
            .groups = "drop")

# Create mdsymbol dataset
mdsymbol <-  data.frame(
  PLOTID = "TRTAF",
  VALUE = c("Treatment A", "Treatment B", "Placebo"),
  color = c("#00857C", "#6ECEB2", "#4E84C4"),
  shape = c(16, 17, 15)
)

# Derive total counts for numerator
adsl_summary <- adsl %>%
  filter(SAFFL == "Y") %>%
  group_by(TRTAF) %>%
  summarise(NALL = n(),
            EALL = sum(ONTRDSFY))

# Derive the percentage of each AEDECOD by TRTAF
stats <-  adae %>%
  filter(!is.na(AEDECOD), SAFFL == "Y", ANL02FL == "Y") %>%
  select(USUBJID, TRTAF, AEDECOD) %>%
  group_by(TRTAF, AEDECOD) %>%
  summarise(
    N = n_distinct(USUBJID),
    E = n(),
    .groups = "drop"
  ) %>%
  left_join(adsl_summary, by = "TRTAF") %>%
  mutate(PCT = (N / NALL) * 100,
         R = (E / EALL) * 100) %>%
  tidyr::complete(nesting(TRTAF, AEDECOD), fill = list(N = 0, PCT = 0))

# Create a sort order for the AEDECOD based on PCT and make it a factor
ord <-  stats %>%
  filter(TRTAF == "Treatment A") %>%
  arrange(desc(PCT), AEDECOD) %>%
  pull(AEDECOD)

# Check if we are missing any AEs and add them to the order
misord <-  stats %>%
  arrange(desc(PCT), AEDECOD) %>%
  pull(AEDECOD) %>%
  unique() %>%
  setdiff(ord)

ord <-  c(ord, misord)

# Use factor to set order of labels
stats_ord <- stats %>%
  group_by(AEDECOD) %>%
  mutate(PCT = round(PCT, 1)) %>%
  filter(any(PCT >= 5)) %>%
  ungroup() %>%
  mutate(aelabel = factor(AEDECOD, levels = rev(ord), labels = rev(ord)))

# Plot --------------------------------------------------------------------------
# plot with rotated axes using geom_point and AEDECOD as the y axis
nn_plot <- ggplot(stats_ord) +
  geom_point(aes(x = PCT / 100, y = aelabel, color = TRTAF, shape = TRTAF),
             size = 1.8
  ) +
  theme_bw() +
  scale_x_continuous(
    limits = c(0, 0.75),
    breaks = c(0, 0.25, 0.50, 0.75),
    labels = scales::percent
  ) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = setNames(mdsymbol$color, mdsymbol$VALUE)) +
  scale_shape_manual(values = setNames(mdsymbol$shape, mdsymbol$VALUE))

# Prepare data for the table plot
table_data <-  stats_ord %>%
  select(TRTAF, aelabel, N, PCT, R) %>%
  mutate(
    N = as.integer(N),
    P = round(PCT, 1),
    R = round(R, 2)
  ) %>%
  pivot_longer(cols = c(N, P, R), names_to = "metric", values_to = "Value") %>%
  mutate(metric = factor(metric, levels = c("N", "P", "R")))

# Create the table plot
table_plot <-
  ggplot(table_data, aes(x = metric, y = aelabel, label = Value)) +
  geom_text(size = 3, hjust = 0.5, vjust = 0.5) +
  facet_wrap(~ TRTAF, ncol = length(unique(table_data$TRTAF)), strip.position = "top") +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, face = "bold"),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 9),
    strip.placement = "outside",
    panel.grid.major.y = element_line(color = "lightgray", size = 0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(20, 5.5, 5.5, -3),  # Removed right margin
    strip.background = element_rect(fill = "white", color = "black", size = 0.5),
    panel.spacing.x = unit(0, "lines")
  ) 

# Modify the existing plot
plot_modified <- nn_plot +
  theme(plot.margin = margin(5.5, 0, 5.5, 5.5),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Combine the plots using patchwork
final_plot <- plot_modified + table_plot +
  plot_layout(widths = c(4, 3))  # Adjust the ratio as needed

# Display the final plot
print(final_plot)
