# Load required libraries
library(ggplot2)
library(dplyr)
library(ggalluvial)


# Read the data
data <- read.csv("202504/WWW_APR2025.csv", stringsAsFactors = FALSE)

# Create a factor for AVALC ordered by AVAL (reversed for plotting)
data <-  data %>%
  mutate(AVALC = factor(AVALC, levels = c("High Response", "Response", "Low Response", "No Response")))

# Define a custom color palette (reversed to match the new factor order)
response_colors <-  c("High Response" = "#66CC66",
                      "Response" = "#99CC99",
                      "Low Response" = "#FFCC99",
                      "No Response" = "#FF9999")

ggplot(data,
       aes(x = AVISITN, stratum = AVALC, alluvium = USUBJID,
           fill = AVALC, label = AVALC)) +
  scale_fill_manual(values = response_colors) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  # geom_text(stat = "stratum", aes(label = after_stat(n)), size = 2) +
  facet_wrap(vars(TRT)) +
  scale_x_continuous(breaks = unique(data$AVISITN), 
                     name = "Visit") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = "State occupancy over time by treatment",
    y = "Count",
    fill = "Response Level"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Save the plot
ggsave("202504/response_over_time.png", width = 12, height = 8, dpi = 200)

