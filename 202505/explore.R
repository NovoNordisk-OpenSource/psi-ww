data <- read.csv("202505/WW_GCdose.csv",
    header = TRUE, sep = ",", quote = "\"",
    dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE
)

names(data)
# [1] "SUBJID"  "ASTDY"   "AVAL1"   "AVISITN" "AVISIT"  "AVAL2"

# Split dataset into two parts

# First dataset: SUBJID, ASTDY, AVAL1
data1 <- data[, c("SUBJID", "ASTDY", "AVAL1")]

# Second dataset: SUBJID, AVISIT, AVISITN, AVAL2
data2 <- data[, c("SUBJID", "AVISIT", "AVISITN", "AVAL2")]

# Remove duplicate rows from data2
data2 <- unique(data2)

# Optional: preview the new datasets
head(data1)
head(data2)

library(dplyr)
library(tidyr)
library(ggplot2)

# data1 is assumed to be already read with columns: SUBJID, ASTDY, AVAL1
# Create a vector of unique subjects (panels)
subjects <- unique(data1$SUBJID)

# Create a new data frame that is a Cartesian join: for each panel (SUBJID = panel),
# attach all rows of data1; then flag rows that come from the panel subject.
panel_data <- expand.grid(panel = subjects, dummy = 1) %>%
    inner_join(data1 %>% mutate(dummy = 1), by = "dummy") %>%
    select(-dummy) %>%
    mutate(highlight = ifelse(SUBJID == panel, "current", "other"))

# Create the plot:
ggplot() +
    # Draw the other subjects first in gray
    geom_line(
        data = panel_data %>% filter(highlight == "other"),
        aes(x = ASTDY, y = AVAL1, group = SUBJID),
        color = "gray", alpha = 1
    ) +
    # Draw the current subject on top in blue
    geom_line(
        data = panel_data %>% filter(highlight == "current"),
        aes(x = ASTDY, y = AVAL1, group = SUBJID),
        color = "blue", size = 1
    ) +
    facet_wrap(~panel, ncol = 3) +
    labs(
        title = "Line Plot per Subject",
        x = "ASTDY", y = "AVAL1"
    ) +
    theme_minimal()

# Create a vector of unique subjects (panels)

panel_data <- expand.grid(panel = subjects, dummy = 1) %>%
    inner_join(data2 %>% mutate(dummy = 1), by = "dummy") %>%
    select(-dummy) %>%
    mutate(highlight = ifelse(SUBJID == panel, "current", "other"))

# Create the plot:
ggplot() +
    # Draw the other subjects first in gray
    geom_line(
        data = panel_data %>% filter(highlight == "other"),
        aes(x = AVISITN, y = AVAL2, group = SUBJID),
        color = "gray", alpha = 1
    ) +
    # Draw the current subject on top in blue
    geom_line(
        data = panel_data %>% filter(highlight == "current"),
        aes(x = AVISITN, y = AVAL2, group = SUBJID),
        color = "blue", size = 1
    ) +
    facet_wrap(~panel, ncol = 3) +
    labs(
        title = "Line Plot per Subject",
        x = "AVISITN", y = "AVAL2"
    ) +
    theme_minimal()
