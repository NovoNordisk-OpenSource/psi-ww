# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)

# Create mock datasets
set.seed(123)

# Create ADSL dataset
create_adsl <-  function(n) {
  data.frame(
    USUBJID = paste0("SUBJ", 1:n),
    TR01AG1F = sample(c("Treatment A", "Treatment B", "Placebo"), n, replace = TRUE),
    SAFFL = sample(c("Y", "N"), n, replace = TRUE, prob = c(0.9, 0.1)),
    AAGE = round(rnorm(n, mean = 50, sd = 10)),
    BMIBL = round(rnorm(n, mean = 28, sd = 5), 1),
    HBA1CBL = round(rnorm(n, mean = 7, sd = 1), 1),
    HB1CCBL = round(rnorm(n, mean = 53, sd = 10)),
    FPGBL = round(rnorm(n, mean = 8, sd = 2), 1),
    FPGCBL = round(rnorm(n, mean = 144, sd = 36)),
    DIABDURY = round(runif(n, 1, 20), 1),
    HISCVFL = sample(c("Y", "N"), n, replace = TRUE),
    STDNM = sample(paste("ONWARDS", 1:6), n, replace = TRUE)
  )
}

# Create ADVS dataset
create_advs <- function(adsl) {
  adsl %>%
    mutate(
      SYSBP = round(rnorm(n(), mean = 130, sd = 10)),
      DIABP = round(rnorm(n(), mean = 80, sd = 5)),
      PARAMCD = rep(c("SYSBP", "DIABP"), each = n() / 2),
      ANELFL = "Y",
      ABLFL = "Y",
      AVAL = ifelse(PARAMCD == "SYSBP", SYSBP, DIABP)
    ) %>%
    select(USUBJID, PARAMCD, AVAL, ANELFL, ABLFL)
}

# Create mock datasets
adsl <- create_adsl(1000)
advs <- create_advs(adsl)

# Helper functions
rmin <- function(x, na.rm = TRUE) {if (any(!is.na(x))) {min(x, na.rm = TRUE)} else {NA_real_}}
rmax <- function(x, na.rm = TRUE) {if (any(!is.na(x))) {max(x, na.rm = TRUE)} else {NA_real_}}

# Columns to present
out_cols <- c("Age (years)" = "AAGE",
              "BMI (kg/m^2)" = "BMIBL",
              "HbA1c (%)" = "HBA1CBL",
              "HbA1c (mmol/mol)" = "HB1CCBL",
              "FPG (mmol/L)" = "FPGBL",
              "FPG (mg/dL)" = "FPGCBL",
              "Duration of diabetes (years)" = "DIABDURY",
              "Systolic Blood Pressure (mmHg)" = "SYSBPBL",
              "Diastolic Blood Pressure (mmHg)" = "DIABPBL",
              "History of established cardiovascular disease" = "HISCVFL")

out_cols_n <-  out_cols[!grepl("FL$", out_cols)]

tcharsum <- function(outname, popfilter = TRUE, add_groups = NULL) {
  # Process ADVS data
  advs_processed <- advs %>% 
    filter(PARAMCD %in% c("SYSBP", "DIABP") & ANELFL == "Y" & ABLFL == "Y") %>% 
    select(USUBJID, PARAMCD, AVAL) %>% 
    pivot_wider(names_from = "PARAMCD", values_from = "AVAL") %>% 
    select(USUBJID, SYSBPBL = SYSBP, DIABPBL = DIABP)
  
  # Process ADSL data
  adsl_processed <-  adsl %>% 
    filter(!!rlang::parse_expr(popfilter)) %>% 
    left_join(advs_processed, by = "USUBJID") %>% 
    mutate(STDNMF = factor(STDNM, levels = paste("ONWARDS", c(1,3,5,2,4,6))),
           STDNMN = match(STDNM, levels(STDNMF))) %>% 
    mutate(TR01AG1 = TR01AG1F)  # For compatibility with the original code
  
  # Derive totals
  totals <-  adsl_processed %>% 
    group_by(TR01AG1F) %>% 
    {if (!is.null(add_groups)) group_by(., across(all_of(add_groups)), .add = TRUE) else .} %>%
    summarise(`Number of subjects` = n_distinct(USUBJID),
              .groups = "keep")
  
  out_groups <-  totals %>% group_vars()
  
  # Main summary
  summary_data <-  adsl_processed %>% 
    mutate(HISCVFL = ifelse(HISCVFL == "Y", TRUE, FALSE)) %>% 
    select(STDNMF, STDNMN, USUBJID, TR01AG1F, all_of(unname(out_cols))) %>% 
    group_by(TR01AG1F) %>% 
    {if (!is.null(add_groups)) group_by(., across(all_of(add_groups)), .add = TRUE) else .} %>% 
    summarise(
      across(all_of(unname(out_cols_n)),
             list(N = ~sum(!is.na(.x)),
                  Mean = ~mean(.x, na.rm = TRUE), 
                  Median = ~median(.x, na.rm = TRUE), 
                  SD = ~sd(.x, na.rm = TRUE),
                  Min = ~rmin(.x), 
                  Max = ~rmax(.x))),
      across(matches("FL$"), 
             list(Yc = ~sum(.x == TRUE, na.rm = TRUE), 
                  PYc = ~mean(.x == TRUE, na.rm = TRUE) * 100,
                  Nc = ~sum(.x == FALSE, na.rm = TRUE), 
                  PNc = ~mean(.x == FALSE, na.rm = TRUE) * 100,
                  NLOG = ~sum(!is.na(.x)), 
                  NLOGP = ~100)),
      .groups = "drop"
    ) %>% 
    pivot_longer(contains("_"), names_to = c("PARAM", ".value"), names_pattern = "(.*)_(.*)") %>% 
    mutate(PARAMF = factor(PARAM, levels = unname(out_cols)))
  
  # Format the summary data
  formatted_summary <-  summary_data %>%
    mutate(
      `N` = sprintf("%.0f", N),
      `Mean (SD)` = sprintf("%.1f (%.1f)", Mean, SD),
      `Median` = sprintf("%.1f", Median),
      `Min ; Max` = sprintf("%.1f ; %.1f", Min, Max),
      `N (%)` = sprintf("%.0f (%.1f)", NLOG, NLOGP),
      `Yes` = sprintf("%.0f (%.1f)", Yc, PYc),
      `No` = sprintf("%.0f (%.1f)", Nc, PNc)
    ) %>%
    select(all_of(c(out_groups, "PARAM", "N", "Mean (SD)", "Median", "Min ; Max", "N (%)", "Yes", "No")))
  
  # Add filter information to system footnote
  filter_info <- glue("adsl.xpt; variable(s): {paste(unname(out_cols), collapse = ', ')}; criteria: {popfilter}")
  
  # Return the formatted summary
  list(
    summary = formatted_summary,
    totals = totals,
    filter_info = filter_info
  )
}

# Example usage
result <- tcharsum("tslblcharsumsas", popfilter = "SAFFL == 'Y'", add_groups = c("STDNMF"))

# Print the results
print(result$summary)
print(result$totals)
print(result$filter_info)
