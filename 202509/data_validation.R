# Data Validation Script
# Compare current data against publication expectations

library(dplyr)

# Read the data
data <- read.csv("LSM_Score.csv")

cat("=== DATA VALIDATION REPORT ===\n\n")

# Check sample sizes
cat("1. SAMPLE SIZES:\n")
sample_sizes <- data %>%
    group_by(Group) %>%
    summarise(n = n())
print(sample_sizes)

cat("\nExpected from publication: Both groups should have n=31\n")
cat(
    "Issue: ",
    ifelse(all(sample_sizes$n == 31), "✓ CORRECT", "✗ MISMATCH"),
    "\n\n"
)

# Check baseline and EOS means
cat("2. MEAN LSM SCORES:\n")
means <- data %>%
    group_by(Group) %>%
    summarise(
        Mean_Baseline = round(mean(Baseline_kPa), 2),
        Mean_EOS = round(mean(EOS_kPa), 2),
        Mean_Change = round(mean(EOS_kPa - Baseline_kPa), 2),
        Pct_Change = round(
            ((mean(EOS_kPa) - mean(Baseline_kPa)) / mean(Baseline_kPa)) * 100,
            1
        )
    )
print(means)

cat("\nExpected from publication Figure 3:\n")
cat("  LIV.52 DS: Baseline ~7.4-7.5 kPa, EOS ~6.0-6.2 kPa (decrease)\n")
cat("  Placebo:   Baseline ~7.4-7.5 kPa, EOS ~7.4-7.6 kPa (minimal change)\n\n")

# Check steatosis distribution
cat("3. STEATOSIS OUTCOMES:\n")
steatosis <- data %>%
    group_by(Group, Steatosis) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(Group) %>%
    mutate(percentage = round((count / sum(count)) * 100, 1)) %>%
    arrange(Group, Steatosis)
print(steatosis)

cat("\nExpected from publication Figure 4:\n")
cat(
    "  LIV.52 DS: Grade improvement ~60%, No Steatosis ~30%, Deteriorate ~10%\n"
)
cat(
    "  Placebo:   Grade improvement ~60%, No Steatosis ~6%, Deteriorate ~34%\n\n"
)

# Check individual data points
cat("4. CHECKING FOR ANOMALIES:\n")
cat("Records where treatment INCREASED LSM (unexpected for LIV.52 DS):\n")
increased <- data %>%
    filter(Group == "LIV.52 DS", EOS_kPa > Baseline_kPa) %>%
    select(Group, Baseline_kPa, EOS_kPa, Steatosis)
print(increased)

cat("\nRecords where placebo DECREASED LSM substantially:\n")
decreased <- data %>%
    filter(Group == "Placebo", EOS_kPa < Baseline_kPa - 2) %>%
    select(Group, Baseline_kPa, EOS_kPa, Steatosis)
print(decreased)

cat("\n=== SUMMARY ===\n")
cat("Based on the publication figures, the data should show:\n")
cat("1. Equal sample sizes (n=31 each group)\n")
cat("2. LIV.52 DS shows clear reduction in LSM scores\n")
cat("3. Placebo shows minimal change or slight increase\n")
cat("4. More 'No Steatosis' outcomes in LIV.52 DS group\n")
cat("5. More 'Deteriorate' outcomes in Placebo group\n")
