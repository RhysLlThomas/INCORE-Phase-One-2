# =============================================================================
# find_reference_condition.R
# -----------------------------------------------------------------------------
# Optional one-off helper. Reports the most common PRIMARY (principal-diagnosis)
# conditions and condition families in the cleaned data. It informed the choice
# of the single reference condition for the split models: "lri", chosen on
# England, is already set in 07_run_split_regressions_h2o.R and must be the
# same in every country -- you do NOT need to run this, but it is kept so the
# choice can be checked against your own data.
#
# The ranking is by primary condition (is_primary == 1), NOT any-position
# prevalence -- a condition common as a comorbidity (e.g. hypertension) can be
# rare as a principal diagnosis and would make a thin, unrepresentative baseline.
#
# Run from the project root. Reads the 02 output; writes both full rankings to
# maps/ and prints the top 5 of each to the console.
# =============================================================================

rm(list = ls())
library(arrow)
library(dplyr)

# Setting input folder
indir <- file.path("data", "02_cleaned_data", "cleaned_data.parquet")

# Condition -> family map, restricted to the modelled set (no _NEC, as in 03)
condition_details <- read_feather(file.path("maps", "condition_details.feather")) %>%
  filter(!endsWith(condition, "_NEC")) %>%
  select(condition, family)

# Counting admissions by their primary condition. is_primary marks the single
# principal-diagnosis row per admission (set by get_primary_condition in 02), so
# one row per admission enters the count.
primary_counts <- open_dataset(indir) %>%
  filter(is_primary == 1) %>%
  group_by(condition) %>%
  summarise(n_admissions = n()) %>%
  collect() %>%
  inner_join(condition_details, by = "condition") %>%
  arrange(desc(n_admissions)) %>%
  mutate(share = n_admissions / sum(n_admissions))

# Same, by the family of the primary condition
family_counts <- primary_counts %>%
  group_by(family) %>%
  summarise(n_admissions = sum(n_admissions), .groups = "drop") %>%
  arrange(desc(n_admissions)) %>%
  mutate(share = n_admissions / sum(n_admissions))

# Saving the full rankings for reference
write.csv(primary_counts, file.path("maps", "primary_condition_counts.csv"), row.names = FALSE)
write.csv(family_counts,  file.path("maps", "primary_family_counts.csv"),    row.names = FALSE)

# Reporting the top 5 of each
fmt <- function(df, label) {
  cat("\nTop 5 primary", label, "\n")
  print(df %>%
          slice_head(n = 5) %>%
          mutate(n_admissions = format(n_admissions, big.mark = ","),
                 share = sprintf("%.1f%%", 100 * share)) %>%
          as.data.frame(),
        row.names = FALSE)
}
fmt(primary_counts %>% select(condition, family, n_admissions, share), "conditions (with their family)")
fmt(family_counts, "families")
cat("\nreference_condition in 07 is fixed at \"lri\" for every country;",
    "the family equation uses that condition's family.\n")
