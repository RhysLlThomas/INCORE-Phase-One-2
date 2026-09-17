# =============================================================================
# 02_clean_data.R
# -----------------------------------------------------------------------------
# Cleans the transformed data, one year/age/sex partition at a time:
#   1. assigns a primary condition to each admission (get_primary_condition,
#      see utils_clean.R for the assignment rules)
#   2. saves the primary-condition proportions to maps/ (save_primary_counts)
#   3. redistributes non-specific _gc and _NEC conditions to specific ones,
#      drawing from those proportions (redistribute_conditions)
# The redistribution is stochastic; set.seed makes reruns reproducible.
# Should not require any modification, since the input data are standardized.
# =============================================================================

rm(list = ls())
library(tidyverse)
library(arrow)
source(file.path("src","utils.R"))
source(file.path("src","utils_clean.R"))

#----------------
##### Setup #####
#----------------

# Redistribution is stochastic; a fixed seed makes reruns reproducible
set.seed(1234)

# Getting all year/age/sex combinations
# Ignoring age_start and sex_id values of -1, which are considered invalid
years <- seq(2014, 2023, 1)
ages <- read_feather(file.path("maps", "age_groups.feather"))$age_start
sexes <- c("M", "F")
partitions <- expand.grid(year_id=years, age_start = ages, sex_id = sexes)

# Reading in dataframe containing condition families for NEC and other conditions
NEC_other_families <- read_feather(file.path("maps", "NEC_other_conditions_lookup.feather"))

# Read in condition details, subsetting to condition and family
condition_families <- read_feather(file.path("maps", "condition_details.feather"))[,c("condition", "family")]

# Setting input folder
indir <- file.path("data", "01_transformed_data", "transformed_data.parquet")

# Creating output folder, if it doesn't already exist
outdir <- file.path("data", "02_cleaned_data")
dir.create(outdir, recursive = TRUE)

# Loading dataset without reading fully into memory
data <- open_dataset(indir)

# Transformed data written before the weights update has no weight column:
# continue unweighted (weight = 1), with a message so the analyst can see it
if (!"weight" %in% names(data)) {
  message("No weight column in the transformed data: continuing unweighted (weight = 1).")
}

#------------------------
##### Cleaning data #####
#------------------------

# Iterating over all year/age/sex combinations
for (i in 1:nrow(partitions)){

  # Getting year/age/sex combination
  year <- partitions[i,'year_id']
  age <- partitions[i,'age_start']
  sex <- partitions[i,'sex_id']

  # Reading in data for specific year/age/sex, keeping only the columns 02 uses
  # (icd_ver and icd_code are not needed downstream, which saves memory)
  t_read <- Sys.time()
  df <- data %>%
    filter((year_id==!!year) & (age_start==!!age) & (sex_id==!!sex)) %>%
    select(bene_id, admission_id, year_id, sex_id, age_start, icd_level, condition, los, any_of("weight")) %>%
    as_tibble()
  if (!"weight" %in% names(df)) df <- df %>% mutate(weight = 1)
  secs_read <- as.numeric(difftime(Sys.time(), t_read, units = "secs"))

  # Skip if there's no data
  if (nrow(df) == 0 ) {
    next
  }

  t0 <- Sys.time()

  # Set condition hierarchy
  df <- get_primary_condition(df, NEC_other_families=NEC_other_families)

  # Saving out primary condition counts
  primary_counts <- save_primary_counts(df, year=year, age=age, sex=sex, condition_families=condition_families)

  # Redistribute _NEC and _gc conditions
  df <- redistribute_conditions(df, primary_counts=primary_counts, condition_families=condition_families)

  #-----------------
  ##### Saving #####
  #-----------------

  # Selecting necessary columns and saving out data as parquet file
  # File is partitioned by year/age/sex, maximum of 2.5M rows per file
  secs_proc <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  t_write <- Sys.time()
  df %>%
    select(bene_id, admission_id, year_id, sex_id, age_start, icd_level, condition, family, is_primary, los, weight) %>%
    group_by(year_id, age_start, sex_id) %>%
    write_dataset(file.path(outdir, "cleaned_data.parquet"),
                  basename_template=paste(c(year, age, sex, "{{i}}.parquet"), collapse='_'),
                  max_rows_per_file = 2.5e6L)
  secs_write <- as.numeric(difftime(Sys.time(), t_write, units = "secs"))

  # Progress with the time split across the three steps, so a slow partition
  # shows WHICH step is slow. Freeing the partition before the next.
  message("Partition ", i, "/", nrow(partitions), " (", year, "/", age, "/", sex, "): ",
          format(nrow(df), big.mark = ","), " rows | read ", round(secs_read, 1),
          "s, process ", round(secs_proc, 1), "s, write ", round(secs_write, 1), "s")
  rm(df, primary_counts); gc()

}
