rm(list = ls())
library(tidyverse)
library(arrow)
library(tools)
source(file.path("src","utils.R"))

#----------------------
##### USER INPUTS #####
#----------------------
# Run this script from the project root (open INCORE.Rproj, or setwd() there
# before sourcing) -- all paths are relative to it.
#
# This is the only script that must be edited for your data source. Set each
# variable below to match your data; variables not present in your data can be
# set to NULL where the comments say so. See src/01_transform_data_England.R
# for a completed example.

# Path to your raw data file, expected inside the data/ folder. One row per
# admission. Supported formats: .csv, .parquet, .dta, .sav, .sas7bdat, .xls,
# .xlsx, .rds (see read_data in utils.R).
filepath <- file.path("data", "ADD HERE")

if (basename(filepath) == "ADD HERE") {
  stop("Edit the USER INPUTS section of src/01_transform_data.R for your data first.")
}

# Existing beneficiary ID column should be unique to individuals by year.
# Individuals can appear multiple times in the data, but beneficiary ID should
# be the same for a single individual
beneficiary_id_col <- "person_id"

# Existing admission ID column is not necessary, but can be used to assign admission IDs
# Data should be unique on admission ID, that is, one row per admission
admission_id_cols <- "admi_id"

# Year can be provided as a standalone column or can be extracted from a
# discharge date column
year_col <- "year"
discharge_date_col <- NULL

# Age can be provided as a standalone column or can be extracted from both a
# birth date column and admission date column to find age at admission
age_col <- "age"
birth_date_col <- NULL
admission_date_col <- NULL

# Sex is expected to be coded in a single column. The values of sex_col_map
# should be modified to correspond to how sex is encoded in the data

# For example, if the data contains "man" for male and "woman" for female...
# sex_col_map <- list('male'='M', 'female'='F')

# Or, if the data contains "1" for male and "2" for female...
# sex_col_map <- list('1'='M', '2'='F')
sex_col <- "sex"
sex_col_map <- list('0'='M', '1'='F')

# ICD version can be provided as a standalone column or can be extracted from a
# discharge date column (we expect any admission discharge after 1st October,
# 2015 to be ICD 10, and anything before to be ICD 9). If you supply a icd_ver_col
# you must also provide a map, icd_ver_col_map, to encode the values correctly.

# For example, if the data contains "10" for ICD 10 and "9" for ICD 9...
# icd_ver_col_map <- list('10'='icd10', '9'='icd9')

# Or, if the data contains "v10" for ICD 10 and "v9" for ICD 9...
# icd_ver_col_map <- list('v10'='icd10', 'v9'='icd9')
icd_ver_col <- "icd_ver"
icd_ver_col_map <- list('10'='icd10')

# Length of stay can be provided as a standalone column or can be extracted from
# both a discharge date column and an admission date column. INCORE counts
# bed-days as calendar days spanned: a same-day separation = 1, a stay spanning
# two consecutive calendar days = 2 -- i.e. (discharge - admission) + 1. A
# standalone column must already follow this convention; if your source
# variable counts nights (same-day = 0), supply the date columns instead and
# the code computes it for you.
los_col <- "los"

# Survey/sampling weight. Admission-level. Leave NULL for an
# unweighted analysis, in which case every record is assigned a weight of 1.
weight_col <- NULL

# ICD codes corresponding to an admission should be provided as multiple columns.
# You can specify each column, or use regular expressions (regex) to find all
# columns using a matching pattern. If you provide each column explicitly,
# ensure that they are in order (primary diagnosis first). Anchored, ordered
# patterns like the example below guarantee the diagnosis priority order
# regardless of how the columns are ordered in the raw data.
icd_cols <- paste0("^icd_", 1:40, "$")

#----------------
##### Setup #####
#----------------

# Read in ICD to condition mapping
icd_condition_map <- read_feather(file.path("maps", "icd_map.feather"))

# Creating output folder, if it doesn't already exist
outdir <- file.path("data", "01_transformed_data")
dir.create(outdir, recursive = TRUE)

# Read in raw data, using custom function to handle various file types
df <- read_data(filepath)

# Subsetting data to specified columns only
df <- subset_cols(df, select_cols=c(beneficiary_id_col, admission_id_cols, year_col, age_col, sex_col,
                                    discharge_date_col, admission_date_col,
                                    birth_date_col,icd_ver_col, los_col, weight_col, icd_cols))
#---------------------
##### Processing #####
#---------------------

# Create column for bene_id
df <- get_unique_id(df, unique_id_cols=beneficiary_id_col, col_name="bene_id")

# Create column for admission_id
df <- get_unique_id(df, unique_id_cols=admission_id_cols, col_name="admission_id")

# Create column for year_id
df <- get_year_id(df, year_col=year_col, discharge_date_col=discharge_date_col)

# Create column for age_start
df <- get_age_bins(df, age_col=age_col, birth_date_col=birth_date_col, admission_date_col=admission_date_col)

# INCORE estimation sample: adults aged 18 and over at admission. Applied here,
# on individual ages, because every later stage sees only 5-year age bands and
# the 15-19 band cannot be split at 18. Rows with missing age are also dropped.
# (If your extract is already adults-only, this removes nothing.)
n_before <- nrow(df)
df <- df %>% filter(age >= 18)
message("18+ sample restriction: removed ", format(n_before - nrow(df), big.mark = ","),
        " of ", format(n_before, big.mark = ","), " admissions")

# Create column for sex_id
df <- get_sexes(df, sex_col=sex_col, sex_col_map=sex_col_map)

# Create column for icd_ver
df <- get_icd_version(df, icd_ver_col=icd_ver_col, icd_ver_col_map=icd_ver_col_map, discharge_date_col=discharge_date_col)

# Create column for los
df <- get_length_of_stay(df, los_col=los_col, discharge_date_col=discharge_date_col, admission_date_col=admission_date_col)

# INCORE inclusion: drop admissions with missing length of stay. LOS is the model
# outcome; an NA los otherwise propagates to every stage and makes LASSO select
# nothing (observed Jan 2026). Filtered here, at sample definition, alongside the
# 18+ rule, so ALL downstream stages share one clean sample.
n_before <- nrow(df)
df <- df %>% filter(!is.na(los))
message("LOS filter: removed ", format(n_before - nrow(df), big.mark = ","),
        " of ", format(n_before, big.mark = ","), " admissions with missing LOS")

# Create column for weight (defaults to 1 if no weight column provided)
if (!is.null(weight_col)) {
  df <- df %>% mutate(weight = as.numeric(.data[[weight_col]]))
  n_bad <- sum(is.na(df$weight) | df$weight <= 0)
  if (n_bad > 0) {
    stop(format(n_bad, big.mark = ","), " admissions have a missing or non-positive '",
         weight_col, "' value -- weights must be positive numbers.")
  }
} else {
  df <- df %>% mutate(weight = 1)
}


# Map ICD codes to conditions, chunked by year. get_conditions() pivots the
# diagnosis columns to long format; on a large dataset a single pivot can exceed
# R's maximum vector length (2^31 - 1 elements). Processing one year at a time
# keeps each pivot small, and each year's result is written to disk so a run can
# be restarted from where it stopped.

out_dir  <- "processed_by_year"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years <- sort(unique(df$year_id))

out_files <- character(length(years))

for (i in seq_along(years)) {
  y <- years[i]
  f <- file.path(out_dir, paste0("df_conditions_", y, ".rds"))
  out_files[i] <- f

  # Reuse a year already mapped by an earlier run, but ONLY if its .rds is newer
  # than the source data -- a cache built from a previous source file must not be
  # silently reused. The mapping is the expensive step (hours), so this makes a
  # re-run after a later failure quick instead of starting over.
  if (isTRUE(file.mtime(f) > file.mtime(filepath))) {
    message("Skipping year_id = ", y, " (", i, "/", length(years),
            ") -- already mapped, cache is newer than the source data")
    next
  }

  message("Processing year_id = ", y, " (", i, "/", length(years), ")")
  df_y <- df[df$year_id == y, , drop = FALSE]
  df_y <- get_conditions(df_y, icd_cols = icd_cols, icd_condition_map = icd_condition_map)
  saveRDS(df_y, f)
  rm(df_y); gc()
}

#-----------------
##### Saving #####
#-----------------

# Selecting necessary columns and saving out data as parquet files, partitioned
# by year/age/sex with a maximum of 2.5M rows per file.
#
# Written one year at a time rather than binding every year into a single object
# first, so peak memory is one year rather than all of them. The output
# directory is removed once, here, before anything is written: write_dataset
# only overwrites files whose names match, so without this a previous run's
# files would survive underneath the new ones and silently feed stale data to
# later stages.

if (dir.exists(file.path(outdir, 'transformed_data.parquet'))) {
  message("Removing previous transformed_data.parquet before writing")
  unlink(file.path(outdir, 'transformed_data.parquet'), recursive = TRUE, force = TRUE)
}

for (i in seq_along(out_files)) {
  message("Writing ", years[i], " (", i, "/", length(out_files), ")")
  # The 18+ filter is repeated at the write step because cached years in
  # processed_by_year/ may have been mapped BEFORE the restriction existed
  # (this makes re-running 01 after the update cheap: the mapping cache is
  # reused and the children are dropped here). On a fresh mapping it removes
  # nothing.
  d <- readRDS(out_files[i]) %>%
    filter(age >= 18, !is.na(los))
  # A cache mapped before the weights update has no weight column. With
  # weight_col unset that is simply an unweighted country (weight = 1); with
  # weight_col SET the cached years are missing the weights, so stop rather
  # than silently write an unweighted dataset.
  if (!"weight" %in% names(d)) {
    if (!is.null(weight_col)) {
      stop("Cached year ", years[i], " in processed_by_year/ predates the weights ",
           "update but weight_col is set. Delete processed_by_year/ (or run ",
           "run_all.R with new_source_data = TRUE) so the weights are mapped in.")
    }
    message("Cached year ", years[i], " has no weight column: continuing unweighted (weight = 1).")
    d <- d %>% mutate(weight = 1)
  }
  d %>%
    select(bene_id, admission_id, year_id, sex_id, age_start, icd_ver, icd_level, icd_code, condition, los, weight) %>%
    group_by(year_id, age_start, sex_id) %>%
    write_dataset(file.path(outdir,'transformed_data.parquet'),
                  basename_template=paste0(file_path_sans_ext(basename(filepath)),'_{{i}}.parquet'),
                  max_rows_per_file = 2.5e6L)
  gc()
}

# Confirming the write completed -- METADATA ONLY. The transformed data is long
# (one row per diagnosis code) and can be very large, so this reports the row
# count from the parquet footers and reads no data at all. Run check_01_output.R
# to scan the los column and confirm its value range against your source data.
ds_chk <- open_dataset(file.path(outdir,'transformed_data.parquet'))
message("files written : ", length(ds_chk$files))
message("rows written  : ", format(ds_chk$num_rows, big.mark = ","), " (from parquet metadata)")
message("Run check_01_output.R to scan the los column and confirm its range.")
