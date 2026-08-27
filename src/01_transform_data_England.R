# =============================================================================
# 01_transform_data_England.R
# -----------------------------------------------------------------------------
# England's completed version of 01_transform_data.R, kept as a worked example
# of a filled-in USER INPUTS section. It is not run by run_all.R -- edit
# 01_transform_data.R for your own data instead.
# =============================================================================

rm(list = ls())
library(tidyverse)
library(arrow)
library(tools)
source(file.path("src","utils.R"))

#----------------------
##### USER INPUTS #####
#----------------------
setwd("/do/INCORE-GitHub")

# England's Stata prep writes hes_los_analysis.dta: one row per admission/spell
filepath <- file.path("data", "hes_los_analysis.dta")

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

# England: sex is coded 0 = Male, 1 = Female, mapped to the standard INCORE
# M/F labels
sex_col <- "sex"
sex_col_map <- list('0'='M', '1'='F')

# England data are all ICD-10; the Stata prep writes icd_ver = "10" for every
# row. The explicit column (not the year-based fallback) is used so 2014-2015
# admissions are not mis-tagged as ICD-9, which would break the condition join.
icd_ver_col <- "icd_ver"
icd_ver_col_map <- list('10'='icd10')

# Length of stay can be provided as a standalone column or can be extracted from
# both a discharge date column and an admission date column.
los_col <- "los"

# England: 40 diagnosis columns icd_1 ... icd_40 (primary diagnosis = icd_1).
# Anchored, ordered patterns guarantee the diagnosis priority order regardless
# of how the columns are ordered in the .dta; each pattern matches exactly one
# column and the `icd_ver` column is excluded.
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
                                    birth_date_col,icd_ver_col, los_col, icd_cols))
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
# (England's Stata prep is already adults-only, so this removes nothing there.)
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

# Map ICD codes to conditions, chunked by year. get_conditions() pivots the
# diagnosis columns to long format; on the full England HES data a single pivot
# would exceed R's maximum vector length (2^31 - 1 elements). Processing one
# year at a time keeps each pivot small, and each year's result is written to
# disk so a run can be restarted from where it stopped.

out_dir  <- "processed_by_year"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years <- sort(unique(df$year_id))

out_files <- character(length(years))

for (i in seq_along(years)) {
  y <- years[i]
  f <- file.path(out_dir, paste0("df_conditions_", y, ".rds"))
  out_files[i] <- f

  # Reuse a year already mapped by an earlier run, but ONLY if its .rds is newer
  # than the source data -- a cache built from a previous .dta must not be
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
  readRDS(out_files[i]) %>%
    filter(age >= 18) %>%
    select(bene_id, admission_id, year_id, sex_id, age_start, icd_ver, icd_level, icd_code, condition, los) %>%
    group_by(year_id, age_start, sex_id) %>%
    write_dataset(file.path(outdir,'transformed_data.parquet'),
                  basename_template=paste0(file_path_sans_ext(basename(filepath)),'_{{i}}.parquet'),
                  max_rows_per_file = 2.5e6L)
  gc()
}

# Confirming the write completed -- METADATA ONLY. The transformed data is long
# (~300M rows on England) and sits on a network drive, so this reports the row
# count from the parquet footers and reads no data at all. Run check_01_output.R
# to scan the los column and confirm its value range against the source data.
ds_chk <- open_dataset(file.path(outdir,'transformed_data.parquet'))
message("files written : ", length(ds_chk$files))
message("rows written  : ", format(ds_chk$num_rows, big.mark = ","), " (from parquet metadata)")
message("Run check_01_output.R to scan the los column and confirm its range.")
