rm(list = ls())
library(data.table)
library(arrow)
source(file.path("src","utils.R"))         # shared helpers (chunked_save, tidyverse verbs)
source(file.path("src","utils_split.R"))   # create_reg_matrices_split

# Overnight (OECD) sensitivity: when enabled, this stage runs the overnight-
# only pipeline (admissions with at least one overnight stay, los >= 2) and
# reads/writes _OECD-suffixed folders, leaving the main pipeline's folders
# untouched. run_all.R enables it via the INCORE_OECD environment variable;
# to run this stage on the overnight pipeline standalone, run
# Sys.setenv(INCORE_OECD = "1") first (or set oecd_inpatient_only to TRUE).
oecd_inpatient_only <- identical(Sys.getenv("INCORE_OECD"), "1")
suffix <- if (oecd_inpatient_only) "_OECD" else ""
if (oecd_inpatient_only) message("OVERNIGHT (OECD) RUN: overnight admissions only; using the _OECD folders.")

# =============================================================================
# 06_prep_inputs_split.R
# -----------------------------------------------------------------------------
# Builds the primary/comorbidity split design matrices (see utils_split.R for
# the two equations). Reads the same 02 output as 03_prep_inputs.R and writes
# two additional equation folders into the same data/03_prepped_inputs
# directory -- the data folders keep their original step numbering --
# (admission_condition_split_eq.parquet / admission_family_age_split_eq.parquet).
# Read by 07_run_split_regressions_h2o.R.
# =============================================================================

#----------------
##### Setup #####
#----------------

# Getting all year/age/sex combinations
# Ignoring age_start and sex_id values of -1, which are considered invalid
years <- seq(2014, 2023, 1)
ages <- read_feather(file.path("maps","age_groups.feather"))$age_start
sexes <- c("M", "F")
partitions <- expand.grid(year_id=years, age_start = ages, sex_id = sexes)

# Getting all conditions and condition families (dropping _NEC, as in 03_prep_inputs.R)
condition_details <- read_feather(file.path("maps", "condition_details.feather")) %>%
  filter(!endsWith(condition, '_NEC'))
conditions <- condition_details  %>%
  pull(condition) %>%
  unique()
families <- condition_details %>%
  pull(family) %>%
  unique()

# Setting regression level and equation names
# The split is only defined at the admission level
reg_level <- "admission"
reg_names <- c("condition_split_eq", "family_age_split_eq")

# Setting input and output folders
indir <- file.path("data", paste0("02_cleaned_data", suffix), "cleaned_data.parquet")
outdir <- file.path("data", paste0("03_prepped_inputs", suffix))

# Loading dataset without reading fully into memory
data <- open_dataset(indir)

#--------------------------
##### Prepping inputs #####
#--------------------------

for (i in 1:nrow(partitions)) {

  # Getting year/age/sex combination
  year <- partitions[i,'year_id']
  age <- partitions[i,'age_start']
  sex <- partitions[i,'sex_id']

  # Reading in data for specific year/age/sex as a datatable
  DT <- data %>%
    filter((year_id==!!year) & (age_start==!!age) & (sex_id==!!sex)) %>%
    as.data.table()

  # Skip if there's no data
  if (nrow(DT) == 0) {
    next
  }

  # Creating split regression matrices
  reg_matrices <- create_reg_matrices_split(DT, years, ages, conditions, families, level=reg_level)

  # For each equation...
  for (reg in reg_names) {

    # Setting directory name and file name. Directory is unique for each
    # regression level/equation while filename is unique for each year/age/sex
    dir_name <- paste0(reg_level, "_", reg, ".parquet")
    file_name <- paste0(paste(c(year, age, sex), collapse = '_'), '.parquet')

    # Create output directory for current regression equation
    if (!dir.exists(file.path(outdir, dir_name))) {
      dir.create(file.path(outdir, dir_name),
                 recursive = TRUE)
      }

    # Write out data in chunks, to help with memory usage
    # Defaults to 100,000 rows per chunk
    chunked_save(
      reg_matrices[[reg]],
      file.path(outdir, dir_name, file_name))
  }

}
