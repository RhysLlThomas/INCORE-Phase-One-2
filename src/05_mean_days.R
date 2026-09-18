rm(list = ls())
library(tidyverse)
library(arrow)

# Computes the OBSERVED (unadjusted) mean length of stay per cell -- for each
# 0/1 dummy column of the design matrices, the mean of los where the dummy is 1
# -- plus an overall mean by sex. No regressions; used for descriptive
# comparison against the model-based results.

# Setting regression level and equation names
# Regression level should be specified as either "admission" or "person_year"
reg_level <- "admission"

# The four original equations plus the two primary/comorbidity split
# equations, so observed cell means and counts cover the split cells too.
# An equation whose matrices are absent (the split pair before 06 has run,
# or any equation at the person_year level) is skipped with a message.
reg_names <- c("age_eq", "condition_eq", "family_age_eq", "family_pair_eq",
               "condition_split_eq", "family_age_split_eq")

# Overnight (OECD) sensitivity: when enabled, this stage runs the overnight-
# only pipeline (admissions with at least one overnight stay, los >= 2) and
# reads/writes _OECD-suffixed folders, leaving the main pipeline's folders
# untouched. run_all.R enables it via the INCORE_OECD environment variable;
# to run this stage on the overnight pipeline standalone, run
# Sys.setenv(INCORE_OECD = "1") first (or set oecd_inpatient_only to TRUE).
oecd_inpatient_only <- identical(Sys.getenv("INCORE_OECD"), "1")
suffix <- if (oecd_inpatient_only) "_OECD" else ""
if (oecd_inpatient_only) message("OVERNIGHT (OECD) RUN: overnight admissions only; using the _OECD folders.")

# Setting input folder
indir <- file.path("data", paste0("03_prepped_inputs", suffix))

# Creating output folder, if it doesn't already exist
outdir <- paste0("results", suffix)
dir.create(outdir, recursive = TRUE)

# Sex codes as they appear in the design-matrix filenames: 03_prep_inputs.R
# renders sex as expand.grid()'s factor code, M -> 1, F -> 2 (see the note in
# 04_run_regressions.R). Set to c("M", "F") only if your design-matrix files
# carry M/F labels instead.
sexes <- c("1", "2")

# The INCORE sample is adults aged 18+, applied on individual ages in
# 01_transform_data.R. Design matrices built BEFORE that restriction still
# contain child admissions; files for age bands below 15 are the unambiguous
# sign of that. (The 15-19 band may legitimately remain: for adults-only data
# it holds 18-19 year olds.) Stop rather than summarise a sample that
# includes children.
stop_if_child_bands <- function(files) {
  age_slot <- suppressWarnings(as.numeric(sapply(strsplit(basename(files), "_"), `[`, 2)))
  bad <- files[!is.na(age_slot) & age_slot < 15]
  if (length(bad) > 0) {
    stop("Design matrices include age bands below 15 (e.g. ", basename(bad[1]),
         "), so they were built without the 18+ sample restriction. Re-run the ",
         "pipeline from 01_transform_data.R (e.g. source(\"run_all.R\")): the ",
         "year-mapping cache is reused, and the 18+ filter is applied on rewrite.",
         call. = FALSE)
  }
  invisible(files)
}

# Accumulate per-sex summary rows here
gender_summary <- list()

for (reg in reg_names) {

  # Getting filename as combination of regression level and regression equation
  filename <- paste0(reg_level, "_", reg)

  # Loop over sex
  for (sex in sexes) {
    print(paste0("Loading data for ", reg_level, " ", reg, " (sex: ", sex, ")."))

    # Load only parquet files for this sex; the pattern is anchored to the third
    # slot of <year>_<age>_<sex>_part-N (see the note in 04_run_regressions.R)
    reg_dir <- file.path(indir, paste0(filename, ".parquet"))
    stop_if_child_bands(list.files(reg_dir))
    all_files <- list.files(reg_dir,
                            pattern = paste0("^[^_]+_[^_]+_", sex, "(_part-|\\.parquet$)"),
                            full.names = TRUE)

    if (length(all_files) == 0) {
      print(paste0("No parquet files found for ", filename, " (sex: ", sex, "). Skipping."))
      next
    }

    # Read and bind all parquet chunks for this sex
    df <- bind_rows(lapply(all_files, read_parquet))

    # Weights are optional: matrices built before the weights update (or at
    # the person_year level) have no weight column and get unweighted means.
    use_weights <- "weight" %in% names(df)
    print(if (use_weights) "Weight column found: computing weighted mean LOS." else
          "No weight column: computing UNWEIGHTED mean LOS (rebuild from 01_transform_data.R if you set weight_col).")
    w_vec <- if (use_weights) df$weight else rep(1, nrow(df))

    # Separate LOS and predictors
    los_vec  <- df$los
    preds_df <- df %>% select(-los, -any_of("weight"))

    # For person_year level, drop n_admissions (not a predictor)
    if (reg_level == "person_year" && "n_admissions" %in% names(preds_df)) {
      preds_df <- preds_df %>% select(-n_admissions)
    }

    # Mean LOS per cell: for each 0/1 dummy column, mean of los where dummy == 1
    cell_counts_vec <- colSums(preds_df, na.rm = TRUE)
    mean_los_vec    <- sapply(preds_df, function(col) weighted.mean(los_vec[col == 1], w_vec[col == 1], na.rm = TRUE))

    # Build and save mean LOS per cell for this sex
    mean_los_df <- data.frame(
      names      = names(preds_df),
      cell_count = cell_counts_vec,
      mean_los   = mean_los_vec,
      stringsAsFactors = FALSE
    )

    write.csv(
      mean_los_df,
      file.path(outdir, paste0(filename, "_", sex, "_mean_los.csv")),
      row.names = FALSE
    )

    print(paste0("Mean LOS per cell saved for ", reg, " (sex: ", sex, ")!"))

    # Accumulate per-sex summary
    gender_summary[[length(gender_summary) + 1]] <- data.frame(
      sex      = sex,
      equation = reg,
      n        = length(los_vec),
      mean_los = weighted.mean(los_vec, w_vec, na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    rm(df, preds_df, los_vec, w_vec)
    gc()
  }
}

# Save mean LOS by sex
gender_summary_df <- do.call(rbind, gender_summary)

write.csv(
  gender_summary_df,
  file.path(outdir, paste0(reg_level, "_mean_los_by_gender.csv")),
  row.names = FALSE
)

print("Mean LOS by gender saved!")
