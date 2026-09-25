# =============================================================================
# 04_run_regressions.R
# -----------------------------------------------------------------------------
# Estimates the four original equations on the design matrices built by
# 03_prep_inputs.R, via h2o (requires Java). For each equation x sex:
#
#   1. LASSO (lambda search) for variable selection
#   2. unpenalised GLM refit on the selected predictors, for the reported
#      coefficients and standard errors
#
# Outputs to results/: *_coefs_LASSO.csv, *_coefs_GLM.csv (with cell counts
# merged in), *_cell_counts.csv, and fit statistics appended to
# LASSO_model_stats.csv / GLM_model_stats.csv.
#
# An equation x sex whose _coefs_GLM.csv already exists is SKIPPED, so a run
# that fails part-way resumes without repeating completed fits; delete the
# results/ CSVs (or use run_all.R with clean_start = TRUE) for a full re-run.
# Stops if the design matrices contain age bands below 15 -- they were built
# without the 18+ sample restriction (see stop_if_child_bands below).
#
# MAY require modification: set reg_level to "person_year" as in 03. Nothing
# else should need editing.
# =============================================================================

rm(list = ls())
library(h2o)
library(tidyverse)
library(arrow)

# Initializing h2o. A bare h2o.init() CONNECTS to whatever cluster is already on
# the default port rather than starting a new one, and h2o JVMs started from R
# can outlive the R session -- a run can end up attached to a dead or stale
# cluster. So: shut down anything on this port, start fresh, then clear the
# key-value store (a no-op on a fresh cluster). The dedicated port keeps this
# separate from 07, which uses 54341.
h2o_port <- 54351
try({ h2o.connect(ip = "localhost", port = h2o_port); h2o.shutdown(prompt = FALSE); Sys.sleep(5) },
    silent = TRUE)
h2o.init(port = h2o_port, nthreads = -1)
h2o.removeAll()

# Confirm the cluster is actually up before spending hours on it
if (!h2o.clusterIsUp()) stop("h2o cluster did not start on port ", h2o_port, ".")

# Setting regression level and equation names
# Regression level should be specified as either "admission" or "person_year"
reg_level <- "admission"
reg_names <- c("age_eq", "condition_eq", "family_age_eq", "family_pair_eq")

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
indir <- "/mnt/share/dex/us_county/05_requests/INCORE/09_03_2026/03_prepped_inputs/"

# Creating output folder, if it doesn't already exist
outdir <- paste0("results", suffix)
outdir <- "/mnt/share/dex/us_county/05_requests/INCORE/09_03_2026/results/"

dir.create(outdir, recursive = TRUE)

# Sex codes as they appear in the design-matrix filenames. 03_prep_inputs.R names
# its output from partitions[i,'sex_id'], and expand.grid() makes that column a
# FACTOR, so paste() renders it as the factor's integer code rather than its
# label: M -> 1, F -> 2. Running 03 as shipped therefore produces
# <year>_<age>_1_part-N.parquet files, which "1"/"2" here matches. Set to
# c("M", "F") only if your design-matrix files carry M/F labels instead.
sexes <- c("1", "2")

# The INCORE sample is adults aged 18+, applied on individual ages in
# 01_transform_data.R. Design matrices built BEFORE that restriction still
# contain child admissions; files for age bands below 15 are the unambiguous
# sign of that. (The 15-19 band may legitimately remain: for adults-only data
# it holds 18-19 year olds.) Stop rather than estimate on a sample that
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

for (reg in reg_names) {

  # Getting filename as combination of regression level and regression equation
  filename <- paste0(reg_level, "_", reg)

  # Loop over sex
  for (sex in sexes) {

    # Skip an equation x sex that an earlier run already completed, so a cluster
    # failure part-way through does not cost the hours already spent. The GLM
    # coefficient file is written last, so its presence marks completion. Delete
    # the results/ CSVs (or use run_all.R with clean_start = TRUE) to force a
    # full re-run.
    done_file <- file.path(outdir, paste0(filename, "_", sex, "_coefs_GLM.csv"))
    if (file.exists(done_file)) {
      print(paste0("Skipping ", reg, " (sex: ", sex, ") -- already complete."))
      next
    }

    print(paste0("Loading data for ", reg_level, " ", reg, " regression (sex: ", sex, ")."))

    # Load only parquet files for this sex. The pattern is anchored to the THIRD
    # slot of <year>_<age>_<sex>_part-N: an unanchored "_1_" would also match the
    # AGE slot, so a file like 2019_1_2_part-1.parquet would be picked up by both
    # sexes and its rows counted twice.
    reg_dir <- file.path(indir, paste0(filename, ".parquet"))
    stop_if_child_bands(list.files(reg_dir))
    all_files <- list.files(reg_dir,
                            pattern = paste0("^[^_]+_[^_]+_", sex, "(_part-|\\.parquet$)"),
                            full.names = TRUE)

    if (length(all_files) == 0) {
      print(paste0("No parquet files found for ", filename, " (sex: ", sex, "). Skipping."))
      next
    }

    # Import all files for this sex into h2o (import first then rbind remaining)
    data <- h2o.importFile(path = all_files[1])
    if (length(all_files) > 1) {
      for (f in all_files[-1]) {
        tmp <- h2o.importFile(path = f)
        data <- h2o.rbind(data, tmp)
      }
    }

    # drop rows with NAs in LOS. VERY IMPORTANT
    data <- data[!is.na(data$los), ]
    
    # Setting predictors as all columns except "los"
    predictors <- setdiff(colnames(data), c("los", "weight"))

    # Weights are optional: design matrices built before the weights update
    # (or at the person_year level) have no weight column, and the fit is
    # then unweighted. A weighted country must rebuild from 01_transform_data.R.
    use_weights <- "weight" %in% colnames(data)
    print(if (use_weights) "Weight column found: fitting weighted regressions." else
          "No weight column: fitting UNWEIGHTED regressions (rebuild from 01_transform_data.R if you set weight_col).")

    # Running LASSO regression, using lambda search
    print("Running regression...")
    fit_LASSO <- h2o.glm(
      x = predictors,
      y = "los",
      weights_column = if (use_weights) "weight" else NULL,
      training_frame = data,
      family = "gaussian",
      alpha = 1,
      lambda_search = TRUE
    )

    # Getting coefficients from fit model and making results dataframe
    coefs <- h2o.coef(fit_LASSO)
    result_df_LASSO <- data.frame(fit_LASSO@model$coefficients_table)

    # Cell counts for this sex: column sums of the 0/1 dummies, summed INSIDE
    # h2o so only the totals cross back into R (pulling the full design matrix
    # into R can exhaust memory on the large equations).
    cell_counts_vec <- as.numeric(as.vector(h2o.sum(data[, predictors], axis = 0, return_frame = TRUE)))

    # Names come from `predictors` (h2o's own column names), which is what the
    # GLM coefficient table is keyed on for the merge below.
    cell_counts_df <- data.frame(
      names      = predictors,
      cell_count = pmin(cell_counts_vec, nrow(data)),
      stringsAsFactors = FALSE
    )

    # Save cell counts separately for this sex
    write.csv(
      cell_counts_df,
      file.path(outdir, paste0(filename, "_", sex, "_cell_counts.csv")),
      row.names = FALSE
    )

    write.csv(result_df_LASSO,
              file.path(outdir, paste0(filename, "_", sex, "_coefs_LASSO.csv")),
              row.names = FALSE)

    print(paste0("LASSO coefficients saved for ", reg, " regression (sex: ", sex, ")!"))

    # Getting model statistics from fit model
    stats_df_LASSO <- data.frame(
      model = paste0(filename, "_", sex),
      MSE = h2o.mse(fit_LASSO),
      RMSE = h2o.rmse(fit_LASSO),
      R2 = h2o.r2(fit_LASSO),
      AIC = h2o.aic(fit_LASSO)
    )

    # Writing all model stats to single csv
    stats_path_LASSO <- file.path(outdir, "LASSO_model_stats.csv")
    write.table(stats_df_LASSO, stats_path_LASSO, sep = ",", append = TRUE,
                row.names = FALSE, col.names = !file.exists(stats_path_LASSO))

    # Selecting predictors that were not dropped in LASSO regression
    selected_predictors <- intersect(names(coefs[coefs != 0]), predictors)

    # If LASSO selected no predictors, skip GLM refit to avoid error
    if (length(selected_predictors) == 0) {
      message("No predictors selected by LASSO for ", filename, " sex=", sex, " — skipping GLM.")
      # LASSO outputs and cell counts are already saved; continue to next sex.
      next
    }

    # Refitting a GLM model with selected predictors and no regularization.
    # remove_collinear_columns is required whenever the selected set contains an
    # exact dependency, which the saturated family_age design does (a family main
    # effect equals the sum of its age interactions, and the age and year dummies
    # each sum to one) -- compute_p_values refuses to run on a singular design.
    # h2o drops the redundant columns instead; they are then simply absent from
    # the coefficient file, so downstream code must read an absent predictor as
    # zero. 07 sets this flag for the same reason.
    fit_GLM <- h2o.glm(
      x = selected_predictors,
      y = "los",
      weights_column = if (use_weights) "weight" else NULL,
      training_frame = data,
      family = "gaussian",
      lambda = 0,
      remove_collinear_columns = TRUE,
      compute_p_values = TRUE
    )

    # Getting coefficients from fit model and making results dataframe
    result_df_GLM <- data.frame(fit_GLM@model$coefficients_table)
    # Merge GLM coefficients with sex-specific cell counts
    result_df_GLM <- merge(result_df_GLM, cell_counts_df, by = "names", all.x = TRUE)
    # Save sex-specific GLM coefficients + counts
    write.csv(result_df_GLM,
              file.path(outdir, paste0(filename, "_", sex, "_coefs_GLM.csv")),
              row.names = FALSE)
    print(paste0("GLM coefficients saved for ", reg, " regression (sex: ", sex, ")!"))

    # Getting model statistics from fit model
    stats_df_GLM <- data.frame(
      model = paste0(filename, "_", sex),
      MSE = h2o.mse(fit_GLM),
      RMSE = h2o.rmse(fit_GLM),
      R2 = h2o.r2(fit_GLM),
      AIC = h2o.aic(fit_GLM)
    )

    # Writing all model stats to single csv
    stats_path_GLM <- file.path(outdir, "GLM_model_stats.csv")
    write.table(stats_df_GLM, stats_path_GLM, sep = ",", append = TRUE,
                row.names = FALSE, col.names = !file.exists(stats_path_GLM))

    h2o.rm(data)
    gc()

  }
}
