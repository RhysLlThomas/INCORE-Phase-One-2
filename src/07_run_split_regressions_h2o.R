# =============================================================================
# 07_run_split_regressions_h2o.R
# -----------------------------------------------------------------------------
# Estimates the primary/comorbidity split models built by 06_prep_inputs_split.R
# (condition_split_eq at condition level; family_age_split_eq at family level).
# Data loading, output files and naming follow 04.
#
# Two estimators are fit for each equation x sex:
#
#   1. OLS (full) -- an unpenalised GLM (lambda = 0) on every predictor. Every
#      primary and comorbidity keeps a coefficient and the intercept is a clean
#      baseline; nothing is zeroed, so the decomposition has no unattributed
#      bed-days.
#
#   2. LASSO -> post-selection OLS, as in 04. h2o's lambda search with 5-fold CV
#      selects the lambda minimising cross-validated deviance (the same rule in
#      every country); the predictors with nonzero coefficients are then refit
#      by unpenalised OLS to recover unbiased coefficients. The selection file
#      records exactly which predictors were kept.
#
# Reference primary condition. The primary dummies sum to one per admission and
# so are collinear with the intercept; one is dropped as the reference (its
# coefficient folds into the intercept, and every other primary is read relative
# to it). reference_condition is set to "lri", chosen with
# find_reference_condition.R on England, and the SAME value is used in every
# country so the intercepts are comparable -- do not change it. The family
# equation drops the FAMILY of that same reference condition, so both equations
# anchor on one clinical choice. (Re-referencing is free under OLS, so the
# choice can be revised later without re-running.)
#
# Outputs (results_split/): *_coefs_OLS.csv, *_coefs_LASSO.csv,
# *_lasso_selection.csv, *_cell_counts.csv (primary and comorbidity counted
# separately) and split_model_stats.csv.
# =============================================================================

rm(list = ls())
library(h2o)
library(tidyverse)
library(arrow)

# Initializing h2o. h2o.init CONNECTS to any cluster already on the port rather
# than starting a new one (and then ignores max_mem_size and keeps the old
# cluster's leftovers), so first shut down anything left on the port, then start
# fresh, then clear the key-value store (a no-op on a fresh cluster).
# nthreads = -1 uses all cores. Do not raise the heap much higher: a very large
# heap makes the JVM crawl in garbage collection.
h2o_port <- 54341
try({ h2o.connect(ip = "localhost", port = h2o_port); h2o.shutdown(prompt = FALSE); Sys.sleep(5) },
    silent = TRUE)
h2o.init(port = h2o_port, nthreads = -1, max_mem_size = "32G")
h2o.removeAll()

# Setting regression level and equation names
# Regression level should be specified as either "admission" or "person_year"
reg_level <- "admission"
reg_names <- c("condition_split_eq", "family_age_split_eq")

# Reference primary condition (see header) -- the SAME value in every country.
# The primary_<reference> column is dropped so the intercept becomes that
# condition's baseline.
reference_condition <- "lri"

# The family equation drops the family of the reference condition, looked up from
# the condition map so it is never a second thing to hard-code
condition_details <- read_feather(file.path("maps", "condition_details.feather"))
reference_family  <- condition_details$family[match(reference_condition, condition_details$condition)]

# One reference column per equation
reference_cols <- c(
  condition_split_eq  = paste0("primary_",     reference_condition),
  family_age_split_eq = paste0("primary_fam_", reference_family)
)

if (is.na(reference_family)) {
  stop("reference_condition '", reference_condition, "' is not in maps/condition_details.feather.")
}

# Fixed fold assignment so the LASSO comorbidity selection is reproducible
h2o_seed <- 1234

# Setting input and output folders
indir  <- file.path("data", "03_prepped_inputs")

# Creating output folder, if it doesn't already exist
outdir <- file.path("results_split")
dir.create(outdir, recursive = TRUE)

# The model-stats file is appended to per fit; start it fresh so rows from an
# earlier (possibly failed) run are not stacked under this run's rows
stats_path <- file.path(outdir, "split_model_stats.csv")
if (file.exists(stats_path)) file.remove(stats_path)

sexes <- c("M", "F")

# Sex codes as they appear in the design-matrix filenames. Running 03 as shipped
# produces files coded 1 = male, 2 = female (see the note in
# 04_run_regressions.R). The loop keeps the standard M/F labels for the OUTPUT
# filenames (so results match every other country) and uses this map only to
# find the input files. Set to c(M = "M", F = "F") only if your design-matrix
# files carry M/F labels instead.
sex_codes <- c(M = "1", F = "2")

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

  # Loop over SEX
  for (sex in sexes) {
    print(paste0("Loading data for ", reg_level, " ", reg, " regression (sex: ", sex, ")."))

    # Load only parquet files for this sex, matching on the filename sex code.
    # Filenames are year_age_sex_part-N.parquet, so the match is anchored to the
    # third slot -- a bare-digit code must not also match an age or year slot.
    sex_code <- sex_codes[[sex]]
    reg_dir <- file.path(indir, paste0(filename, ".parquet"))
    stop_if_child_bands(list.files(reg_dir))
    all_files <- list.files(reg_dir,
                            pattern = paste0("^[^_]+_[^_]+_", sex_code, "_part-"),
                            full.names = TRUE)

    # Fail loudly rather than skip: both equations are known to exist, so no files
    # means a wrong path or sex code, and a silent skip would produce an empty run
    if (length(all_files) == 0) {
      stop("No parquet files found for ", filename, " (sex: ", sex, ") in ", reg_dir,
           " -- check sex_codes and that 06_prep_inputs_split.R has been run.")
    }

    # Import all parts in one call -- h2o parses the vector of files into a single
    # frame server-side, avoiding an O(parts^2) h2o.rbind loop
    data <- h2o.importFile(path = all_files)

    # Setting predictors as all columns except "los" and "weight"
    predictors <- setdiff(colnames(data), c("los", "weight"))

    # Weights are optional: design matrices built before the weights update
    # (or at the person_year level) have no weight column, and the fit is
    # then unweighted. A weighted country must rebuild from 01_transform_data.R.
    use_weights <- "weight" %in% colnames(data)
    print(if (use_weights) "Weight column found: fitting weighted regressions." else
          "No weight column: fitting UNWEIGHTED regressions (rebuild from 01_transform_data.R if you set weight_col).")

    # Column groups. Comorbidities are the secondary_ / secondary_fam_ dummies and
    # (family model) their age interactions; everything else is forced in.
    is_comorbid  <- grepl("(^secondary_)|(__secondary_)", predictors)
    comorbid_cols <- predictors[is_comorbid]
    forced_cols   <- predictors[!is_comorbid]

    # Reference column for this equation (condition-level or family-level primary)
    reference_col <- reference_cols[[reg]]

    # -------------------------------------------------------------------------
    # Cell counts: column sums of the 0/1 dummies, computed in h2o. Primary and
    # comorbidity are separate columns, so they are counted separately.
    # -------------------------------------------------------------------------
    cell_counts_vec <- as.numeric(as.vector(h2o.sum(data[, predictors], axis = 0, return_frame = TRUE)))
    cell_counts_df <- data.frame(
      names      = predictors,
      cell_count = pmin(cell_counts_vec, nrow(data)),
      stringsAsFactors = FALSE
    )
    write.csv(cell_counts_df,
              file.path(outdir, paste0(filename, "_", sex, "_cell_counts.csv")),
              row.names = FALSE)

    # Guard the reference before dropping it: it must exist and be non-empty in this
    # country/sex, otherwise the remaining primaries stay collinear with the intercept
    # and h2o silently re-references to an arbitrary primary (see header).
    if (!(reference_col %in% predictors)) {
      stop("Reference column '", reference_col, "' not found for ", reg, ", sex ", sex, ".")
    }
    if (cell_counts_df$cell_count[cell_counts_df$names == reference_col] == 0) {
      stop("Reference column '", reference_col, "' has zero admissions for ", reg, ", sex ", sex,
           " -- choose a reference common in every country and both sexes.")
    }

    # Drop the reference primary from the forced-in set so it becomes the baseline
    forced_cols <- setdiff(forced_cols, reference_col)

    # Predictors that never occur for this sex (cell count 0) are constant columns:
    # they carry no information and could never be selected. Set them aside here,
    # primary and comorbidity alike, and record them in the selection file. This
    # also keeps h2o from having to drop them itself: its R package can error
    # ("invalid 'y' type in 'x && y'") when it reports several dropped constant
    # columns at once. Common for sex-specific conditions (e.g. maternal terms in
    # the male sample).
    zero_cols <- cell_counts_df$names[cell_counts_df$cell_count == 0]
    forced_cols   <- setdiff(forced_cols, zero_cols)
    comorbid_cols <- setdiff(comorbid_cols, zero_cols)
    if (length(zero_cols) > 0) {
      print(paste0(length(zero_cols), " predictor(s) with zero admissions for sex ",
                   sex, " set aside: ", paste(head(zero_cols, 6), collapse = ", "),
                   if (length(zero_cols) > 6) ", ..." else ""))
    }

    # -------------------------------------------------------------------------
    # Estimator 1: OLS (full) on every predictor (reference primary excluded).
    # -------------------------------------------------------------------------
    print("Fitting OLS (unpenalised GLM, all predictors)...")
    fit_OLS <- h2o.glm(
      x = c(forced_cols, comorbid_cols),
      y = "los",
      weights_column = if (use_weights) "weight" else NULL,
      training_frame = data,
      family = "gaussian",
      lambda = 0,
      remove_collinear_columns = TRUE,
      compute_p_values = TRUE
    )
    result_df_OLS <- data.frame(fit_OLS@model$coefficients_table)
    write.csv(result_df_OLS,
              file.path(outdir, paste0(filename, "_", sex, "_coefs_OLS.csv")),
              row.names = FALSE)
    print(paste0("OLS coefficients saved for ", reg, " regression (sex: ", sex, ")!"))

    # -------------------------------------------------------------------------
    # Estimator 2 (selection step): LASSO on all predictors, exactly as in 04 --
    # h2o's lambda search with 5-fold CV picks the lambda minimising cross-
    # validated deviance and returns the model fit at that lambda. The nonzero
    # coefficients are the selected set. Same rule in every country.
    # -------------------------------------------------------------------------
    print("Running LASSO (lambda search, 5-fold CV)...")
    # early_stopping = FALSE: by default h2o stops walking the lambda path as soon
    # as the TRAINING deviance stops improving by a small tolerance. LOS is very
    # noisy (R^2 ~ 1e-4), so that halts the path almost immediately at a large
    # lambda -- before the comorbidities enter -- and CV can only choose among a
    # truncated path (observed: primaries kept, no comorbidity selected at all).
    # Walking the full path lets the cross-validation rule decide sparsity.
    fit_LASSO_sel <- h2o.glm(
      x = c(forced_cols, comorbid_cols),
      y = "los",
      weights_column = if (use_weights) "weight" else NULL,
      training_frame = data,
      family = "gaussian",
      alpha = 1,
      lambda_search = TRUE,
      early_stopping = FALSE,
      nfolds = 5,
      seed = h2o_seed
    )
    coefs_sel <- h2o.coef(fit_LASSO_sel)
    selected <- intersect(names(coefs_sel)[coefs_sel != 0], c(forced_cols, comorbid_cols))

    # Save the LASSO selection now, before the refit. Zero-count predictors are
    # listed with status "no_admissions" so the record is complete.
    all_names <- c(forced_cols, comorbid_cols, zero_cols)
    write.csv(data.frame(names  = all_names,
                         status = ifelse(all_names %in% zero_cols, "no_admissions",
                                  ifelse(all_names %in% selected, "selected", "dropped")),
                         stringsAsFactors = FALSE),
              file.path(outdir, paste0(filename, "_", sex, "_lasso_selection.csv")),
              row.names = FALSE)
    print(paste0(length(selected), " of ", length(c(forced_cols, comorbid_cols)),
                 " predictors selected by cross-validated LASSO (",
                 sum(selected %in% forced_cols), " primary/age/year, ",
                 sum(selected %in% comorbid_cols), " comorbidity) at lambda = ",
                 signif(fit_LASSO_sel@model$lambda_best, 4)))

    h2o.rm(fit_LASSO_sel)

    # If LASSO selected no predictors, skip the refit (as 04 does): record the OLS
    # stats and move on rather than ask h2o for a model with no columns
    if (length(selected) == 0) {
      message("No predictors selected by LASSO for ", filename, " sex=", sex, " -- skipping refit.")
      stats_df <- data.frame(model = paste0(filename, "_", sex, "_OLS"),
                             MSE = h2o.mse(fit_OLS), RMSE = h2o.rmse(fit_OLS),
                             R2 = h2o.r2(fit_OLS), AIC = h2o.aic(fit_OLS))
      write.table(stats_df, stats_path, sep = ",", append = TRUE,
                  row.names = FALSE, col.names = !file.exists(stats_path))
      h2o.rm(data)
      gc()
      next
    }

    # -------------------------------------------------------------------------
    # Estimator 2 (refit step): post-selection OLS on the selected predictors,
    # unpenalised. Recovers unbiased coefficients for the retained set.
    # -------------------------------------------------------------------------
    print("Refitting OLS on the selected set (post-selection LASSO)...")
    fit_LASSO <- h2o.glm(
      x = selected,
      y = "los",
      weights_column = if (use_weights) "weight" else NULL,
      training_frame = data,
      family = "gaussian",
      lambda = 0,
      remove_collinear_columns = TRUE,
      compute_p_values = TRUE
    )
    result_df_LASSO <- data.frame(fit_LASSO@model$coefficients_table)
    write.csv(result_df_LASSO,
              file.path(outdir, paste0(filename, "_", sex, "_coefs_LASSO.csv")),
              row.names = FALSE)
    print(paste0("Post-selection coefficients saved for ", reg, " regression (sex: ", sex, ")!"))

    # -------------------------------------------------------------------------
    # Model statistics for both fits, appended to a single csv (as in 04/06)
    # -------------------------------------------------------------------------
    stats_df <- data.frame(
      model = c(paste0(filename, "_", sex, "_OLS"), paste0(filename, "_", sex, "_LASSO")),
      MSE   = c(h2o.mse(fit_OLS),  h2o.mse(fit_LASSO)),
      RMSE  = c(h2o.rmse(fit_OLS), h2o.rmse(fit_LASSO)),
      R2    = c(h2o.r2(fit_OLS),   h2o.r2(fit_LASSO)),
      AIC   = c(h2o.aic(fit_OLS),  h2o.aic(fit_LASSO))
    )
    write.table(stats_df, stats_path, sep = ",", append = TRUE,
                row.names = FALSE, col.names = !file.exists(stats_path))

    h2o.rm(data)
    gc()

  }
}
