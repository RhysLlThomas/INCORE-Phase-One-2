# =============================================================================
# run_all.R  --  runs the full INCORE pipeline end to end.
#
#   01_transform_data.R -> 02_clean_data.R
#     -> [03_prep_inputs.R -> 04_run_regressions.R]
#     -> 06_prep_inputs_split.R -> 05_mean_days.R
#     -> 07_run_split_regressions_h2o.R
#
# 05 runs AFTER 06 (out of numeric order) so its observed cell means and
# counts cover the primary/comorbidity split equations as well as the
# original four.
#
# HOW TO RUN. From the project root (open INCORE.Rproj, or setwd() there), since
# every script uses paths relative to it (src/, data/, maps/, results/):
#
#   In RStudio / R:    source("run_all.R")
#   From a terminal:   Rscript run_all.R
#
# Edit the USER INPUTS section of src/01_transform_data.R for your data first.
#
# WHY THE STAGES ARE WRAPPED IN A FUNCTION. Every stage script begins with
# rm(list = ls()), which clears the GLOBAL environment. Running the
# orchestration inside a function keeps the timings and settings in the
# function's own frame, out of the stages' reach. Do not move this logic to the
# top level.
#
# Between stages only file and row counts are reported, straight from the
# parquet footers -- no data is scanned (scans are very slow on network
# shares). check_01_output.R scans the los column if you want the value range
# confirmed after stage 01.
# =============================================================================

run_pipeline <- function(

  # Delete each stage's outputs before it runs. Needed because no stage removes
  # what it does not overwrite: arrow's write_dataset overwrites files whose
  # names match and leaves the rest, so a previous run's files can survive
  # underneath a new one. Only the outputs of stages that will run are cleared.
  clean_start = TRUE,

  # Set TRUE when the SOURCE data file has changed. Two caches would otherwise
  # hand back results built from the previous dataset:
  #   * processed_by_year/ -- 01 reuses a year's .rds if it is NEWER than the
  #     source file. Copying a file can preserve its old timestamp, so that
  #     guard is not enough on its own; this deletes the cache outright.
  #   * results/ -- 04 SKIPS any equation x sex whose _coefs_GLM.csv exists,
  #     so stale files would make it silently keep the old results.
  new_source_data = FALSE,

  # Also run the ORIGINAL four equations (03_prep_inputs.R -> 04). Note that
  # 05 runs either way, after 06: it computes observed means and counts for
  # whichever design matrices exist, split equations included.
  run_main_pipeline = FALSE,

  # Overnight (OECD) sensitivity: after the standard pass, re-run stages 02
  # onwards on admissions with at least one overnight stay (los >= 2), into
  # _OECD-suffixed data and results folders. The overnight pass repeats
  # whatever the standard pass ran (02, 06, 05, 07, plus 03/04 when
  # run_main_pipeline is TRUE) and leaves the standard outputs untouched.
  oecd_sensitivity = FALSE,

  # Run ONLY the overnight pass, reusing data/01_transformed_data from an
  # earlier run -- for (re)producing the overnight results without repeating
  # the standard pipeline. Implies oecd_sensitivity = TRUE.
  oecd_only = FALSE,

  # Resume point: "01", "02", "06" or "07". Earlier stages are skipped and
  # their outputs left alone, so a completed 01 is not thrown away when a
  # later stage fails.
  start_from = "01"
) {

  library(arrow)

  started <- Sys.time()
  timings <- data.frame()

  # ---- helpers (nested, so the stages' rm(list = ls()) cannot remove them) ----

  hdr <- function(...) message("\n", strrep("=", 70), "\n", ..., "\n", strrep("=", 70))

  stage <- function(label, file) {
    hdr("STAGE: ", label, "   (", format(Sys.time(), "%H:%M"), ")")
    t0 <- Sys.time()
    ok <- tryCatch({ source(file.path("src", file)); TRUE },
                   error = function(e) { message("\nSTAGE FAILED: ", label, "\n",
                                                 conditionMessage(e)); FALSE })
    mins <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
    if (!ok) stop("Pipeline stopped at ", label, " after ", mins, " min.", call. = FALSE)
    message("-- ", label, " completed in ", mins, " min")
    timings <<- rbind(timings, data.frame(stage = label, minutes = mins))
    gc()
  }

  # File and row counts from the parquet footers. No data is read.
  report <- function(path, label) {
    if (!dir.exists(path)) stop(label, ": ", path, " was not created.", call. = FALSE)
    ds <- open_dataset(path)
    message("   ", label, ": ", length(ds$files), " files, ",
            format(ds$num_rows, big.mark = ","), " rows")
    invisible(ds$num_rows)
  }

  # ---- resume point and clean start -----------------------------------------

  stage_order <- c("01", "02", "06", "07")
  if (!start_from %in% stage_order) {
    stop("start_from must be one of: ", paste(stage_order, collapse = ", "), call. = FALSE)
  }
  if (new_source_data && start_from != "01") {
    stop("new_source_data = TRUE requires start_from = \"01\": a changed source file ",
         "has to be re-read and re-mapped from the beginning.", call. = FALSE)
  }
  if (oecd_only) {
    oecd_sensitivity <- TRUE
    if (new_source_data) {
      stop("new_source_data = TRUE re-reads the raw data from stage 01, which ",
           "oecd_only skips. Run the standard pipeline first.", call. = FALSE)
    }
    if (!dir.exists(file.path("data", "01_transformed_data", "transformed_data.parquet"))) {
      stop("oecd_only = TRUE reuses data/01_transformed_data from an earlier run, ",
           "which does not exist yet. Run the standard pipeline first.", call. = FALSE)
    }
  }
  from <- match(start_from, stage_order)
  runs <- function(st) !oecd_only && match(st, stage_order) >= from
  if (from > 1 && !oecd_only) message("\nResuming at stage ", start_from, "; stages ",
                        paste(stage_order[seq_len(from - 1)], collapse = ", "),
                        " are skipped and their outputs left in place.")

  outputs <- c("01" = file.path("data", "01_transformed_data"),
               "02" = file.path("data", "02_cleaned_data"),
               "06" = file.path("data", "03_prepped_inputs"),
               "07" = "results_split")

  if (clean_start && !oecd_only) {
    hdr("CLEAN START: removing intermediate outputs")
    for (d in outputs[stage_order[from:length(stage_order)]]) {
      if (dir.exists(d)) {
        unlink(d, recursive = TRUE, force = TRUE)
        message("   removed ", d)
      } else {
        message("   (absent)  ", d)
      }
    }
    # results/ holds the ORIGINAL four equations' output. 04 skips an equation x
    # sex whose _coefs_GLM.csv already exists, so those files must go whenever 04
    # is about to run on a fresh start -- otherwise it silently keeps the old ones.
    if (run_main_pipeline || new_source_data) {
      eqs <- c("age_eq", "condition_eq", "family_age_eq", "family_pair_eq")
      pat <- paste0("^admission_(", paste(eqs, collapse = "|"), ")_[^_]+_",
                    "(coefs_LASSO|coefs_GLM|cell_counts|mean_los)\\.csv$")
      old_csv <- c(list.files("results", pattern = pat, full.names = TRUE),
                   file.path("results", c("LASSO_model_stats.csv", "GLM_model_stats.csv",
                                          "admission_mean_los_by_gender.csv")))
      n <- 0
      for (f in old_csv) if (file.exists(f)) { file.remove(f); n <- n + 1 }
      message("   removed ", n, " file(s) from results/ (the original four equations)")
    }

    if (new_source_data) {
      if (dir.exists("processed_by_year")) {
        unlink("processed_by_year", recursive = TRUE, force = TRUE)
        message("   removed processed_by_year/  (year-mapping cache -- 01 will re-map)")
      }
      pcp <- file.path("maps", "primary_condition_proportions.parquet")
      if (dir.exists(pcp)) { unlink(pcp, recursive = TRUE, force = TRUE)
                             message("   removed ", pcp) }
    } else {
      message("\n   processed_by_year/ is kept: 01 reuses any year already mapped ",
              "from the current source file. Pass new_source_data = TRUE if it changed.")
    }
  }

  # ---- stages ---------------------------------------------------------------

  if (runs("01")) {
    stage("01 transform data", "01_transform_data.R")
    report(file.path("data", "01_transformed_data", "transformed_data.parquet"),
           "transformed data")
  }

  if (runs("02")) {
    stage("02 clean data", "02_clean_data.R")
    report(file.path("data", "02_cleaned_data", "cleaned_data.parquet"), "cleaned data")
  }

  if (run_main_pipeline && !oecd_only) {
    stage("03 prep inputs (original four equations)", "03_prep_inputs.R")
    stage("04 run regressions", "04_run_regressions.R")
  }

  if (runs("06")) {
    stage("06 prep split inputs", "06_prep_inputs_split.R")
    for (eq in c("condition_split_eq", "family_age_split_eq")) {
      report(file.path("data", "03_prepped_inputs", paste0("admission_", eq, ".parquet")), eq)
    }
  }

  # 05 runs AFTER 06 so the observed means cover the split equations as well
  # as the original four; it skips any equation whose matrices are absent.
  # Each equation's means are written next to its regression output
  # (results/ for the original four, results_split/ for the split pair).
  if (!oecd_only) {
    stage("05 mean days", "05_mean_days.R")
  }

  if (runs("07")) {
    stage("07 split regressions", "07_run_split_regressions_h2o.R")
  }

  # ---- overnight (OECD) pass ------------------------------------------------
  # Same stages, run again with the INCORE_OECD switch set: 02 filters to
  # los >= 2 and every stage reads/writes the _OECD folders. The standard
  # outputs above are not touched.

  if (oecd_sensitivity) {
    hdr("OVERNIGHT (OECD) PASS: admissions with los >= 2, into the _OECD folders")

    if (clean_start) {
      oecd_dirs <- c(file.path("data", "02_cleaned_data_OECD"),
                     file.path("data", "03_prepped_inputs_OECD"),
                     "results_split_OECD",
                     file.path("maps", "primary_condition_proportions_OECD.parquet"))
      for (d in oecd_dirs) {
        if (dir.exists(d)) { unlink(d, recursive = TRUE, force = TRUE); message("   removed ", d) }
        else message("   (absent)  ", d)
      }
      if (run_main_pipeline) {
        eqs <- c("age_eq", "condition_eq", "family_age_eq", "family_pair_eq")
        pat <- paste0("^admission_(", paste(eqs, collapse = "|"), ")_[^_]+_",
                      "(coefs_LASSO|coefs_GLM|cell_counts|mean_los)\\.csv$")
        old_csv <- c(list.files("results_OECD", pattern = pat, full.names = TRUE),
                     file.path("results_OECD", c("LASSO_model_stats.csv", "GLM_model_stats.csv",
                                                 "admission_mean_los_by_gender.csv")))
        n <- 0
        for (f in old_csv) if (file.exists(f)) { file.remove(f); n <- n + 1 }
        message("   removed ", n, " file(s) from results_OECD/")
      }
    }

    # The switch survives each stage's rm(list = ls()) because it lives in
    # the environment, not the R workspace; on.exit clears it even on failure
    Sys.setenv(INCORE_OECD = "1")
    on.exit(Sys.unsetenv("INCORE_OECD"), add = TRUE)

    stage("OECD 02 clean data (overnight sample)", "02_clean_data.R")
    report(file.path("data", "02_cleaned_data_OECD", "cleaned_data.parquet"), "OECD cleaned data")

    if (run_main_pipeline) {
      stage("OECD 03 prep inputs (original four equations)", "03_prep_inputs.R")
      stage("OECD 04 run regressions", "04_run_regressions.R")
    }

    stage("OECD 06 prep split inputs", "06_prep_inputs_split.R")
    for (eq in c("condition_split_eq", "family_age_split_eq")) {
      report(file.path("data", "03_prepped_inputs_OECD", paste0("admission_", eq, ".parquet")),
             paste0("OECD ", eq))
    }

    # As in the standard pass, 05 runs after 06 so the observed means cover
    # the split equations too
    stage("OECD 05 mean days", "05_mean_days.R")

    stage("OECD 07 split regressions", "07_run_split_regressions_h2o.R")

    Sys.unsetenv("INCORE_OECD")
  }

  # ---- summary --------------------------------------------------------------

  hdr("PIPELINE COMPLETE")
  for (stats_path in c(file.path("results_split", "split_model_stats.csv"),
                       file.path("results_split_OECD", "split_model_stats.csv"))) {
    if (file.exists(stats_path)) {
      message("\nModel statistics (", stats_path, "):\n")
      print(read.csv(stats_path, stringsAsFactors = FALSE), row.names = FALSE)
    }
  }
  message("\nSanity check: RMSE should be near the standard deviation of los in ",
          "your source data. A value far above it suggests contaminated inputs.")
  message("\nStage timings:\n")
  print(timings, row.names = FALSE)
  message("\nTotal: ", round(as.numeric(difftime(Sys.time(), started, units = "hours")), 2),
          " hours.  Outputs in results/ and results_split/",
          if (oecd_sensitivity) " (and their _OECD counterparts)" else "", ".")
  invisible(timings)
}

# Full run on a new source dataset: every stage, both the split models and the
# original four equations, plus the overnight (OECD, los >= 2) pass, in one
# execution. Point filepath in src/01_transform_data.R at your data first.
# To re-run ONLY the overnight pass later, call
# run_pipeline(run_main_pipeline = TRUE, oecd_only = TRUE) instead.
run_pipeline(new_source_data = TRUE, run_main_pipeline = TRUE, oecd_sensitivity = TRUE)
