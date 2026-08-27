# =============================================================================
# run_all_main.R  --  rebuilds only the ORIGINAL four equations and their results.
#
#   03_prep_inputs.R  ->  04_run_regressions.R  ->  05_mean_days.R
#
# run_all.R already includes these stages (run_main_pipeline = TRUE); use this
# script when the transformed/cleaned data are in place and only the original
# four equations need to be (re)built. Run from the project root:
#
#   In RStudio / R:    source("run_all_main.R")
#   From a terminal:   Rscript run_all_main.R
#
# The stages are wrapped in a function because each stage script begins with
# rm(list = ls()), which clears the GLOBAL environment. Keeping the timings in
# the function's own frame puts them out of reach. Do not move this to the top
# level. Progress between stages is metadata only -- no data is scanned.
#
# WHAT clean_start REMOVES. Only what these three stages regenerate:
#   * the four admission_<eq>.parquet folders in data/03_prepped_inputs
#     (the two *_split_eq folders are LEFT ALONE -- they are the split inputs)
#   * results/LASSO_model_stats.csv and results/GLM_model_stats.csv, which are
#     APPENDED to per fit and so must start empty
# Coefficient and mean_los CSVs are overwritten by name and need no cleaning.
# Nothing else is touched -- not the source data, not maps/, not results_split/.
# =============================================================================

run_main_pipeline <- function(clean_start = TRUE) {

  library(arrow)

  started <- Sys.time()
  timings <- data.frame()

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
  }

  eqs <- c("age_eq", "condition_eq", "family_age_eq", "family_pair_eq")

  if (clean_start) {
    hdr("CLEAN START: removing what these stages regenerate")
    for (eq in eqs) {
      d <- file.path("data", "03_prepped_inputs", paste0("admission_", eq, ".parquet"))
      if (dir.exists(d)) { unlink(d, recursive = TRUE, force = TRUE); message("   removed ", d) }
      else message("   (absent)  ", d)
    }
    for (f in file.path("results", c("LASSO_model_stats.csv", "GLM_model_stats.csv"))) {
      if (file.exists(f)) { file.remove(f); message("   removed ", f) }
    }
    message("\n   results_split/ and the *_split_eq design matrices are untouched.")
  }

  stage("03 prep inputs (original four equations)", "03_prep_inputs.R")
  for (eq in eqs) {
    report(file.path("data", "03_prepped_inputs", paste0("admission_", eq, ".parquet")), eq)
  }

  stage("04 run regressions", "04_run_regressions.R")
  stage("05 mean days",       "05_mean_days.R")

  hdr("MAIN PIPELINE COMPLETE")
  for (f in file.path("results", c("LASSO_model_stats.csv", "GLM_model_stats.csv"))) {
    if (file.exists(f)) {
      message("\n", basename(f), ":\n")
      print(read.csv(f, stringsAsFactors = FALSE), row.names = FALSE)
    }
  }
  message("\nSanity check: RMSE should be near the standard deviation of los in ",
          "your source data. A value far above it suggests contaminated inputs.")
  message("\nStage timings:\n")
  print(timings, row.names = FALSE)
  message("\nTotal: ", round(as.numeric(difftime(Sys.time(), started, units = "hours")), 2),
          " hours.  Outputs in results/.")
  invisible(timings)
}

run_main_pipeline()
