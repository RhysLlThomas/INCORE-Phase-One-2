# =============================================================================
# check_01_output.R -- fast verification that 01 wrote what the source data holds.
#
# Run from the project root:   source("check_01_output.R")
#
# Reads only what is cheap (important when the data sit on a network drive):
#   * the row count comes from the parquet footers (ds$num_rows), no data read
#   * only the `los` column is scanned -- small integers, compresses well
# Progress prints per file so a slow share is visible rather than silent.
#
# Compare the reported los range against your source data: the minimum should
# match how your data code a same-day stay (INCORE expects 1), and the maximum
# should not exceed the maximum los in your source file.
# =============================================================================

library(arrow)

path <- file.path("data", "01_transformed_data", "transformed_data.parquet")
ds   <- open_dataset(path)

message("files      : ", length(ds$files))
message("rows       : ", format(ds$num_rows, big.mark = ","), "   (from parquet metadata)")

message("\nScanning the los column only...")
t0  <- Sys.time()
mx  <- -Inf; mn <- Inf; n <- 0
for (i in seq_along(ds$files)) {
  v  <- read_parquet(ds$files[i], col_select = "los")$los
  mx <- max(mx, max(v)); mn <- min(mn, min(v)); n <- n + length(v)
  if (i %% 25 == 0 || i == length(ds$files)) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    message("  ", i, "/", length(ds$files), " files  |  los so far ", mn, "-", mx,
            "  |  ", round(el, 1), " min elapsed, ~",
            round(el / i * (length(ds$files) - i), 1), " min left")
  }
}

message("\nRESULT")
message("  rows scanned : ", format(n, big.mark = ","))
message("  los range    : ", mn, " to ", mx)
message("\nCheck this range against your source data before running 02.")
