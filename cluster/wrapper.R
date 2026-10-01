# Runs one INCORE stage inside a Slurm job. Forces repo cwd + renv library,
# regardless of how execRscript.sh starts R.
REPO <- "/mnt/share/homes/hkl1/repos/INCORE-Phase-One-2"
setwd(REPO)
source("renv/activate.R")

# Arguments from submit_pipeline.R:
#   [1] stage script, e.g. "04_run_regressions.R"
#   [2] OECD flag, "1" for the overnight (OECD) pass, "0"/absent otherwise.
#
# The flag is passed explicitly as an argument rather than inherited from the
# submitting session's environment: sbatch would carry it via --export=ALL, but
# execRscript.sh hands off to singularity, which may not pass it through. An
# OECD job that silently lost the flag would run as a standard job and overwrite
# the standard results, so it is set here where it is visible in the job's args.
# Each stage reads INCORE_OECD itself and appends "_OECD" to its own folders.
args  <- commandArgs(trailingOnly = TRUE)
stage <- args[1]
oecd  <- length(args) >= 2 && identical(args[2], "1")

# Set or clear deliberately, so a standard job cannot inherit a stray
# INCORE_OECD from the submitting environment
if (oecd) Sys.setenv(INCORE_OECD = "1") else Sys.unsetenv("INCORE_OECD")

message("=== src/", stage, " | OECD: ", oecd, " | lib: ", .libPaths()[1], " ===")
source(file.path("src", stage))
