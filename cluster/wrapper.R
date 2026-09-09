# Runs one INCORE stage inside a Slurm job. Forces repo cwd + renv library,
# regardless of how execRscript.sh starts R.
REPO <- "/mnt/share/homes/hkl1/repos/INCORE-Phase-One-2"  
setwd(REPO)
source("renv/activate.R")                                   

stage <- commandArgs(trailingOnly = TRUE)[1]                # e.g. "04_run_regressions.R"
message("=== src/", stage, " | lib: ", .libPaths()[1], " ===")
source(file.path("src", stage))