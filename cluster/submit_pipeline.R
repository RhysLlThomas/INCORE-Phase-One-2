
#  Launch the INCORE jobs such that they won't take over the rstudio IDE for hours on end. 
#    Note: NO HOLDS in this script
#

source("/mnt/share/homes/hkl1/repos/dex_us_county/R/cluster_utils.R")   # source SUBMIT_JOB FUNCTION
REPO    <- "/mnt/share/homes/hkl1/repos/INCORE-Phase-One-2"   # Repo directory
setwd(REPO)
WRAPPER <- file.path(REPO, "cluster", "wrapper.R")
IMG     <- "/ihme/singularity-images/rstudio/latest.img"      # same image as your IDE


# --- submit; make sure script is ready to run exactly as is! ---
jid_03 <- SUBMIT_JOB(name="incore_3", script=WRAPPER, args="03_prep_inputs.R",
                     queue="long.q", memory="100G", threads="10", time="24:00:00", img=IMG)

# These are profiled
jid_04 <- SUBMIT_JOB(name="incore_4", script=WRAPPER, args="04_run_regressions.R",
                     queue="long.q", memory="60G", threads="10", time="3:00:00",
                     img=IMG)

jid_05 <- SUBMIT_JOB(name="incore_5", script=WRAPPER, args="05_mean_days.R",
                     queue="long.q", memory="300G",  threads="6",  time="1:00:00",
                     img=IMG)

jid_06 <- SUBMIT_JOB(name="incore_6", script=WRAPPER, args="06_prep_inputs_split.R",
                     queue="long.q", memory="30G", threads="10", time="1:00:00", img=IMG)

jid_07 <- SUBMIT_JOB(name="incore_7", script=WRAPPER, args="07_run_split_regressions_h2o.R",
                     queue="long.q", memory="30G", threads="10", time="24:00:00",
                     img=IMG)
