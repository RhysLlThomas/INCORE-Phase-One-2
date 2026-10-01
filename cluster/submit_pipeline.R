
#  Launch the INCORE jobs such that they won't take over the rstudio IDE for hours on end.
#
#  Set OECD below to choose which pass this submission is:
#    FALSE -> standard analysis:   01, 02, 03, 04, 06, 05, 07
#    TRUE  -> overnight (OECD):        02, 03, 04, 06, 05, 07
#
#  01 runs on the standard pass only: the OECD pass starts from the same
#  transformed data and filters to overnight admissions (los >= 2) in 02.
#
#  The OECD flag is passed to wrapper.R as a second argument, which sets
#  INCORE_OECD for that stage. Each stage then appends "_OECD" to its own
#  data/ and results folders, so the two passes never overwrite each other.
#
#  Jobs are chained with afterok holds, so a stage starts only if what it
#  depends on SUCCEEDED. Note 05 runs after both 04 and 06, so the observed
#  means cover the split equations as well as the original four.
#
#  Run the standard pass to completion before submitting the OECD pass: 04
#  starts an h2o cluster on a fixed localhost port (54351; 07 uses 54341), and
#  two 04 jobs landing on the same node would fight over it.
#

source("/mnt/share/homes/hkl1/repos/dex_us_county/R/cluster_utils.R")   # source SUBMIT_JOB FUNCTION
REPO    <- "/mnt/share/homes/hkl1/repos/INCORE-Phase-One-2"   # Repo directory
setwd(REPO)
WRAPPER <- file.path(REPO, "cluster", "wrapper.R")
IMG     <- "/ihme/singularity-images/rstudio/latest.img"      # same image as your IDE

#--------------------------------------
##### Which pass is this? #####
#--------------------------------------

OECD   <- TRUE   # TRUE for the overnight (OECD) pass
RUN_01 <- FALSE    # FALSE to resume from 02 with 01 already done (ignored when OECD)

FLAG  <- if (OECD) "1" else "0"
nm    <- function(x) paste0("incore_", x, if (OECD) "_oecd" else "")
stage_args <- function(script) c(script, FLAG)

message(if (OECD) "Submitting the OVERNIGHT (OECD) pass -- stages write the _OECD folders."
        else      "Submitting the STANDARD pass.")

# --- submit; make sure script is ready to run exactly as is! ---

# 01 transform data. Standard pass only; the OECD pass reuses its output.
# NOTE: memory/time here are a first guess -- 01 collects a full NIS year into
# memory and re-maps every year (the processed_by_year cache is date-stamped to
# the new run root), so check these against how long 01 actually took for you.
jid_01 <- NULL
if (!OECD && RUN_01) {
  jid_01 <- SUBMIT_JOB(name=nm("1"), script=WRAPPER, args=stage_args("01_transform_data.R"),
                       queue="long.q", memory="300G", threads="10", time="24:00:00", img=IMG)
}

# 02 clean data. NOTE: memory/time also a first guess -- 02 was not in this
# launcher before.
jid_02 <- SUBMIT_JOB(name=nm("2"), script=WRAPPER, args=stage_args("02_clean_data.R"),
                     queue="long.q", memory="200G", threads="10", time="24:00:00", img=IMG,
                     hold=if (!is.null(jid_01)) paste0("afterok:", jid_01))

jid_03 <- SUBMIT_JOB(name=nm("3"), script=WRAPPER, args=stage_args("03_prep_inputs.R"),
                     queue="long.q", memory="100G", threads="10", time="24:00:00", img=IMG,
                     hold=paste0("afterok:", jid_02))

# These are profiled
jid_04 <- SUBMIT_JOB(name=nm("4"), script=WRAPPER, args=stage_args("04_run_regressions.R"),
                     queue="long.q", memory="60G", threads="10", time="3:00:00",
                     img=IMG, hold=paste0("afterok:", jid_03))

jid_06 <- SUBMIT_JOB(name=nm("6"), script=WRAPPER, args=stage_args("06_prep_inputs_split.R"),
                     queue="long.q", memory="30G", threads="10", time="1:00:00", img=IMG,
                     hold=paste0("afterok:", jid_02))

# 05 after 04 AND 06: it reads every equation's design matrices, so the split
# pair from 06 must exist, and this keeps the collaborator's stated order
jid_05 <- SUBMIT_JOB(name=nm("5"), script=WRAPPER, args=stage_args("05_mean_days.R"),
                     queue="long.q", memory="600G",  threads="6",  time="5:00:00",
                     img=IMG, hold=paste0("afterok:", jid_04, ":", jid_06))

jid_07 <- SUBMIT_JOB(name=nm("7"), script=WRAPPER, args=stage_args("07_run_split_regressions_h2o.R"),
                     queue="long.q", memory="30G", threads="10", time="24:00:00",
                     img=IMG, hold=paste0("afterok:", jid_06))

message("Submitted: ",
        paste(na.omit(c(jid_01, jid_02, jid_03, jid_04, jid_06, jid_05, jid_07)), collapse=" "))
