# =============================================================================
# utils_split.R
# -----------------------------------------------------------------------------
# Design-matrix builder for the primary/comorbidity split models. Sourced by
# 06_prep_inputs_split.R; the shared 01/02 helpers come from utils.R.
#
# create_reg_matrices_split() mirrors create_reg_matrices() in utils.R but builds
# ONLY the two split equations, at the admission level:
#
#   condition_split_eq   los ~ primary_<c> + secondary_<c> + year
#   family_age_split_eq  los ~ primary_fam_<f> + secondary_fam_<f> + age
#                              + <age>__primary_fam_<f> + <age>__secondary_fam_<f> + year
#
# Condition level (first equation): primary_<c> / secondary_<c> flag condition c as
# the admission's principal diagnosis vs a comorbidity, using is_primary from 02
# (get_primary_condition). Primary takes precedence: a condition that is the
# admission's primary is not also counted as a comorbidity, even if it recurs in
# a later diagnosis slot. The two roles are mutually exclusive.
#
# Family level (second equation, the age-interaction analogue of family_age_eq):
# primary_fam_<f> = 1 if the primary condition belongs to family f;
# secondary_fam_<f> = 1 if any comorbidity condition belongs to family f. At the
# family level the two roles CAN overlap -- an admission with primary heart failure
# and comorbid ischaemic heart disease is 1 on both primary_fam and secondary_fam
# for the cardiovascular family. This is deliberate: it distinguishes "primary in
# family f" from "carrying additional f-family burden", and mirrors how
# family_age_eq treats family presence.
# =============================================================================

library(data.table)
library(Matrix)

create_reg_matrices_split <- function(DT, years, ages, conditions, families, level = "admission") {

  # The split is defined at the admission level only -- a person-year pools several
  # admissions, each with its own primary.
  stopifnot(level == "admission")

  # Sort by admission_id so DT_base$los (taken in first-appearance order) lines up
  # positionally with the sparse blocks, which place rows in sorted admission_id order.
  setorder(DT, admission_id)

  # One row per admission, holding age/year/los
  DT_base <- DT[,
                .SD[1],
                by = admission_id,
                .SDcols = c("age_start", "year_id", "los")]

  # Converting age_start and year_id to factors/dummies
  DT_base[, age_start := factor(age_start, levels = ages)]
  DT_base[, year_id := factor(year_id, levels = years)]

  # Function to directly construct a sparse matrix from rows in a datatable. When
  # all_ids is supplied, rows are placed against that master id list rather than the
  # ids present in DT, so the primary and comorbidity matrices stay full height and
  # row-aligned with each other and with the age/year matrices.
  create_sparse_mat <- function(DT, id_col, feature_col, all_features, all_ids = NULL) {

    if (is.null(all_ids)) {
      DT[, id_idx := as.integer(factor(get(id_col)))]
      n_ids <- length(unique(DT[[id_col]]))
    } else {
      DT[, id_idx := match(get(id_col), all_ids)]
      n_ids <- length(all_ids)
    }
    DT[, feat_idx := match(get(feature_col), all_features)]
    n_feats <- length(all_features)

    sparse_mat <- sparseMatrix(
      i = DT$id_idx,
      j = DT$feat_idx,
      x = 1,
      dims = c(n_ids, n_feats),
      dimnames = list(NULL, all_features)
    )
    return(sparse_mat)
  }

  # Creating one-hot encoded age sparse matrix
  age_pairs <- DT_base[, .(id = admission_id, age_start)]
  age_mat  <- create_sparse_mat(age_pairs,  "id", "age_start", ages)

  # Creating one-hot encoded year sparse matrix
  year_pairs <- DT_base[, .(id = admission_id, year_id)]
  year_mat <- create_sparse_mat(year_pairs, "id", "year_id", years)

  # Creating one-hot encoded primary and comorbidity condition sparse matrices.
  # prim_ids is the master admission list (sorted), so both matrices are full height
  # and aligned with age_mat/year_mat/DT_base. sec_pairs drops any (admission,
  # condition) that is the admission's primary (primary precedence).
  prim_ids   <- sort(unique(DT$admission_id))
  prim_pairs <- unique(DT[is_primary == 1, .(admission_id, condition)])
  sec_pairs  <- unique(DT[is_primary == 0, .(admission_id, condition)])
  sec_pairs  <- sec_pairs[!prim_pairs, on = .(admission_id, condition)]

  # Every admission must carry exactly one primary (get_primary_condition sets it);
  # fail loudly rather than emit an all-zero primary row that mimics the reference.
  stopifnot(nrow(prim_pairs) == length(prim_ids))

  primary_mat   <- create_sparse_mat(prim_pairs, "admission_id", "condition", conditions, all_ids = prim_ids)
  secondary_mat <- create_sparse_mat(sec_pairs,  "admission_id", "condition", conditions, all_ids = prim_ids)
  colnames(primary_mat)   <- paste0("primary_",   conditions)
  colnames(secondary_mat) <- paste0("secondary_", conditions)

  # Creating one-hot encoded primary and comorbidity FAMILY sparse matrices, for the
  # age-interaction equation. Built from the same primary/comorbidity rows as above,
  # collapsed to family. Roles may overlap at family level (see header).
  prim_fam_pairs <- unique(DT[is_primary == 1, .(admission_id, family)])
  sec_fam_pairs  <- unique(DT[is_primary == 0, .(admission_id, family)])
  primary_fam_mat   <- create_sparse_mat(prim_fam_pairs, "admission_id", "family", families, all_ids = prim_ids)
  secondary_fam_mat <- create_sparse_mat(sec_fam_pairs,  "admission_id", "family", families, all_ids = prim_ids)
  colnames(primary_fam_mat)   <- paste0("primary_fam_",   families)
  colnames(secondary_fam_mat) <- paste0("secondary_fam_", families)

  # Creating primary-family x age interaction matrix. drop = FALSE keeps each column
  # product sparse -- single-column indexing would coerce to a dense vector.
  primary_age_list <- list()
  primary_age_names <- character()
  idx <- 1
  for (k in 1:ncol(age_mat)) {
    for (j in 1:ncol(primary_fam_mat)) {
      primary_age_list[[idx]] <- primary_fam_mat[, j, drop = FALSE] * age_mat[, k, drop = FALSE]
      primary_age_names[idx] <- paste0(colnames(age_mat)[k], "__", colnames(primary_fam_mat)[j])
      idx <- idx + 1
    }
  }
  primary_age_mat <- do.call(cbind, primary_age_list)
  colnames(primary_age_mat) <- primary_age_names

  # Creating comorbidity-family x age interaction matrix
  secondary_age_list <- list()
  secondary_age_names <- character()
  idx <- 1
  for (k in 1:ncol(age_mat)) {
    for (j in 1:ncol(secondary_fam_mat)) {
      secondary_age_list[[idx]] <- secondary_fam_mat[, j, drop = FALSE] * age_mat[, k, drop = FALSE]
      secondary_age_names[idx] <- paste0(colnames(age_mat)[k], "__", colnames(secondary_fam_mat)[j])
      idx <- idx + 1
    }
  }
  secondary_age_mat <- do.call(cbind, secondary_age_list)
  colnames(secondary_age_mat) <- secondary_age_names

  # Creating regression matrix for each split equation and adding to list
  reg_matrices <- list(
    condition_split_eq = cbind(los = DT_base$los, primary_mat, secondary_mat, year_mat),
    family_age_split_eq = cbind(los = DT_base$los, primary_fam_mat, secondary_fam_mat, age_mat,
                                primary_age_mat, secondary_age_mat, year_mat)
  )

  return(reg_matrices)
}
