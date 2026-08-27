# =============================================================================
# utils_clean.R
# -----------------------------------------------------------------------------
# The two heavy functions used by 02_clean_data.R, implemented with vectorised
# data.table joins so large partitions stay fast and memory-light:
#
#   get_primary_condition     assigns one primary condition per admission
#   redistribute_conditions   reassigns _gc/_NEC rows to specific conditions,
#                             one sample() draw per group
#
# (utils.R provides the third 02 function, save_primary_counts.)
# =============================================================================

library(data.table)

get_primary_condition <- function(df, NEC_other_families){

  # Assignment rules:
  # 1) The candidate primary is the first non-garbage condition (an admission
  #    with only garbage codes keeps its last row)
  # 2) An exp_well_/rf_ candidate jumps to the first later "normal" condition,
  #    if there is one
  # 3) A _NEC candidate jumps to the first later condition among its family's
  #    non-NEC conditions, if there is one
  # Jumps are repeated until no candidate moves (chains are at most two steps)

  # Converting to data.table and setting order for better efficiency
  DT <- as.data.table(df)
  setorder(DT, admission_id, icd_level)

  # Adding flags for different conditions
  DT[, condition_flag := fifelse(
    condition == "_gc", "gc",
    fifelse(
      startsWith(condition, "rf_") | startsWith(condition, "exp_well_"), "rf_exp",
      fifelse(endsWith(condition, "_NEC"), "NEC", "normal")
    )
  )]
  DT[, row_num := rowid(admission_id)]

  # One row per admission: candidate = first non-gc row, else the last (gc) row
  adm <- DT[, .(n_rows = .N), by = admission_id]
  first_non_gc <- DT[condition_flag != "gc", .(cand = row_num[1]), by = admission_id]
  adm[first_non_gc, on = "admission_id", cand := i.cand]
  adm[is.na(cand), cand := n_rows]
  adm[DT, on = .(admission_id, cand == row_num),
      `:=`(cand_flag = i.condition_flag, cand_cond = i.condition)]

  # Family lookups: the NEC condition's entry gives the family; the family-keyed
  # entry gives the valid replacement conditions
  nof <- as.data.table(NEC_other_families)
  fam_of      <- unique(nof[, .(cand_cond = NEC_or_other_condition, family)])
  good_by_fam <- unique(nof[, .(family_key = NEC_or_other_condition,
                                condition  = non_NEC_or_other_condition)])

  norm_rows <- DT[condition_flag == "normal", .(admission_id, row_num)]

  # Applying primary assignment rules
  adm[, resolved := cand_flag %chin% c("normal", "gc")]
  iter <- 0
  while (any(!adm$resolved) && iter < 10) {
    iter <- iter + 1

    # exp_well_/rf_ candidates jump to the first later "normal" row
    rf <- adm[resolved == FALSE & cand_flag == "rf_exp", .(admission_id, cand)]
    if (nrow(rf) > 0) {
      jump <- norm_rows[rf, on = .(admission_id, row_num > cand), mult = "first",
                        .(admission_id = i.admission_id, new_cand = x.row_num)]
      adm[jump[is.na(new_cand)], on = "admission_id", resolved := TRUE]
      adm[jump[!is.na(new_cand)], on = "admission_id", cand := i.new_cand]
    }

    # _NEC candidates jump to the first later condition in the same family
    nec <- adm[resolved == FALSE & cand_flag == "NEC", .(admission_id, cand, cand_cond)]
    if (nrow(nec) > 0) {
      nec[fam_of, on = "cand_cond", family := i.family]
      later <- DT[nec, on = "admission_id",
                  .(admission_id, row_num = x.row_num, condition = x.condition,
                    cand = i.cand, family = i.family)][row_num > cand]
      hits <- later[good_by_fam, on = .(family == family_key, condition), nomatch = 0]
      hits <- hits[, .(new_cand = min(row_num)), by = admission_id]
      adm[nec[!hits, on = "admission_id"], on = "admission_id", resolved := TRUE]
      adm[hits, on = "admission_id", cand := i.new_cand]
    }

    # Refresh flag/condition at the (possibly moved) candidates and re-check
    adm[DT, on = .(admission_id, cand == row_num),
        `:=`(cand_flag = i.condition_flag, cand_cond = i.condition)]
    adm[resolved == FALSE & cand_flag %chin% c("normal", "gc"), resolved := TRUE]
  }

  # Flagging the final candidate row as primary
  DT[adm, on = "admission_id", cand := i.cand]
  DT[, is_primary := as.integer(row_num == cand)]

  # Converting back to tibble and removing uneeded columns
  df <- as_tibble(DT) %>%
    select(-c("condition_flag", "row_num", "cand"))

  return(df)
}

redistribute_conditions <- function(df, primary_counts, condition_families){

  # Converting to data.tables
  DT <- as.data.table(df)
  pc <- as.data.table(primary_counts)
  cf <- as.data.table(condition_families)

  # Summarizing primary count proportions to be family specific
  fam_pc <- pc[, .(n = sum(n)), by = .(family, condition)]
  fam_pc[, prop := n / sum(n), by = family]

  # Finding all rows in dataframe with NEC or _gc condition
  DT[, NEC_gc_flag := fifelse(endsWith(condition, "_NEC"), "NEC",
                              fifelse(condition == "_gc", "gc", NA_character_))]

  # Reassigning NEC conditions to family-specific conditions, drawing from the
  # family's proportions in one sample() per family. NEC rows whose family has
  # no primary counts fall back to the partition-wide proportions
  nec_idx <- which(DT$NEC_gc_flag == "NEC")
  if (length(nec_idx) > 0) {
    nec <- DT[nec_idx, .(condition)]
    nec[cf, on = "condition", family := i.family]
    nec[, in_fam_pc := family %chin% unique(fam_pc$family)]
    nec[in_fam_pc == TRUE, condition_new := {
      tab <- fam_pc[family == .BY$family]
      sample(tab$condition, .N, replace = TRUE, prob = tab$prop)
    }, by = family]
    if (any(nec$in_fam_pc == FALSE)) {
      nec[in_fam_pc == FALSE,
          condition_new := sample(pc$condition, .N, replace = TRUE, prob = pc$prop)]
    }
    DT[nec_idx, condition := nec$condition_new]
  }

  # Reassigning _gc conditions to any condition, drawing from the age/sex/year-
  # specific proportions in a single sample() call
  if (any(DT$NEC_gc_flag == "gc", na.rm = TRUE)) {
    DT[NEC_gc_flag == "gc",
       condition := sample(pc$condition, .N, replace = TRUE, prob = pc$prop)]
  }

  # Updating data with condition families and removing uneeded column
  DT[cf, on = "condition", family := i.family]
  DT[, NEC_gc_flag := NULL]

  df <- as_tibble(DT)

  return(df)
}
