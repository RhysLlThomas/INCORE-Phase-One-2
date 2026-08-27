# =============================================================================
# utils.R
# -----------------------------------------------------------------------------
# Shared helper functions for the data-preparation steps, grouped by the
# script that uses them (section banners below). Sourced by
# 01_transform_data.R, 02_clean_data.R, 03_prep_inputs.R and
# 06_prep_inputs_split.R -- never run directly, and nothing here should need
# editing across countries.
# =============================================================================

library(readr)
library(haven)
library(arrow)
library(tidyverse)
library(data.table)
library(Matrix)

#############################
##### 01_transform_data #####
#############################

read_data <- function(filepath) {
  
  # Getting file extension
  ext <- tools::file_ext(filepath)
  
  # Reading in data, using different methods depending on file extension
  if (ext == "csv") {
    df <- readr::read_csv(filepath)
  } else if (ext == "parquet") {
    df <- arrow::read_parquet(filepath)
  } else if (ext == "dta") {
    df <- haven::read_dta(filepath)
  } else if (ext == "sav") {
    df <- haven::read_sav(filepath)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(filepath)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(filepath)
  } else if (ext == "rds") {
    df <- readRDS(filepath)
  } else {
    stop(paste0("Unsupported file type:", ext))
  }
  
  # Converting data to tibble
  df <- df %>%
    as_tibble()
  
  return (df)
}

subset_cols <- function(df, select_cols){
  
  # Getting columns to search
  col_names <- colnames(df)
  
  # If column name is exact match, treat is as an exact name
  # Otherwise, treat it as a regular expression
  cols <- c()
  for (col in select_cols) {
    if (col %in% col_names) {
      cols <- append(cols, col)
    } else {
      cols <- append(cols, str_subset(col_names, col))
    }
  }
  
  # Selecting subset columns
  df <- df %>%
    select(all_of(unique(cols)))
  
  return (df)
}

get_unique_id <- function(df, unique_id_cols=NULL, col_name=NULL){
  
  # If unique_id_cols are provided...
  if (!is.null(unique_id_cols)) {
    
    message("Using ",  paste(unique_id_cols, collapse = ", "), " columns to get ", col_name, "...")
    
    # Create an integer id for each unique grouping of the admission_id_cols
    df <- df %>%
      group_by(across(all_of(unique_id_cols))) %>%
      mutate(!!col_name := cur_group_id()) %>%
      ungroup()
    
  } else {
    
    message("No unique_id_cols provided. Assigning unique integer as ", col_name, " for each row.")
    
    # If no unique_id_cols are provided, use an integer for each row in the data
    df <- df %>%
      mutate(!!col_name := row_number())
  }
  return (df)
}

get_year_id <- function(df, year_col=NULL, discharge_date_col=NULL){
  
  # Checking that either year_col or discharge_date_col are supplied
  if (is.null(year_col) & is.null(discharge_date_col)){
    stop("Missing required columns. You must provided either a single length of stay column, or discharge date and admission date columns.")
  } else if (!is.null(year_col)) {
    
    # Converting year_col to a symbol
    year_col = rlang::sym(year_col)
    
    # Creating copy of year column
    message("Using ",  year_col, " column to get year...")
    df <- df %>%
      mutate(year_id = !!year_col)
    
  } else if (!is.null(discharge_date_col)) {
    
    # Converting discharge_date_col to symbol
    discharge_date_col = rlang::sym(discharge_date_col)
    
    
    # Creating new year_id column, using discharge date year
    message("Using ",  discharge_date_col, " columns to get year...")
    df <- df %>%
      mutate(year_id = year(!!discharge_date_col))
    
  }
  
  # Returning dataframe with new year_id column
  return(df)
}

get_age_bins <- function(df, age_col=NULL, birth_date_col=NULL, admission_date_col=NULL){
  
  # Checking that either age_col or birth_date_col and admission_date_col are supplied
  if (is.null(age_col) & (is.null(birth_date_col) & is.null(admission_date_col))){
    stop("Missing required columns. You must provided either a single age column, or birth date and admission date columns.")
  } else if (!is.null(age_col)) {
    
    # Converting age_col to a symbol
    age_col = rlang::sym(age_col)
    
    # Creating copy of age column to bin
    message("Using ",  age_col, " column to get age bins...")
    df <- df %>%
      mutate(age = !!age_col)
    
  } else if (!is.null(birth_date_col) & !is.null(admission_date_col)) {
    
    # Converting birth_date_col and admission_date_col to symbols
    birth_date_col = rlang::sym(birth_date_col)
    admission_date_col = rlang::sym(admission_date_col)
    
    
    # Creating new age_start column using bins
    message("Using ",  birth_date_col, " and ", admission_date_col, " columns to get age bins...")
    df <- df %>%
      mutate(age = time_length(difftime(!!admission_date_col, !!birth_date_col), "years"))
    
  }
  
  # Reading in age bins
  age_bins = read_feather(file.path("maps", "age_groups.feather"))
  
  # Creating a new column, corresponding to the starting age for each bin
  df <- df %>%
    mutate(age_start = case_when(age < 0 ~ -1,
                                 age == 0 ~ 0,
                                 TRUE ~ as.numeric(as.character(cut(
                                   age,
                                   breaks = c(age_bins[1,]$age_start, age_bins[-1,]$age_end),
                                   labels = age_bins[-1,]$age_start))))) 
  
  # Returning dataframe with new age_start column
  return(df)
}

get_sexes <- function(df, sex_col, sex_col_map){
  
  # Converting sex_col to a symbol
  sex_col = rlang::sym(sex_col)
  
  # Checking if all values in sex_col are included in sex_col_map, printing warning if not
  if (all(unique(df[[sex_col]]) %in% names(sex_col_map))) {
    invisible()
  }
  else {
    warning("Values found in ",  sex_col, " that do not exist in sex_col_map. These will be set to -1.")
  }
  
  # Creating sex_id column by mapping sex_col values
  sex_col_map = c(sex_col_map, list(".default"="-1"))
  df <- df %>% mutate(sex_id=recode(!!sex_col,
                                    !!!(sex_col_map)))
  
  # Returning dataframe with new sex_id column
  return(df)
}

get_icd_version <- function(df, icd_ver_col=NULL, icd_ver_col_map=NULL, discharge_date_col=NULL){
  
  # Checking if either discharge_date_col or icd_ver_col and icd_ver_col_map are supplied
  if (is.null(discharge_date_col) & (is.null(icd_ver_col) & is.null(icd_ver_col_map))){
    warning("No ICD version mapping columns or discharge date columns provided. Defaulting to using year_id.")
  }
  
  if (!is.null(icd_ver_col) & !is.null(icd_ver_col_map)) {
    
    # Converting icd_ver_col to a symbol
    icd_ver_col = rlang::sym(icd_ver_col)
    
    # Checking if all values in icd_ver_col are included in icd_ver_col_map, printing warning if not
    if (all(unique(df[[icd_ver_col]]) %in% names(icd_ver_col_map))) {
      invisible()
    }
    else {
      warning("Values found in ",  icd_ver_col, " that do not exist in icd_ver_col_map. These will be set to NULL.")
    }
    
    # Creating icd_ver_1 column by mapping icd_ver_col values
    icd_ver_col_map = c(icd_ver_col_map, list(".default"=NULL))
    df <- df %>% mutate(icd_ver_1=recode(!!icd_ver_col,
                                      !!!(icd_ver_col_map)))
  } else {
    
    # Setting value to NA if not provided
    df <- df %>% mutate(icd_ver_1=NA)
  }
  
  if (!is.null(discharge_date_col)) {
    
    # Converting discharge_date_col to symbol
    discharge_date_col = rlang::sym(discharge_date_col)
    
    # If discharge date is after 1st October, 2015 then assume ICD 10, otherwise assume ICD 9
    df <- df %>%
      mutate(icd_ver_2 = ifelse((difftime(!!discharge_date_col, "2015-10-1", units="days")>0), "icd10", "icd9"))
    
  } else {
    
    # Setting value to NA if not provided
    df <- df %>% mutate(icd_ver_2=NA)
  }
    
  # If year_id is > 2015 then assume ICD 10, otherwise assume ICD 9
  df <- df %>%
    mutate(icd_ver_3 = ifelse(year_id>2015, "icd10", "icd9"))
  
  # Coalescing columns encoded ICD version > discharge date derived ICD version > year_id derived ICD version 
  # Dropping icd_ver_1, icd_ver_2, and icd_ver_3 columns
  df <- df %>%
    mutate(icd_ver = coalesce(icd_ver_1, icd_ver_2, icd_ver_3)) %>%
    select(-c(icd_ver_1, icd_ver_2, icd_ver_3))
  
  return(df)
  
}

get_length_of_stay <- function(df, los_col=NULL, discharge_date_col=NULL, admission_date_col=NULL){
  
  # Checking that either los_col or discharge_date_col and admission_date_col are supplied
  if (is.null(los_col) & (is.null(discharge_date_col) & is.null(admission_date_col))){
    stop("Missing required columns. You must provided either a single length of stay column, or discharge date and admission date columns.")
  } else if (!is.null(los_col)) {
    
    # Converting los_col to a symbol
    los_col = rlang::sym(los_col)
    
    # Creating copy of length of stay column
    message("Using ",  los_col, " column to get length of stay...")
    df <- df %>%
      mutate(los = !!los_col)
    
  } else if (!is.null(discharge_date_col) & !is.null(admission_date_col)) {
    
    # Converting discharge_date_col and admission_date_col to symbols
    discharge_date_col = rlang::sym(discharge_date_col)
    admission_date_col = rlang::sym(admission_date_col)
    
    
    # Creating new length of stay column, in days, using date difference
    # Setting any negative values to -1
    message("Using ",  discharge_date_col, " and ", admission_date_col, " columns to get length of stay...")
    df <- df %>%
      mutate(los = as.integer(difftime(!!discharge_date_col, !!admission_date_col, units="days")),
             los = ifelse(los < 0, -1, los))
  }
  
  # Returning dataframe with new los column
  return(df)
}

get_conditions <- function(df, icd_cols, icd_condition_map){
  
  # Select ICD columns, allowing for regex
  cols = c()
  for (col in icd_cols) {
    cols <- append(cols, str_subset(colnames(df), col))
  }
  
  # Ensure all ICD code columns are character type, remove all non-alphanumeric
  # characters, and convert to uppercase
  df <- df %>%
    mutate(across(all_of(cols),
                  ~ as.character(.x) %>%
                    str_remove_all("[^[:alnum:]]") %>% 
                    str_to_upper()))
  
  # Setting names with new standard name  (icd_#)
  cols <- setNames(cols, paste0("icd_", seq_along(cols)))
  
  # Pivot df to be long on condition columns, which are first renamed
  # Ensuring icd_level is treated as a factor
  df <- df %>%
    rename(!!!cols) %>%
    pivot_longer(cols = all_of(names(cols)), names_to = "icd_level", values_to = "icd_code") %>%
    drop_na(icd_code, icd_level) %>%
    mutate(icd_level = factor(icd_level, levels = names(cols)))
  
  # Finding all valid ICD lengths in icd_condition_map and sorting
  icd_lengths <- nchar(icd_condition_map$icd_code) %>%
    unique() %>%
    na.omit() %>%
    sort(decreasing = TRUE)
  
  # Initializing dataframes
  unmapped_data <- df
  mapped_data <- tibble()
  
  # Map conditions to ICD codes. This process will attempt to map an ICD code
  # to a condition at the most specific level (ICD code length corresponding to
  # specificity). If an ICD code does not map, it is truncated and the mapping 
  # is attempted again. This is repeated until not valid codes are available to
  # map to
  for (len in icd_lengths) {
    
    # Subset map to conditions that map to current ICD length
    current_icd_map <- icd_condition_map %>% filter(nchar(icd_code)==len)
    
    # Subset data to rows with ICD codes of current length or greater
    current_unmapped <- unmapped_data %>%
      filter(nchar(icd_code) >= len) %>%
      mutate(trunc_code = substr(icd_code, 1, len))
    
    # Any data shorter than current length can be skipped in this loop
    short_data <- unmapped_data %>%
      filter(nchar(icd_code) <  len)
    
    # If no data of current length, then skip this iteration
    if (nrow(current_unmapped) == 0){
      unmapped_data <- short_data
      next
    }
    
    # Attempt to join conditions onto current length of ICD code
    joined <- current_unmapped %>%
      left_join(current_icd_map, by = c("icd_ver" = "icd_ver", "trunc_code" = "icd_code"))
    
    # Keep any data that mapped to a condition
    current_mapped <- joined %>%
      filter(!is.na(condition)) %>%
      select(-trunc_code)
    
    # Any data that did not map to a condition is unmapped
    current_unmapped <- joined %>%
      filter(is.na(condition)) %>%
      select(-trunc_code, -condition)
    
    # Store mapped data and attempt another truncated mapping with unmapped data
    mapped_data <- bind_rows(mapped_data, current_mapped)
    unmapped_data <- bind_rows(short_data, current_unmapped)
  }
  
  # Saving out unmapped data, for reference
  unmapped_data %>%
    group_by(icd_code) %>%
    summarize(count = n(),
              .groups = "drop") %>%
    arrange(desc(count)) %>%
    write.csv(file.path("maps", "unmapped_icd_codes.csv"), row.names = FALSE)
  
  # Saving out _gc data, for reference
  mapped_data %>%
    filter(condition == "_gc") %>%
    group_by(icd_code) %>%
    summarize(count = n(),
              .groups = "drop") %>%
    arrange(desc(count)) %>%
    write.csv(file.path("maps", "gc_icd_codes.csv"), row.names = FALSE)
  
  return(mapped_data)
}

#########################
##### 02_clean_data #####
#########################

save_primary_counts <- function(df, year, age, sex, condition_families){
  
  # Getting primary condition row counts by year/age/sex/family/condition
  primary_counts <- df %>%
    filter(is_primary==1 &
             condition!="_gc" &
             !endsWith(condition, "_NEC")
           ) %>%
    left_join(condition_families,
              by="condition") %>%
    count(age_start,sex_id, year_id, family, condition, name = "n") %>%
    group_by(age_start, sex_id, year_id, family) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # Saving out counts, partitioned by year/age/sex
  primary_counts %>%
    group_by(year_id, age_start, sex_id) %>%
    write_dataset(file.path("maps", "primary_condition_proportions.parquet"),
                  basename_template=paste0(year,"_",age,"_",sex,"_","_{{i}}.parquet"))
  
  return(primary_counts)
}

##########################
##### 03_prep_inputs #####
##########################


create_reg_matrices <- function(DT, years, ages, conditions, families, level = c("admission", "person_year")) {
  
  # Using provided regression level, defaults to admission
  level <- match.arg(level)

  # Creating a datatable that's unique for either admission or person-year
  # and contains age/year/los/number of admissions
  if (level == "admission") {
    
    DT_base <- DT[,
                  .SD[1],
                  by = admission_id,
                  .SDcols = c("age_start", "year_id", "los")]
    row_id <- "admission_id"
    n_admissions_vec <- NULL
    
  } else if (level == "person_year") {
    
    DT_base <- DT[, .(
      los = sum(los, na.rm = TRUE),
      age_start = age_start[1],
      n_admissions = .N
    ), by = .(bene_id, year_id)]
    DT_base[, person_year_id := paste0(bene_id, "_", year_id)]
    row_id <- "person_year_id"
    n_admissions_vec <- DT_base$n_admissions
  }
  
  # Converting age_start and year_id to factors/dummies
  DT_base[, age_start := factor(age_start, levels = ages)]
  DT_base[, year_id := factor(year_id, levels = years)]
  
  # Function to directly construct a sparse matrix from rows in a datatable
  create_sparse_mat <- function(DT, id_col, feature_col, all_features) {
    
    # Convert IDs and feature pairs to indexes
    DT[, id_idx := as.integer(factor(get(id_col)))]
    DT[, feat_idx := match(get(feature_col), all_features)]
    
    # Get dimensions
    n_ids <- length(unique(DT[[id_col]]))
    n_feats <- length(all_features)
    
    if (any(is.na(DT$feat_idx))) {
      print(id_col)
      print(feature_col)
      print(all_features)
    }
    
    # Create sparse matrix
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
  age_pairs <- DT_base[, .(id = get(row_id), age_start)]
  age_mat  <- create_sparse_mat(age_pairs,  "id", "age_start", ages)
  
  # Creating one-hot encoded year sparse matrix
  year_pairs <- DT_base[, .(id = get(row_id), year_id)]
  year_mat <- create_sparse_mat(year_pairs, "id", "year_id",   years)
  
  # Creating one-hot encoded condition sparse matrix
  if (level == "admission") {
    cond_pairs <- unique(DT[, .(admission_id, condition)])
    condition_mat <- create_sparse_mat(cond_pairs, "admission_id", "condition", conditions)
  } else {
    cond_pairs <- unique(DT[, .(bene_id, year_id, condition)])
    cond_pairs[, id := paste0(bene_id, "_", year_id)]
    condition_mat <- create_sparse_mat(cond_pairs, "id", "condition", conditions)
  }
  
  # Creating one-hot condition family sparse matrix
  if (level == "admission") {
    fam_pairs <- unique(DT[, .(admission_id, family)])
    family_mat <- create_sparse_mat(fam_pairs, "admission_id", "family", families)
  } else {
    fam_pairs <- unique(DT[, .(bene_id, year_id, family)])
    fam_pairs[, id := paste0(bene_id, "_", year_id)]
    family_mat <- create_sparse_mat(fam_pairs, "id", "family", families)
  }
  
  # Creating condition family-age interaction matrix
  family_age_list <- list()
  family_age_names <- character()
  idx <- 1
  for (k in 1:ncol(age_mat)) {
    for (j in 1:ncol(family_mat)) {
      family_age_list[[idx]] <- family_mat[, j] * age_mat[, k]
      family_age_names[idx] <- paste0(colnames(age_mat)[k], "__", colnames(family_mat)[j])
      idx <- idx + 1
    }
  }
  family_age_mat <- do.call(cbind, family_age_list)
  colnames(family_age_mat) <- family_age_names
  
  # Creating family-pair interaction matrix
  family_pair_list <- list()
  family_pair_names <- character()
  idx <- 1
  for (i in 1:(ncol(family_mat) - 1)) {
    for (j in (i + 1):ncol(family_mat)) {
      family_pair_list[[idx]] <- family_mat[, i] * family_mat[, j]
      family_pair_names[idx] <- paste0(colnames(family_mat)[i], "__", colnames(family_mat)[j])
      idx <- idx + 1
    }
  }
  family_pair_mat <- do.call(cbind, family_pair_list)
  colnames(family_pair_mat) <- family_pair_names
  
  # Creating regression matrix for each equation and adding to list
  reg_matrices <- list(
    age_eq = if (!is.null(n_admissions_vec)) {
      cbind(los = DT_base$los, n_admissions = n_admissions_vec, age_mat, year_mat)
    } else {
      cbind(los = DT_base$los, age_mat, year_mat)
    },
    condition_eq = if (!is.null(n_admissions_vec)) {
      cbind(los = DT_base$los, n_admissions = n_admissions_vec, condition_mat, year_mat)
    } else {
      cbind(los = DT_base$los, condition_mat, year_mat)
    },
    family_age_eq = if (!is.null(n_admissions_vec)) {
      cbind(los = DT_base$los, n_admissions = n_admissions_vec, family_mat, age_mat, family_age_mat, year_mat)
    } else {
      cbind(los = DT_base$los, family_mat, age_mat, family_age_mat, year_mat)
    },
    family_pair_eq = if (!is.null(n_admissions_vec)) {
      cbind(los = DT_base$los, n_admissions = n_admissions_vec, family_mat, family_pair_mat, year_mat)
    } else {
      cbind(los = DT_base$los, family_mat, family_pair_mat, year_mat)
    }
  )
  
  return(reg_matrices)
}



chunked_save <- function(sparse_mat, out_file, chunk_size = 100000) {
  
  # Finding number of rows to iterate over
  n_rows <- nrow(sparse_mat)
  
  # Initializing row index counter
  idx_start <- 1
  
  # Saving chunks of data...
  while (idx_start <= n_rows) {
    
    # Get the row index to end at for current chunk
    idx_end <- min(idx_start + chunk_size - 1, n_rows)
    
    # Extract chunk of rows, between idx_start and idx_end. Converting from a
    # sparse matrix to a dataframe in order to save out
    chunked_DT <- as.data.frame(as.matrix(sparse_mat[idx_start:idx_end, , drop = FALSE]))
    
    # Saving to parquet format
    write_parquet(chunked_DT,
                  sub("\\.parquet$", paste0("_part-", idx_start, ".parquet"), out_file))
    
    # Increasing row index counter
    idx_start <- idx_end + 1
  }
}