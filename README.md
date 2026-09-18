# International Network on COmparative REsource use (INCORE)

This repository contains the INCORE Phase 1 analysis pipeline: a set of R scripts that each country team runs locally on its own hospital administrative data. The pipeline transforms raw admission-level data to a standardised format, maps ICD codes to a common set of conditions, and estimates age- and disease-specific hospital bed-day (length of stay) models. Only aggregate model outputs leave your institution -- no patient-level data ever needs to be shared.

## Requirements

- **R** (the pipeline was built on R 4.4.2; the exact package versions are pinned in `renv.lock`) and, ideally, RStudio
- **Java** -- the `h2o` package used for the regressions runs on a Java virtual machine. Install a recent JDK (e.g. [Temurin](https://adoptium.net/)) and check `java -version` works before running steps 04 and 07
- Enough disk space for the intermediate data: the transformed data is long on diagnosis code (one row per code per admission), so expect it to be several times the size of your raw data

## Running the Code

1. **Clone (or unzip) the repository and open `INCORE.Rproj` in RStudio**
   - Opening the project activates `renv` automatically (via `.Rprofile`). If you are not using RStudio, `setwd()` to the project folder in your R session
2. **Restore the R environment**
   - Run `renv::restore()` to install the exact package versions this project uses (recorded in `renv.lock`)
3. **Read through this README to understand the data, code, etc.**
4. **Place your raw data file in the `data/` folder**
5. **Modify the USER INPUTS section of `src/01_transform_data.R` for your data**
   - See the Source Code section below; `src/01_transform_data_England.R` is a completed example
6. **Run the pipeline**
   - Easiest: `source("run_all.R")` runs every stage in order with progress reporting, timing, and cache handling
   - Or run each step manually:
     - `src/01_transform_data.R`
     - `src/02_clean_data.R`
     - `src/03_prep_inputs.R`
     - `src/04_run_regressions.R`
     - `src/06_prep_inputs_split.R`
     - `src/05_mean_days.R` (after 06, so the observed means cover the split equations too)
     - `src/07_run_split_regressions_h2o.R`
7. **Share the outputs**
   - When the run completes, share the contents of `results/`, `results_split/`, `results_OECD/` and `results_split_OECD/` with the coordinating team. These contain only aggregate coefficients, counts, and fit statistics -- but apply your own institution's disclosure rules (e.g. small-cell suppression) before sharing

## Project Structure

- `data/` > directory for data -- place your raw data file here
  - `01_transformed_data/` > created and written by `01_transform_data.R`
  - `02_cleaned_data/` > created and written by `02_clean_data.R`
  - `03_prepped_inputs/` > created and written by `03_prep_inputs.R` and `06_prep_inputs_split.R`
- `processed_by_year/` > per-year cache created by `01_transform_data.R` so an interrupted run can resume
- `src/` > source code directory
  - `01_transform_data.R` > transforms raw data to a standardised format (**edit this one**)
  - `01_transform_data_England.R` > England's completed version, kept as a worked example
  - `02_clean_data.R` > assigns primary conditions and redistributes non-specific codes
  - `03_prep_inputs.R` > builds the design matrices for the four original equations
  - `04_run_regressions.R` > runs the four original equations (LASSO then GLM refit, via h2o)
  - `05_mean_days.R` > computes observed (unadjusted) mean length of stay per cell, for all six equations
  - `06_prep_inputs_split.R` > builds the design matrices for the two primary/comorbidity split equations
  - `07_run_split_regressions_h2o.R` > runs the split equations (full OLS and LASSO -> post-selection OLS, via h2o)
  - `find_reference_condition.R` > optional helper documenting how the split models' reference condition was chosen
  - `utils.R` > shared helper functions for steps 01-03
  - `utils_clean.R` > the primary-assignment and redistribution functions used by step 02
  - `utils_split.R` > the split design-matrix builder used by `06_prep_inputs_split.R`
- `run_all.R` > runs the full pipeline end to end (recommended entry point)
- `run_all_main.R` > re-runs only the original four equations (steps 03 -> 04 -> 05)
- `check_01_output.R` > optional fast check of step 01's output
- `results/` > created by steps 04 and 05 (the original four equations' regression output and observed mean LOS files, plus the overall mean by sex)
- `results_split/` > created by steps 05 and 07 (the split equations' regression output and observed mean LOS files)
- `results_OECD/`, `results_split_OECD/` > the same results for the overnight-only (LOS >= 2) sample; the overnight pass also writes `data/02_cleaned_data_OECD/` and `data/03_prepped_inputs_OECD/`
- `maps/` > directory for various maps
  - `age_groups.feather` > the binned age groups used in this project
  - `icd_map.feather` > map from ICD codes (versions 9 and 10) to the conditions used in this project
  - `condition_details.feather` > details about conditions (name, condition family, etc.)
  - `NEC_other_conditions_lookup.feather` > lookup for redistributing "NEC" (not elsewhere classified) conditions, only needed by `02_clean_data.R`
- `renv/`, `renv.lock` > R package environment (restored with `renv::restore()`)
- `README.md` > you are here
- `INCORE.Rproj` > R Project options file

## Data

Data used in this project varies between teams. Generally, we expect to use admission-level data, where **each row in a dataset corresponds to a single visit**. If you wish to use data that is less aggregated than this, you **will** need to do aggregation before running the code here. We expect each row in the data to contain the following columns:

- Year of data
- Unique patient identifier
- Patient age
  - OR patient date of birth and date of admission
- Patient sex
- ICD version used
  - OR date of discharge
- Diagnoses coded using the above ICD version
- Length of stay

Missing any of these variables in the data is okay, but some modification to the code may be required. For example, if the year of data is identified in the name of a file but does not appear as a column in the data, it should be added as a column before the `01_transform_data.R` step.

**Length-of-stay definition:** bed-days are counted as calendar days spanned -- a same-day separation counts as 1 bed-day, a stay spanning two consecutive calendar days counts as 2, and so on; equivalently, (separation date − admission date) + 1. If you give `01_transform_data.R` your admission and discharge date columns, it computes this for you. If you supply a ready-made length-of-stay column instead, it must already follow this convention -- if your source variable counts *nights* (same-day = 0), supply the date columns rather than the variable.

**Sample restriction:** the INCORE estimation sample is adults aged 18 and over at admission. `01_transform_data.R` applies this restriction (`age >= 18`) automatically, so you do not need to pre-filter your extract -- though it is fine if it is already adults-only. The restriction must happen at this stage because all later stages see only 5-year age bands, and the 15-19 band cannot be split at 18. The regression and mean-days scripts (04, 05 and 07) refuse to run on design matrices that were built without the restriction (they check for age bands below 15). Admissions with a missing length of stay are also dropped at this stage.

**Survey weights:** countries whose data are a weighted sample (survey-design or discharge weights) set `weight_col` in `01_transform_data.R`. The weight is validated (it must be positive), carried into every design matrix, applied as an observation weight in all regressions, and used for the weighted mean LOS in step 05. Everyone else leaves `weight_col = NULL`, which assigns every admission a weight of 1 and reproduces the unweighted analysis exactly. The estimation scripts also accept design matrices built before weights existed: they fall back to an unweighted fit and print a message saying so.

**Overnight-only (OECD) analysis:** alongside the standard analysis of all admissions, the pipeline produces a second, parallel set of results restricted to admissions with at least one overnight stay (LOS >= 2 in our coding), matching the OECD inpatient definition. The restriction is applied at step 02, before the primary-condition assignment and redistribution, so the redistribution proportions are computed on the overnight sample itself. Everything the overnight pass produces goes to `_OECD`-suffixed folders (`data/02_cleaned_data_OECD`, `data/03_prepped_inputs_OECD`, `results_OECD/`, `results_split_OECD/`), leaving the standard outputs untouched. See "The overnight (OECD) pass" under Source Code for how to run it.

### 01_transformed_data

The data contained in this folder is written out by the `01_transform_data.R` script. This data contains standardised column names, pivots each row to be long on diagnoses/ICD codes (i.e. each admission will have 1 row per ICD code), and maps ICD codes to standard conditions.

This data uses the "parquet" file format, which is useful for larger datasets. The parquet file format is used throughout this project, and can be read using most programming languages. Parquet files are compressed by default, so **do NOT** try to open files directly (i.e. don't click on them, it can cause weird issues).

### 02_cleaned_data

The data contained in this folder is written out by the `02_clean_data.R` script. This data is further cleaned, including a column to indicate a "primary" condition for each admission and redistributing non-specific conditions to specific ones.

### 03_prepped_inputs

The data contained in this folder is written out by `03_prep_inputs.R` and `06_prep_inputs_split.R` (the `data/` folders keep their original step numbering). This data is formatted as design matrices used for the regression models run in steps 04 and 07. There is one folder per equation, with one file per year/age/sex cell (split into parts of at most 100,000 rows).

## Source Code

The code in this project is all written in the R statistical programming language. The exact package versions are recorded in `renv.lock` and restored with `renv::restore()`.

The scripts are run in numerical order, with one exception: `05_mean_days.R` runs after `06_prep_inputs_split.R`, so its observed means cover the split equations as well as the original four (`run_all.R` orders the stages this way automatically). Most scripts are intended to run without any modification; the exception is `01_transform_data.R`, due to differences in raw input data.

Each script contains section titles and comments to help explain various processes. Section titles appear as:

~~~
#------------------------
##### SECTION TITLE #####
#------------------------
~~~

An important note about these scripts is that they are NOT parallelised. Due to potential differences in available technology, these scripts are written to run with minimal computational resources. This means that ***THIS CODE CAN BE VERY SLOW AT TIMES*** -- on large datasets, expect the full pipeline to take many hours. Since most of the steps are embarrassingly parallel, they can be adapted if desired.

### run_all.R

The recommended way to run the pipeline. It runs every stage in order, reports file/row counts between stages (from parquet metadata only -- no slow data scans), times each stage, and prints the model statistics at the end. Its arguments control:

- `clean_start` (default `TRUE`) -- delete each stage's outputs before it runs, so files from a previous run can never survive underneath a new one
- `new_source_data` (default `TRUE` in the call at the bottom of the script) -- also clear the two caches that would otherwise reuse results built from a previous version of your raw data (`processed_by_year/` and the completed-equation marker files in `results/`)
- `run_main_pipeline` (default `TRUE` in the call at the bottom) -- include the original four equations (03 -> 04) as well as the split models; the observed means (05) run either way, after 06, covering whichever matrices exist
- `oecd_sensitivity` (default `TRUE` in the call at the bottom) -- after the standard pass, run the overnight (OECD) pass: stages 02 onwards re-run on admissions with LOS >= 2, into the `_OECD` folders
- `oecd_only` -- run ONLY the overnight pass, reusing `data/01_transformed_data` from an earlier run
- `start_from` -- resume the standard pass from "01", "02", "06" or "07" after a failure, keeping earlier stages' outputs

`run_all_main.R` is a smaller companion that re-runs only the original four equations (03 -> 04 -> 05) when the transformed and cleaned data are already in place.

### The overnight (OECD) pass

The overnight analysis (see the Data section) is the same pipeline run a second time with a switch set: step 02 keeps only admissions with LOS >= 2, and every stage from 02 onwards reads and writes `_OECD`-suffixed folders. Step 01 is shared between the two passes, so the expensive raw-data processing happens once.

Three ways to run it:

- **As part of the full run** -- `run_all.R` as shipped runs the standard pass and then the overnight pass
- **On its own** -- `run_pipeline(run_main_pipeline = TRUE, oecd_only = TRUE)` re-runs just the overnight pass against the existing step-01 output
- **Stage by stage** -- run `Sys.setenv(INCORE_OECD = "1")` in your session, then source any of steps 02-07 to run that stage on the overnight pipeline (each script prints "OVERNIGHT (OECD) RUN" so you can see which mode it is in). `Sys.unsetenv("INCORE_OECD")` returns to the standard pipeline

### 01_transform_data.R

This script reads in raw input data, creates standardised columns, maps ICD codes to conditions, and saves the resulting data to a parquet dataset within the `data/01_transformed_data` directory.

***This script will require modification before use!***

Because the sources of data used in the project are varied, this script will need to be altered to ensure that it works correctly. There are several helper functions in the `utils.R` file that this script uses, which are designed to be flexible for different data sources.

There are several important factors that need to be considered before running this code:

1. **Is your data aggregated to the admission/visit level?**

   We expect that a single row in the raw input data corresponds to a single admission/visit. Less aggregated data will not fail at this step, but it **will** cause issues down the line and any results will be misleading.

2. **What is the format of your data?**

   This code has a function to load raw data files in any of the following formats: `.csv`, `.parquet`, `.dta`, `.sav`, `.sas7bdat`, `.xlsx`, `.xls`, or `.rds`. If your data is in a different format, you will need to modify how this script reads in data.

3. **What are the columns and column names available in your data?**

   Data sources will have varying names and columns, requiring some manual changes to how the standardised columns are created. There is a section at the top of this script titled "USER INPUTS". The code in this section should be modified for your specific data source. Any variables that are not present in the data can be made `NULL`, but some variables are required. Consult the script comments for specific details about each variable, and see `01_transform_data_England.R` for a completed example.

This script also applies the adult (18+) sample restriction on individual ages -- see the Data section above. The filter is applied both before the condition mapping and again when the transformed data is written, so if you built your data with an earlier version of this script (before the restriction existed), re-running `01_transform_data.R` is cheap: the year-mapping cache is reused and the under-18 admissions are dropped at the write step.

The ICD-to-condition mapping is the expensive step (it can take hours on large data), so it is chunked by year and each year's result is cached in `processed_by_year/`. An interrupted run resumes from the last completed year; the cache is only reused while it is newer than the source data file. Codes that cannot be mapped are recorded in `maps/unmapped_icd_codes.csv`, and codes mapped to `_gc` (garbage codes) in `maps/gc_icd_codes.csv`.

This step transforms the data to be long on ICD code: if a single admission had 5 ICD codes, it becomes 5 rows. The transformed data can therefore be very large and take significant disk space.

After this step, `check_01_output.R` can be run to cheaply scan the `los` column and confirm its range matches your source data.

### 02_clean_data.R

This script reads in transformed data, assigns a primary condition to each admission, redistributes non-specific conditions, and saves the resulting data to a parquet dataset within the `data/02_cleaned_data` directory. This script **should NOT** require any modification, since the input data are standardised. The redistribution step is random, and a fixed seed (`set.seed(1234)`) makes it reproducible across runs.

The first step is to assign one condition as the primary condition for each admission, following this hierarchy (implemented in `utils_clean.R`):

1. Flag all non-specific conditions
   - Non-specific conditions include `_gc` (garbage codes), `rf_*` (risk factor codes), `exp_well_*` (wellness check-up codes), and `*_NEC` (not elsewhere classified codes). Specific conditions are anything else
2. The candidate primary is the first condition (the condition of the first ICD code)
3. Then, until the candidate no longer moves:
   1. If the candidate is a specific condition, mark it as primary
   2. If the candidate is `_gc`, move to the next condition and repeat
   3. If the candidate is `rf_*` or `exp_well_*`, jump to the first later specific condition if there is one; otherwise keep the current candidate as primary
   4. If the candidate is `*_NEC`, jump to the first later specific condition within the same condition family if there is one; otherwise keep the current candidate as primary

The second step is to redistribute the remaining non-specific conditions (`_gc` and `*_NEC` only) to specific conditions. The proportions of conditions assigned as primary are computed first (written to `maps/primary_condition_proportions.parquet`) and then used as redistribution probabilities. The redistribution always uses year-age-sex specific proportions:

- `*_NEC` conditions are redistributed within their condition family, using family-specific proportions. If family-specific proportions are not available, the `_gc` logic is applied instead, so `*_NEC` conditions can occasionally map outside their family
- `_gc` conditions are redistributed across all possible conditions

### 03_prep_inputs.R

***This script MAY require modification before use!***

This script reads in cleaned data, creates the design matrices for the four original regression equations, and saves them within the `data/03_prepped_inputs` directory:

| Equation | Model |
|----------|-------|
| `age_eq` | los ~ age + year |
| `condition_eq` | los ~ condition + year |
| `family_age_eq` | los ~ family + age + family x age + year |
| `family_pair_eq` | los ~ family + family-pair interactions + year |

The only modification it may need: if person-year level matrices are desired, set `reg_level <- "person_year"` (the default `"admission"` builds admission-level matrices).

All right-hand-side variables are one-hot encoded (1 if it applies to an admission, otherwise 0). The matrices are built with `data.table` and sparse `Matrix` objects for memory efficiency, and written in chunks of 100,000 rows.

### 04_run_regressions.R

This script runs the four original equations on the design matrices and outputs the results to `results/`. Regressions are run using the `h2o` library (which requires Java). If person-year level regressions are desired, set `reg_level <- "person_year"` as in step 03.

For each equation and sex, two regressions are run: a LASSO regression (lambda search) used for variable selection, and a standard GLM refit on the selected predictors used for the reported coefficients and standard errors. Outputs per equation x sex: `*_coefs_LASSO.csv`, `*_coefs_GLM.csv`, `*_cell_counts.csv`, plus fit statistics (MSE, RMSE, R^2, AIC) appended to `LASSO_model_stats.csv` and `GLM_model_stats.csv`.

Four behaviours to be aware of:

- **Resume:** an equation x sex whose `_coefs_GLM.csv` already exists is skipped, so a failure part-way through a multi-hour run does not cost the completed fits. Delete the `results/` CSVs (or use `run_all.R` with `clean_start = TRUE`) to force a full re-run
- **Collinear columns:** the GLM refit drops exactly-collinear columns (e.g. the reference age band and year). A predictor absent from the coefficient file should be read as zero downstream
- **Adults-only check:** the script stops if the design matrices contain files for age bands below 15, which means they were built without the 18+ sample restriction (see the Data section). The fix is to re-run from `01_transform_data.R`, which is cheap because the year-mapping cache is reused. Steps 05 and 07 run the same check
- **Weights:** if the design matrices carry a `weight` column (see Survey weights in the Data section), the LASSO and GLM are fitted with observation weights; otherwise the fit is unweighted and a message says so. Step 07 behaves the same way

### 05_mean_days.R

Computes the OBSERVED (unadjusted) mean length of stay for every cell of every design matrix -- for each 0/1 dummy, the mean `los` where the dummy is 1 -- plus an overall mean by sex. No regressions and no h2o. It covers all six equations: the original four plus the two primary/comorbidity split equations, so the split cells (each `primary_*` and `secondary_*` dummy) get observed means and counts too. An equation whose matrices have not been built is skipped with a message, which is why the script runs after `06_prep_inputs_split.R` in `run_all.R` despite its number. When the matrices carry a `weight` column the means are weighted (cell counts remain sample row counts); otherwise they are plain means, with a message either way. Each equation's `*_mean_los.csv` is saved next to its regression output (the original four equations to `results/`, the split equations to `results_split/`), and the overall `admission_mean_los_by_gender.csv` goes to `results/`. These observed means are used for descriptive comparison against the model-based results.

### 06_prep_inputs_split.R

Builds the design matrices for the two **primary/comorbidity split** equations, which separate a condition's contribution when it is the principal diagnosis from its contribution as a comorbidity:

| Equation | Model |
|----------|-------|
| `condition_split_eq` | los ~ primary condition + secondary (comorbid) condition + year |
| `family_age_split_eq` | los ~ primary family + secondary family + age + interactions of both with age + year |

It reads the same cleaned data as `03_prep_inputs.R` and writes two additional equation folders into the same `data/03_prepped_inputs` directory (the `data/` folders keep their original step numbering). The split is defined at the admission level only. No modification should be needed.

### 07_run_split_regressions_h2o.R

Runs the two split equations and outputs the results to `results_split/`. For each equation and sex, two estimators are fit:

1. **OLS (full):** an unpenalised GLM on every predictor. Every primary and comorbidity keeps a coefficient, so the decomposition has no unattributed bed-days
2. **LASSO -> post-selection OLS:** cross-validated LASSO selects predictors (the same rule in every country), then an unpenalised OLS refit on the selected set recovers unbiased coefficients. The selection is recorded in `*_lasso_selection.csv`

The primary-condition dummies sum to one per admission, so one primary is dropped as the **reference condition**: `reference_condition <- "lri"` (lower respiratory infections), chosen on the England data with `find_reference_condition.R`. **The same value must be used in every country** so the intercepts are comparable -- do not change it. Outputs per equation x sex: `*_coefs_OLS.csv`, `*_coefs_LASSO.csv`, `*_lasso_selection.csv`, `*_cell_counts.csv` (primary and comorbidity counted separately), plus `split_model_stats.csv`.

### find_reference_condition.R

Optional. Ranks the most common primary conditions and families in your cleaned data (written to `maps/primary_condition_counts.csv` and `maps/primary_family_counts.csv`). It documents how the fixed reference condition in step 07 was chosen, and lets you check that choice against your own data. You do not need to run it.

### utils.R, utils_clean.R, utils_split.R

Most of the working code lives in these files, to keep the numbered scripts clean and simple. `utils.R` holds the shared helpers for steps 01-03 (reading data, standardising columns, mapping conditions, building design matrices, chunked saving). `utils_clean.R` holds the primary-assignment and redistribution functions for step 02. `utils_split.R` holds the split design-matrix builder for `06_prep_inputs_split.R`. None of these should need to be changed.

## Maps

The maps folder contains the key inputs shared by every country:

- **age_groups.feather** -- how ages are binned into groups (starting age and name of each group)
- **icd_map.feather** -- mappings from ICD-9 and ICD-10 codes to the 169 conditions used in this project
- **condition_details.feather** -- information about the 169 conditions, including full name and condition family
- **NEC_other_conditions_lookup.feather** -- `*_NEC` conditions and their families, used by `02_clean_data.R` for redistribution

Running the pipeline adds files generated from **your own data** (not shipped with the repository, and ignored by git):

- **unmapped_icd_codes.csv** / **gc_icd_codes.csv** -- ICD codes that could not be mapped, or were mapped to `_gc`, in step 01
- **primary_condition_proportions.parquet** -- year/age/sex-specific primary-condition proportions, from step 02
- **primary_condition_counts.csv** / **primary_family_counts.csv** -- primary-condition rankings, from `find_reference_condition.R`

## What's New in This Version

For teams that ran an earlier version of the pipeline (steps 01-04 only), this version adds:

- **Adult (18+) sample restriction** -- `01_transform_data.R` now filters to ages 18 and over on individual ages (later stages only see 5-year bands, so the cut must happen there), and steps 04, 05 and 07 refuse to run on design matrices built without it. If you built your data with an earlier version, re-run from step 01 -- the year-mapping cache is reused, so this is much cheaper than the original run
- **`run_all.R` / `run_all_main.R`** -- one-command orchestration with progress reporting, stage timing, clean-start handling, and resume points
- **`05_mean_days.R`** -- observed mean length of stay per cell, for descriptive comparison with the model-based results; covers the split equations as well as the original four
- **Primary/comorbidity split models** -- `06_prep_inputs_split.R` (+ `utils_split.R`) and `07_run_split_regressions_h2o.R`, estimating each condition's contribution as principal diagnosis separately from its contribution as a comorbidity, with a common reference condition (`lri`) across countries
- **Faster step 02** -- the cleaning step was reimplemented with vectorised joins (`utils_clean.R`); same rules and outputs, much faster on large data, and now seeded for reproducibility
- **Resume and caching** -- step 01 caches its per-year condition mapping in `processed_by_year/`, and step 04 skips already-completed fits, so interrupted multi-hour runs resume instead of restarting
- **Overnight-only (OECD) analysis** -- a parallel set of results for admissions with at least one overnight stay (LOS >= 2), produced by re-running stages 02 onwards into `_OECD`-suffixed folders. Included in `run_all.R` by default, and runnable on its own via `oecd_only` or per stage via the `INCORE_OECD` environment variable
- **Optional survey weights** -- countries with weighted samples set `weight_col` in step 01; the weight flows into the design matrices, all regressions, and the observed means. Unweighted countries are unaffected: `weight = 1` reproduces the previous results exactly, and the estimation scripts fall back to unweighted fits on matrices built before weights existed
- **Missing-LOS exclusion** -- admissions with a missing length of stay are dropped in step 01, alongside the 18+ rule, so every downstream stage shares one clean sample
- **`check_01_output.R`** -- a fast sanity check of step 01's output
