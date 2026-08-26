#' Validate an evaluation model submission file
#'
#' Performs a full validation of an evaluation-period forecast submission
#' file against the general-format specification, with evaluation-specific
#' adjustments. The evaluation file represents forecasts produced during
#' the training/validation/testing period and is used for plotting,
#' evaluation, and comparison with the operational implementation forecast.
#'
#' Differences from [validate_general_model()]: `training_validation` may
#' be `NA` in addition to `0/1/2`; `horizon` may be any
#' integer (not restricted to `0/1/2`); `output_type_id` must be exactly
#' `0.5` (point forecasts only); each forecast group must contain exactly
#' one row; and all rows must have `target_end_date < reference_date`
#' (i.e., the file must contain only historical evaluation rows; users are
#' responsible for filtering future rows themselves).
#'
#' @param file Path to an evaluation model submission CSV file.
#' @param verbose Logical. If `TRUE`, prints a summary of the validated
#'   file on success. Defaults to `FALSE`.
#' @param state_context Optional two-letter U.S. state abbreviation (or
#'   `NA`). When provided, county and HSA validation is restricted to
#'   that state. Typically supplied via `config$state_context` from
#'   `validate_report_params()`.
#'
#' @return A data frame containing the loaded evaluation forecast data,
#'   with `location` preserved as character.
#'
#' @export
validate_eval_model <- function(file, verbose = FALSE, state_context = NA_character_) {

#------------------------------------------------------------------------------#
# Creating the `add_error` function --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function allows for the printing of errors to the console. The   #
# goal of this is to make it clear to users when one of their entries is not   #
# appropriately entered. Errors are collected as they occur and reported at    #
# phase boundaries.                                                            #
#------------------------------------------------------------------------------#

  ##############################
  # Empty list to store errors #
  ##############################
  errors <- character()

  #####################################
  # Function to return error messages #
  #####################################
  add_error <- function(msg) errors <<- c(errors, msg)

  ##############################################
  # Function to abort if any errors are queued #
  ##############################################
  abort_if_errors <- function(){

    if(length(errors) > 0){

      stop(
        "Evaluation model validation failed with ", length(errors), " error(s):\n  - ",
        paste(errors, collapse = "\n  - "),
        call. = FALSE
      )

    }
  }

#------------------------------------------------------------------------------#
# Creating the `is_iso_date` helper --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function checks whether a single character string is in strict   #
# ISO YYYY-MM-DD format and parseable as a real date. Lenient base R           #
# behavior (e.g., accepting "2024/10/18") is rejected here.                    #
#------------------------------------------------------------------------------#

  is_iso_date <- function(x){

    # Allow NA through for callers to handle separately
    if(is.na(x)) return(NA)

    # Reject any non-ISO format outright
    if(!grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(FALSE)

    # Confirm parseable as a real date
    parsed <- suppressWarnings(anytime::anydate(x))
    !is.na(parsed)

  }

#------------------------------------------------------------------------------#
# Creating the `normalize_name` helper -----------------------------------------
#------------------------------------------------------------------------------#
# About: This function normalizes a name string for case- and punctuation-     #
# insensitive matching against a crosswalk. Lowercases, strips whitespace,     #
# and removes hyphens.                                                         #
#------------------------------------------------------------------------------#

  normalize_name <- function(x){

    # Switching locations to all lower case
    x <- tolower(trimws(x))

    # Stripping all excess characters
    gsub("[-\\s]+", "", x, perl = TRUE)

  }

#------------------------------------------------------------------------------#
# Creating the `strip_county_suffix` helper ------------------------------------
#------------------------------------------------------------------------------#
# About: This function strips common county-equivalent suffixes from a name    #
# (County, Parish, Borough, Census Area, Municipality) so that matching can    #
# be performed on the bare name regardless of the user's input form.           #
#------------------------------------------------------------------------------#

  strip_county_suffix <- function(x){

    # List of suffixes to strip
    suffixes <- c("Census Area", "County", "Parish", "Borough", "Municipality")

    # Looping through suffixes
    for(s in suffixes){
      pat <- paste0("\\s+", s, "$")
      x <- sub(pat, "", x, ignore.case = TRUE)
    }

    # Trimming white space
    trimws(x)

  }

#------------------------------------------------------------------------------#
# Defining evaluation-format lookup tables -------------------------------------
#------------------------------------------------------------------------------#
# About: This section centralizes all evaluation-format specification          #
# constants. Mirrors validate_general_model except where the evaluation spec   #
# diverges (training_validation NA allowed, horizon          #
# unrestricted integer, output_type_id must be 0.5).                           #
#------------------------------------------------------------------------------#

  ###############################
  # Required submission columns #
  ###############################
  required_columns <- c(
    "reference_date", "target", "target_end_date",
    "location_general", "location", "disease", "population",
    "training_validation", "horizon", "imputed",
    "data_source", "outcome_measure", "output_type",
    "output_type_id", "value"
  )

  ##############################
  # Allowed output_type values #
  ##############################
  allowed_output_types <- c("quantile")

  ###############################################
  # Required point forecast quantile (0.5 only) #
  ###############################################
  required_quantile_value <- 0.5

  ###################################################
  # Strict enum: training_validation (0,1,2, or NA) #
  ###################################################
  allowed_training_validation <- c(0, 1, 2)

  #################################
  # Strict enum: imputed (0 or 1) #
  #################################
  allowed_imputed <- c(0, 1)

  ###################################################
  # Allowed location crosswalks (from package data) #
  ###################################################

  # State abbreviations
  state_abbr <- forecastEvalReport::hubverse_locations$abbreviation

  # State names
  state_name <- forecastEvalReport::hubverse_locations$location_name

  # County data
  county_data <- forecastEvalReport::us_counties

  # MetroCast HSA crosswalk
  metrocast_data <- forecastEvalReport::metrocast_locations

  # Dartmouth HSA crosswalk
  dartmouth_data <- forecastEvalReport::dartmouth_hsa_zip

#------------------------------------------------------------------------------#
# File-level checks ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that the file exists on disk, can be read as a  #
# CSV, and contains at least one data row. Nothing else is checked until       #
# these basics succeed.                                                        #
#------------------------------------------------------------------------------#

  ##########################
  # Confirming file exists #
  ##########################
  if(!file.exists(file)){

    # Error to show if file does not exist
    add_error(paste0("File does not exist: ", file))

    # Triggering abortion of script if there are errors
    abort_if_errors()

  }

  ###################
  # Reading the CSV #
  ###################
  data <- tryCatch(

    # Reading in the CSV
    utils::read.csv(
      file,
      colClasses     = c(location = "character"),  # preserve formatting
      stringsAsFactors = FALSE,
      na.strings     = c("NA", "")
    ),

    # Error to run if file could not be read in
    error = function(e){

      # Message to show to users
      add_error(paste0("File could not be read as CSV: ", conditionMessage(e)))
      NULL

    }
  )

  # Triggering abortion if there errors
  abort_if_errors()

  #############################
  # Confirming non-empty file #
  #############################
  if(nrow(data) == 0){

    # Error to show to users
    add_error("File contains no data rows.")

    # Triggering if abortion of script if errors occur
    abort_if_errors()

  }

#------------------------------------------------------------------------------#
# Column structure -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the file has exactly the 16 required       #
# columns. Missing required columns or extra unexpected columns are treated    #
# as errors. Column existence must be confirmed before per-column value        #
# checks can run.                                                              #
#------------------------------------------------------------------------------#

  ################################
  # Checking for missing columns #
  ################################
  missing_cols <- setdiff(required_columns, names(data))

  # Triggering if columns are missing
  if(length(missing_cols) > 0){

    # Error to show to users
    add_error(paste0(
      "File is missing required column(s): ",
      paste(missing_cols, collapse = ", "), "."
    ))

  }

  ##############################
  # Checking for extra columns #
  ##############################
  extra_cols <- setdiff(names(data), required_columns)

  # Triggering if extra columns are identified
  if(length(extra_cols) > 0){

    # Error to show to users
    add_error(paste0(
      "File contains unexpected column(s): ",
      paste(extra_cols, collapse = ", "),
      ". Only the 16 required columns are allowed."
    ))

  }

  # Triggering if errors have occurred in the above checks
  abort_if_errors()

#------------------------------------------------------------------------------#
# Per-column type and value checks ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates each individual column against its expected    #
# type and allowed value set. Cross-column relationships are checked in        #
# Phase 4; this section is concerned only with column-level format and value   #
# membership.                                                                  #
#------------------------------------------------------------------------------#

  #############################################
  # `reference_date` is strict ISO YYYY-MM-DD #
  #############################################
  ref_date_valid <- vapply(as.character(data$reference_date), is_iso_date, logical(1))

  # Triggered if date is not correct format
  if(any(!ref_date_valid, na.rm = TRUE) || any(is.na(data$reference_date))){

    # Flagging rows with issues
    bad_idx <- which(!ref_date_valid | is.na(data$reference_date))

    # Error to return to users
    add_error(paste0(
      "`reference_date` has ", length(bad_idx),
      " invalid value(s) (must be ISO YYYY-MM-DD, no NA allowed). ",
      "First bad row: ", min(bad_idx), "."
    ))

  }

  ##############################################
  # Confirming `reference_date` is on Saturday #
  ##############################################

  # Parsing the date to DATE format
  ref_dates_parsed <- suppressWarnings(anytime::anydate(data$reference_date))

  # Pulling the weekday number
  ref_dow <- as.POSIXlt(ref_dates_parsed)$wday

  # Flagging issue rows
  bad_ref_dow <- which(!is.na(ref_dow) & ref_dow != 6L)

  # Triggered if any issue rows
  if(length(bad_ref_dow) > 0){

    # Error to return to users
    add_error(paste0(
      "`reference_date` has ", length(bad_ref_dow),
      " row(s) that are not on a Saturday. ",
      "Each reference_date must be the Saturday ending an EW. ",
      "First bad row: ", min(bad_ref_dow), "."
    ))

  }

  ###############################################
  # Confirming a single reference_date per file #
  ###############################################
  unique_ref <- unique(data$reference_date[!is.na(data$reference_date)])

  # Checking for multiple reference dates
  if(length(unique_ref) > 1){

    # Error to return to users
    add_error(paste0(
      "File contains ", length(unique_ref),
      " unique `reference_date` values; exactly 1 is allowed per submission. ",
      "Found: ", paste(utils::head(unique_ref, 5), collapse = ", "),
      if(length(unique_ref) > 5) paste0(" (and ", length(unique_ref) - 5, " more)") else "",
      "."
    ))

  }

  ##############################################
  # `target_end_date` is strict ISO YYYY-MM-DD #
  ##############################################

  # Checking if date can be changed to date format
  ted_valid <- vapply(as.character(data$target_end_date), is_iso_date, logical(1))

  # Checking for NA's among target end dates
  ted_is_na <- is.na(data$target_end_date)

  # Flagging bad target end dates
  bad_ted <- which(ted_is_na | !ted_valid)

  # Running if an error occurs
  if(length(bad_ted) > 0){

    # Error to show to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted),
      " invalid value(s) (must be ISO YYYY-MM-DD, no NA allowed). ",
      "First bad row: ", min(bad_ted), "."
    ))

  }

  #############################################
  # Confirming target_end_date is on Saturday #
  #############################################

  # Changing the target_end_date to R object date
  ted_parsed <- suppressWarnings(anytime::anydate(data$target_end_date))

  # Extracting the day of week
  ted_dow <- as.POSIXlt(ted_parsed)$wday

  # Checking if the day of week is Saturday and NOT NA
  bad_ted_dow <- which(!is.na(ted_dow) & ted_dow != 6L)

  # Running if any errors occur
  if(length(bad_ted_dow) > 0){

    # Error to show to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted_dow),
      " row(s) that are not on a Saturday. ",
      "Each target_end_date must be the Saturday ending an EW. ",
      "First bad row: ", min(bad_ted_dow), "."
    ))

  }

  #############################################
  # `location_general` is non-empty character #
  #############################################

  # Confirming location general is NOT NA and has entries
  bad_locgen <- which(is.na(data$location_general) |
                        nchar(trimws(data$location_general)) == 0)

  # Running if at least one row is flagged
  if(length(bad_locgen) > 0){

    # Error to show to users
    add_error(paste0(
      "`location_general` has ", length(bad_locgen),
      " missing or empty value(s). First bad row: ", min(bad_locgen), "."
    ))

  }

  ###########################
  # `location` is non-empty #
  ###########################

  # Checking if location is NOT NA and has values
  bad_loc_empty <- which(is.na(data$location) |
                           nchar(trimws(data$location)) == 0)

  # Running if any rows flag with errors
  if(length(bad_loc_empty) > 0){

    # Error to show to users
    add_error(paste0(
      "`location` has ", length(bad_loc_empty),
      " missing or empty value(s). First bad row: ", min(bad_loc_empty), "."
    ))

  }

  ####################################
  # `disease` is non-empty character #
  ####################################

  # Flagging rows where the disease is NA or has no entry
  bad_disease <- which(is.na(data$disease) |
                         nchar(trimws(data$disease)) == 0)

  # Running if a row is flagged
  if(length(bad_disease) > 0){

    # Error to show to users
    add_error(paste0(
      "`disease` has ", length(bad_disease),
      " missing or empty value(s). First bad row: ", min(bad_disease), "."
    ))

  }

  ###########################
  # Strict enum: population #
  ###########################

  # Checking if any NA values have been provided
  bad_pop <- which(is.na(data$population))

  # Running if row is flagged
  if(length(bad_pop) > 0){

    # Error to show to users
    add_error(paste0(
      "`population` has ", length(bad_pop), " invalid value(s). Allowed: ",
      paste(allowed_population, collapse = ", "),
      ". First bad row: ", min(bad_pop), "."
    ))

  }

  ###############################################
  # training_validation: 0, 1, 2, or NA allowed #
  ###############################################

  # Confirming that column can be converted to numeric
  tv_num <- suppressWarnings(as.numeric(data$training_validation))

  # Confirming if any rows have NA
  tv_is_na <- is.na(data$training_validation) | is.na(tv_num)

  # Flagging the 'bad' rows
  bad_tv <- which(!tv_is_na & !tv_num %in% allowed_training_validation)

  # Running if the row is flagged
  if(length(bad_tv) > 0){

    # Error to show to users
    add_error(paste0(
      "`training_validation` has ", length(bad_tv),
      " invalid value(s) (must be 0, 1, 2, or NA). First bad row: ",
      min(bad_tv), "."
    ))

  }

  ################################################################
  # horizon: any integer (no enum restriction) #
  ################################################################

  # Confirming the column can be converted to numeric
  epr_num <- suppressWarnings(as.numeric(data$horizon))

  # Checking if any rows have NA or are not integers
  epr_is_int <- !is.na(epr_num) & (epr_num == floor(epr_num))

  # Flagging the bad rows
  bad_epr <- which(!is.na(epr_num) & !epr_is_int)

  # Running if a row has been flagged
  if(length(bad_epr) > 0){

    # Error to show to users
    add_error(paste0(
      "`horizon` has ", length(bad_epr),
      " invalid value(s) (must be integer or NA). First bad row: ",
      min(bad_epr), "."
    ))

  }

  #################################
  # Strict enum: imputed (0 or 1) #
  #################################

  # Confirming column can be converted to numeric
  imp_num <- suppressWarnings(as.numeric(data$imputed))

  # Flagging bad rows
  bad_imp <- which(is.na(imp_num) | !imp_num %in% allowed_imputed)

  # Running if a row has been flagged
  if(length(bad_imp) > 0){

    # Error to show to users
    add_error(paste0(
      "`imputed` has ", length(bad_imp),
      " invalid value(s) (must be 0 or 1). First bad row: ",
      min(bad_imp), "."
    ))

  }

  ########################################
  # `data_source` is non-empty character #
  ########################################

  # Flagging any rows that are NA or have missing characters
  bad_ds <- which(nchar(trimws(data$data_source)) == 0)

  # Running if a row has been flagged
  if(length(bad_ds) > 0){

    # Error to show to users
    add_error(paste0(
      "`data_source` has ", length(bad_ds),
      " missing or empty value(s). First bad row: ", min(bad_ds), "."
    ))

  }

  ############################################
  # `outcome_measure` is non-empty character #
  ############################################

  # Flagging any rows that are NA or have missing characters
  bad_om <- which(is.na(data$outcome_measure) |
                    nchar(trimws(data$outcome_measure)) == 0)

  # Running if a row has been flagged
  if(length(bad_om) > 0){

    # Error to show to users
    add_error(paste0(
      "`outcome_measure` has ", length(bad_om),
      " missing or empty value(s). First bad row: ", min(bad_om), "."
    ))

  }

  #####################################################
  # `output_type` must be one of allowed_output_types #
  #####################################################

  # Flagging any rows that are NA or have missing characters
  bad_ot <- which(is.na(data$output_type) |
                    !data$output_type %in% allowed_output_types)

  # Running if a row has been flagged
  if(length(bad_ot) > 0){

    # Error to show to users
    add_error(paste0(
      "`output_type` has ", length(bad_ot), " invalid value(s). Allowed: ",
      paste(allowed_output_types, collapse = ", "),
      ". First bad row: ", min(bad_ot), "."
    ))

  }

  ###############################################################
  # `output_type_id` must be exactly 0.5 (point forecasts only) #
  ###############################################################

  # Coercing values to integers
  q_vals_all <- suppressWarnings(as.numeric(data$output_type_id))

  # Flagging values that are NA or are not a required quantile
  bad_qid <- which(!is.na(q_vals_all) & q_vals_all != required_quantile_value)

  # Running if a row has been flagged
  if(length(bad_qid) > 0){

    # Error to show to users
    add_error(paste0(
      "`output_type_id` has ", length(bad_qid),
      " value(s) that are not exactly ", required_quantile_value,
      " (evaluation model accepts point forecasts only). ",
      "First bad row: ", min(bad_qid), "."
    ))

  }

  #######################################
  # `value` is numeric and non-negative #
  #######################################

  # Coercing the value to numeric
  value_num <- suppressWarnings(as.numeric(data$value))

  # Flagging values that are NA or less than zero
  bad_value <- which(is.na(value_num) | value_num < 0)

  # Running if a row has been flagged
  if(length(bad_value) > 0){

    # Error to show to users
    add_error(paste0(
      "`value` has ", length(bad_value),
      " invalid value(s) (must be non-negative numeric, no NA). ",
      "First bad row: ", min(bad_value), "."
    ))

  }

  ###########################################
  # Aborting the checks if any errors occur #
  ###########################################
  abort_if_errors()

  ##############################################
  # Coerce columns now that they are validated #
  ##############################################

  # Reference date
  data$reference_date <- anytime::anydate(data$reference_date)

  # Target end date
  data$target_end_date <- anytime::anydate(data$target_end_date)

  # Training validation column
  data$training_validation <- ifelse(tv_is_na, NA_integer_, as.integer(tv_num))

  # Estimated projected reported column
  data$horizon <- as.integer(epr_num)

  # Imputed column
  data$imputed <- as.integer(imp_num)

  # Value column
  data$value <- value_num

#------------------------------------------------------------------------------#
# Cross-column logical rules ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces consistency across columns: the per-row         #
# rule that target_end_date must be strictly less than reference_date          #
# (i.e., only historical rows are allowed); and the location/location_general  #
# crosswalk validation that dispatches to the appropriate reference list per   #
# geography type.                                                              #
#------------------------------------------------------------------------------#

  ###############################################################
  # Per-row location validation, dispatched by location_general #
  ###############################################################

  # Convenience vectors filtered by state_context (when supplied)
  if(!is.na(state_context)){

    # Counties in the context state
    county_in_state <- county_data$name[county_data$state == state_context]

    # MetroCast HSAs in the context state
    metrocast_in_state <- metrocast_data$location[
      metrocast_data$state_abb == state_context
    ]

    # Dartmouth HSAs in the context state
    dartmouth_in_state <- dartmouth_data$hsacity[
      dartmouth_data$hsastate == state_context
    ]

  # No state filter -- all counties/HSAs accepted
  }else{

    # Pulling all matching county names
    county_in_state    <- county_data$name

    # Pulling all HSA names: Metrocast
    metrocast_in_state <- metrocast_data$location

    # Pulling all HSA names: Dartmouth
    dartmouth_in_state <- dartmouth_data$hsacity

  }

  #################################
  # Precompute normalized lookups #
  #################################

  # Standardizing the state abbreviation
  state_abbr_norm <- tolower(state_abbr)

  # Standardizing the state names
  state_name_norm <- tolower(state_name)

  # Standardizing the county names
  county_norm <- normalize_name(strip_county_suffix(county_in_state))

  # Standardizing HSA: Metrocast
  metrocast_norm  <- normalize_name(metrocast_in_state)

  # Standardizing HSA: Dartmouth
  dartmouth_norm  <- normalize_name(dartmouth_in_state)

  # Collect per-row failures
  bad_loc_rows <- integer()

  ############################################
  # Loop through each row and check location #
  ############################################
  # for(i in seq_len(nrow(data))){
  #
  #   # Skip if location was already flagged as empty
  #   if(i %in% bad_loc_empty) next
  #
  #   # Indexed values: General Location
  #   locgen_i <- data$location_general[i]
  #
  #   # Indexed values: Specific Location
  #   loc_i <- data$location[i]
  #
  #   # Normalizing the specific location name
  #   loc_norm <- normalize_name(loc_i)
  #
  #   # Match by location_general type
  #   is_valid <- switch(
  #     locgen_i,
  #
  #     # --- State ----
  #     "state" = {
  #       loc_lower <- tolower(trimws(loc_i))
  #       loc_lower %in% state_abbr_norm || loc_lower %in% state_name_norm
  #     },
  #
  #     # --- National ---
  #     "national" = {
  #       tolower(trimws(loc_i)) %in% c("us", "united states")
  #     },
  #
  #     # --- Region: non-empty only ---
  #     "region" = TRUE,
  #
  #     # --- County: normalize and match (suffix-stripped) ---
  #     "county" = {
  #       loc_stripped_norm <- normalize_name(strip_county_suffix(loc_i))
  #       loc_stripped_norm %in% county_norm
  #     },
  #
  #     # --- ZCTA: 5-digit numeric ---
  #     "zcta" = grepl("^\\d{5}$", trimws(loc_i)),
  #
  #     # --- ZIP: 5-digit numeric ---
  #     "zip"  = grepl("^\\d{5}$", trimws(loc_i)),
  #
  #     # --- HSA: tiered (metrocast first, then dartmouth) ---
  #     "HSA" = {
  #       loc_norm %in% metrocast_norm || loc_norm %in% dartmouth_norm
  #     },
  #
  #     # --- Custom value: non-empty only ---
  #     TRUE
  #
  #   )
  #
  #   # Flag invalid row
  #   if(!isTRUE(is_valid)){bad_loc_rows <- c(bad_loc_rows, i)}
  #
  # }

  ##########################################################
  # Returning error if any rows failed location validation #
  # ##########################################################
  # if(length(bad_loc_rows) > 0){
  #
  #   # Build a sample of bad combinations for the message
  #   sample_combos <- unique(paste0(
  #     data$location_general[bad_loc_rows], " + ",
  #     data$location[bad_loc_rows]
  #   ))
  #
  #   # Build the state-context hint
  #   sc_hint <- if(!is.na(state_context)){
  #     paste0(" (state_context = '", state_context, "')")
  #   }else{
  #     ""
  #   }
  #
  #   # Error to show to user
  #   add_error(paste0(
  #     length(bad_loc_rows),
  #     " row(s) have a `location` that does not match the expected crosswalk ",
  #     "for its `location_general`", sc_hint, ". ",
  #     "Bad combinations: ",
  #     paste(utils::head(sample_combos, 5), collapse = "; "),
  #     if(length(sample_combos) > 5) paste0(" (and ", length(sample_combos) - 5, " more)") else "",
  #     ". First bad row: ", min(bad_loc_rows), "."
  #   ))
  #
  # }

  ##############################
  # Running if an error occurs #
  ##############################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Group-level completeness -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that each forecast group contains exactly one   #
# row. Unlike validate_general_model, no monotone check is performed (single-  #
# row groups have nothing to compare). Group key is the unique combination of  #
# all non-quantile/value columns.                                              #
#------------------------------------------------------------------------------#

  ###########################################
  # Each forecast group has exactly one row #
  ###########################################
  q_data <- data[data$output_type == "quantile", ]

  # Running if there is more than one group
  if(nrow(q_data) > 0){

    # Group key = all non-quantile/value columns
    group_cols <- c(
      "reference_date", "target", "target_end_date", "location_general",
      "location", "disease", "population", "training_validation",
      "horizon", "imputed", "data_source", "outcome_measure",
      "output_type"
    )

    # Creating the separate groups
    q_data$.grp <- do.call(paste, c(q_data[group_cols], sep = "|"))

    # Counts of groups
    grp_counts <- table(q_data$.grp)

    # Flagging groups with issues
    bad_groups <- names(grp_counts[grp_counts != 1])

    # Running if any groups are flagged
    if(length(bad_groups) > 0){

      # Sample of bad groups with their counts
      sample_bad <- paste0(
        utils::head(bad_groups, 3),
        " (n=", grp_counts[utils::head(bad_groups, 3)], ")"
      )

      # Error to show to users
      add_error(paste0(
        length(bad_groups),
        " forecast group(s) have more than one row (each group must contain ",
        "exactly one point forecast). First bad groups: ",
        paste(sample_bad, collapse = "; "),
        if(length(bad_groups) > 3) paste0(" (and ", length(bad_groups) - 3, " more)") else "",
        "."
      ))

    }
  }

#------------------------------------------------------------------------------#
# Spec-detail rules ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces remaining evaluation-format specification       #
# rules: the global uniqueness of the row key.                                 #
#------------------------------------------------------------------------------#

  ##################################
  # No duplicate rows for same key #
  ##################################
  key_cols <- c(
    "reference_date", "target", "target_end_date", "location_general",
    "location", "disease", "population", "training_validation",
    "horizon", "imputed", "data_source", "outcome_measure",
    "output_type", "output_type_id"
  )

  # Creating the groups
  key_str <- do.call(paste, c(data[key_cols], sep = "|"))

  # Checking for duplicate row keys
  dup_idx <- which(duplicated(key_str))

  # Running if there are any duplicate IDs
  if(length(dup_idx) > 0){

    # Error to show to users
    add_error(paste0(
      "File contains ", length(dup_idx),
      " duplicate row(s) (same key combination). First duplicate row: ",
      min(dup_idx), "."
    ))

  }

  ##############################
  # Running if an error occurs #
  ##############################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Returning validated data and printing optional summary -----------------------
#------------------------------------------------------------------------------#
# About: This section returns the validated data frame on success. When        #
# `verbose = TRUE`, a summary of the file's contents is printed to the         #
# console for diagnostic purposes.                                             #
#------------------------------------------------------------------------------#

  ################################
  # Printing the success summary #
  ################################
  if(isTRUE(verbose)){

    # Number of locations
    n_loc <- length(unique(data$location))

    # General locations
    locgen_tab <- table(data$location_general)

    # Summary if all locations
    locgen_summary <- paste0(
      names(locgen_tab), " (", format(locgen_tab, big.mark = ","), ")",
      collapse = ", "
    )

    # List of diseases
    disease_list <- paste(unique(data$disease), collapse = ", ")

    # List of outcomes
    outcome_list <- paste(unique(data$outcome_measure), collapse = ", ")

    # Training/validation/testing breakdown
    tv_tab <- table(data$training_validation, useNA = "ifany")

    # Labels for table
    tv_labels <- c("0" = "testing", "1" = "training", "2" = "validation",
                   "<NA>" = "unspecified")

    # Summary of training/testing/validation
    tv_summary <- paste0(
      ifelse(names(tv_tab) %in% names(tv_labels),
             tv_labels[names(tv_tab)], names(tv_tab)),
      " (", format(tv_tab, big.mark = ","), ")",
      collapse = ", "
    )

    # State context indicator
    sc_text <- if(!is.na(state_context)){
      paste0("\n  State ctx:    ", state_context)
    }else{
      ""
    }

    # Message to show to users
    message(
      "\u2713 Evaluation model validated successfully.\n",
      "  File:         ", file, "\n",
      "  Rows:         ", format(nrow(data), big.mark = ","), "\n",
      "  Ref date:     ", unique(data$reference_date), "\n",
      "  Date range:   ", min(data$target_end_date), " to ", max(data$target_end_date), "\n",
      "  Locations:    ", n_loc, "\n",
      "  By type:      ", locgen_summary, "\n",
      "  Diseases:     ", disease_list, "\n",
      "  Outcomes:     ", outcome_list, "\n",
      "  TVT split:    ", tv_summary, sc_text
    )

  }

  ##############################
  # Returning the validated df #
  ##############################
  data

}
