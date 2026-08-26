#' Validate a FluSight forecast submission file
#'
#' Performs a full validation of a forecast submission file against the
#' FluSight 2025-2026 hubverse-format specification. Checks file structure,
#' column types, allowed values, cross-column logical consistency, and
#' group-level completeness rules. Errors are collected within each phase
#' and reported together; a phase that produces errors halts subsequent
#' phases that depend on it.
#'
#' Modeling-correctness rules (such as whether a `pmf` rate-change
#' category was correctly assigned given the underlying incidence forecast)
#' are out of scope: this function validates file format, not modeling
#' decisions.
#'
#' @param file Path to a FluSight forecast submission CSV file.
#' @param verbose Logical. If `TRUE`, prints a summary of the validated
#'   file on success. Defaults to `FALSE`.
#'
#' @return A data frame containing the loaded forecast data, with
#'   `location` preserved as character to keep leading zeros in FIPS codes.
#'
#' @export
validate_flusight_model <- function(file, verbose = FALSE) {

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
        "FluSight model validation failed with ", length(errors), " error(s):\n  - ",
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
    parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    !is.na(parsed)

  }

#------------------------------------------------------------------------------#
# Defining FluSight 2025-2026 lookup tables ------------------------------------
#------------------------------------------------------------------------------#
# About: This section centralizes all FluSight specification constants. When   #
# the FluSight spec changes year-over-year, update the values here and the     #
# rest of the function adapts automatically.                                   #
#------------------------------------------------------------------------------#

  ###############################
  # Required submission columns #
  ###############################
  required_columns <- c(
    "reference_date", "target", "horizon", "target_end_date",
    "location", "output_type", "output_type_id", "value"
  )

  ###################
  # Allowed targets #
  ###################
  allowed_targets <- c(
    "wk inc flu hosp",
    "wk flu hosp rate change",
    "wk inc flu prop ed visits",
    "peak week inc flu hosp",
    "peak inc flu hosp"
  )

  ################
  # Peak targets #
  ################
  peak_targets <- c("peak week inc flu hosp", "peak inc flu hosp")

  ##############################
  # Allowed output_type values #
  ##############################
  allowed_output_types <- c("quantile", "pmf", "sample")

  #################################################
  # Map from target to allowed output_type values #
  #################################################
  target_output_type_map <- list(
    "wk inc flu hosp"           = c("quantile", "sample"),
    "wk flu hosp rate change"   = "pmf",
    "wk inc flu prop ed visits" = "quantile",
    "peak week inc flu hosp"    = "pmf",
    "peak inc flu hosp"         = "quantile"
  )

  #######################################
  # Required quantile levels (23 total) #
  #######################################
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

  ##################################
  # Allowed rate-change categories #
  ##################################
  rate_change_categories <- c(
    "large_decrease", "decrease", "stable", "increase", "large_increase"
  )

  ###############################
  # Peak-week season date range #
  ###############################

  # Season start date
  peak_week_season_start <- as.Date("2025-11-22")

  # Season end date
  peak_week_season_end <- as.Date("2026-05-23")

  ##################################################
  # Valid season Saturdays for peak-week PMF dates #
  ##################################################
  peak_week_valid_dates <- seq.Date(
    from = peak_week_season_start,
    to   = peak_week_season_end,
    by   = "week"
  )

  ####################################
  # Targets requiring integer values #
  ####################################
  integer_value_targets <- c("wk inc flu hosp", "peak inc flu hosp")

  ################################################################
  # Required sample count per (reference_date, location, target) #
  ################################################################
  required_sample_count <- 100

  ############################################################
  # Allowed horizon values for step-ahead (non-peak) targets #
  ############################################################
  allowed_horizons <- -1:3

  ##############################################
  # Allowed horizon values for rate-change PMF #
  ##############################################
  rate_change_horizons <- 0:3

  ##############################################
  # Required horizons present per sample group #
  ##############################################
  required_sample_horizons <- -1:3

  #####################################
  # Tolerance for PMF sum-to-1 checks #
  #####################################
  pmf_sum_tolerance <- 1e-5

  #################################################
  # Allowed locations from the FluSight crosswalk #
  #################################################
  allowed_locations <- forecastEvalReport::hubverse_locations$location

#------------------------------------------------------------------------------#
# PHASE 1: File-level checks ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that the file exists on disk, can be read as a  #
# CSV, and contains at least one data row. Nothing else is checked until       #
# these basics succeed.                                                        #
#------------------------------------------------------------------------------#

  ##########################
  # Confirming file exists #
  ##########################
  if(!file.exists(file)){

    # Error to show if the file is not loaded
    add_error(paste0("File does not exist: ", file))

    # Running the error function
    abort_if_errors()

  }

  ###################
  # Reading the CSV #
  ###################
  data <- tryCatch(

    # Reading in the CSV
    utils::read.csv(
      file,
      colClasses     = c(location = "character"),  # preserve leading zeros
      stringsAsFactors = FALSE,
      na.strings     = c("NA", "")
    ),

    # Error to show if no CSV read in successfully
    error = function(e){
      add_error(paste0("File could not be read as CSV: ", conditionMessage(e)))
      NULL
    }
  )

  #################################
  # Checking if should be aborted #
  #################################
  abort_if_errors()

  #############################
  # Confirming non-empty file #
  #############################
  if(nrow(data) == 0){

    # Message to run if empty data
    add_error("File contains no data rows.")

    # Stopping if error occurs
    abort_if_errors()

  }

#------------------------------------------------------------------------------#
# PHASE 2: Column structure ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the file has exactly the 8 required        #
# FluSight columns. Missing required columns or extra unexpected columns are   #
# treated as errors. Column existence must be confirmed before per-column      #
# value checks can run.                                                        #
#------------------------------------------------------------------------------#

  ################################
  # Checking for missing columns #
  ################################
  missing_cols <- setdiff(required_columns, names(data))

  # Error that runs if missing columns
  if(length(missing_cols) > 0){

    add_error(paste0(
      "File is missing required column(s): ",
      paste(missing_cols, collapse = ", "), "."
    ))

  }

  ##############################
  # Checking for extra columns #
  ##############################
  extra_cols <- setdiff(names(data), required_columns)

  # Error to run if extra columns
  if(length(extra_cols) > 0){

    add_error(paste0(
      "File contains unexpected column(s): ",
      paste(extra_cols, collapse = ", "),
      ". Only the 8 required columns are allowed."
    ))

  }

  ############################
  # Stopping if error occurs #
  ############################
  abort_if_errors()

#------------------------------------------------------------------------------#
# PHASE 3: Per-column type and value checks ------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates each individual column against its expected    #
# type and allowed value set. Cross-column relationships are checked in        #
# Phase 4; this section is concerned only with the contents of one column at   #
# a time.                                                                      #
#------------------------------------------------------------------------------#

  #############################################
  # `reference_date` is strict ISO YYYY-MM-DD #
  #############################################
  ref_date_valid <- vapply(as.character(data$reference_date), is_iso_date, logical(1))

  # Error to run if reference date is not right format
  if(any(!ref_date_valid, na.rm = TRUE) || any(is.na(data$reference_date))){

    # Identifying the rows with issues
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

  # Coerce locally so this check can run before the global coercion step
  ref_dates_parsed <- suppressWarnings(as.Date(data$reference_date))

  # Saturday corresponds to POSIXlt$wday == 6 (independent of locale)
  ref_dow <- as.POSIXlt(ref_dates_parsed)$wday

  # Determine non-Saturday rows
  bad_ref_dow <- which(!is.na(ref_dow) & ref_dow != 6L)

  # Returning error if necessary
  if(length(bad_ref_dow) > 0){

    # Error message to return to users
    add_error(paste0(
      "`reference_date` has ", length(bad_ref_dow),
      " row(s) that are not on a Saturday. ",
      "FluSight reference_date must be the Saturday ending the EW containing the ",
      "Forecast Due Date. First bad row: ", min(bad_ref_dow), "."
    ))

  }

  ###############################################
  # Confirming a single reference_date per file #
  ###############################################

  # Pull unique non-NA reference_date values
  unique_ref <- unique(data$reference_date[!is.na(data$reference_date)])

  # Returning error if more than one
  if(length(unique_ref) > 1){

    # Error message to return to users
    add_error(paste0(
      "File contains ", length(unique_ref),
      " unique `reference_date` values; exactly 1 is allowed per submission. ",
      "Found: ", paste(utils::head(unique_ref, 5), collapse = ", "),
      if(length(unique_ref) > 5) paste0(" (and ", length(unique_ref) - 5, " more)") else "",
      "."
    ))

  }

  ##################################################
  # target_end_date is strict ISO YYYY-MM-DD OR NA #
  ##################################################

  # Checking if the TED is valid
  ted_valid   <- vapply(as.character(data$target_end_date), is_iso_date, logical(1))

  # Checking if the TED is NA
  ted_is_na   <- is.na(data$target_end_date)

  # Determining if row is a peak estimate
  is_peak_row <- data$target %in% peak_targets

  # Checking for errors
  bad_ted <- which((!is_peak_row & ted_is_na) | (!ted_is_na & !ted_valid))

  # Message to run if TED is incorrect
  if(length(bad_ted) > 0){

    # Error to return to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted),
      " invalid value(s) (must be ISO YYYY-MM-DD; NA allowed only for peak targets). ",
      "First bad row: ", min(bad_ted), "."
    ))

  }

  #############################################
  # Confirming target_end_date is on Saturday #
  #############################################

  # Local Date coercion for early checking
  ted_parsed <- suppressWarnings(as.Date(data$target_end_date))

  # Saturday weekday check (skipping NAs which are valid for peak rows)
  ted_dow <- as.POSIXlt(ted_parsed)$wday

  # Determine non-Saturday rows (NA wday is OK; only flag concrete non-Saturdays)
  bad_ted_dow <- which(!is.na(ted_dow) & ted_dow != 6L)

  # Returning error if necessary
  if(length(bad_ted_dow) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted_dow),
      " row(s) that are not on a Saturday. ",
      "Each target_end_date must be the Saturday ending an epidemiological week. ",
      "First bad row: ", min(bad_ted_dow), "."
    ))

  }

  #################################################
  # `target` value must be one of allowed_targets #
  #################################################

  # Checking the target value against list
  bad_target <- which(is.na(data$target) | !data$target %in% allowed_targets)

  # Returning error if necessary
  if(length(bad_target) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target` has ", length(bad_target), " invalid value(s). Allowed: ",
      paste(allowed_targets, collapse = ", "),
      ". First bad row: ", min(bad_target), "."
    ))

  }

  #############################################################
  # `horizon` is integer-coercible (NA only for peak targets) #
  #############################################################

  # Checking horizon can be turned to integer
  horizon_num <- suppressWarnings(as.numeric(data$horizon))

  # Checking if horizon is NA
  horizon_is_na <- is.na(data$horizon) | is.na(horizon_num)

  # Confirming horizon is an integer
  horizon_is_integer <- !horizon_is_na & (horizon_num == floor(horizon_num))

  # Determining if any horizons 'errored' out
  bad_horizon <- which((!is_peak_row & horizon_is_na) |
                         (!horizon_is_na & !horizon_is_integer))

  # Returning error if necessary
  if(length(bad_horizon) > 0){

    # Error message to return to users
    add_error(paste0(
      "`horizon` has ", length(bad_horizon),
      " invalid value(s) (must be integer; NA allowed only for peak targets). ",
      "First bad row: ", min(bad_horizon), "."
    ))

  }

  ######################################################
  # Confirming horizon falls within allowed -1:3 range #
  ######################################################

  # Identify rows that need a horizon range check (non-peak, non-NA horizon)
  horizon_check_rows <- which(!is_peak_row & !horizon_is_na)

  # Determine which of those are out of range
  bad_horizon_range <- horizon_check_rows[
    !horizon_num[horizon_check_rows] %in% allowed_horizons
  ]

  # Returning error if necessary
  if(length(bad_horizon_range) > 0){

    # Error message to return to users
    add_error(paste0(
      "`horizon` has ", length(bad_horizon_range),
      " row(s) outside the allowed range (",
      paste(range(allowed_horizons), collapse = " to "),
      "). First bad row: ", min(bad_horizon_range), "."
    ))

  }

  #####################################################
  # `output_type` must be one of allowed_output_types #
  #####################################################

  # Checking if any 'output_types' do not match
  bad_ot <- which(is.na(data$output_type) | !data$output_type %in% allowed_output_types)

  # Returning error if necessary
  if(length(bad_ot) > 0){

    # Error message to return to users
    add_error(paste0(
      "`output_type` has ", length(bad_ot), " invalid value(s). Allowed: ",
      paste(allowed_output_types, collapse = ", "),
      ". First bad row: ", min(bad_ot), "."
    ))

  }

  ###########################
  # `location` is non-empty #
  ###########################

  # Checking if any locations are empty
  bad_loc_empty <- which(is.na(data$location) | nchar(trimws(data$location)) == 0)

  # Returning error if necessary
  if(length(bad_loc_empty) > 0){

    # Error message to return to users
    add_error(paste0(
      "`location` has ", length(bad_loc_empty), " missing or empty value(s). ",
      "First bad row: ", min(bad_loc_empty), "."
    ))

  }

  #######################################
  # `value` is numeric and non-negative #
  #######################################

  # Checking if value can be coercied to number
  value_num <- suppressWarnings(as.numeric(data$value))

  # Checking if any value are NA or non-integers
  bad_value <- which(is.na(value_num) | value_num < 0)

  # Returning error if necessary
  if(length(bad_value) > 0){

    # Error message to return to users
    add_error(paste0(
      "`value` has ", length(bad_value),
      " invalid value(s) (must be non-negative numeric, no NA). ",
      "First bad row: ", min(bad_value), "."
    ))

  }

  ##############################################
  # location must be in the FluSight crosswalk #
  ##############################################

  # Checking if location is not flagged above
  loc_present <- !(seq_len(nrow(data)) %in% bad_loc_empty)

  # Checking that location is in cross walk
  bad_loc_unknown <- which(loc_present & !data$location %in% allowed_locations)

  # Returning error if necessary
  if(length(bad_loc_unknown) > 0){

    # Flagging unknown locations
    unknown_vals <- unique(data$location[bad_loc_unknown])

    # Error message to return to users
    add_error(paste0(
      "`location` has ", length(bad_loc_unknown),
      " row(s) with values not in the FluSight location crosswalk. ",
      "Unknown value(s): ", paste(utils::head(unknown_vals, 5), collapse = ", "),
      if(length(unknown_vals) > 5) paste0(" (and ", length(unknown_vals) - 5, " more)") else "",
      ". First bad row: ", min(bad_loc_unknown), "."
    ))

  }

  ###########################################
  # Aborting the checks if any errors occur #
  ###########################################
  abort_if_errors()

  ##############################################
  # Coerce columns now that they are validated #
  ##############################################

  # Reference date to 'date'
  data$reference_date  <- as.Date(data$reference_date)

  # Target end date to 'date'
  data$target_end_date <- as.Date(data$target_end_date)

  # Horizon to numeric
  data$horizon <- as.integer(horizon_num)

  # Value to numeric
  data$value <- value_num

#------------------------------------------------------------------------------#
# PHASE 4: Cross-column logical rules ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces consistency across columns: the relationship    #
# between reference_date, horizon, and target_end_date; the compatibility of   #
# target with output_type; and the format of output_type_id given its          #
# associated output_type.                                                      #
#------------------------------------------------------------------------------#

  ##################################################
  # target_end_date = reference_date + horizon * 7 #
  ##################################################

  # Making sure row has horizon
  has_horizon  <- !is.na(data$horizon)

  # Calculating the expected TED
  expected_ted <- data$reference_date + data$horizon * 7L

  # Determining if the TED is incorrect
  bad_ted_calc <- which(has_horizon & !is.na(data$target_end_date) &
                          data$target_end_date != expected_ted)

  # Returning error if necessary
  if(length(bad_ted_calc) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target_end_date` does not equal `reference_date + horizon * 7` for ",
      length(bad_ted_calc), " row(s). First bad row: ", min(bad_ted_calc), "."
    ))

  }

  ##################################################
  # Peak rows must have NA horizon / target_end_dt #
  ##################################################

  # Peak rows with a non-NA horizon (invalid)
  bad_peak_horizon <- which(is_peak_row & !is.na(data$horizon))

  # Returning error if necessary
  if(length(bad_peak_horizon) > 0){

    # Error message to return to users
    add_error(paste0(
      "`horizon` must be NA for peak targets but was non-NA in ",
      length(bad_peak_horizon), " row(s). First bad row: ",
      min(bad_peak_horizon), "."
    ))

  }

  # Peak rows with a non-NA target_end_date (invalid)
  bad_peak_ted <- which(is_peak_row & !is.na(data$target_end_date))

  # Returning error if necessary
  if(length(bad_peak_ted) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target_end_date` must be NA for peak targets but was non-NA in ",
      length(bad_peak_ted), " row(s). First bad row: ",
      min(bad_peak_ted), "."
    ))

  }

  ####################################################
  # `output_type` must match what each target allows #
  ####################################################

  # Collect violating rows in a vector (no early break -- consistent with
  # the rest of the validator's collect-all-errors pattern)
  bad_target_ot <- integer()

  for(i in seq_len(nrow(data))){

    # Indexed target
    target_i <- data$target[i]

    # Indexed output type
    ot_i <- data$output_type[i]

    # Checking if the output_type is allowed given the target
    if(!is.na(target_i) && target_i %in% names(target_output_type_map)){

      # Allowed output type values
      allowed <- target_output_type_map[[target_i]]

      # Flagging the row if not allowed
      if(!ot_i %in% allowed){
        bad_target_ot <- c(bad_target_ot, i)
      }

    }
  }

  # Returning a single aggregated error if any violations
  if(length(bad_target_ot) > 0){

    # Sample of bad combinations for the message
    sample_combos <- unique(paste0(
      data$target[bad_target_ot], " + ",
      data$output_type[bad_target_ot]
    ))

    add_error(paste0(
      length(bad_target_ot),
      " row(s) have an `output_type` not allowed for their `target`. ",
      "Bad combinations: ",
      paste(utils::head(sample_combos, 5), collapse = "; "),
      if(length(sample_combos) > 5) paste0(" (and ", length(sample_combos) - 5, " more)") else "",
      ". First bad row: ", min(bad_target_ot), "."
    ))

  }

  #################################################
  # Rate-change PMF rows must have horizon in 0:3 #
  #################################################

  # Identify rate-change rows
  rc_rows <- which(data$target == "wk flu hosp rate change")

  # Find rate-change rows whose horizon is outside 0:3 (or NA)
  bad_rc_horizon <- rc_rows[
    is.na(data$horizon[rc_rows]) |
      !data$horizon[rc_rows] %in% rate_change_horizons
  ]

  # Returning error if necessary
  if(length(bad_rc_horizon) > 0){

    # Error message to return to users
    add_error(paste0(
      "`wk flu hosp rate change` has ", length(bad_rc_horizon),
      " row(s) with horizon outside the allowed range (",
      paste(range(rate_change_horizons), collapse = " to "),
      "). First bad row: ", min(bad_rc_horizon), "."
    ))

  }

  ################################################
  # output_type_id format depends on output_type #
  ################################################

  # Determining what rows correspond to quantiles
  q_rows  <- which(data$output_type == "quantile")

  # Confirming quantiles can be coerced to numeric
  q_vals  <- suppressWarnings(as.numeric(data$output_type_id[q_rows]))

  # Confirming rows that have issues
  bad_qid <- q_rows[is.na(q_vals) | q_vals <= 0 | q_vals >= 1]

  # Returning error if necessary
  if(length(bad_qid) > 0){

    # Error message to show to users
    add_error(paste0(
      "`output_type_id` has ", length(bad_qid),
      " invalid quantile value(s) (must be numeric strictly between 0 and 1). ",
      "First bad row: ", min(bad_qid), "."
    ))

  }

  # Determining rows where output type should be a PMF (Rate Change)
  pmf_rc_rows <- which(data$output_type == "pmf" & data$target == "wk flu hosp rate change")

  # Confirming rows that have issues
  bad_pmf_rc  <- pmf_rc_rows[!data$output_type_id[pmf_rc_rows] %in% rate_change_categories]

  # Returning error if necessary
  if(length(bad_pmf_rc) > 0){

    # Error message to show to users
    add_error(paste0(
      "`output_type_id` has ", length(bad_pmf_rc),
      " invalid rate-change category value(s). Allowed: ",
      paste(rate_change_categories, collapse = ", "),
      ". First bad row: ", min(bad_pmf_rc), "."
    ))

  }

  ##################################################
  # Peak-week PMF dates must be valid EW Saturdays #
  ##################################################

  # Identify peak-week PMF rows
  pmf_pw_rows <- which(data$output_type == "pmf" &
                         data$target == "peak week inc flu hosp")

  # Coerce output_type_id to Date for those rows
  pmf_pw_dates <- suppressWarnings(as.Date(data$output_type_id[pmf_pw_rows]))

  # Flag rows whose date is not in the set of valid season Saturdays
  bad_pmf_pw <- pmf_pw_rows[
    is.na(pmf_pw_dates) | !pmf_pw_dates %in% peak_week_valid_dates
  ]

  # Returning error if necessary
  if(length(bad_pmf_pw) > 0){

    # Error message to show to users
    add_error(paste0(
      "`output_type_id` for peak-week PMF has ", length(bad_pmf_pw),
      " value(s) that are not valid season Saturdays (must be one of ",
      length(peak_week_valid_dates), " Saturdays from ",
      peak_week_season_start, " to ", peak_week_season_end,
      "). First bad row: ", min(bad_pmf_pw), "."
    ))

  }

  # Determining the `sample` rows
  s_rows  <- which(data$output_type == "sample")

  # Identifying issue rows
  bad_sid <- s_rows[is.na(data$output_type_id[s_rows]) |
                      nchar(trimws(data$output_type_id[s_rows])) == 0]

  # Returning error if necessary
  if(length(bad_sid) > 0){

    # Error message to show to users
    add_error(paste0(
      "`output_type_id` has ", length(bad_sid),
      " invalid sample identifier(s) (must be non-empty). ",
      "First bad row: ", min(bad_sid), "."
    ))

  }

  # Stopping the code if an error occurs
  abort_if_errors()

#------------------------------------------------------------------------------#
# PHASE 5: Group-level completeness --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that grouped predictions form complete sets:    #
# all 23 quantile levels per quantile group, monotone quantile values, all 5   #
# rate-change categories present and summing to 1, peak-week pmf summing to    #
# 1, and 100 unique sample IDs per sample group with full horizon coverage.    #
#------------------------------------------------------------------------------#

  #############################################
  # Confirming that all quantiles are present #
  #############################################

  # Pulling the quantile column
  q_data <- data[data$output_type == "quantile", ]

  # Looping through each quantile
  if(nrow(q_data) > 0){

    # Pulling the quantile in the row
    q_data$.qnum <- as.numeric(q_data$output_type_id)

    # Pulling the `unique` indicators for the quantile
    q_data$.grp  <- paste(q_data$reference_date, q_data$location,
                          q_data$target, q_data$horizon, sep = "|")

    # Creating a group based on unique indicators
    grp_split   <- split(q_data$.qnum, q_data$.grp)

    # Empty vector to store 'off' rows
    bad_groups  <- character()

    # Looping through quantile groupings
    for(g in names(grp_split)){

      # Sorting the quantiles
      qs <- sort(unique(grp_split[[g]]))

      # Determining if there are any 'off' quantiles
      if(!isTRUE(all.equal(qs, required_quantiles))){

        # Populating the empty vector
        bad_groups <- c(bad_groups, g)

      }

    }

    # Returning error if necessary
    if(length(bad_groups) > 0){

      # Error message to show to user
      add_error(paste0(
        length(bad_groups), " quantile group(s) are missing required quantile levels ",
        "(expected 23 levels: ", paste(required_quantiles, collapse = ", "),
        "). First bad group: ", bad_groups[1], "."
      ))

    }
  }

  ########################################################
  # Quantile values monotone non-decreasing within group #
  ########################################################
  if(nrow(q_data) > 0){

    # Empty vector to store violation rows
    bad_mono  <- character()

    # Looking at unique groupings w/quantiles
    grp_split <- split(q_data, q_data$.grp)

    # Looping through the unique groups
    for(g in names(grp_split)){

      # Indexed quantile group
      sub <- grp_split[[g]]

      # Checking for montone non-decreasing
      sub <- sub[order(sub$.qnum), ]

      # Checking if there were any violations
      if(any(diff(sub$value) < 0)){bad_mono <- c(bad_mono, g)}

    }

    # Returning error if necessary
    if(length(bad_mono) > 0){

      # Error message to show to users
      add_error(paste0(
        length(bad_mono), " quantile group(s) have non-monotone values. ",
        "First bad group: ", bad_mono[1], "."
      ))

    }

  }

  ###################################################
  # PMF rate change: 5 categories present, sum to 1 #
  ###################################################

  # Pulling the PMF & Rate Change rows
  pmf_rc_data <- data[data$output_type == "pmf" &
                      data$target == "wk flu hosp rate change", ]

  # Checking if the rows exist
  if(nrow(pmf_rc_data) > 0){

    # Creating the unique groups for horizons
    pmf_rc_data$.grp <- paste(pmf_rc_data$reference_date, pmf_rc_data$location,
                              pmf_rc_data$horizon, sep = "|")

    # Splitting data into groups
    grp_split <- split(pmf_rc_data, pmf_rc_data$.grp)

    # Empty vector to store violating categories
    bad_cat <- character()

    # Empty vector to store violating sums
    bad_sum <- character()

    # Looping through unique groups
    for(g in names(grp_split)){

      # Indexing the group
      sub  <- grp_split[[g]]

      # Sorting group by category
      cats <- sort(unique(sub$output_type_id))

      # Determining if the category is a violation
      if(!setequal(cats, rate_change_categories)){bad_cat <- c(bad_cat, g)}

      # Determining if the sum is in violation
      if(!isTRUE(all.equal(sum(sub$value), 1, tolerance = pmf_sum_tolerance))){bad_sum <- c(bad_sum, g)}

    }

    # Returning error if necessary
    if(length(bad_cat) > 0){

      # Error message to show to users
      add_error(paste0(
        length(bad_cat), " rate-change pmf group(s) are missing categories. ",
        "First bad group: ", bad_cat[1], "."
      ))

    }

    # Returning error if necessary
    if(length(bad_sum) > 0){

      # Error message to show to users
      add_error(paste0(
        length(bad_sum), " rate-change pmf group(s) do not sum to 1. ",
        "First bad group: ", bad_sum[1], "."
      ))

    }
  }

  ############################################
  # PMF peak week: values sum to 1 per group #
  ############################################

  # Pulling PMF & Peak Week rows
  pmf_pw_data <- data[data$output_type == "pmf" &
                        data$target == "peak week inc flu hosp", ]

  # Running if only PMF & Peak Weeks Rows are present
  if(nrow(pmf_pw_data) > 0){

    # Creating the unique groups
    pmf_pw_data$.grp <- paste(pmf_pw_data$reference_date,
                              pmf_pw_data$location, sep = "|")

    # Splitting data by group
    grp_split <- split(pmf_pw_data, pmf_pw_data$.grp)

    # Empty data frame to store bad sums
    bad_sum <- character()

    # Looping through created groups
    for(g in names(grp_split)){

      # Indexed group
      sub <- grp_split[[g]]

      # Determining if bad sum
      if(!isTRUE(all.equal(sum(sub$value), 1, tolerance = pmf_sum_tolerance))){bad_sum <- c(bad_sum, g)}

    }

    # Returning error if necessary
    if(length(bad_sum) > 0){

      # Error message to show to users
      add_error(paste0(
        length(bad_sum), " peak-week pmf group(s) do not sum to 1. ",
        "First bad group: ", bad_sum[1], "."
      ))

    }
  }

  ########################################################
  # Sample completeness: 100 unique IDs per sample group #
  ########################################################

  # Pulling rows that are samples
  s_data <- data[data$output_type == "sample", ]

  # Running if samples are present
  if(nrow(s_data) > 0){

    # Creating unique group
    s_data$.grp <- paste(s_data$reference_date, s_data$location,
                         s_data$target, sep = "|")

    # Splitting data by group
    grp_split <- split(s_data$output_type_id, s_data$.grp)

    # Empty vector to store bad count
    bad_count <- character()

    # Looping through unique groups
    for(g in names(grp_split)){

      # Number of unique groups
      n_unique <- length(unique(grp_split[[g]]))

      # Determining if bad count
      if(n_unique != required_sample_count){bad_count <- c(bad_count, paste0(g, " (", n_unique, ")"))}

    }

    # Returning error if necessary
    if(length(bad_count) > 0){

      # Error message to show to users
      add_error(paste0(
        length(bad_count), " sample group(s) do not have exactly ",
        required_sample_count, " unique sample IDs. First bad group: ",
        bad_count[1], "."
      ))

    }
  }

  ###############################################
  # Sample IDs must cover all required horizons #
  ###############################################

  # Pull sample rows again with full row data (5e used IDs only)
  s_data_traj <- data[data$output_type == "sample", ]

  # Running if any sample types are present
  if(nrow(s_data_traj) > 0){

    # Build group key (one trajectory bundle per ref-date/location/target)
    s_data_traj$.grp <- paste(
      s_data_traj$reference_date,
      s_data_traj$location,
      s_data_traj$target,
      sep = "|"
    )

    # Empty vectors to collect bad groups
    bad_traj_horizon <- character()
    bad_traj_count   <- character()

    # Split by group
    grp_split_traj <- split(s_data_traj, s_data_traj$.grp)

    # Loop over each group
    for(g in names(grp_split_traj)){

      # Subset for the group
      sub <- grp_split_traj[[g]]

      # For each sample ID, find the set of horizons it covers
      horizon_by_id <- split(sub$horizon, sub$output_type_id)

      # Check each ID covers exactly the required horizon set, once each
      for(id in names(horizon_by_id)){

        # Indexed horizon
        h_vec <- horizon_by_id[[id]]

        # Wrong horizon set
        if(!setequal(h_vec, required_sample_horizons)){
          bad_traj_horizon <- c(bad_traj_horizon, paste0(g, " / id=", id))
          break  # one violation per group is enough
        }

        # Duplicate horizons within an ID
        if(length(h_vec) != length(required_sample_horizons)){
          bad_traj_count <- c(bad_traj_count, paste0(g, " / id=", id))
          break
        }

      }
    }

    # Returning error if any IDs lack full horizon coverage
    if(length(bad_traj_horizon) > 0){

      # Error to show to user
      add_error(paste0(
        length(bad_traj_horizon),
        " sample trajectory(ies) do not span all required horizons (",
        paste(range(required_sample_horizons), collapse = " to "),
        "). First bad: ", bad_traj_horizon[1], "."
      ))

    }

    # Returning error if any IDs have duplicate horizon rows
    if(length(bad_traj_count) > 0){

      # Error to show to user
      add_error(paste0(
        length(bad_traj_count),
        " sample trajectory(ies) have duplicate horizon rows for a single ID. ",
        "First bad: ", bad_traj_count[1], "."
      ))

    }
  }

#------------------------------------------------------------------------------#
# PHASE 6: Spec-detail rules ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces remaining FluSight specification rules: ED      #
# visit values constrained to [0, 1], integer-only enforcement on weekly       #
# incidence and peak incidence targets, and global uniqueness of the row key.  #
#------------------------------------------------------------------------------#

  #####################################
  # ED visit values must be in [0, 1] #
  #####################################

  # Pulling ED rows
  ed_rows <- which(data$target == "wk inc flu prop ed visits")

  # Checking if ED row values are not correct
  bad_ed  <- ed_rows[data$value[ed_rows] < 0 | data$value[ed_rows] > 1]

  # Returning error if any IDs have duplicate horizon rows
  if(length(bad_ed) > 0){

    # Error to show to user
    add_error(paste0(
      "`value` has ", length(bad_ed),
      " out-of-range value(s) for target 'wk inc flu prop ed visits' ",
      "(must be in [0, 1]). First bad row: ", min(bad_ed), "."
    ))

  }

  ################################################
  # Integer-only enforcement on specific targets #
  ################################################

  # Checking which targets can be integers
  int_rows <- which(data$target %in% integer_value_targets)

  # Pulling mis-match integer rows
  bad_int  <- int_rows[data$value[int_rows] != floor(data$value[int_rows])]

  # Returning errors if necessary
  if(length(bad_int) > 0){

    # Error to show to the user
    add_error(paste0(
      "`value` has ", length(bad_int),
      " non-integer value(s) for integer-only targets (",
      paste(integer_value_targets, collapse = ", "),
      "). First bad row: ", min(bad_int), "."
    ))

  }

  ##################################
  # No duplicate rows for same key #
  ##################################

  # Key columns
  key_cols <- c("reference_date", "target", "horizon", "target_end_date",
                "location", "output_type", "output_type_id")

  # Pulling unique columns
  key_str  <- do.call(paste, c(data[key_cols], sep = "|"))

  # Looking for duplicate rows
  dup_idx  <- which(duplicated(key_str))

  # Returning errors if necessary
  if(length(dup_idx) > 0){

    # Error to show to the user
    add_error(paste0(
      "File contains ", length(dup_idx),
      " duplicate row(s) (same key combination). First duplicate row: ",
      min(dup_idx), "."
    ))

  }

  # Stopping if errors occured
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

    # Tabulated the output type
    ot_table    <- table(data$output_type)

    # Tabulating the target type
    target_list <- paste(unique(data$target), collapse = ", ")

    # Creating the success summary
    ot_summary  <- paste0(
      names(ot_table), " (", format(ot_table, big.mark = ","), ")",
      collapse = ", "
    )

    # Number of locations
    n_loc    <- length(unique(data$location))

    # Has US function
    has_us   <- "US" %in% data$location

    # Text about location
    loc_text <- if(has_us) paste0(n_loc, " (incl. US)") else as.character(n_loc)

    # Message to print
    message(
      "\u2713 FluSight model validated successfully.\n",
      "  File:         ", file, "\n",
      "  Rows:         ", format(nrow(data), big.mark = ","), "\n",
      "  Date range:   ", min(data$reference_date), " to ", max(data$reference_date), "\n",
      "  Locations:    ", loc_text, "\n",
      "  Targets:      ", target_list, "\n",
      "  Output types: ", ot_summary
    )

  }

  ##############################
  # Returning the validated df #
  ##############################
  data

}
