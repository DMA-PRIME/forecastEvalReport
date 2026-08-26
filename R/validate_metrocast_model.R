#' Validate a Flu MetroCast forecast submission file
#'
#' Performs a full validation of a forecast submission file against the Flu
#' MetroCast Hub hubverse-format specification. Checks file structure,
#' column types, allowed values, cross-column logical consistency, and
#' group-level completeness rules. Errors are collected within each phase
#' and reported together; a phase that produces errors halts subsequent
#' phases that depend on it.
#'
#' Modeling-correctness rules are out of scope: this function validates
#' file format, not modeling decisions.
#'
#' @param file Path to a Flu MetroCast Hub submission CSV file.
#' @param verbose Logical. If `TRUE`, prints a summary of the validated
#'   file on success. Defaults to `FALSE`.
#'
#' @return A data frame containing the loaded forecast data, with
#'   `location` preserved as character.
#'
#' @export
validate_metrocast_model <- function(file, verbose = FALSE) {

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
        "MetroCast model validation failed with ", length(errors), " error(s):\n  - ",
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
# Defining MetroCast Hub lookup tables -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section centralizes all Flu MetroCast specification constants.   #
# When the spec changes, update the values here and the rest of the function   #
# adapts automatically.                                                        #
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
    "Flu ED visits pct",
    "ILI ED visits pct"
  )

  #################################################
  # Targets restricted to specific locations only #
  #################################################
  # ILI ED visits pct is NYC-only per the spec
  nyc_only_targets <- c("ILI ED visits pct")

  ##############################
  # Allowed output_type values #
  ##############################
  allowed_output_types <- c("quantile")

  ######################################
  # Required quantile levels (9 total) #
  ######################################
  required_quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

  #####################################
  # Allowed horizon values (0:3 only) #
  #####################################
  allowed_horizons <- 0:3

  ##################################################
  # Allowed locations from the MetroCast crosswalk #
  ##################################################
  allowed_locations <- forecastEvalReport::metrocast_locations$location

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
      colClasses     = c(location = "character"),  # preserve hyphenated names
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
# MetroCast columns. Missing required columns or extra unexpected columns are  #
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
      "MetroCast reference_date must be the Saturday following the Forecast ",
      "Due Date. First bad row: ", min(bad_ref_dow), "."
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

  ##############################################
  # `target_end_date` is strict ISO YYYY-MM-DD #
  ##############################################

  # Checking if the TED is valid
  ted_valid <- vapply(as.character(data$target_end_date), is_iso_date, logical(1))

  # Checking if the TED is NA
  ted_is_na <- is.na(data$target_end_date)

  # Checking for errors -- MetroCast has no peak targets, NA never allowed
  bad_ted <- which(ted_is_na | !ted_valid)

  # Message to run if TED is incorrect
  if(length(bad_ted) > 0){

    # Error to return to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted),
      " invalid value(s) (must be ISO YYYY-MM-DD, no NA allowed). ",
      "First bad row: ", min(bad_ted), "."
    ))

  }

  #############################################
  # Confirming target_end_date is on Saturday #
  #############################################

  # Local Date coercion for early checking
  ted_parsed <- suppressWarnings(as.Date(data$target_end_date))

  # Saturday weekday check
  ted_dow <- as.POSIXlt(ted_parsed)$wday

  # Determine non-Saturday rows
  bad_ted_dow <- which(!is.na(ted_dow) & ted_dow != 6L)

  # Returning error if necessary
  if(length(bad_ted_dow) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted_dow),
      " row(s) that are not on a Saturday. ",
      "Each target_end_date must be the Saturday ending an EW. ",
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

  ##################################
  # `horizon` is integer-coercible #
  ##################################

  # Checking horizon can be turned to integer
  horizon_num <- suppressWarnings(as.numeric(data$horizon))

  # Checking if horizon is NA
  horizon_is_na <- is.na(data$horizon) | is.na(horizon_num)

  # Confirming horizon is an integer
  horizon_is_integer <- !horizon_is_na & (horizon_num == floor(horizon_num))

  # Determining if any horizons errored out -- MetroCast never allows NA horizon
  bad_horizon <- which(horizon_is_na | !horizon_is_integer)

  # Returning error if necessary
  if(length(bad_horizon) > 0){

    # Error message to return to users
    add_error(paste0(
      "`horizon` has ", length(bad_horizon),
      " invalid value(s) (must be integer, no NA allowed). ",
      "First bad row: ", min(bad_horizon), "."
    ))

  }

  #####################################################
  # Confirming horizon falls within allowed 0:3 range #
  #####################################################

  # Identify rows that need a horizon range check (non-NA horizon)
  horizon_check_rows <- which(!horizon_is_na)

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

  # Checking if value can be coerced to number
  value_num <- suppressWarnings(as.numeric(data$value))

  # Checking if any value are NA or negative
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

  ###############################################
  # location must be in the MetroCast crosswalk #
  ###############################################

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
      " row(s) with values not in the MetroCast location crosswalk. ",
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

  # Horizon to integer
  data$horizon <- as.integer(horizon_num)

  # Value to numeric
  data$value <- value_num

#------------------------------------------------------------------------------#
# PHASE 4: Cross-column logical rules ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces consistency across columns: the relationship    #
# between reference_date, horizon, and target_end_date; the format of          #
# output_type_id given output_type; and the target/location pairing rule       #
# that restricts ILI ED visits pct forecasts to NYC.                           #
#------------------------------------------------------------------------------#

  ##################################################
  # target_end_date = reference_date + horizon * 7 #
  ##################################################

  # Calculating the expected TED
  expected_ted <- data$reference_date + data$horizon * 7L

  # Determining if the TED is incorrect
  bad_ted_calc <- which(!is.na(data$horizon) & !is.na(data$target_end_date) &
                          data$target_end_date != expected_ted)

  # Returning error if necessary
  if(length(bad_ted_calc) > 0){

    # Error message to return to users
    add_error(paste0(
      "`target_end_date` does not equal `reference_date + horizon * 7` for ",
      length(bad_ted_calc), " row(s). First bad row: ", min(bad_ted_calc), "."
    ))

  }

  ##############################################
  # ILI ED visits pct must use NYC location    #
  ##############################################

  # Identify NYC-only target rows
  nyc_only_rows <- which(data$target %in% nyc_only_targets)

  # Find any with a non-NYC location
  bad_nyc_only <- nyc_only_rows[data$location[nyc_only_rows] != "nyc"]

  # Returning error if necessary
  if(length(bad_nyc_only) > 0){

    # Error message to return to users
    add_error(paste0(
      "`ILI ED visits pct` is restricted to location `nyc`, but ",
      length(bad_nyc_only), " row(s) used other locations. ",
      "First bad row: ", min(bad_nyc_only), "."
    ))

  }

  ###################################################
  # output_type_id must be a valid quantile (0,1)   #
  ###################################################

  # Determining what rows correspond to quantiles (all rows for MetroCast)
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

  # Stopping the code if an error occurs
  abort_if_errors()

#------------------------------------------------------------------------------#
# PHASE 5: Group-level completeness --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that grouped predictions form complete sets:    #
# all 9 quantile levels per quantile group, and quantile values are monotone   #
# non-decreasing within group.                                                 #
#------------------------------------------------------------------------------#

  #############################################
  # Confirming that all quantiles are present #
  #############################################

  # Pulling the quantile column (all rows for MetroCast)
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
        "(expected 9 levels: ", paste(required_quantiles, collapse = ", "),
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

      # Checking for monotone non-decreasing
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

#------------------------------------------------------------------------------#
# PHASE 6: Spec-detail rules ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces remaining MetroCast specification rules: the    #
# global uniqueness of the row key.                                            #
#------------------------------------------------------------------------------#

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

    # Tabulating the target type
    target_list <- paste(unique(data$target), collapse = ", ")

    # Number of locations
    n_loc    <- length(unique(data$location))

    # State / aggregate breakdown -- look up state via crosswalk
    locs_in_data <- unique(data$location)
    matched_states <- forecastEvalReport::metrocast_locations$state_abb[
      match(locs_in_data, forecastEvalReport::metrocast_locations$location)
    ]
    n_states <- length(unique(matched_states[!is.na(matched_states)]))

    # Message to print
    message(
      "\u2713 MetroCast model validated successfully.\n",
      "  File:         ", file, "\n",
      "  Rows:         ", format(nrow(data), big.mark = ","), "\n",
      "  Date range:   ", min(data$reference_date), " to ", max(data$reference_date), "\n",
      "  Locations:    ", n_loc, " across ", n_states, " state(s)\n",
      "  Targets:      ", target_list
    )

  }

  ##############################
  # Returning the validated df #
  ##############################
  data

}
