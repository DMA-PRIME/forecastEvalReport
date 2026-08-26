#' Validate a general-format implementation model submission file
#'
#' Performs a full validation of a forecast submission file against the
#' general-format specification used internally for Software and Internal
#' reasons. Supports multiple `location_general` geographies (state,
#' national, county, HSA, ZCTA, ZIP, region, and custom). Cross-walks are
#' applied per geography type, with optional `state_context` to disambiguate
#' county and HSA matches.
#'
#' For HSA validation, the validator first checks the location against
#' [metrocast_locations], then falls back to [dartmouth_hsa_zip] if no
#' match is found. ZIP and ZCTA are checked for length and digit format
#' only; no crosswalk membership is enforced.
#'
#' @param file Path to a general-format submission CSV file.
#' @param verbose Logical. If `TRUE`, prints a summary of the validated
#'   file on success. Defaults to `FALSE`.
#' @param state_context Optional two-letter U.S. state abbreviation (or
#'   `NA`). When provided, county and HSA validation is restricted to
#'   that state. Typically supplied via `config$state_context` from
#'   `validate_report_params()`.
#'
#' @return A data frame containing the loaded forecast data, with
#'   `location` preserved as character.
#'
#' @export
validate_general_model <- function(file, verbose = FALSE, state_context = NA_character_) {


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
        "General model validation failed with ", length(errors), " error(s):\n  - ",
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

    # Switching location to lower case
    x <- tolower(trimws(x))

    # Stripping excess information
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
# Defining general-format lookup tables ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section centralizes all general-format specification constants.  #
# When the spec changes, update the values here and the rest of the function   #
# adapts automatically.                                                        #
#------------------------------------------------------------------------------#

  ###############################
  # Required submission columns #
  ###############################
  required_columns <- c(
    "reference_date", "target", "target_end_date",
    "location_general", "location", "disease", "population",
    "training_validation", "estimate_projected_report", "imputed",
    "data_source", "outcome_measure", "output_type",
    "output_type_id", "value"
  )

  ###################################
  # Predefined location_general set #
  ###################################

  # Custom strings are also allowed (any non-empty character)
  predefined_location_general <- c(
    "state", "national", "region", "county", "zcta", "zip", "HSA"
  )

  ##############################
  # Allowed output_type values #
  ##############################
  allowed_output_types <- c("quantile", NA)

  #######################################
  # Required quantile levels (23 total) #
  #######################################
  required_quantiles <- c(0.5)

  ##############################################
  # Strict enum: training_validation (0, 1, 2) #
  ##############################################
  allowed_training_validation <- c(0, 1, 2)

  ##################################################
  # Strict enum: estimate_projected_report (0,1,2) #
  ##################################################
  allowed_estimate_projected_report <- c(0, 1, 2)

  ##################################
  # Strict enum: imputed (0 or 1)  #
  ##################################
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
# File-level checks ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that the file exists on disk, can be read as a  #
# CSV, and contains at least one data row. Nothing else is checked until       #
# these basics succeed.                                                        #
#------------------------------------------------------------------------------#

  ##########################
  # Confirming file exists #
  ##########################
  if(!file.exists(file)){

    # Error to return if needed
    add_error(paste0("File does not exist: ", file))

    # Stopping the check if error occurs
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

    # Error if there way an issue reading in
    error = function(e){
      add_error(paste0("File could not be read as CSV: ", conditionMessage(e)))
      NULL
    }
  )

  # Stop if error occurs
  abort_if_errors()

  #############################
  # Confirming non-empty file #
  #############################
  if(nrow(data) == 0){

    # Error to show to users
    add_error("File contains no data rows.")

    # Stop if error occurs
    abort_if_errors()

  }

#------------------------------------------------------------------------------#
# Column structure -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the file has exactly the 16 required       #
# general-format columns. Missing required columns or extra unexpected         #
# columns are treated as errors. Column existence must be confirmed before     #
# per-column value checks can run.                                             #
#------------------------------------------------------------------------------#

  ################################
  # Checking for missing columns #
  ################################
  missing_cols <- setdiff(required_columns, names(data))

  # Runs if there is a missing column
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

  # Runs if there is extra columns
  if(length(extra_cols) > 0){

    # Error to show to users
    add_error(paste0(
      "File contains unexpected column(s): ",
      paste(extra_cols, collapse = ", "),
      ". Only the 16 required columns are allowed."
    ))

  }

  # Stopping if there are errors
  abort_if_errors()

#------------------------------------------------------------------------------#
# Per-column type and value checks ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates each individual column against its expected    #
# type and allowed value set. Per-row location cross-walk checking is in       #
# Phase 4 (after coercion); this section is concerned only with column-level   #
# format and value membership.                                                 #
#------------------------------------------------------------------------------#

  #############################################
  # `reference_date` is strict ISO YYYY-MM-DD #
  #############################################
  ref_date_valid <- vapply(as.character(data$reference_date), is_iso_date, logical(1))

  # Runs if there is an issue with the dates
  if(any(!ref_date_valid, na.rm = TRUE) || any(is.na(data$reference_date))){

    # Pulling the bad dates
    bad_idx <- which(!ref_date_valid | is.na(data$reference_date))

    # Error to show to users
    add_error(paste0(
      "`reference_date` has ", length(bad_idx),
      " invalid value(s) (must be ISO YYYY-MM-DD, no NA allowed). ",
      "First bad row: ", min(bad_idx), "."
    ))

  }

  #############################################################
  # reference_date and target_end_date share one day of week  #
  #############################################################

  # Parsing reference dates to dates
  ref_dates_parsed <- suppressWarnings(anytime::anydate(data$reference_date))

  # Parsing target end date to date
  ted_parsed       <- suppressWarnings(anytime::anydate(data$target_end_date))

  # Day of week for each (0 = Sun ... 6 = Sat); drop NAs
  all_dow <- c(as.POSIXlt(ref_dates_parsed)$wday,
               as.POSIXlt(ted_parsed)$wday)

  # Filtering out all NAs
  all_dow <- all_dow[!is.na(all_dow)]

  # Require a single consistent day of the week across both columns
  if(length(unique(all_dow)) > 1){

    # Map weekday numbers to names for a clearer message (2023-01-01 is a Sunday)
    dow_names <- weekdays(as.Date("2023-01-01") + sort(unique(all_dow)))

    # Error to return to user
    add_error(paste0(
      "`reference_date` and `target_end_date` fall on multiple days of the ",
      "week (", paste(dow_names, collapse = ", "), "). All reference_date and ",
      "target_end_date values must share the same day of the week."
    ))

  }

  ##############################################
  # `target_end_date` is strict ISO YYYY-MM-DD #
  ##############################################

  # Checking that dates can be converted to ISO
  ted_valid <- vapply(as.character(data$target_end_date), is_iso_date, logical(1))

  # Pulling any dates that cause an NA
  ted_is_na <- is.na(data$target_end_date)

  # Determining if there are any 'bad' rows
  bad_ted <- which(ted_is_na | !ted_valid)

  # Running if there are rows WITH issues
  if(length(bad_ted) > 0){

    # Error to show to users
    add_error(paste0(
      "`target_end_date` has ", length(bad_ted),
      " invalid value(s) (must be ISO YYYY-MM-DD, no NA allowed). ",
      "First bad row: ", min(bad_ted), "."
    ))

  }

  #############################################################
  # reference_date and target_end_date share one day of week  #
  #############################################################

  # Parsing reference dates to dates
  ref_dates_parsed <- suppressWarnings(anytime::anydate(data$reference_date))

  # Parsing target end date to date
  ted_parsed       <- suppressWarnings(anytime::anydate(data$target_end_date))

  # Day of week for each (0 = Sun ... 6 = Sat); drop NAs
  all_dow <- c(as.POSIXlt(ref_dates_parsed)$wday,
               as.POSIXlt(ted_parsed)$wday)

  # Filtering out all NAs
  all_dow <- all_dow[!is.na(all_dow)]

  # Require a single consistent day of the week across both columns
  if(length(unique(all_dow)) > 1){

    # Map weekday numbers to names for a clearer message (2023-01-01 is a Sunday)
    dow_names <- weekdays(as.Date("2023-01-01") + sort(unique(all_dow)))

    # Error to return to user
    add_error(paste0(
      "`reference_date` and `target_end_date` fall on multiple days of the ",
      "week (", paste(dow_names, collapse = ", "), "). All reference_date and ",
      "target_end_date values must share the same day of the week."
    ))

  }

  #############################################
  # `location_general` is non-empty character #
  #############################################
  bad_locgen <- which(is.na(data$location_general) |
                      nchar(trimws(data$location_general)) == 0)

  # Running if there is a general location missing
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
  bad_loc_empty <- which(is.na(data$location) |
                           nchar(trimws(data$location)) == 0)

  # Running if location is empty
  if(length(bad_loc_empty) > 0){

    # Error to show to the users
    add_error(paste0(
      "`location` has ", length(bad_loc_empty),
      " missing or empty value(s). First bad row: ", min(bad_loc_empty), "."
    ))

  }

  ####################################
  # `disease` is non-empty character #
  ####################################
  bad_disease <- which(is.na(data$disease) |
                         nchar(trimws(data$disease)) == 0)

  # Checking if the disease has been indicated
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
  bad_pop <- which(is.na(data$population))

  # Running if ANY issues with the population column
  if(length(bad_pop) > 0){

    # Error to show to users
    add_error(paste0(
      "`population has not been indicated",
      ". First bad row: ", min(bad_pop), "."
    ))

  }

  ############################################
  # Strict enum: training_validation (0,1,2) #
  ############################################

  # Converting the training/validation to numeric
  tv_num <- suppressWarnings(as.numeric(data$training_validation))

  # Checking if there are any that can not be converted to numeric
  bad_tv <- which(is.na(tv_num) | !tv_num %in% allowed_training_validation)

  # Running if ANY can not be converted to numeric
  if(length(bad_tv) > 0){

    # Error to show to users
    add_error(paste0(
      "`training_validation` has ", length(bad_tv),
      " invalid value(s) (must be 0, 1, or 2). First bad row: ",
      min(bad_tv), "."
    ))

  }

  ##################################################
  # Strict enum: estimate_projected_report (0,1,2) #
  ##################################################

  # Converting the estimate/projected/report to numeric
  epr_num <- suppressWarnings(as.numeric(data$estimate_projected_report))

  # Checking if there are any that can not be convert to numeric
  bad_epr <- which(is.na(epr_num) | !epr_num %in% allowed_estimate_projected_report)

  # Running if ANY can not be concerted to numeric
  if(length(bad_epr) > 0){

    # Error to show to users
    add_error(paste0(
      "`estimate_projected_report` has ", length(bad_epr),
      " invalid value(s) (must be 0, 1, or 2). First bad row: ",
      min(bad_epr), "."
    ))

  }

  #################################
  # Strict enum: imputed (0 or 1) #
  #################################

  # Converting the imputed column to numeric
  imp_num <- suppressWarnings(as.numeric(data$imputed))

  # Checking if any imputed values flag incorrectly
  bad_imp <- which(is.na(imp_num) | !imp_num %in% allowed_imputed)

  # Running if there is an issue with the imputation column
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
  bad_ds <- which(nchar(trimws(data$data_source)) == 0)

  # Running if there is an issue with the data source column
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
  bad_om <- which(is.na(data$outcome_measure) |
                    nchar(trimws(data$outcome_measure)) == 0)

  # Running if there is an issue
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
  bad_ot <- which(!data$output_type %in% allowed_output_types)

  # Running if there is an issue
  if(length(bad_ot) > 0){

    # Error to show to users
    add_error(paste0(
      "`output_type` has ", length(bad_ot), " invalid value(s). Allowed: ",
      paste(allowed_output_types, collapse = ", "),
      ". First bad row: ", min(bad_ot), "."
    ))

  }

  #######################################
  # `value` is numeric and non-negative #
  #######################################

  # Converting the value to a number
  value_num <- suppressWarnings(as.numeric(data$value))

  # Checking if value is non-numeric or negative
  bad_value <- which(is.na(value_num) | value_num < 0)

  # Running if there is an issue
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

  # Converting reference date to date
  data$reference_date <- anytime::anydate(data$reference_date)

  # Converting the target end date to date
  data$target_end_date <- anytime::anydate(data$target_end_date)

  # Converting the training validation column to numeric
  data$training_validation <- as.integer(tv_num)

  # Converting the estimated, projected, reported column to numeric
  data$estimate_projected_report <- as.integer(epr_num)

  # Converting the imputed column to numeric
  data$imputed <- as.integer(imp_num)

  # Saving the value
  data$value <- value_num

#------------------------------------------------------------------------------#
# Cross-column logical rules ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces consistency across columns: the relationship    #
# between reference_date and target_end_date (no horizon column in this        #
# format, so it cannot be checked here); the format of output_type_id given    #
# output_type; and the location/location_general crosswalk validation that     #
# dispatches to the appropriate reference list per geography type.             #
#------------------------------------------------------------------------------#

  #################################################
  # output_type_id must be a valid quantile (0,1) #
  #################################################

  # Extracting the quantile rows
  q_rows  <- which(data$output_type == "quantile")

  # Converting the quantile to numeric
  q_vals  <- suppressWarnings(as.numeric(data$output_type_id[q_rows]))

  # Flagging bad rows
  bad_qid <- q_rows[!is.na(q_vals) & (q_vals <= 0 | q_vals >= 1)]

  # Running if any rows break the rules
  if(length(bad_qid) > 0){

    # Error to run for users
    add_error(paste0(
      "`output_type_id` has ", length(bad_qid),
      " invalid quantile value(s) (must be numeric strictly between 0 and 1). ",
      "First bad row: ", min(bad_qid), "."
    ))

  }

  ###############################################################
  # Per-row location validation, dispatched by location_general #
  ###############################################################

  # Convenience vectors filtered by state_context (when supplied)
  if(!is.na(state_context)){

    # Counties in the context state
    county_in_state <- county_data$name[county_data$state == state_context]

    # MetroCast HSAs in the context state -- match by state abbreviation
    metrocast_in_state <- metrocast_data$location[
      metrocast_data$state_abb == state_context
    ]

    # Dartmouth HSAs in the context state
    dartmouth_in_state <- dartmouth_data$hsacity[
      dartmouth_data$hsastate == state_context
    ]

  # Running if state_context is not provided
  }else{

    # General counties that match that provided by the user
    county_in_state    <- county_data$name

    # General locations that match the provided by the user
    metrocast_in_state <- metrocast_data$location

    # General locations that match that provided by the user
    dartmouth_in_state <- dartmouth_data$hsacity

  }

  ########################################################################
  # Pre-compute normalized lookups (faster than re-normalizing each row) #
  ########################################################################

  # Normalizing the state abbreviation
  state_abbr_norm <- tolower(state_abbr)

  # Normalizing the state name
  state_name_norm <- tolower(state_name)

  # Normalizing the county name
  county_norm <- normalize_name(strip_county_suffix(county_in_state))

  # Normalizing the HSA's
  metrocast_norm <- normalize_name(metrocast_in_state)

  # Normalizing the HSA's part 2
  dartmouth_norm <- normalize_name(dartmouth_in_state)

  ############################
  # Collect per-row failures #
  ############################
  bad_loc_rows <- integer()

  ############################################
  # Loop through each row and check location #
  ############################################
  for(i in seq_len(nrow(data))){

    # Skip if location was already flagged as empty
    if(i %in% bad_loc_empty) next

    # Indexed general location
    locgen_i <- data$location_general[i]

    # Indexed location name
    loc_i <- data$location[i]

    # Normalizing the indexed location
    loc_norm <- normalize_name(loc_i)

    # Match by location_general type
    is_valid <- switch(locgen_i,

      # --- State ----
      "state" = {
        loc_lower <- tolower(trimws(loc_i))
        loc_lower %in% state_abbr_norm || loc_lower %in% state_name_norm
      },

      # --- National ---
      "national" = {
        tolower(trimws(loc_i)) %in% c("us", "united states")
      },

      # --- Region: non-empty only ---
      "region" = TRUE,

      # --- County: normalize and match (suffix-stripped) ---
      "county" = {
        loc_stripped_norm <- normalize_name(strip_county_suffix(loc_i))
        loc_stripped_norm %in% county_norm
      },

      # --- ZCTA: 5-digit numeric ---
      "zcta" = grepl("^\\d{5}$", trimws(loc_i)),

      # --- ZIP: 5-digit numeric ---
      "zip"  = grepl("^\\d{5}$", trimws(loc_i)),

      # --- HSA: tiered (metrocast first, then dartmouth) ---
      "HSA" = {
        loc_norm %in% metrocast_norm || loc_norm %in% dartmouth_norm
      },

      # --- Custom value: non-empty only ---
      TRUE

    )

    # Flag invalid row
    if(!isTRUE(is_valid)){bad_loc_rows <- c(bad_loc_rows, i)}

  }

  ##########################################################
  # Returning error if any rows failed location validation #
  ##########################################################
  if(length(bad_loc_rows) > 0){

    # Build a sample of bad combinations for the message
    sample_combos <- unique(paste0(
      data$location_general[bad_loc_rows], " + ",
      data$location[bad_loc_rows]
    ))

    # Build the state-context hint
    sc_hint <- if(!is.na(state_context)){paste0(" (state_context = '", state_context, "')")

      }else{""}

    # Error to show to users
    add_error(paste0(
      length(bad_loc_rows),
      " row(s) have a `location` that does not match the expected crosswalk ",
      "for its `location_general`", sc_hint, ". ",
      "Bad combinations: ",
      paste(utils::head(sample_combos, 5), collapse = "; "),
      if(length(sample_combos) > 5) paste0(" (and ", length(sample_combos) - 5, " more)") else "",
      ". First bad row: ", min(bad_loc_rows), "."
    ))

  }

  ################################
  # Stopping if any errors occur #
  ################################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Group-level completeness -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section verifies that grouped predictions form complete sets:    #
# all 23 quantile levels per quantile group, and quantile values monotone      #
# non-decreasing within group. Group key is the unique combination of all      #
# non-quantile/value columns.                                                  #
#------------------------------------------------------------------------------#

  ###############################
  # Pulling rows with quantiles #
  ###############################
  q_data <- data[data$output_type == "quantile", ]

  ######################################################
  # Running various check if ANY quantiles are present #
  ######################################################
  if(nrow(q_data) > 0){

    # Converting the quantiles to numeric
    q_data$.qnum <- as.numeric(q_data$output_type_id)

    # Keep only rows with non-NA output_type_id for the completeness check
    q_data_complete <- q_data[!is.na(q_data$.qnum), ]

    # Group key = all non-quantile/value columns
    group_cols <- c(
      "reference_date", "target", "target_end_date", "location_general",
      "location", "disease", "population", "training_validation",
      "estimate_projected_report", "imputed", "data_source", "outcome_measure",
      "output_type"
    )

    # Creating the unique groups of all above variables
    q_data$.grp <- do.call(paste, c(q_data[group_cols], sep = "|"))

    # Splitting the groups: Done to check for quantile completion
    grp_split  <- split(q_data$.qnum, q_data$.grp)

    # Empty vector to store 'bad' groups
    bad_groups <- character()

    ##############################
    # Looping through each group #
    ##############################
    for(g in names(grp_split)){

      # Sorting the indexed group by quantile
      qs <- sort(unique(grp_split[[g]]))

      # Checking if all quantiles are present per group
      if(!isTRUE(all.equal(qs, required_quantiles))){
        bad_groups <- c(bad_groups, g)
      }

    }

    ########################################
    # Running if any quantiles are missing #
    ########################################
    if(length(bad_groups) > 0){

      # Error to show to users
      add_error(paste0(
        length(bad_groups),
        " quantile group(s) are missing required quantile levels. ",
        "First bad group: ", bad_groups[1], "."
      ))

    }
  }

  ########################################################
  # Quantile values monotone non-decreasing within group #
  ########################################################
  if(nrow(q_data) > 0){

    # Empty vector to store bad quantiles
    bad_mono  <- character()

    # Splitting the above created groups
    grp_split <- split(q_data, q_data$.grp)

    # Looping through groups
    for(g in names(grp_split)){

      # Indexed group
      sub <- grp_split[[g]]

      # Keep only rows with non-NA output_type_id before checking monotonicity
      sub <- sub[!is.na(sub$.qnum), ]

      # Pulling the quantiles
      sub <- sub[order(sub$.qnum), ]

      # Checking if any violate monotonacity
      if(any(diff(sub$value) < 0)){bad_mono <- c(bad_mono, g)}

    }

    ##############################################
    # Running if any quantiles violate the rules #
    ##############################################
    if(length(bad_mono) > 0){

      # Error to show to users
      add_error(paste0(
        length(bad_mono),
        " quantile group(s) have non-monotone values. ",
        "First bad group: ", bad_mono[1], "."
      ))

    }

  }

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
    n_loc      <- length(unique(data$location))

    # Number of general locations
    locgen_tab <- table(data$location_general)

    # Summary table of locations
    locgen_summary <- paste0(
      names(locgen_tab), " (", format(locgen_tab, big.mark = ","), ")",
      collapse = ", "
    )

    # List of diseases
    disease_list <- paste(unique(data$disease), collapse = ", ")

    # List of outcomes
    outcome_list <- paste(unique(data$outcome_measure), collapse = ", ")

    # Flagging if the state context was provided
    sc_text <- if(!is.na(state_context)){
      paste0("\n  State ctx:    ", state_context)
    }else{
      ""
    }

    # Summary message to print to users
    message(
      "\u2713 General model validated successfully.\n",
      "  File:         ", file, "\n",
      "  Rows:         ", format(nrow(data), big.mark = ","), "\n",
      "  Date range:   ", min(data$reference_date), " to ", max(data$reference_date), "\n",
      "  Locations:    ", n_loc, "\n",
      "  By type:      ", locgen_summary, "\n",
      "  Diseases:     ", disease_list, "\n",
      "  Outcomes:     ", outcome_list, sc_text
    )

  }

  ##############################
  # Returning the validated df #
  ##############################
  data

}
