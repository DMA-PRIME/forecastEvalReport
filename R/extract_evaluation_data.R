#' Extract metadata and period subsets from a validated evaluation model
#'
#' Processes a validated evaluation model data frame and partitions it into
#' training, validation, and testing subsets based on the
#' `training_validation` column. Derives date ranges for each subset.
#' When no implementation model is available, also extracts forecast
#' metadata (reference date, outcome, spatial scale, locations, population)
#' from the evaluation file directly. When an implementation model is
#' provided, its metadata is copied into the output so downstream chunks
#' always have one consistent place to look.
#'
#' @param evaluation_model A validated evaluation model data frame produced
#'   by `validate_eval_model()`.
#' @param config A validated configuration list produced by
#'   `validate_report_params()`.
#' @param impl_meta The metadata list returned by
#'   `extract_implementation_data()`, or `NULL` if no implementation model
#'   was provided. When `NULL`, metadata is extracted from the evaluation
#'   model instead.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{reference_date}{Reference date.}
#'   \item{outcome}{Outcome label.}
#'   \item{spatial_scale}{Spatial scale.}
#'   \item{target_population}{Target population.}
#'   \item{population_label}{User-supplied population label from config.}
#'   \item{locations}{Human-readable location names.}
#'   \item{n_models}{Model count from config.}
#'   \item{time_step}{Detected primary time step in days.}
#'   \item{training_data}{Training subset data frame, or NULL if absent.}
#'   \item{training_start}{Training period start date, or NULL.}
#'   \item{training_end}{Training period end date, or NULL.}
#'   \item{validation_data}{Validation subset data frame, or NULL.}
#'   \item{validation_start}{Validation period start date, or NULL.}
#'   \item{validation_end}{Validation period end date, or NULL.}
#'   \item{testing_data}{Testing subset data frame, or NULL.}
#'   \item{testing_start}{Testing period start date, or NULL.}
#'   \item{testing_end}{Testing period end date, or NULL.}
#'   \item{evaluation_model}{The filtered evaluation model data frame.}
#' }
#'
#' @export
extract_evaluation_data <- function(evaluation_model,
                                    config,
                                    impl_meta = NULL) {

#------------------------------------------------------------------------------#
# Creating the `add_error` function --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function allows for the printing of errors to the console. The   #
# goal of this is to make it clear to users when one of their entries is not   #
# appropriately entered.                                                       #
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

    # Triggered if errors occured
    if(length(errors) > 0){

      # Stopping script and returning errors
      stop(
        "extract_evaluation_data failed with ", length(errors), " error(s):\n  - ",
        paste(errors, collapse = "\n  - "),
        call. = FALSE
      )

    }
  }

#------------------------------------------------------------------------------#
# Creating the location normalization helper -----------------------------------
#------------------------------------------------------------------------------#
# About: Converts location codes to human-readable names using the available   #
# crosswalks. Uses reason to prioritize the correct crosswalk since the eval   #
# file is always general format but the hub context determines which location  #
# codes are likely present.                                                    #
#------------------------------------------------------------------------------#

  #########################################
  # Normalizing a single location string  #
  #########################################
  normalize_location_display <- function(loc){

    # Trimming the white space around the location
    loc         <- trimws(as.character(loc))

    #####################################################################
    # User-supplied crosswalk takes precedence over the built-in tables #
    #####################################################################
    # If the user provided a location crosswalk, an exact (trimmed) match on
    # the raw code returns their clean display name before any package lookup.
    loc_xwalk <- config$location_crosswalk
    if(!is.null(loc_xwalk) && !is.na(loc) && loc %in% names(loc_xwalk)){
      return(unname(loc_xwalk[[loc]]))
    }

    # Pulling the reason for the report
    reason      <- config$reason

    # Standard set of hubverse options
    hub_reasons <- c("FluSight", "COVIDHub", "RSVHub")

    #######################################################
    # Hubverse hub reasons: prioritize hubverse_locations #
    #######################################################
    if(reason %in% hub_reasons){

      # Looking for FIPS code matches
      idx <- match(loc, forecastEvalReport::hubverse_locations$location)

      # Triggered if any matches occur
      if(!is.na(idx)){return(forecastEvalReport::hubverse_locations$location_name[idx])}

      # Looking for abbreviation matches
      idx <- match(toupper(loc), forecastEvalReport::hubverse_locations$abbreviation)

      # Triggering if any matches occur
      if(!is.na(idx)){return(forecastEvalReport::hubverse_locations$location_name[idx])}

    #############################################
    # MetroCast: prioritize metrocast_locations #
    #############################################
    }else if(reason == "MetroCast"){

      # Identifying matching metrocast locations
      idx <- match(loc, forecastEvalReport::metrocast_locations$location)

      # Triggering if any matches occur
      if(!is.na(idx)){return(forecastEvalReport::metrocast_locations$location_name[idx])}

    }

    ############################################################
    # General fallback for all reasons: Location Normalization #
    ############################################################

    # Try hubverse_locations (FIPS or US)
    idx <- match(loc, forecastEvalReport::hubverse_locations$location)

    # Triggering if any matches occur
    if(!is.na(idx)){return(forecastEvalReport::hubverse_locations$location_name[idx])}

    # Trying hubverse locations (Abbreviations)
    idx <- match(toupper(loc), forecastEvalReport::hubverse_locations$abbreviation)

    # Triggering if any matches occur
    if(!is.na(idx)){return(forecastEvalReport::hubverse_locations$location_name[idx])}

    # Try county crosswalk
    county_match <- match(
      tolower(loc),
      tolower(forecastEvalReport::us_counties$name)
    )

    # Triggering if any matches occur
    if(!is.na(county_match)){return(forecastEvalReport::us_counties$name[county_match])}

    # Try Dartmouth HSA crosswalk
    hsa_match <- match(
      tolower(loc),
      tolower(forecastEvalReport::dartmouth_hsa_zip$hsacity)
    )

    # Triggering if any matches occur
    if(!is.na(hsa_match)){return(forecastEvalReport::dartmouth_hsa_zip$hsacity[hsa_match])}

    # No match -- return as-is
    loc

  }

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms that the evaluation model is a non-empty data frame and that #
# the required config fields are present before any extraction is attempted.   #
#------------------------------------------------------------------------------#

  ################################################
  # Confirm evaluation_model is valid data frame #
  ################################################
  if(!is.data.frame(evaluation_model)){

    # Error to show to user if issue
    add_error(paste0(
      "`evaluation_model` must be a data frame but received: ",
      class(evaluation_model)[1], "."
    ))

  #################################################
  # Triggering error if evaluation model is empty #
  #################################################
  }else if(nrow(evaluation_model) == 0){

    # Error to show to users
    add_error("`evaluation_model` is an empty data frame.")

  }

  ########################################
  # Function that accumulates the errors #
  ########################################
  abort_if_errors()

  ##################################
  # Confirm required config fields #
  ##################################

  # List of required values from configuration file
  required_config <- c("model_descriptions", "population_label")

  # Flagging any missing config file entries
  missing_config <- required_config[
    vapply(required_config, function(f) is.null(config[[f]]), logical(1))
  ]

  # Triggering if any config fields are missing
  if(length(missing_config) > 0){

    # Error to show to user
    add_error(paste0(
      "The following required config fields are missing: ",
      paste(missing_config, collapse = ", "), "."
    ))

  }

  ########################################
  # Function that accumulates the errors #
  ########################################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Setup ------------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up for the below sections, including creating the   #
# creating the empty output list that will be returned at the end of this      #
# function.                                                                    #
#------------------------------------------------------------------------------#

  ###############
  # Output list #
  ###############
  output <- list()

#------------------------------------------------------------------------------#
# Coerce date columns ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Ensures date columns are proper Date objects before any filtering or  #
# date arithmetic is performed.                                                #
#------------------------------------------------------------------------------#

  ###########################
  # Coercing reference date #
  ###########################

  # Checking if reference date is date
  if(!inherits(evaluation_model$reference_date, "Date")){

    # Converting reference date to date format
    evaluation_model$reference_date <- as.Date(evaluation_model$reference_date)

  }

  ############################
  # Coercing target end date #
  ############################

  # Checking if target date is a date
  if(!inherits(evaluation_model$target_end_date, "Date")){

    # Converting target end date to date format
    evaluation_model$target_end_date <- as.Date(evaluation_model$target_end_date)

  }

#------------------------------------------------------------------------------#
# Filter to historical rows when implementation model is present ---------------
#------------------------------------------------------------------------------#
# About: When an implementation model is provided, the evaluation file is      #
# filtered to only rows where target_end_date < the implementation model's     #
# reference date. This ensures only historical evaluation rows are used        #
# downstream, regardless of what the user pre-filtered before providing the    #
# file. When no implementation model is present, the file is used as-is.       #
#------------------------------------------------------------------------------#

  ###################################################
  # Running only if implementation file is provided #
  ###################################################
  if(!is.null(impl_meta)){

    ######################################################################
    # Use the single reference date -- if multiple, take the most recent #
    ######################################################################

    # Pulling the reference date from the implementation meta data
    impl_ref_date <- impl_meta$reference_date

    # Triggering if more than one reference date
    if(length(impl_ref_date) > 1){

      # Keeping the most recent reference date
      impl_ref_date <- max(impl_ref_date)

    }

    ##################################
    # Filter to historical rows only #
    ##################################

    # Original length of evaluation model file
    n_before <- nrow(evaluation_model)

    # Filtering the evaluation model file to keep only the historical rows
    evaluation_model <- evaluation_model[
      !is.na(evaluation_model$target_end_date) &
        evaluation_model$target_end_date < impl_ref_date, ]

    # New length of the evaluation model file
    n_after <- nrow(evaluation_model)

    # Message to show to users
    message(
      "Info: Evaluation model filtered to target_end_date < ",
      format(impl_ref_date, "%Y-%m-%d"),
      " (", n_before - n_after, " row(s) removed, ",
      n_after, " remaining)."
    )

    # Warn if filtering removed everything
    if(nrow(evaluation_model) == 0){

      # Warning that shows to users
      warning(
        "After filtering to target_end_date < ",
        format(impl_ref_date, "%Y-%m-%d"),
        ", the evaluation model has no rows remaining. ",
        "Check that your evaluation file contains historical data relative ",
        "to the implementation model's reference date.",
        call. = FALSE
      )

    }

  }

#------------------------------------------------------------------------------#
# Detecting the primary time step ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Infers whether the evaluation data is weekly, daily, or monthly by    #
# computing the differences between consecutive unique target_end_dates and    #
# taking the most common gap. Used in date range calculations to replace the   #
# hardcoded assumption of 7-day (weekly) intervals.                            #
#------------------------------------------------------------------------------#

  ###########################
  # Detecting the time step #
  ###########################
  detect_time_step <- function(dates){

    # Get sorted unique dates
    unique_dates <- sort(unique(dates[!is.na(dates)]))

    # Need at least 2 dates to compute a gap
    if(length(unique_dates) < 2) return(7L)

    # Compute gaps in days between consecutive dates
    gaps <- as.integer(diff(unique_dates))

    # Creating the table of date gaps
    gap_table    <- table(gaps)

    # Determine the most frequent data step
    dominant_gap <- as.integer(names(gap_table)[which.max(gap_table)])

    # Returning the time step
    dominant_gap

  }

  ###################################
  # Computing the time step in days #
  ###################################

  # Applying the time step function
  time_step <- detect_time_step(evaluation_model$target_end_date)

  # Friendly label for the message
  step_label <- switch(

    as.character(time_step),
    "1"  = "daily",
    "7"  = "weekly",
    "28" = ,
    "29" = ,
    "30" = ,
    "31" = "monthly",
    "other"
  )

  # Message to show to users
  message(
    "Info: Detected primary time step = ", time_step,
    " day(s) (", step_label, ")."
  )

  # Store for downstream use
  output$time_step <- time_step

#------------------------------------------------------------------------------#
# Extracting or copying metadata -----------------------------------------------
#------------------------------------------------------------------------------#
# About: When impl_meta is NULL, metadata is extracted directly from the       #
# evaluation model. When impl_meta is provided, its metadata is copied into    #
# the output so downstream chunks always have one consistent place to look     #
# regardless of which model combination was provided.                          #
#------------------------------------------------------------------------------#

  ####################################################
  # Triggered if implementation file is not provided #
  ####################################################
  if(is.null(impl_meta)){

    ##################
    # Reference date #
    ##################

    # Pulling unique reference dates
    ref_dates <- unique(evaluation_model$reference_date)

    # Handling only one reference date
    output$reference_date <- if(length(ref_dates) == 1){

      # Returning the reference date
      ref_dates

    # Handling multiple reference dates
    }else{

      # Warning to show to users
      warning(
        "Evaluation model contains ", length(ref_dates),
        " unique reference dates. Using the most recent.",
        call. = FALSE
      )

      # Keeping the max reference date
      max(ref_dates)

    }

    #################
    # Outcome label #
    #################

    # Checking if outcome_measure column is present
    output$outcome <- if("outcome_measure" %in% names(evaluation_model)){

      # Extracting unique outcomes
      unique(evaluation_model$outcome_measure)

    # Checking if target column is present
    }else if("target" %in% names(evaluation_model)){

      # Extracting unique targets
      unique(evaluation_model$target)

    # No target or outcome columns found
    }else{NA_character_}

    #################
    # Spatial scale #
    #################

    # Extracting unique locations
    unique_locations <- unique(evaluation_model$location)

    # Checking for spatial scale column in data
    if("location_general" %in% names(evaluation_model)){

      # Extracting unique spatial scales
      loc_gen_vals <- unique(evaluation_model$location_general)

      # Format: all caps if <= 4 letters, else title case
      loc_gen_vals <- vapply(loc_gen_vals, function(x){

        # Checking number of words in spatial scale
        words <- strsplit(x, "[_ ]")[[1]]

        # Function to check length of each word in the spatial scale
        words <- vapply(words, function(word){

          # Making all letters uppercase if less than or equal to 4 letters
          if(nchar(word) <= 4) toupper(word)

          # Making only first letter uppercase if more than 4 letters
          else paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))

        }, character(1))

        # Collapsing label
        paste(words, collapse = " ")

      }, character(1))

      ################################################################
      # Adding & between spatial scales if more than one is included #
      ################################################################
      output$spatial_scale <- paste(loc_gen_vals, collapse = " & ")

    #############################################
    # Running if no spatial scale is identified #
    #############################################
    }else{output$spatial_scale <- NA_character_}

    #####################
    # Target population #
    #####################

    # Checking if population name is provided in file
    output$target_population <- if("population" %in% names(evaluation_model)){

      # Pulling the population name
      paste(unique(evaluation_model$population), collapse = ", ")

    }else{NA_character_}

    ##########################
    # Locations (normalized) #
    ##########################

    # Applying function to normalize locations
    normalized_locs <- vapply(
      unique_locations,
      normalize_location_display,
      character(1)
    )

    # Assigning the cleaned names
    names(normalized_locs) <- unique_locations

    # Exporting the cleaned names
    output$locations <- normalized_locs

    ###############
    # Model count #
    ###############

    # Checking if model descriptions are provided in configuration file
    output$n_models <- if(is.data.frame(config$model_descriptions)){

      # Number of unique models
      nrow(config$model_descriptions)

    # No model descriptions provided in configuration file
    }else{NA_integer_}

  ##################################################
  # Triggering if implementation model is provided #
  ##################################################
  }else{

    ###############################################
    # Copying over implementation model meta data #
    ###############################################

    # Reference date
    output$reference_date    <- impl_meta$reference_date

    # Outcome
    output$outcome           <- impl_meta$outcome

    # Spatial scale
    output$spatial_scale     <- impl_meta$spatial_scale

    # Crude target population
    output$target_population <- impl_meta$target_population

    # Locations
    output$locations         <- impl_meta$locations

    # Number of models
    output$n_models          <- impl_meta$n_models

  }

  #############################
  # Population label (always) #
  #############################
  output$population_label <- config$population_label

#------------------------------------------------------------------------------#
# Partitioning into training / validation / testing subsets -------------------#
#------------------------------------------------------------------------------#
# About: Filters the evaluation model by training_validation value. For each   #
# subset, derives the start and end dates of the period using the detected     #
# time step. Returns NULL for any subset that is absent from the file.         #
#------------------------------------------------------------------------------#

  #########################################
  # Helper Function: Creating the subsets #
  #########################################
  extract_subset <- function(data, tv_value){

    # Filter to the requested training_validation value
    subset_data <- data[
      !is.na(data$training_validation) &
        data$training_validation == tv_value, ]

    # Returning NULL if no rows are present post filtering
    if(nrow(subset_data) == 0) return(NULL)

    # Returning the subset
    subset_data

  }

  ##############################################
  # Training subset (training_validation == 1) #
  ##############################################

  # Creating the training data subset
  training_data <- extract_subset(evaluation_model, 1)

  # Triggering if there is training data present
  if(!is.null(training_data)){

    # End date adjusted for horizon using the detected time step
    max_horizon <- suppressWarnings(
      max(as.numeric(training_data$horizon), na.rm = TRUE)
    )

    # Training rows may carry no usable horizon (e.g. observed fitting data
    # with a blank / NA / non-numeric horizon column). In that case max()
    # returns -Inf, which would propagate through the arithmetic below to an
    # infinite end date. Fall back to no horizon adjustment (max_horizon = 0)
    # so the end date is simply the last training target date.
    if(!is.finite(max_horizon)) max_horizon <- 0

    # Training data
    output$training_data  <- training_data

    # Training data start date
    output$training_start <- min(evaluation_model$target_end_date,
                                 na.rm = TRUE) - (time_step - 1L)

    # Latest training target date (NA-safe: all-NA dates -> non-finite)
    max_training_ted <- suppressWarnings(
      max(training_data$target_end_date, na.rm = TRUE)
    )

    # Training data end date -- NULL when no usable target dates exist, so the
    # table shows a blank rather than an infinite or nonsensical value.
    output$training_end   <- if(is.finite(as.numeric(max_training_ted))){
      max_training_ted - (time_step * max_horizon) + time_step - 1
    }else{
      NULL
    }

    # Message to show to users
    message("\u2713 Training data: ", nrow(training_data), " rows.")

  # Triggering if there is NO training data present
  }else{

    # NULL training data
    output$training_data  <- NULL

    # Null training data start date
    output$training_start <- NULL

    # NULL training data end date
    output$training_end   <- NULL

    # Message to show to users
    message("Info: No training data found (training_validation == 1).")

  }

  ################################################
  # Validation subset (training_validation == 2) #
  ################################################

  # Extracting the validation data
  validation_data <- extract_subset(evaluation_model, 2)

  # Triggering if there is validation data present
  if(!is.null(validation_data)){

    # End date adjusted for horizon using the detected time step
    max_horizon <- suppressWarnings(
      max(as.numeric(validation_data$horizon), na.rm = TRUE)
    )

    # Extracting the validation subset
    output$validation_data  <- validation_data

    # Extracting the validation start date
    output$validation_start <- min(validation_data$target_end_date,
                                   na.rm = TRUE) - (time_step - 1L)

    # Extracting the validation end date
    output$validation_end   <- max(validation_data$target_end_date,
                                   na.rm = TRUE) -
      (time_step * max_horizon) + time_step

    # Message to show to users
    message("\u2713 Validation data: ", nrow(validation_data), " rows.")

  # Triggering if there is no validation data present
  }else{

    # NULL validation data
    output$validation_data  <- NULL

    # NULL validation start date
    output$validation_start <- NULL

    # NULL validation end date
    output$validation_end   <- NULL

    # Message to show to users
    message("Info: No validation data found (training_validation == 2).")

  }

  #############################################
  # Testing subset (training_validation == 0) #
  #############################################

  # Creating the testing data subset
  testing_data <- extract_subset(evaluation_model, 0)

  # Triggering if there is testing data present
  if(!is.null(testing_data)){

    # Extracting the testing start date
    output$testing_start <- min(testing_data$target_end_date,
                                na.rm = TRUE) - (time_step - 1L)

    # Extracting the testing end date
    output$testing_end   <- max(testing_data$target_end_date,
                                na.rm = TRUE)

    # Extracting the testing subset
    output$testing_data  <- evaluation_model %>%
      dplyr::filter(target_end_date >= (min(testing_data$target_end_date,
                                           na.rm = TRUE) - (time_step - 1L)) &
                      target_end_date <= (max(testing_data$target_end_date,
                                             na.rm = TRUE)))

    # Message to show to users
    message("\u2713 Testing data: ", nrow(testing_data), " rows.")

  # Triggering if there is no testing data present
  }else{

    # NULL testing data
    output$testing_data  <- NULL

    # NULL testing start date
    output$testing_start <- NULL

    # NULL testing end date
    output$testing_end   <- NULL

    # Message to show to users
    message("Info: No testing data found (training_validation == 0).")

  }

#------------------------------------------------------------------------------#
# Storing the filtered evaluation model ----------------------------------------
#------------------------------------------------------------------------------#
# About: Returns the filtered evaluation model data frame as part of the       #
# output list so downstream chunks can reference it directly rather than       #
# re-filtering.                                                                #
#------------------------------------------------------------------------------#

  ##############################
  # Storing the filtered model #
  ##############################
  output$evaluation_model <- evaluation_model

  ##############################################
  # Returning the evaluation model output list #
  ##############################################
  output

}
