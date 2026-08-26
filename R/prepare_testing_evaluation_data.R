#' Prepare the testing-period evaluation dataset
#'
#' Builds the dataset used for forecast evaluation during the testing period by
#' combining the evaluation model's testing-period point forecasts with the
#' observed (truth) outcome values. Testing forecasts come from
#' `eval_meta$testing_data` (the `training_validation == 0` subset). Observed
#' values come from the assembled `master_data` outcome rows
#' (`variable_type == "outcome_data"`), joined by location and date. The
#' reference date is back-calculated from `target_end_date` and `horizon` using
#' the detected `time_step`.
#'
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Uses
#'   `eval_meta$testing_data` and, when available, `eval_meta$locations`.
#' @param master_data Assembled master data frame from
#'   `assemble_report_data()`. Outcome rows supply the observed values.
#' @param locations Optional named character vector mapping raw location codes
#'   (names) to display names (values). When `NULL`, falls back to
#'   `eval_meta$locations`, then to an identity map.
#' @param time_step Integer. The data's dominant time step in days (e.g. 7 for
#'   weekly), as detected by `extract_evaluation_data()` and stored in
#'   `eval_meta$time_step`. Used to back-calculate `reference_date` from
#'   `target_end_date` and `horizon`. Default 7.
#'
#' @return A data frame with columns `reference_date`, `location` (raw code),
#'   `target_end_date`, `value` (forecast), `horizon`, and `Observed`
#'   (observed outcome value), or an empty data frame when no testing data is
#'   available.
#'
#' @keywords internal
#' @noRd
prepare_testing_evaluation_data <- function(eval_meta,
                                            master_data,
                                            locations = NULL,
                                            time_step = 7) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks for the testing data, and handles the situations  #
# when there IS an evaluation model but there is no testing period included    #
# within the input data set. It first creates the empty result to return to    #
# the user if no testing data is present. It then checks for the testing data, #
# if its not present it returns 'empty_result', otherwise it proceeds as       #
# normal with the rest of the script.                                          #
#------------------------------------------------------------------------------#

  ###############################################
  # Empty data set to return if no testing data #
  ###############################################
  empty_result <- data.frame(
    reference_date  = as.Date(character(0)),
    location        = character(0),
    target_end_date = as.Date(character(0)),
    value           = numeric(0),
    horizon         = numeric(0),
    output_type_id  = numeric(0),
    Observed        = numeric(0),
    stringsAsFactors = FALSE
  )

  #####################################################################
  # Check for missing testing data: Returns empty result if not found #
  #####################################################################
  if(is.null(eval_meta) ||
     is.null(eval_meta$testing_data) ||
     !is.data.frame(eval_meta$testing_data) ||
     nrow(eval_meta$testing_data) == 0){

    # Returning the empty result
    return(empty_result)

  }

  ######################################
  # Testing data is inputted and saved #
  ######################################
  testing <- eval_meta$testing_data

#------------------------------------------------------------------------------#
# Resolving the location display <-> raw code map ------------------------------
#------------------------------------------------------------------------------#
# About: This section handles the display location vs raw location provided in #
# the data file. Testing forecasts key location by raw code; master_data keys  #
# observed values by display name. The locations vector (names = raw code,     #
# values = display name) bridges the two. Falls back to eval_meta$locations,   #
# then to an identity map when neither is supplied.                            #
#------------------------------------------------------------------------------#

  ############################################################################
  # Checking for missing location input from user: Location Input is Missing #
  ############################################################################
  if(is.null(locations) || length(locations) == 0){

    ###########################################################
    # Location is available via the evaluation meta data file #
    ###########################################################
    locations <- if(!is.null(eval_meta$locations) &&
                    length(eval_meta$locations) > 0){

      # Extracting the locations from the evaluation meta data
      eval_meta$locations

    ##########################################
    # Location is available via testing file #
    ##########################################
    }else{

      # Extracting the unique locations
      locs <- unique(testing$location)

      # Creating a name vector where the names and the values are identical (cross walk)
      setNames(locs, locs)

    }

  }

#------------------------------------------------------------------------------#
# Preparing observed (truth) values from master_data ---------------------------
#------------------------------------------------------------------------------#
# About: This section selects and prepares the observed truth values used      #
# to score the testing-period forecasts. Training data, when supplied by       #
# the user and overlapping the testing window, is used in place of the         #
# outcome data; otherwise the outcome data is used. The user is always         #
# told which source was used and, when training is skipped, why. A clear       #
# message is returned when no truth data is available at all.                  #
#------------------------------------------------------------------------------#

  ############################################################
  # The exact target dates the testing forecasts evaluate at #
  ############################################################
  testing_dates <- if("target_end_date" %in% names(testing)){

    # Parsing the testing-period target end dates
    suppressWarnings(anytime::anydate(testing$target_end_date))

  # No target_end_date column yet: treat the testing window as empty
  }else{

    # Empty date vector so no training row can claim an overlap
    as.Date(character(0))

  }

  # Dropping any dates that could not be parsed
  testing_dates <- testing_dates[!is.na(testing_dates)]

  ###############################################################
  # Pre-building the window text used in the no-overlap message #
  ###############################################################
  window_text <- if(length(testing_dates) > 0){

    # Human-readable span of the testing window
    paste0(format(min(testing_dates)), " to ", format(max(testing_dates)))

  # No parseable testing dates to describe
  }else{

    # Fallback text when the window could not be determined
    "unavailable (no testing dates could be parsed)"

  }

  ###############################################################
  # Tracks which truth source the selection logic ends up using #
  ###############################################################
  truth_source <- "none"

  ###############################################
  # Creating the empty vector for observed data #
  ###############################################
  observed <- NULL

  #######################################
  # Trying to prepare the observed data #
  #######################################
  observed <- tryCatch({

    ##############################################################
    # Helper: pull one variable_type's rows into the truth shape #
    ##############################################################
    build_truth_frame <- function(vtype){

      # Selecting only the rows of the requested variable type
      rows <- master_data[
        !is.na(master_data$variable_type) &
          master_data$variable_type == vtype, ]

      # Returning NULL when this variable type contributes no rows
      if(nrow(rows) == 0) return(NULL)

      # Confirm the rows carry the columns we rely on
      needed <- c("location", "date", "value")

      # Flagging any missing columns
      missing_cols <- setdiff(needed, names(rows))

      # Triggering the error if columns are missing
      if(length(missing_cols) > 0){

        # Error to show to users
        stop(vtype, " rows are missing required column(s): ",
             paste(missing_cols, collapse = ", "), ".")

      }

      # Building the per-source observed data frame
      df <- data.frame(
        location_display = as.character(rows$location),
        target_end_date  = anytime::anydate(rows$date),
        Observed         = suppressWarnings(as.numeric(rows$value)),
        stringsAsFactors = FALSE
      )

      # One observed value per location/date, facilitates later merging
      df[!duplicated(
        df[c("location_display", "target_end_date")]), , drop = FALSE]

    }

    #######################################
    # Running if master data is available #
    #######################################
    if(!is.null(master_data) && is.data.frame(master_data) &&
       "variable_type" %in% names(master_data)){

      # Training truth rows (NULL when the user supplied none)
      training_obs <- build_truth_frame("training_data")

      # Outcome truth rows, the default / fallback source
      outcome_obs  <- build_truth_frame("outcome_data")

      # Does any training date land on an exact testing target date?
      training_overlaps <- !is.null(training_obs) &&
        any(training_obs$target_end_date %in% testing_dates)

      ############################################################
      # Option 1: training supplied AND overlaps -> use training #
      ############################################################
      if(!is.null(training_obs) && training_overlaps){

        # Recording the chosen source for the message below
        truth_source <- "training"

        # Using the training data as the evaluation truth
        training_obs

      #############################################################
      # Option 2: training supplied but NO overlap -> use outcome #
      #############################################################
      }else if(!is.null(training_obs) && !training_overlaps){

        # Recording that training was skipped for non-overlap
        truth_source <- "outcome_no_overlap"

        # Using the outcome data because training does not overlap
        outcome_obs

      #################################################
      # Option 3: no training supplied -> use outcome #
      #################################################
      }else{

        # Recording the chosen source for the message below
        truth_source <- "outcome"

        # Using the outcome data as the evaluation truth
        outcome_obs

      }

    ###################################
    # No master data set is available #
    ###################################
    }else{

      # Recording that there was no source to read from
      truth_source <- "none"

      # Returning nothing
      NULL

    }

  ##############################################
  # Triggered if an error message occurs above #
  ##############################################
  }, error = function(e){

    # Checking for master data column names & Type
    md_cols <- if(is.data.frame(master_data)) paste(names(master_data), collapse = ", ")
    else "master_data is not a data frame"

    # Message to show to users
    message(
      "prepare_testing_evaluation_data(): could not build the observed ",
      "(truth) values from master_data, so Observed will be all NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - master_data columns present: ", md_cols, "\n",
      "  - Expected outcome/training rows where variable_type is ",
      "'outcome_data' or 'training_data' with columns location, date, value.\n",
      "  - Check that assemble_report_data() produced these rows for this ",
      "report."
    )

    # Returning nothing
    NULL

  })

#------------------------------------------------------------------------------#
# Reporting which truth source was used ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section reports which truth source the testing-period            #
# metrics are scored against. When training data was supplied but did not      #
# overlap the testing window, the message also explains why the outcome        #
# data was used in its place. When neither source yields truth rows, a         #
# single no-data message is returned and Observed stays all NA.                #
#------------------------------------------------------------------------------#

  #############################################################
  # Whether the chosen source actually yielded any truth rows #
  #############################################################
  have_truth <- !is.null(observed) && nrow(observed) > 0

  ###############################################################
  # No truth data at all: warn the user that Observed is all NA #
  ###############################################################
  if(!have_truth){

    # Message to users that no evaluation truth could be found
    message("prepare_testing_evaluation_data(): no outcome or training ",
            "truth data was found for the testing period, so Observed ",
            "will be all NA and the testing-period metrics cannot be ",
            "scored.")

  ##################################################
  # Training data was used as the evaluation truth #
  ##################################################
  }else if(truth_source == "training"){

    # Message naming the evaluation truth source
    message("prepare_testing_evaluation_data(): testing-period forecasts ",
            "are evaluated against the user-supplied TRAINING data.")

  ###################################################################
  # Training supplied but skipped for non-overlap: say what AND why #
  ###################################################################
  }else if(truth_source == "outcome_no_overlap"){

    # Message naming the source AND why training was skipped
    message("prepare_testing_evaluation_data(): training data was ",
            "supplied but none of its dates fall within the testing ",
            "window (", window_text, "), so testing-period forecasts ",
            "are evaluated against the OUTCOME data instead.")

  #################################################
  # Outcome data was used as the evaluation truth #
  #################################################
  }else{

    # Message naming the evaluation truth source
    message("prepare_testing_evaluation_data(): testing-period forecasts ",
            "are evaluated against the OUTCOME data.")

  }

#------------------------------------------------------------------------------#
# Assembling the testing period evaluation data --------------------------------
#------------------------------------------------------------------------------#
# About: This section assembles the data frame needed for the testing period   #
# evaluation, including back calculating the reference date from the target    #
# end date and horizon. The back-calculation is needed to ensure graphing is   #
# a smooth process in later steps.                                             #
#------------------------------------------------------------------------------#

  #######################################
  # Trying to prepare the forecast file #
  #######################################
  forecasts <- tryCatch({

    # Confirm the testing data has the columns we rely on
    needed <- c("location", "target_end_date", "value", "horizon", "output_type_id")

    # Flagging any missing columns
    missing_cols <- setdiff(needed, names(testing))

    # Triggered if any columns are missing
    if(length(missing_cols) > 0){

      # Messages to show to users
      stop("eval_meta$testing_data is missing required column(s): ",
           paste(missing_cols, collapse = ", "), ".")

    }

    # Triggered if time step is not valid
    if(!is.numeric(time_step) || length(time_step) != 1 ||
       is.na(time_step) || time_step <= 0){

      # Message to show to users
      stop("time_step must be a single positive number (days per step); got: ",
           paste(utils::capture.output(str(time_step)), collapse = " "), ".")

    }

    ############################################################
    # Creating the data frame for the testing period forecasts #
    ############################################################
    fc <- data.frame(
      location        = as.character(testing$location),
      target_end_date = anytime::anydate(testing$target_end_date),
      value           = suppressWarnings(as.numeric(testing$value)),
      horizon         = suppressWarnings(as.numeric(testing$horizon)),
      output_type_id  = suppressWarnings(as.numeric(testing$output_type_id)),
      stringsAsFactors = FALSE
    )

    # Triggered if no dates could be parsed to dates
    if(all(is.na(fc$target_end_date))){

      # Message to show to users
      stop("none of testing$target_end_date could be parsed as dates.")

    }

    # Triggered if no horizons could be parsed to numbers
    if(all(is.na(fc$horizon))){

      # Message to show to users
      stop("none of testing$horizon could be coerced to numeric, so the ",
           "reference date cannot be back-calculated.")

    }

    #######################################
    # Back calculating the reference date #
    #######################################
    fc$reference_date <- fc$target_end_date - (fc$horizon * time_step)

    #####################################################
    # Mapping the 'raw' locations to the 'display' name #
    #####################################################
    fc$location_display <- unname(locations[fc$location])

    # Fall back to the raw code when a mapping is missing
    fc$location_display[is.na(fc$location_display)] <-
      fc$location[is.na(fc$location_display)]

    # Returning the forecast data frame
    fc

  ##############################################
  # Triggered if an error message occurs above #
  ##############################################
  }, error = function(e){

    # Checking if 'testing' is a data frame
    testing_cols <- if(is.data.frame(testing)) paste(names(testing), collapse = ", ")
    else "testing is not a data frame"

    # Message to return to users
    message(
      "prepare_testing_evaluation_data(): could not assemble the testing ",
      "forecast frame, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - testing_data columns present: ", testing_cols, "\n",
      "  - testing_data rows: ",
      if(is.data.frame(testing)) nrow(testing) else NA, "\n",
      "  - time_step supplied: ", time_step, "\n",
      "  - Expected columns: location, target_end_date, value, horizon."
    )

    # Returning nothing if error occurs
    NULL
  })

  ######################################################
  # Returning empty result if forecast assembly failed #
  ######################################################
  if(is.null(forecasts)) return(empty_result)

#------------------------------------------------------------------------------#
# Joining observed values ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section merges the observed data with the forecast data, thereby #
# creating one master data set that we can use for all plotting, tabulations,  #
# and later evaluation metrics.                                                #
#------------------------------------------------------------------------------#
  forecasts <- tryCatch({

    #########################################
    # Running if observed data is available #
    #########################################
    if(!is.null(observed)){

      # Combining the forecast and observed data
      merged <- merge(
        forecasts, observed,
        by    = c("location_display", "target_end_date"),
        all.x = TRUE,
        sort  = FALSE
      )

      # Flags if an issue happens with the above merging
      if(all(is.na(merged$Observed))){

        # Message to show to users
        message(
          "prepare_testing_evaluation_data(): the observed-value join matched ",
          "no rows, so Observed is all NA.\n",
          "  - Forecast location_display values: ",
          paste(utils::head(unique(forecasts$location_display), 5), collapse = ", "),
          if(length(unique(forecasts$location_display)) > 5) ", ..." else "", "\n",
          "  - Observed location_display values: ",
          paste(utils::head(unique(observed$location_display), 5), collapse = ", "),
          if(length(unique(observed$location_display)) > 5) ", ..." else "", "\n",
          "  - Check that the location mapping (raw code -> display name) and ",
          "the target_end_date values align between the two sources."
        )
      }

      # Returning NA or merged data frame
      merged

    ############################################
    # Running if no observed data is available #
    ############################################
    }else{

      # Filling in observed columns with NA
      forecasts$Observed <- NA_real_

      # Returning empty data set
      forecasts

    }

  ######################################
  # Triggered if an error occurs above #
  ######################################
  }, error = function(e){

    # Message to show to users
    message(
      "prepare_testing_evaluation_data(): could not join observed values to ",
      "forecasts; Observed will be all NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - Join keys required on both frames: location_display, target_end_date.\n",
      "  - forecasts rows: ", nrow(forecasts),
      " | observed rows: ", if(is.data.frame(observed)) nrow(observed) else 0, "."
    )

    # Filling in observed columns with NA
    forecasts$Observed <- NA_real_

    # Returning empty data set
    forecasts

  })

#------------------------------------------------------------------------------#
# Final clean frame ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the final data frame to be used for evaluation  #
# across the rest of the reports and for the stand-alone testing period        #
# evaluation function.                                                         #
#------------------------------------------------------------------------------#

  #########################################
  # Trying to create the final data frame #
  #########################################
  result <- tryCatch({

    # Confirm every column the downstream metrics expect is present
    needed <- c("reference_date", "location", "target_end_date",
                "value", "horizon", "output_type_id", "Observed")

    # Flagging any missing columns
    missing_cols <- setdiff(needed, names(forecasts))

    # Triggering an error if columns are missing
    if(length(missing_cols) > 0){

      # Message to show to user if error occurs
      stop("the assembled frame is missing expected column(s): ",
           paste(missing_cols, collapse = ", "), ".")

    }

    ##################################
    # Preparing the final data frame #
    ##################################
    res <- forecasts %>%
      dplyr::select(dplyr::all_of(needed)) %>%
      dplyr::arrange(location, reference_date, horizon, output_type_id)

    # Removing row names
    rownames(res) <- NULL

    # Returning the final data set
    res

   ######################################
  # Triggered if an error occurs above #
  ######################################
  }, error = function(e){

    # Message to show to users
    message(
      "prepare_testing_evaluation_data(): could not build the final result ",
      "frame, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - Columns present at this stage: ",
      paste(names(forecasts), collapse = ", "), "\n",
      "  - Columns required: reference_date, location, target_end_date, ",
      "value, horizon, Observed."
    )

    # Returning an empty result
    empty_result
  })

  ##############################
  # Returning the final result #
  ##############################
  result

}
