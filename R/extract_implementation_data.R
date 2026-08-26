#' Extract metadata from a validated implementation model
#'
#' Processes a validated implementation model data frame and extracts all
#' metadata required for report generation, including reference dates,
#' outcome labels, spatial scale, target population, geographic coverage,
#' and projection periods. Location codes are normalized to human-readable
#' names where a crosswalk is available. The filtered forecast (current
#' projections only) is saved to a `Forecasts/` subdirectory of the
#' current working directory, unless `save_data = FALSE`.
#'
#' @param implementation_model A validated implementation model data frame
#'   produced by the appropriate hub validator (e.g.,
#'   `validate_flusight_model()`) or `validate_general_model()`.
#' @param config A validated configuration list produced by
#'   `validate_report_params()`.
#' @param save_data Logical. When `TRUE` (default), the filtered forecast is
#'   written to a `Forecasts/` subdirectory. When `FALSE`, extraction runs
#'   normally but nothing is written to disk -- used when a caller needs only
#'   the returned metadata (e.g. the plot-only export path). Defaults to
#'   `TRUE`.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{reference_date}{The unique reference date in the file.}
#'   \item{outcome}{The outcome label(s) extracted from the file.}
#'   \item{spatial_scale}{The spatial scale of the forecast.}
#'   \item{target_population}{The target population from the file or config.}
#'   \item{population_label}{The user-supplied population display label.}
#'   \item{locations}{Human-readable location names.}
#'   \item{n_models}{Number of models described in the config.}
#'   \item{projection_start}{Start date of the current projection period.}
#'   \item{projection_end}{End date of the current projection period.}
#'   \item{historical_start}{Start date of the historical projection period
#'     (non-hub only; `NULL` for hub submissions).}
#'   \item{historical_end}{End date of the historical projection period
#'     (non-hub only; `NULL` for hub submissions).}
#'   \item{historical_data}{Data frame of historical projection rows
#'     (non-hub only; `NULL` for hub submissions).}
#'   \item{forecast_path}{Full path to the saved forecast CSV (`NA` when
#'     `save_data = FALSE`).}
#' }
#'
#' @export
extract_implementation_data <- function(implementation_model,
                                        config,
                                        save_data = TRUE) {

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

    # Triggering if an error occurs
    if(length(errors) > 0){

      # Stopping the script if an error occurs
      stop(
        "extract_implementation_data failed with ", length(errors), " error(s):\n  - ",
        paste(errors, collapse = "\n  - "),
        call. = FALSE
      )

    }
  }

#------------------------------------------------------------------------------#
# Creating the path sanitization helper ----------------------------------------
#------------------------------------------------------------------------------#
# About: Converts a string into a safe folder/file name component. Lowercases, #
# replaces spaces and special characters with underscores, collapses           #
# consecutive underscores, strips leading/trailing underscores, and truncates  #
# to a maximum character length to protect against PATH_MAX limits.            #
#------------------------------------------------------------------------------#

  #########################################
  # Sanitizing a string for use in a path #
  #########################################
  sanitize_path_component <- function(x, max_chars = 20){

    # Collapse multiple values if a vector was passed
    x <- paste(x, collapse = "_")

    # Lowercase
    x <- tolower(x)

    # Replace anything that isn't alphanumeric or underscore with underscore
    x <- gsub("[^a-z0-9]+", "_", x)

    # Collapse consecutive underscores
    x <- gsub("_+", "_", x)

    # Strip leading and trailing underscores
    x <- gsub("^_|_$", "", x)

    # Truncate to max_chars
    if(nchar(x) > max_chars) x <- substr(x, 1, max_chars)

    # Strip trailing underscore again after truncation
    x <- gsub("_$", "", x)

    # Returning the path
    x

  }

#------------------------------------------------------------------------------#
# Creating the location normalization helper -----------------------------------
#------------------------------------------------------------------------------#
# About: Converts location codes (FIPS, abbreviations, HSA codes) to their     #
# full human-readable equivalents using available crosswalks. Falls back to    #
# the original value when no match is found.                                   #
#------------------------------------------------------------------------------#

  ########################################
  # Normalizing a single location string #
  ########################################
  normalize_location_display <- function(loc, reason){

    # Trim whitespace
    loc <- trimws(as.character(loc))

    #####################################################################
    # User-supplied crosswalk takes precedence over the built-in tables #
    #####################################################################
    # If the user provided a location crosswalk, an exact (trimmed) match on
    # the raw code returns their clean display name before any package lookup.
    loc_xwalk <- config$location_crosswalk
    if(!is.null(loc_xwalk) && !is.na(loc) && loc %in% names(loc_xwalk)){
      return(unname(loc_xwalk[[loc]]))
    }

    ################################################################
    # Triggering if reason is a HUB forecast format: Non-Metrocast #
    ################################################################
    if(reason %in% hub_reasons){

      ##################
      # Try FIPS match #
      ##################

      # Looking for a FIPS match
      idx <- match(loc, forecastEvalReport::hubverse_locations$location)

      # Triggering if a match is found
      if(!is.na(idx)){
        return(forecastEvalReport::hubverse_locations$location_name[idx])
      }

      ##########################
      # Try abbreviation match #
      ##########################

      # Looking for an abbreviation match
      idx <- match(toupper(loc), forecastEvalReport::hubverse_locations$abbreviation)

      # Triggering if a match is found
      if(!is.na(idx)){
        return(forecastEvalReport::hubverse_locations$location_name[idx])
      }

    ############################################################
    # Triggering if reason is a HUB forecast format: Metrocast #
    ############################################################
    }else if(reason == "MetroCast"){

      # MetroCast: look up in metrocast_locations
      idx <- match(loc, forecastEvalReport::metrocast_locations$location)

      # Triggering if a match is found
      if(!is.na(idx)){
        return(forecastEvalReport::metrocast_locations$location_name[idx])
      }

    #######################################################
    # General/Software/Internal: best-effort lookup chain #
    #######################################################
    }else{

      #######################################################################
      # 1. Try hubverse_locations (handles US + state FIPS + abbreviations) #
      #######################################################################

      # Looking for FIPS matches
      idx <- match(loc, forecastEvalReport::hubverse_locations$location)

      # Triggering if a match is found
      if(!is.na(idx)){
        return(forecastEvalReport::hubverse_locations$location_name[idx])
      }

      # Looking for abbreviation matches
      idx <- match(toupper(loc), forecastEvalReport::hubverse_locations$abbreviation)

      # Triggering if a match is found
      if(!is.na(idx)){
        return(forecastEvalReport::hubverse_locations$location_name[idx])
      }

      ###########################
      # 2. Try county crosswalk #
      ###########################

      # Looking for a county match
      county_match <- match(tolower(loc),
                            tolower(forecastEvalReport::us_counties$name))

      # Triggering if a match is found
      if(!is.na(county_match)){
        return(forecastEvalReport::us_counties$name[county_match])
      }

      ##################################
      # 3. Try Dartmouth HSA crosswalk #
      ##################################

      # Looking for an HSA match
      hsa_match <- match(tolower(loc),
                         tolower(forecastEvalReport::dartmouth_hsa_zip$hsacity))

      # Triggering if a match is found
      if(!is.na(hsa_match)){
        return(forecastEvalReport::dartmouth_hsa_zip$hsacity[hsa_match])
      }

    }

    ##################################
    # No match found -- return as-is #
    ##################################
    loc

  }

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms that the implementation model is a non-empty data frame and  #
# that config contains the required fields before any extraction is attempted. #
#------------------------------------------------------------------------------#

  ##############################################
  # Confirm implementation_model is valid type #
  ##############################################
  if(!is.data.frame(implementation_model)){

    # Error to show to users
    add_error(paste0(
      "`implementation_model` must be a data frame but received: ",
      class(implementation_model)[1], "."
    ))

  ###############################################
  # Triggering if implementation model is empty #
  ###############################################
  }else if(nrow(implementation_model) == 0){

    # Error to show to users
    add_error("`implementation_model` is an empty data frame.")

  }

  ########################################
  # Function that accumulates the errors #
  ########################################
  abort_if_errors()

  ##################################
  # Confirm required config fields #
  ##################################

  # Required configuration fields
  required_config <- c("reason", "model_descriptions", "population_label",
                       "outcome_name")

  # Flagging any missing configuration file entries
  missing_config <- required_config[
    vapply(required_config, function(f) is.null(config[[f]]), logical(1))
  ]

  # Triggered if an error occurs
  if(length(missing_config) > 0){

    # Error to show to users
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
# list of hub reasons, and creating the empty output list that will be         #
# returned at the end of this function.                                        #
#------------------------------------------------------------------------------#

  ###############
  # Hub reasons #
  ###############
  hub_reasons <- c("FluSight", "COVIDHub", "RSVHub")

  #####################
  # Convenience alias #
  #####################
  reason <- config$reason

  ###############
  # Output list #
  ###############
  output <- list()

#------------------------------------------------------------------------------#
# Coerce date columns ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Ensures reference_date and target_end_date are proper Date objects    #
# before any date-based filtering or extraction is performed.                  #
#------------------------------------------------------------------------------#

  ###########################
  # Coercing reference date #
  ###########################
  if(!inherits(implementation_model$reference_date, "Date")){

    # Ensuring reference date is a date format
    implementation_model$reference_date <- as.Date(
      implementation_model$reference_date
    )

  }

  ############################
  # Coercing target end date #
  ############################
  if(!inherits(implementation_model$target_end_date, "Date")){

    # Ensuring that target end date is in date format
    implementation_model$target_end_date <- as.Date(
      implementation_model$target_end_date
    )
  }

#------------------------------------------------------------------------------#
# Extracting the reference date ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Pulls the unique reference date from the file. Hub validators enforce #
# a single reference date; general files should also have one.                 #
#------------------------------------------------------------------------------#

  #########################
  # Unique reference date #
  #########################

  # Pulling all unique reference dates
  ref_dates <- unique(implementation_model$reference_date)

  ###############################################
  # Handling reference dates: Only 1 is present #
  ###############################################
  output$reference_date <- if(length(ref_dates) == 1){

    # Returning the reference date
    ref_dates

  #############################################################
  # Handling multiple reference dates: Taking the most recent #
  #############################################################
  }else{

    # Warning to show to users
    warning(
      "Implementation model contains ", length(ref_dates),
      " unique reference dates. Using the most recent.",
      call. = FALSE
    )

    # Pulling the max reference date
    max(ref_dates)

  }

#------------------------------------------------------------------------------#
# Extracting the outcome label -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Hub files use the `target` column; general-format files use the       #
# `outcome_measure` column.                                                    #
#------------------------------------------------------------------------------#

  #############################
  # Outcome label: Hub format #
  #############################
  output$outcome <- if(reason %in% hub_reasons){

    # Extracting the target column values
    unique(implementation_model$target)

  #################################
  # Outcome labels: Other formats #
  #################################
  }else{

    # Pulling the outcome measure column
    if("outcome_measure" %in% names(implementation_model)){

      # Unique outcomes
      unique(implementation_model$outcome_measure)

    # Returning NA if no outcome column is present
    }else{NA_character_}

  }

#------------------------------------------------------------------------------#
# Extracting the spatial scale -------------------------------------------------
#------------------------------------------------------------------------------#
# About: For hub files, spatial scale is derived from whether US and/or state  #
# locations are present. For general files it comes from location_general.     #
#------------------------------------------------------------------------------#

  ####################
  # Unique locations #
  ####################
  unique_locations <- unique(implementation_model$location)

  ##############################################
  # Deriving spatial scale: Non-metrocast Hubs #
  ##############################################
  if(reason %in% hub_reasons){

    # Checking for any national rows
    has_us <- any(c("US", "United States") %in% unique_locations)

    # Checking if it has any state rows
    has_state <- any(
      !unique_locations %in% c("US", "United States") &
        nchar(trimws(unique_locations)) > 0
    )

    # Creating the label: State + National
    output$spatial_scale <- if(has_us && has_state){"State & National"

    # Creating the label: National
    }else if(has_us && !has_state){"National"

    # Creating the label: State
    }else{"State"}

  #########################################
  # Deriving spatial scale: Metrocast Hub #
  #########################################
  }else if(reason == "MetroCast"){

    # Look up each location in 'metrocast_locations' to determine its type
    mc <- forecastEvalReport::metrocast_locations

    # Original_location_code == "All" means state-level
    has_state_mc <- any(vapply(unique_locations, function(loc){

      # Checking for matches between user location and cross walk
      idx <- match(loc, mc$location)

      # Not match found
      if(is.na(idx)) return(FALSE)

      # Pulling any rows with "All"
      mc$original_location_code[idx] == "All"

    }, logical(1)))

    # Anything not "All" is an HSA
    has_hsa_mc <- any(vapply(unique_locations, function(loc){

      # Checking for matches between user location and cross walk
      idx <- match(loc, mc$location)

      # No match found
      if(is.na(idx)) return(FALSE)

      # Pulling any rows that DO NOT have 'All'
      mc$original_location_code[idx] != "All"

    }, logical(1)))

    # Creating the label: HSA + State
    output$spatial_scale <- if(has_state_mc && has_hsa_mc){
      "HSA & State"

    # Creating the label: State
    }else if(has_state_mc && !has_hsa_mc){"State"

    # Creating the label: HSA
    }else if(has_hsa_mc && !has_state_mc){"HSA"

    # Fallback label if no locations matched the crosswalk
    }else{"MetroCast"}

  #########################################
  # Deriving spatial scale: General Model #
  #########################################
  }else{

    # General format: read from location_general column
    if("location_general" %in% names(implementation_model)){

      # Get unique values
      loc_gen_vals <- unique(implementation_model$location_general)

      # Capitalize first letter of each word, or all caps if <= 4 letters
      loc_gen_vals <- vapply(loc_gen_vals, function(x){

        # Split on underscores or spaces
        words <- strsplit(x, "[_ ]")[[1]]

        # Function to Format each word
        words <- vapply(words, function(word){

          # Using all CAPS for short words
          if(nchar(word) <= 4){toupper(word)

          # Cap only first letter for longer words
          }else{paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))}

        }, character(1))

        # Collapsing list
        paste(words, collapse = " ")

      }, character(1))

      # Join with " & "
      output$spatial_scale <- paste(loc_gen_vals, collapse = " & ")

    #########################################
    # Returning NA if nothing can be parsed #
    #########################################
    }else{output$spatial_scale <- NA_character_}

  }

#------------------------------------------------------------------------------#
# Extracting the target population ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Hub files always use general_population. General-format files read    #
# from the population column.                                                  #
#------------------------------------------------------------------------------#

  ####################################
  # Target population: Forecast Hubs #
  ####################################
  output$target_population <- if(reason %in% hub_reasons){

    "general_population"

  ###################################
  # Target population: user Entered #
  ###################################
  }else{

    # Returning user entry if present
    if("population" %in% names(implementation_model)){

      # Pasting the user entry
      paste(unique(implementation_model$population), collapse = ", ")

    # Returning NA if no entry is present
    }else{NA_character_}

  }

  ##################################
  # User-supplied population label #
  ##################################
  output$population_label <- config$population_label

#------------------------------------------------------------------------------#
# Normalizing locations to human-readable names --------------------------------
#------------------------------------------------------------------------------#
# About: Converts location codes to their full human-readable equivalents      #
# using the available crosswalks. Falls back to the original value when no     #
# match is found. The returned vector is named by the original code.           #
#------------------------------------------------------------------------------#

  #############################
  # Normalizing each location #
  #############################
  output$locations <- vapply(
    unique_locations,
    normalize_location_display,
    character(1),
    reason = reason
  )

  # Name by original code for downstream reference
  names(output$locations) <- unique_locations

#------------------------------------------------------------------------------#
# Extracting model count -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section extracts the number of unique models provided by the     #
# user. In some cases, a single model may be made up of many models, each of   #
# which may have their own names in descriptions. This section counts how many #
# unique model entries are provided by the user.                               #
#------------------------------------------------------------------------------#

  ####################
  # Number of models #
  ####################
  output$n_models <- if(is.data.frame(config$model_descriptions)){

    # Number of unique models
    nrow(config$model_descriptions)

  ###################################
  # Returning NA if can not extract #
  ###################################
  }else{NA_integer_}

#------------------------------------------------------------------------------#
# Filtering to current projections ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Current projections are rows where target_end_date >= reference_date. #
# This is the filtered dataset that gets saved to disk.                        #
#------------------------------------------------------------------------------#

  ##################################
  # Filter to current projections  #
  ##################################
  current_projections <- implementation_model[
    !is.na(implementation_model$target_end_date) &
      !is.na(implementation_model$reference_date) &
      implementation_model$target_end_date >= implementation_model$reference_date,
  ]

#------------------------------------------------------------------------------#
# Extracting projection periods ------------------------------------------------
#------------------------------------------------------------------------------#
# About: For hub files, all current projection rows define the period. For     #
# general files, rows where estimate_projected_report == 1 are current         #
# projections; rows where estimate_projected_report != 2 and                   #
# training_validation == 0 are historical projections.                         #
#------------------------------------------------------------------------------#

  ##################################################
  # Current projection period: Forecast Hub Format #
  ##################################################
  if(reason %in% hub_reasons){

    # Keeping all rows
    proj_rows <- current_projections

  ######################################################
  # Current projection period: General Forecast Format #
  ######################################################
  }else{

    # Checking if estimated projected reported is a column name
    if("estimate_projected_report" %in% names(implementation_model)){

      # Extracting the projection rows
      proj_rows <- implementation_model[
        !is.na(implementation_model$estimate_projected_report) &
          implementation_model$estimate_projected_report == 1, ]

    # Returning all rows as default
    }else{proj_rows <- current_projections}

  }

  #########################################################################
  # Extracting the start date of the projections: Projection Rows Present #
  #########################################################################
  output$projection_start <- if(nrow(proj_rows) > 0){

    # Pulling the min date
    min(proj_rows$target_end_date, na.rm = TRUE)

  ########################################################################
  # Extracting the start date of projections: No projection rows present #
  ########################################################################
  }else{NA}

  #######################################################################
  # Extracting the end date of the projections: Projection Rows present #
  #######################################################################
  output$projection_end <- if(nrow(proj_rows) > 0){

    # Pulling the max date
    max(proj_rows$target_end_date, na.rm = TRUE)

  ##########################################################################
  # Extracting the end date of the projections: No projection rows present #
  ##########################################################################
  }else{NA}

  #####################################################
  # Historical projection period: Forecast Hub Format #
  #####################################################
  if(reason %in% hub_reasons){

    # Returning NULL for historical start date
    output$historical_start <- NULL

    # Returning NULL for historical end date
    output$historical_end   <- NULL

    # Returning NULL for historical data
    output$historical_data  <- NULL

  ################################################
  # Historical projection period: General Format #
  ################################################
  }else{

    # Checking for the estimated_projected_report column
    if(all(c("estimate_projected_report") %in%
           names(implementation_model))){

      # Creating the historical data set
      historical <- implementation_model[
        !is.na(implementation_model$estimate_projected_report) &
          implementation_model$estimate_projected_report == 0, ]

      #####################################################
      # Extracting the start date of historical estimates #
      #####################################################
      output$historical_start <- if(nrow(historical) > 0){

        # Pulling the MIN date
        min(historical$target_end_date, na.rm = TRUE)

      ##############################################################################
      # Extracting the start date of historical estimates: No historical estimates #
      ##############################################################################
      }else{NULL}

      ###################################################
      # Extracting the end date of historical estimates #
      ###################################################
      output$historical_end <- if(nrow(historical) > 0){

        # Pulling the MAX date
        max(historical$target_end_date, na.rm = TRUE)

      ############################################################################
      # Extracting the end date of historical estimates: No historical estimates #
      ############################################################################
      }else{NULL}

      #################################
      # Returning the historical data #
      #################################
      output$historical_data <- if(nrow(historical) > 0){

        historical

      #################################
      # No historical data is present #
      #################################
      }else{NULL}

    ####################################################################
    # Returning NULL for all fields if no historical data is available #
    ####################################################################
    }else{

      # Historical start date
      output$historical_start <- NULL

      # Historical end date
      output$historical_end   <- NULL

      # Historical estimates data
      output$historical_data  <- NULL

    }

  }

#------------------------------------------------------------------------------#
# Building the forecast save path ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Constructs a safe, cross-platform folder path from the key metadata   #
# components. Each component is sanitized (lowercased, special chars replaced  #
# with underscores, truncated) to protect against PATH_MAX limits on Windows   #
# (260 chars) and other systems.                                               #
#                                                                              #
# Structure:                                                                   #
#                                                                              #
#           <getwd()>/Forecasts/<reason>-<outcome>-<population>-<scale>-       #
#           <model-name-abb>/ forecast-<YYYY-MM-DD>.csv                        #
#------------------------------------------------------------------------------#

  ##################################
  # Sanitizing each path component #
  ##################################

  # Reason: already short, max 12 chars
  s_reason <- sanitize_path_component(config$reason, max_chars = 12)

  # Outcome: use config$outcome_name (one stable value)
  s_outcome <- sanitize_path_component(config$outcome_name, max_chars = 20)

  # Population: use population_label if present
  pop_raw <- if(!is.null(config$population_label) &&
                !is.na(config$population_label) &&
                nchar(trimws(config$population_label)) > 0){

    config$population_label

  # Population: Using target if population label is not present
  }else{output$target_population}

  # Sanitizing the population
  s_population <- sanitize_path_component(pop_raw, max_chars = 20)

  # Determining number of locations
  loc_codes_fc <- unique(stats::na.omit(current_projections$location))

  # Creating the spatial-scale indicator: 1 Location
  s_scale <- if(length(loc_codes_fc) == 1){sanitize_path_component(output$spatial_scale, max_chars = 15)

  # Creating the spatial-scale indicator: +1 Locations
  }else{paste0(sanitize_path_component(output$spatial_scale, max_chars = 15), "_MULTI")}

  # General model type -- sanitized and abbreviated to keep path short
  s_models <- sanitize_path_component(config$general_model_type, max_chars = 15)

  ##############################
  # Assembling the folder name #
  ##############################
  folder_name <- paste(

    # Piece of folder name
    s_reason, s_outcome, s_population, s_scale, s_models,

    # Separator
    sep = "-"

  )

  ###########################
  # Assembling the filename #
  ###########################

  # Formatting the reference date
  ref_date_str <- format(output$reference_date, "%Y-%m-%d")

  #######################################################
  # Creating the file name for the implementation model #
  #######################################################

  # Pulling unique location
  loc_codes_fc <- unique(stats::na.omit(current_projections$location))

  # Determining the location tag: One Location
  loc_tag <- if(length(loc_codes_fc) == 1){

    # Show location name
    paste0("-", sanitize_path_component(as.character(loc_codes_fc[1]),
                                        max_chars = 20))

  # Determining the location tag: Multiple Locations
  }else{""}

  # Creating the file name
  file_name    <- paste0("Forecast", stringr::str_to_title(loc_tag), "-", ref_date_str, ".csv")

  ######################
  # Full path assembly #
  ######################

  # Use options file directory if available, else fall back to getwd()
  base_dir <- if(!is.null(config$output_dir) &&
                 !is.na(config$output_dir) &&
                 nchar(trimws(config$output_dir)) > 0){

    # Base directory: File directory
    config$output_dir

  # Base directory: Working Directory
  }else{getwd()}

  # Creating the folder
  forecast_dir <- file.path(base_dir, "Forecasts", folder_name)

  # Creating the forecast file path
  forecast_path <- file.path(forecast_dir, file_name)

#------------------------------------------------------------------------------#
# Creating the directory and saving the forecast -------------------------------
#------------------------------------------------------------------------------#
# About: Creates the forecast directory if it does not already exist, then     #
# writes the filtered current-projections data frame as a CSV. If the file     #
# already exists it is overwritten silently (re-running the report should      #
# always produce the latest version).                                          #
#------------------------------------------------------------------------------#

  ###############################################
  # Only writing to disk when save_data is TRUE #
  ###############################################
  if(isTRUE(save_data)){

    ################################
    # Creating directory if needed #
    ################################
    tryCatch(

      # Creating the directory to save the forecast
      dir.create(forecast_dir, recursive = TRUE, showWarnings = FALSE),

      ############################################################
      # Triggered if an error occurs with creating the directory #
      ############################################################
      error = function(e){

        # Warning to show to users
        warning(
          "Could not create forecast directory: ", forecast_dir, "\n",
          "The forecast CSV will not be saved. R error: ", conditionMessage(e),
          call. = FALSE
        )

      }
    )

    ####################################################
    # Saving the filtered forecast: If directory exist #
    ####################################################
    if(dir.exists(forecast_dir)){

      ########################################
      # Trying to save the filtered forecast #
      ########################################
      tryCatch(

        # Saving the forecast
        utils::write.csv(current_projections, forecast_path, row.names = FALSE),

        ##################################################
        # Triggered if error occurs with saving forecast #
        ##################################################
        error = function(e){

          # Warning to show to users
          warning(
            "Could not save forecast CSV to: ", forecast_path, "\n",
            "R error: ", conditionMessage(e),
            call. = FALSE
          )

        }
      )

      ##########################################
      # Message to show when forecast is saved #
      ##########################################
      message("\u2713 Forecast saved to: ", forecast_path)

      # Outputting the path the forecast was saved to
      output$forecast_path <- forecast_path

    #################################################
    # Running if the directory could not be created #
    #################################################
    }else{output$forecast_path <- NA_character_}

  ###################################################
  # Skipping the disk write when save_data is FALSE #
  ###################################################
  }else{

    # No forecast CSV written; reporting the path as NA
    output$forecast_path <- if(dir.exists(forecast_dir)){
      forecast_path
    }else{
      NA_character_
    }

  }

  ######################
  # Returning the list #
  ######################
  output

}
