#' Assemble the master data set for report generation
#'
#' Reads the outcome data file and any auxiliary variable files listed in
#' the variables crosswalk, normalizes location values, filters to the
#' locations present in the forecast files, and stacks everything into a
#' single long-format master data frame. The master data is saved to a
#' `Created-Data/` subdirectory of the output directory and the file path is
#' returned invisibly.
#'
#' This function is called internally by the report template and is not
#' intended for direct user interaction.
#'
#' @param config A validated configuration list produced by
#'   `validate_report_params()`.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`.
#' @param impl_meta The metadata list returned by
#'   `extract_implementation_data()`, or `NULL`.
#' @param eval_meta The metadata list returned by
#'   `extract_evaluation_data()`, or `NULL`.
#' @param implementation_model The validated implementation model data
#'   frame, or `NULL`.
#' @param evaluation_model The validated evaluation model data frame,
#'   or `NULL`.
#' @param save_data Logical. When `TRUE` (default), the assembled master data
#'   is written to a `Created-Data/` subdirectory. When `FALSE`, assembly runs
#'   normally but nothing is written to disk -- used when a caller needs only
#'   the in-memory data (e.g. the plot-only export path). Defaults to `TRUE`.
#'
#' @return Invisibly returns a list with the assembled `data` frame and the
#'   `path` to the saved master data CSV (`NA` when `save_data = FALSE`).
#'
#' @keywords internal
#' @noRd
assemble_report_data <- function(config,
                                 variables_crosswalk,
                                 impl_meta            = NULL,
                                 eval_meta            = NULL,
                                 implementation_model = NULL,
                                 evaluation_model     = NULL,
                                 save_data            = TRUE) {

#------------------------------------------------------------------------------#
# Creating the `add_warning` function ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section collects non-fatal warnings to display at the end of     #
# assembly rather than stopping the function when a single source file or      #
# variable is missing.                                                         #
#------------------------------------------------------------------------------#

  ################################
  # Empty vector to store issues #
  ################################
  assembly_warnings <- character()

  ########################################
  # Function to collect warning messages #
  ########################################
  add_warning <- function(msg) assembly_warnings <<- c(assembly_warnings, msg)

#------------------------------------------------------------------------------#
# Creating the path sanitization helper ----------------------------------------
#------------------------------------------------------------------------------#
# About: Converts a string into a safe folder/file name component. Matches the #
# pattern used in extract_implementation_data() so folder names are consistent.#
#------------------------------------------------------------------------------#

  #########################################
  # Sanitizing a string for use in a path #
  #########################################
  sanitize_path_component <- function(x, max_chars = 20){

    # Collapsing spaces to _
    x <- paste(x, collapse = "_")

    # Converting everything to lower case
    x <- tolower(x)

    # Converting non-alphanumeric symbols to underscores
    x <- gsub("[^a-z0-9]+", "_", x)

    # Collapse double _ into 1
    x <- gsub("_+", "_", x)

    # Stripping leading and trailing underscores
    x <- gsub("^_|_$", "", x)

    # Keeping length at 20 characters
    if(nchar(x) > max_chars) x <- substr(x, 1, max_chars)

    # Stripping leading and trailing underscores
    x <- gsub("_$", "", x)

    # Returning the path
    x

  }

#------------------------------------------------------------------------------#
# Creating the location normalization helper -----------------------------------
#------------------------------------------------------------------------------#
# About: Normalizes a single location string to its human-readable form.       #
# Uses reason-aware prioritization (same crosswalk priority as forecast files) #
# with a general best-effort chain as a final fallback.                        #
#------------------------------------------------------------------------------#

  #########################################
  # Normalizing a single location string  #
  #########################################
  normalize_location <- function(loc){

    # Trimming white space
    loc         <- trimws(as.character(loc))

    #####################################################################
    # User-supplied crosswalk takes precedence over the built-in tables #
    #####################################################################
    # Keeps the master data set's normalized `location` column in sync with the
    # report's display names, so the forecast-to-truth join stays intact.
    loc_xwalk <- config$location_crosswalk
    if(!is.null(loc_xwalk) && !is.na(loc) && loc %in% names(loc_xwalk)){
      return(unname(loc_xwalk[[loc]]))
    }

    # Extracting the reason for the report
    reason      <- config$reason

    # List of hubverse models
    hub_reasons <- c("FluSight", "COVIDHub", "RSVHub")

    ####################################
    # Location matching for hub models #
    ####################################
    if(reason %in% hub_reasons){

      # Looking for the FIPS match
      idx <- match(loc, forecastEvalReport::hubverse_locations$location)

      # Triggering if a FIPS match is found
      if(!is.na(idx)){

        # Returning normalized name
        return(forecastEvalReport::hubverse_locations$location_name[idx])

      }

      # Looking for an abbreviation match
      idx <- match(toupper(loc),
                   forecastEvalReport::hubverse_locations$abbreviation)

      # Triggering if there is an abbreviation match
      if(!is.na(idx)){

        # Returning normalized name
        return(forecastEvalReport::hubverse_locations$location_name[idx])

      }

    ###################################
    # Location matching for Metrocast #
    ###################################
    }else if(reason == "MetroCast"){

      # Looking for metrocast location matches
      idx <- match(loc, forecastEvalReport::metrocast_locations$location)

      # Triggering if there is a location match
      if(!is.na(idx)){

        # Returning normalized name
        return(forecastEvalReport::metrocast_locations$location_name[idx])

      }

    }

    ##############################################
    # General best-effort fallback (all reasons) #
    ##############################################

    # Looking for FIPS code match
    idx <- match(loc, forecastEvalReport::hubverse_locations$location)

    # Trigger if a FIPS code match is found
    if(!is.na(idx)){

      # Returning normalized name
      return(forecastEvalReport::hubverse_locations$location_name[idx])

    }

    # Triggering for abbreviation match
    idx <- match(toupper(loc),
                 forecastEvalReport::hubverse_locations$abbreviation)

    # Trigger if a abbreviation match is found
    if(!is.na(idx)){

      # Returning normalized name
      return(forecastEvalReport::hubverse_locations$location_name[idx])

    }

    # Try full location name match (already normalized)
    idx <- match(tolower(loc),
                 tolower(forecastEvalReport::hubverse_locations$location_name))

    # Trigger if a match is found
    if(!is.na(idx)){

      # Returning normalized location if found
      return(forecastEvalReport::hubverse_locations$location_name[idx])

    }

    # Try county crosswalk
    county_match <- match(tolower(loc),
                          tolower(forecastEvalReport::us_counties$name))

    # Trigger if a county match is found
    if(!is.na(county_match)){

      # Returning normalized location
      return(forecastEvalReport::us_counties$name[county_match])
    }

    # Try Dartmouth HSA crosswalk
    hsa_match <- match(tolower(loc),
                       tolower(forecastEvalReport::dartmouth_hsa_zip$hsacity))

    # Triggering if HSA match is found
    if(!is.na(hsa_match)){

      # Returning normalized location
      return(forecastEvalReport::dartmouth_hsa_zip$hsacity[hsa_match])

    }

    ############################
    # No match -- return as-is #
    ############################
    loc

  }

#------------------------------------------------------------------------------#
# Detecting the location column ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Looks for a column in a data frame whose name matches one of the      #
# expected location column names (case-insensitive). Returns the column name   #
# if found, NULL otherwise.                                                    #
#------------------------------------------------------------------------------#

  ######################################
  # Detecting the location column name #
  ######################################
  detect_location_col <- function(df){

    # Candidate names in priority order
    candidates <- c("location", "state", "region", "county", "zip", "zcta")

    # Case-insensitive match against data frame column names
    col_names_lower <- tolower(names(df))

    # Looping through candidate name list and actual locations
    for(candidate in candidates){

      # Looking for matches among column names
      idx <- match(candidate, col_names_lower)

      # Returning name of location column if found
      if(!is.na(idx)) return(names(df)[idx])

    }

    ###################################
    # Returning NULL if nothing found #
    ###################################
    NULL

  }

#------------------------------------------------------------------------------#
# Detecting the date column ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Loops through all columns and attempts to parse each as a Date.       #
# Prefers columns whose name contains "date" or "week". Returns the column     #
# name of the best date column found, or NULL if none is detected.             #
#------------------------------------------------------------------------------#

  ##################################
  # Detecting the date column name #
  ##################################
  detect_date_col <- function(df){

    # Extracting all column names
    col_names <- names(df)

    # Prefer columns whose name contains "date" or "week" or "day
    preferred <- col_names[grepl("date|week|day", col_names, ignore.case = TRUE)]

    # Extracting column names that do not include date, week, or day
    remaining <- col_names[!col_names %in% preferred]

    # Creating the vector with all possible column names
    ordered_cols <- c(preferred, remaining)

    # Looping through all column names
    for(col in ordered_cols){

      # Indexed column
      vals <- df[[col]]

      # Skip if already a Date
      if(inherits(vals, "Date")) return(col)

      # Try parsing as character date
      parsed <- tryCatch(
        anytime::anydate(as.character(vals)),
        error = function(e) rep(NA, length(vals))
      )

      # Determining average number of rows that can be parsed
      pct_valid <- mean(!is.na(parsed))

      # Returning column if average parsed to date > 50%
      if(pct_valid >= 0.5) return(col)

    }

    #################################################
    # Returning NULL if no column can be identified #
    #################################################
    NULL

  }

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms that the crosswalk is available and that at least one model  #
# file is present before attempting assembly.                                  #
#------------------------------------------------------------------------------#

  #############################
  # Crosswalk must be present #
  #############################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0){

    # Stopping if cross walk can not be found
    stop(
      "assemble_report_data requires a valid variables crosswalk. ",
      "variables_crosswalk is NULL or empty.",
      call. = FALSE
    )

  }

  #################################
  # At least one model must exist #
  #################################
  if(is.null(implementation_model) && is.null(evaluation_model)){

    # Stopping if both implementation and evaluation model are missing
    stop(
      "assemble_report_data requires at least one of implementation_model ",
      "or evaluation_model to determine target locations.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Determining target locations -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Gets the raw (un-normalized) locations from whichever forecast file   #
# is available, then normalizes them. Source files are filtered to rows whose  #
# normalized location matches one of these normalized forecast locations.      #
#------------------------------------------------------------------------------#

  #################################################################
  # Raw locations from forecast: Implementation model is provided #
  #################################################################
  raw_forecast_locs <- if(!is.null(implementation_model)){

    # Pulling unique locations
    unique(implementation_model$location)

  #############################################################
  # Raw locations from forecast: Evaluation model is provided #
  #############################################################
  }else{

    # Pulling unique locations
    unique(evaluation_model$location)

  }

  #################################
  # Normalized forecast locations #
  #################################
  normalized_forecast_locs <- vapply(
    raw_forecast_locs,
    normalize_location,
    character(1)
  )

  # Named vector: raw code -> normalized name
  names(normalized_forecast_locs) <- raw_forecast_locs

#------------------------------------------------------------------------------#
# Building the save path -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Constructs the Created-Data/ folder path using the same sanitization  #
# pattern as the Forecasts/ folder so runs stay paired.                        #
#------------------------------------------------------------------------------#

  ##############################
  # Sanitizing path components #
  ##############################

  # Cleaned reason
  s_reason  <- sanitize_path_component(config$reason, max_chars = 12)

  # Cleaned outcome
  s_outcome <- sanitize_path_component(config$outcome_name,  max_chars = 20)

  # Crude population label: From configuration file
  pop_raw <- if(!is.null(config$population_label) &&
                !is.na(config$population_label) &&
                nchar(trimws(config$population_label)) > 0){config$population_label

  # Crude population label: From Implementation File
  }else if(!is.null(impl_meta)){impl_meta$target_population

  # Crude population label: From evaluation file
  }else if(!is.null(eval_meta)){eval_meta$target_population

  # Crude population label: Could not be determined
  }else{"unknown"}

  # Cleaned population label
  s_population <- sanitize_path_component(pop_raw, max_chars = 20)

  # Pulling the crude spatial scale: Implementation model
  spatial_raw <- if(!is.null(impl_meta)){impl_meta$spatial_scale

  # Pulling the crude spatial scale: Evaluation model
  }else if(!is.null(eval_meta)){eval_meta$spatial_scale

  # Pulling the crude spatial scale: Could not be determined
  }else{"unknown"}

  # Cleaned spatial scale
  s_scale  <- sanitize_path_component(spatial_raw, max_chars = 15)

  # Cleaned model name
  s_models <- sanitize_path_component(config$general_model_type, max_chars = 15)

  ############################
  # Creating the folder name #
  ############################
  folder_name <- paste(s_reason, s_outcome, s_population, s_scale, s_models,
                       sep = "-")

  ###############################
  # Reference date for filename #
  ###############################

  # Reference date for implementation file
  ref_date <- if(!is.null(impl_meta)){impl_meta$reference_date

  # Reference date for evaluation file
  }else if(!is.null(eval_meta)){eval_meta$reference_date

  # Using current date if no reference date could be determined
  }else{Sys.Date()}

  # Handling if more than one reference date: uses max
  if(length(ref_date) > 1) ref_date <- max(ref_date)

  # Formatting the reference date for the file name
  ref_date_str <- format(ref_date, "%Y-%m-%d")

  ######################
  # Full path assembly #
  ######################
  base_dir <- if(!is.null(config$output_dir) &&
                 !is.na(config$output_dir) &&
                 nchar(trimws(config$output_dir)) > 0){

    # Using the already existing output directory for base
    config$output_dir

  # Using working directory for base if no folder created yet
  }else{getwd()}

  # Creating the folder path
  data_dir  <- file.path(base_dir, "Created-Data", folder_name)

  # Creating the file path
  data_path <- file.path(data_dir, paste0("master_data-", ref_date_str, ".csv"))

#------------------------------------------------------------------------------#
# Filtering crosswalk to outcome and aux_variable rows ------------------------#
#------------------------------------------------------------------------------#
# About: Only outcome and aux_variable rows contribute data to the master      #
# data set. data_source and general_term rows are excluded.                    #
#------------------------------------------------------------------------------#

  #####################################
  # Filter crosswalk to relevant rows #
  #####################################
  xwalk_rows <- variables_crosswalk[
    variables_crosswalk$variable_type %in% c("outcome", "aux_variable", "training"), ]

  # Triggering if resulting data frame is empty
  if(nrow(xwalk_rows) == 0){

    # Error to show to user
    stop(
      "No outcome or aux_variable rows found in the variables crosswalk. ",
      "The master data set cannot be assembled.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Grouping crosswalk rows by file path ----------------------------------------#
#------------------------------------------------------------------------------#
# About: Multiple crosswalk rows may reference the same file (e.g., two        #
# aux_variable rows from the same source file). Grouping by file means each    #
# file is read only once, then all required variables are extracted from it.   #
#------------------------------------------------------------------------------#

  #####################################
  # Split crosswalk rows by file path #
  #####################################
  xwalk_by_file <- split(xwalk_rows, xwalk_rows$file)

#------------------------------------------------------------------------------#
# Assembling the master data set -----------------------------------------------
#------------------------------------------------------------------------------#
# About: For each unique file, reads the file, detects the location and date   #
# columns, normalizes locations, filters to forecast locations, then extracts  #
# each variable column referenced by a crosswalk row into a long-format chunk. #
# All chunks are stacked into one master data frame.                           #
#------------------------------------------------------------------------------#

  ################################
  # Empty list to collect chunks #
  ################################
  chunks <- list()

  ####################################
  # Looping through each unique file #
  ####################################
  for(file_path in names(xwalk_by_file)){

    # Rows from the crosswalk that reference this file
    file_rows <- xwalk_by_file[[file_path]]

    ###############################
    # Trying to read indexed file #
    ###############################
    source_data <- tryCatch(

      # Reading in the file
      utils::read.csv(
        file_path,
        stringsAsFactors = FALSE,
        na.strings       = c("NA", "")
      ),

      ###########################################
      # Error occurred with reading in the file #
      ###########################################
      error = function(e){

        # Warning to show to users
        add_warning(paste0(
          "Could not read file: ", file_path, ". ",
          "All variables from this file will be skipped. ",
          "R error: ", conditionMessage(e)
        ))

        # Returning NULL
        NULL

      }

    )

    ##################################
    # Skip if file could not be read #
    ##################################
    if(is.null(source_data)) next

    #################################
    # Detecting the location column #
    #################################
    loc_col <- detect_location_col(source_data)

    # Triggering if there is an issue with selecting a location column
    if(is.null(loc_col)){

      # Warning to show to users
      add_warning(paste0(
        "No location column detected in: ", file_path, ". ",
        "Expected a column named one of: location, state, region, ",
        "county, zip, zcta (case-insensitive). ",
        "All variables from this file will be skipped."
      ))

      # Skipping to next file in the loop
      next

    }

    #############################
    # Detecting the date column #
    #############################
    date_col <- detect_date_col(source_data)

    # Triggering if a date column could not be detected
    if(is.null(date_col)){

      # Warning to show to users
      add_warning(paste0(
        "No date column detected in: ", file_path, ". ",
        "All variables from this file will be skipped."
      ))

      # Skipping to next file in the loop
      next

    }

    ###################################
    # Normalizing the location column #
    ###################################
    source_data$location_normalized <- vapply(
      source_data[[loc_col]],
      normalize_location,
      character(1)
    )

    ##############################################################
    # Filtering the data to keep locations only in forecast file #
    ##############################################################
    source_data <- source_data[
      source_data$location_normalized %in% normalized_forecast_locs, ]

    # Triggering if no rows remain after filtering
    if(nrow(source_data) == 0){

      # Warning to show to users
      add_warning(paste0(
        "No matching locations found in: ", file_path, ". ",
        "All variables from this file will be skipped."
      ))

      # Skipping to next file in the loop
      next

    }

    ###########################################
    # Coercing the date column to date format #
    ###########################################
    source_data[[date_col]] <- tryCatch(

      # Changing to date format
      anytime::anydate(as.character(source_data[[date_col]])),

      # Triggered if error occurs
      error = function(e){

        # Warning to show to users
        add_warning(paste0(
          "Date column '", date_col, "' in file: ", file_path,
          " could not be parsed. This file will be skipped."
        ))

        # Repeat for each row
        rep(NA, nrow(source_data))
      }
    )

    #####################################################################
    # Extracting each variable referenced by this file's crosswalk rows #
    #####################################################################
    for(i in seq_len(nrow(file_rows))){

      # Indexed crosswalk row
      xwalk_row <- file_rows[i, ]

      # Variable column to extract
      var_col <- xwalk_row$variable

      # Check the variable column exists in the source file
      if(!var_col %in% names(source_data)){

        # Warning to show to users
        add_warning(paste0(
          "Variable '", var_col, "' not found in: ", file_path, ". ",
          "This variable will be skipped."
        ))

        # Skipping to next file in the loop
        next

      }

      #####################################
      # Map variable_type to output label #
      #####################################
      vtype_label <- switch(
        xwalk_row$variable_type,
        "outcome"      = "outcome_data",
        "aux_variable" = "aux_data",
        "training" = "training_data",
        xwalk_row$variable_type
      )

      #################################################
      # Build the long-format chunk for this variable #
      #################################################
      chunk <- data.frame(
        spatial_scale      = xwalk_row$spatial_scale,
        location           = source_data$location_normalized,
        disease_name_clean = xwalk_row$disease_name_clean,
        variable_type      = vtype_label,
        variable           = var_col,
        data_source        = xwalk_row$data_source,
        date               = source_data[[date_col]],
        value              = suppressWarnings(
          as.numeric(source_data[[var_col]])
        ),

        # Not included strings as factors
        stringsAsFactors   = FALSE
      )

      # Drop rows where value is NA
      chunk <- chunk[!is.na(chunk$value), ]

      ################################
      # Adding another row if needed #
      ################################
      if(nrow(chunk) > 0){
        chunks[[length(chunks) + 1]] <- chunk
      }

    }

  }

#------------------------------------------------------------------------------#
# Stacking all chunks into the master data frame -------------------------------
#------------------------------------------------------------------------------#
# About: This section combines all of the above created chunks into a single   #
# data set that will be used throughout the rest of the report. It contains    #
# all of the non-forecast data for plotting and metrics calculations.          #
#------------------------------------------------------------------------------#

  ##############################################
  # Triggering if there are no chunks to stack #
  ##############################################
  if(length(chunks) == 0){

    # Stopping script if an error occured
    stop(
      "No data could be assembled. Check that your outcome and auxiliary ",
      "variable files exist, contain a location column, a date column, and ",
      "the variables referenced in the crosswalk.",
      call. = FALSE
    )

  }

  #######################
  # Stacking the chunks #
  #######################
  master_data <- do.call(rbind, chunks)

  # Removing all row names
  rownames(master_data) <- NULL

  # Sort by variable_type, location, data_source, variable, date
  master_data <- master_data[
    order(master_data$variable_type,
          master_data$location,
          master_data$data_source,
          master_data$variable,
          master_data$date), ]

  # Message to share to user that the data has been assembled
  message(
    "\u2713 Master data assembled: ",
    format(nrow(master_data), big.mark = ","), " rows, ",
    length(unique(master_data$variable)), " variable(s), ",
    length(unique(master_data$location)), " location(s)."
  )

#------------------------------------------------------------------------------#
# Saving the master data to disk -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Creates the Data/ folder if needed and writes the master data CSV.    #
# Overwrites silently on re-run since this is derived output.                  #
#------------------------------------------------------------------------------#

  ###############################################
  # Only writing to disk when save_data is TRUE #
  ###############################################
  if(isTRUE(save_data)){

    ################################
    # Creating directory if needed #
    ################################
    tryCatch(

      # Creating the directory to store the data
      dir.create(data_dir, recursive = TRUE, showWarnings = FALSE),

      ##########################################################
      # Triggered if an error occurs in creating the directory #
      ##########################################################
      error = function(e){

        # Warning to show to users
        add_warning(paste0(
          "Could not create Data directory: ", data_dir,
          ". Master data will not be saved. R error: ", conditionMessage(e)
        ))

      }

    )

    ########################################
    # Checking if the directory is present #
    ########################################
    if(dir.exists(data_dir)){

      ###########################
      # Trying to save the data #
      ###########################
      tryCatch(

        # Saving the data
        utils::write.csv(master_data, data_path, row.names = FALSE),

        ####################################################
        # Triggered if error occurs in trying to save data #
        ####################################################
        error = function(e){

          # Warning to show to users
          add_warning(paste0(
            "Could not save master data to: ", data_path,
            ". R error: ", conditionMessage(e)
          ))
        }
      )

      ################################
      # Message of successful saving #
      ################################
      message("\u2713 Master data saved to: ", data_path)

    }

  ###################################################
  # Skipping the disk write when save_data is FALSE #
  ###################################################
  }else{

    # Letting the user know assembly stayed in memory only
    message("\u2139 Master data assembled in memory; not written to disk.")

  }

#------------------------------------------------------------------------------#
# Reporting any assembly warnings ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Non-fatal warnings collected during assembly are printed together     #
# at the end so the user sees the full picture rather than one at a time.      #
#------------------------------------------------------------------------------#

  ####################################
  # Runs if any errors occured above #
  ####################################
  if(length(assembly_warnings) > 0){

    # Warning to show to users
    warning(
      "assemble_report_data completed with ", length(assembly_warnings),
      " warning(s):\n  - ",
      paste(assembly_warnings, collapse = "\n  - "),
      call. = FALSE
    )

  }

  ###########################
  # Returning the file path #
  ###########################
  invisible(list(
    data = master_data,  # the assembled data frame
    path = if(isTRUE(save_data)) data_path else NA_character_
  ))

}
