#' Prepare auxiliary variable data for plotting from the master data set
#'
#' Filters the master data set (produced by `assemble_report_data()`) to
#' auxiliary variable rows for a specific location and formats them for use
#' in `add_parameter_traces_by_disease()`. Returns NULL when no auxiliary
#' variable data is available for the requested location so downstream
#' plotting functions can skip the parameter trace step gracefully.
#'
#' @param master_data A data frame produced by `assemble_report_data()`.
#'   Must contain columns: `variable_type`, `location`, `disease_name_clean`,
#'   `data_source`, `variable`, `date`, `value`.
#' @param location Character. The normalized location name to filter to.
#'   Should match the normalized location values in `master_data$location`.
#'
#' @return A data frame of auxiliary variable rows for the requested location,
#'   ready for `add_parameter_traces_by_disease()`, or `NULL` if no data is
#'   available for that location.
#'
#' @keywords internal
#' @noRd
prepare_parameter_data <- function(master_data, location) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns NULL immediately when the master data is         #
# empty or missing, or the location is blank, so downstream callers can        #
# skip the parameter trace step without errors.                                #
#------------------------------------------------------------------------------#

  ###########################################
  # Guard: master_data must be a data frame #
  ###########################################

  # Returning NULL when master_data is absent, not a data frame, or empty
  if(is.null(master_data) || !is.data.frame(master_data) ||
     nrow(master_data) == 0){

    # Returning NULL if no data available
    return(NULL)

  }

  ####################################
  # Guard: location must be a string #
  ####################################

  # Returning NULL when no usable location was supplied
  if(is.null(location) || is.na(location) || nchar(trimws(location)) == 0){

    # Returning NULL if no data available
    return(NULL)

  }

#------------------------------------------------------------------------------#
# Filter to aux_data rows for the requested location ---------------------------
#------------------------------------------------------------------------------#
# About: This section keeps only auxiliary variable rows                       #
# (variable_type == "aux_data") for the current geography. The location        #
# column is already normalized by assemble_report_data(), so a direct          #
# string match is safe here.                                                   #
#------------------------------------------------------------------------------#

  ####################################################
  # Filter to aux_data rows for the current location #
  ####################################################

  # Keeping aux_data rows for the requested (already-normalized) location
  result <- master_data[
    !is.na(master_data$variable_type) &
      master_data$variable_type == "aux_data" &
      !is.na(master_data$location) &
      master_data$location == location, ]

  #################################
  # Return NULL if no rows remain #
  #################################

  # Returning NULL when the location has no auxiliary variable rows
  if(nrow(result) == 0) return(NULL)

#------------------------------------------------------------------------------#
# Format for add_parameter_traces_by_disease() ---------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the columns that                                #
# add_parameter_traces_by_disease() consumes are present, then coerces         #
# date to Date and value to numeric, and drops rows where either               #
# coercion failed.                                                             #
#                                                                              #
# Expected columns consumed downstream:                                        #
#                                                                              #
#   data_source        -- the data source label (legend group)                 #
#   disease_name_clean -- the display disease name                             #
#   date               -- the date of observation                              #
#   value              -- the observed value                                   #
#   variable           -- the variable name (parameter label)                  #
#------------------------------------------------------------------------------#

  ####################################
  # Confirm required columns present #
  ####################################

  # Columns add_parameter_traces_by_disease() needs to draw the traces
  required_cols <- c("data_source", "disease_name_clean", "date",
                     "value", "variable")

  # Any required columns absent from the filtered data
  missing_cols <- setdiff(required_cols, names(result))

  # Warning + NULL when the expected standardized columns are not present
  if(length(missing_cols) > 0){

    # Warning to print to users
    warning(
      "prepare_parameter_data: master_data is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Returning NULL.",
      call. = FALSE
    )

    # Returning NULL if there is an issue
    return(NULL)

  }

  ####################################
  # Coerce date column to Date class #
  ####################################
  if(!inherits(result$date, "Date")){

    # Date to date format
    result$date <- suppressWarnings(anytime::anydate(result$date))

  }

  ##################################
  # Coerce value column to numeric #
  ##################################
  if(!is.numeric(result$value)){

    # Converting result to numeric
    result$value <- suppressWarnings(as.numeric(result$value))

  }

  ###################################
  # Drop rows where coercion failed #
  ###################################

  # Dropping rows where date or value failed to coerce
  result <- result[!is.na(result$date) & !is.na(result$value), ]

  # Returning NULL if nothing survived coercion
  if(nrow(result) == 0) return(NULL)

  ############################
  # Return the prepared data #
  ############################

  # Returning the formatted auxiliary variable rows
  result

}
