#' Prepare current projection data from the implementation model
#'
#' Filters the implementation model to point forecasts
#' (output_type_id == 0.5) for a specific location where
#' target_end_date >= reference_date. Returns only the current
#' projection rows for use in the forecast plot.
#'
#' The geography argument accepts either the raw location code (as it
#' appears in the implementation model's location column) or the
#' normalized display name. Both are tried so the function works
#' regardless of which form is passed.
#'
#' @param implementation.model A validated implementation model data
#'   frame.
#' @param geography Character. The location to filter to. Can be the
#'   raw location code or the normalized display name.
#' @param reason Character. The forecasting reason (e.g., "FluSight",
#'   "Software"). Not used for filtering but kept for API compatibility.
#'
#' @return A data frame of current projection rows for the requested
#'   location, or an empty data frame if no rows match.
#'
#' @keywords internal
#' @noRd
prepare_forecast_data <- function(implementation.model, geography, reason) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure they are in the     #
# correct format. If they are not, an empty data frame is returned to ensure   #
# that remainder of the plotting scripts can run like normal.                  #
#------------------------------------------------------------------------------#

  #####################################
  # Guard: model must be a data frame #
  #####################################
  if(is.null(implementation.model) || !is.data.frame(implementation.model) ||
     nrow(implementation.model) == 0){

    # Returning an empty data frame
    return(data.frame())

  }

  ######################################
  # Guard: geography must be a string  #
  ######################################
  if(is.null(geography) || is.na(geography) ||
     nchar(trimws(geography)) == 0){

    # Returning an empty data frame
    return(data.frame())

  }

#------------------------------------------------------------------------------#
# Coercing date columns --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section ensures date columns are proper Date objects before      #
# filtering.                                                                   #
#------------------------------------------------------------------------------#

  ##########################################
  # Forcing target end date to date object #
  ##########################################
  if(!inherits(implementation.model$target_end_date, "Date")){

    # Changing target end date to date
    implementation.model$target_end_date <- anytime::anydate(
      implementation.model$target_end_date
    )

  }

  #########################################
  # Forcing reference date to date object #
  #########################################
  if(!inherits(implementation.model$reference_date, "Date")){

    # Changing reference date to date
    implementation.model$reference_date <- anytime::anydate(
      implementation.model$reference_date
    )

  }

#------------------------------------------------------------------------------#
# Filtering to the requested location ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section tries to match the geography against the raw location    #
# column first. If no rows match, tries a case-insensitive string match as a   #
# fallback. This handles both raw location codes (e.g., "45") and normalized   #
# display names (e.g., "South Carolina").                                      #
#------------------------------------------------------------------------------#

  ######################################
  # Try exact match on location column #
  ######################################
  forecast <- implementation.model[
    !is.na(implementation.model$location) &
      implementation.model$location == geography, ]

  ####################################
  # Fallback: case-insensitive match #
  ####################################
  if(nrow(forecast) == 0){

    # Pulling the projections
    forecast <- implementation.model[
      !is.na(implementation.model$location) &
        tolower(trimws(implementation.model$location)) ==
        tolower(trimws(geography)), ]

  }

  # Return empty if still no match
  if(nrow(forecast) == 0) return(data.frame())

#------------------------------------------------------------------------------#
# Filtering to point forecasts and current projections -------------------------
#------------------------------------------------------------------------------#
# About: This section filters the point forecasts and current projections. It  #
# keeps only rows where output_type_id == 0.5 (point forecast) and             #
# target_end_date >= reference_date (forward-looking rows only). Other         #
# sections create the ribbons around the point forecasts.                      #
#------------------------------------------------------------------------------#

  ###################################
  # Sub-setting the point forecasts #
  ###################################
  forecast <- forecast[
    !is.na(forecast$output_type_id) &
      forecast$output_type_id == 0.5 &
      !is.na(forecast$target_end_date) &
      !is.na(forecast$reference_date) &
      forecast$target_end_date >= forecast$reference_date, ]

  ###############################
  # Returning the filtered data #
  ###############################
  forecast

}
