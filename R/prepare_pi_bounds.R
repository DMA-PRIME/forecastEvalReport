#' Extract prediction-interval bounds for a geography across quantile pairs
#'
#' Pulls the lower and upper quantile forecast rows that define each
#' prediction interval (e.g. the 50% and 95% PIs) for a single geography from
#' an implementation model's forecast output. The model is filtered to the
#' requested location, its `target_end_date` is coerced to a Date, and then
#' for every requested quantile pair the matching lower and upper quantile
#' rows are returned. A pair contributes `NULL` when either side has no rows,
#' so the caller can skip any interval that cannot be drawn.
#'
#' @param implementation.model A data frame of forecast output in long form,
#'   carrying at least `location`, `target_end_date`, and `output_type_id`
#'   (the quantile level) columns.
#' @param geography Character. The (already normalized) location to extract;
#'   matched directly against `implementation.model$location`.
#' @param quantile_pairs A list of length-2 numeric vectors, each giving the
#'   lower and upper quantile levels of one prediction interval, e.g.
#'   `list(c(0.25, 0.75), c(0.025, 0.975))`.
#' @param outcome.data.label Character. Outcome-data label accepted for
#'   interface consistency with the other prepare_* helpers; not referenced
#'   in the body at present.
#'
#' @return A list the same length as `quantile_pairs`. Each element is either
#'   a named list with `lower` and `upper` data frames (the matching quantile
#'   rows for that interval) or `NULL` when either bound has no rows.
#'
#' @keywords internal
#' @noRd
prepare_pi_bounds <- function(implementation.model, geography,
                              quantile_pairs, outcome.data.label){

#------------------------------------------------------------------------------#
# Normalize the geography ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section normalizes the requested geography to a trimmed          #
# character string so it can be matched directly against the model's           #
# location column.                                                             #
#------------------------------------------------------------------------------#

  #####################################################################
  # Coercing the geography to a trimmed character string for matching #
  #####################################################################
  norm_geography <- trimws(as.character(geography))

#------------------------------------------------------------------------------#
# Filter to the current location -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section keeps only the forecast rows for the requested           #
# geography (dropping rows whose location is NA), then coerces the             #
# target end date to a Date so the returned bounds are ready to plot.          #
#------------------------------------------------------------------------------#

  ##############################################
  # Subset the model to the requested location #
  ##############################################
  model_filtered <- implementation.model[

    # Drop rows with a missing location
    !is.na(implementation.model$location) &

      # Keep only rows matching the requested geography
      implementation.model$location == norm_geography, ]

  ########################################
  # Coerce the target end date to a Date #
  ########################################

  # Standardizing target_end_date so downstream plotting gets real Dates
  model_filtered$target_end_date <- anytime::anydate(model_filtered$target_end_date)

#------------------------------------------------------------------------------#
# Extract the lower/upper rows for each quantile pair --------------------------
#------------------------------------------------------------------------------#
# About: This section pulls, for every requested prediction interval,          #
# the forecast rows at the lower and upper quantile levels. When either        #
# side has no matching rows the interval cannot be drawn, so that pair         #
# contributes NULL and is skipped by the caller.                               #
#------------------------------------------------------------------------------#

  #######################################################
  # Looping over each quantile pair to build its bounds #
  #######################################################
  bounds <- lapply(quantile_pairs, function(pair){

    # Lower quantile level of this prediction interval
    lower_q <- pair[1]

    # Upper quantile level of this prediction interval
    upper_q <- pair[2]

    ####################################
    # Rows at the lower quantile level #
    ####################################
    lower <- model_filtered[

      # Drop rows with a missing quantile level
      !is.na(model_filtered$output_type_id) &

        # Keep only rows at the lower quantile
        model_filtered$output_type_id == lower_q, ]

    ####################################
    # Rows at the upper quantile level #
    ####################################
    upper <- model_filtered[

      # Drop rows with a missing quantile level
      !is.na(model_filtered$output_type_id) &

        # Keep only rows at the upper quantile
        model_filtered$output_type_id == upper_q, ]

    # Skip this interval if either bound is empty (nothing to draw)
    if(nrow(lower) == 0 | nrow(upper) == 0) return(NULL)

    # Returning the matched lower/upper rows for this interval
    list(lower = lower, upper = upper)

  })

  #############################################
  # Returning the list of per-interval bounds #
  #############################################
  bounds

}
