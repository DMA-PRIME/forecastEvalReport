#' Calculate percent agreement across forecasts, horizons, and overall
#'
#' Computes percent agreement between forecasted and observed values at the row
#' level (ratio of the smaller to the larger value, as a percentage), then
#' summarizes by forecast horizon and overall within each location using the
#' mean, range (min/max), median, and a set of quantiles. Operates on the
#' testing evaluation dataset from `prepare_testing_evaluation_data()`.
#' Off-season rows (those in `non_transmission_months`) are retained for
#' plotting but flagged via `is_transmission` and excluded from all aggregate
#' summaries.
#'
#' @param data.for.evaluation Testing evaluation data frame from
#'   `prepare_testing_evaluation_data()`. Must contain `target_end_date`,
#'   `value`, `horizon`, `location`, and `Observed`.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the non-transmission season; excluded from aggregates only.
#'   Default c(5, 6, 7) (May-July).
#'
#' @return The input data frame with row-level `per_agreement` added, plus
#'   horizon-level and overall summary columns (mean, min, max, median, and the
#'   configured quantiles) broadcast across each group.
#'
#' @keywords internal
#' @noRd
percentAgreementCalculation <- function(data.for.evaluation,
                                        non_transmission_months = c(5, 6, 7)){

#------------------------------------------------------------------------------#
# Confirming function should be run --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks that the function which prepares the evaluation   #
# data ran properly, and that both observed and forecast data is available for #
# percent agreement calculations. If there is no evaluation data available,    #
# this function returns the NULL file.                                         #
#------------------------------------------------------------------------------#

  ###########################################################
  # Checking that the evaluation/observed data is available #
  ###########################################################
  if(is.null(data.for.evaluation) ||
     !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0){

    # Returning the empty evaluation/observed data
    return(data.for.evaluation)

  }

  # Point metrics use the median forecast only (the 0.5 quantile)
  data.for.evaluation <- data.for.evaluation %>%
    dplyr::filter(output_type_id == 0.5)

#------------------------------------------------------------------------------#
# Quantile grid + summary column names -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides the set up for the results of this function.    #
# This includes the specification of the quantiles needed for bounds           #
# bounds calculations, and the column headers for the summary tables.          #
#------------------------------------------------------------------------------#

  ###########################################
  # List of quantiles to produce for bounds #
  ###########################################
  pa_quantile_probs <- c(0.10, 0.20, 0.30, 0.40, 0.50,
                         0.60, 0.70, 0.80, 0.90, 0.95)

  ####################################################################
  # Order here defines the column order produced by pa_summary_tbl() #
  ####################################################################
  stat_names <- c("mean", "min", "max", "median",
                  paste0("q", pa_quantile_probs * 100))

#------------------------------------------------------------------------------#
# Graceful fallback helper -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: If a stage fails, we still return the frame with the expected metric  #
# columns present (as NA) so downstream plotting/tabulation doesn't choke on   #
# a missing column. The message tells you WHY they're NA. This function will   #
# only have an impact on the returned data set when an error occurs.           #
#------------------------------------------------------------------------------#

  ##############################################################
  # Function to return NAs for the data frame (error behavior) #
  ##############################################################
  add_missing_as_na <- function(df){

    # Expected metric columns, derived from stat_names so they stay in sync
    metric_cols <- c("forecastValue", "targetValue", "per_agreement",
                     paste0(stat_names, "Horizon"),
                     paste0(stat_names, "Overall"))

    # Looping through column headers
    for(col in metric_cols){

      # Checking if the column is present in data frame and putting NA
      if(!col %in% names(df)) df[[col]] <- NA_real_

    }

    # Handling the month exclusion column
    if(!"is_transmission" %in% names(df)) df[["is_transmission"]] <- NA

    # Returning the generated data frame
    df

  }

#------------------------------------------------------------------------------#
# Per-Group Summary Helper -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This function takes in the row-by-row metrics and suffix indicator    #
# indicating what the grouping metric is for the value show (horizon or        #
# overall). It returns a single row data frame with the mean, range, median,   #
# and quantiles for the given group. All NA or empty inputs will yield a one   #
# row frame of NA so the code will still continue to run.                      #
#------------------------------------------------------------------------------#

  #####################################################
  # Creating the function to create the summary table #
  #####################################################
  pa_summary_tbl <- function(x, suffix){

    # Dropping missing values up front
    x <- x[!is.na(x)]

    ###################################################
    # All-NA / empty group: return NA for every stat  #
    ###################################################
    if(length(x) == 0){

      # Handling all NA rows
      vals <- rep(NA_real_, length(stat_names))

    ###################################################
    # Otherwise compute the full set of summary stats #
    ###################################################
    }else{

      # Calculating the per group statistics
      vals <- c(
        mean(x),
        min(x),
        max(x),
        stats::median(x),
        stats::quantile(x, probs = pa_quantile_probs, na.rm = TRUE, names = FALSE)
      )

    }

    # Creating the column names for the one-row data frame
    names(vals) <- paste0(stat_names, suffix)

    # Returning the data frame
    as.data.frame(as.list(vals), check.names = FALSE, stringsAsFactors = FALSE)

  }

#------------------------------------------------------------------------------#
# Precondition: required columns present ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks for the required columns in the evaluation data   #
# set, and if an error occurs, returns a message and triggers the NA data      #
# frame function. Therefore, an error that occurs here will not break other    #
# pieces of the script.                                                        #
#------------------------------------------------------------------------------#

  # Columns needed from evaluation file
  needed <- c("value", "Observed", "target_end_date", "horizon", "location")

  # Checking for missing columns
  missing_cols <- setdiff(needed, names(data.for.evaluation))

  # Triggered if any column names are missing
  if(length(missing_cols) > 0){

    # Message to show to user
    message(
      "percentAgreementCalculation(): input is missing required column(s), ",
      "so percent-agreement metrics are returned as NA.\n",
      "  - Missing: ", paste(missing_cols, collapse = ", "), "\n",
      "  - Columns present: ", paste(names(data.for.evaluation), collapse = ", "), "\n",
      "  - Expected: ", paste(needed, collapse = ", ")
    )

    # Returning the NA data frame
    return(add_missing_as_na(data.for.evaluation))

  }

#------------------------------------------------------------------------------#
# Calculating the row-level percent agreement ----------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the row-level percent agreement. Percent      #
# agreement is calculated as the ratio of the smaller value to the larger      #
# value, and then shown as a percentage. If both the forecast and observed     #
# data produce zero values, this section returns 100%. NA is used when any     #
# value is missing, and off-season rows are flagged and kept so they can still #
# be plotted.                                                                  #
#------------------------------------------------------------------------------#

  #############################################
  # Trying to calculate the row level metrics #
  #############################################
  percent.agreement <- tryCatch({

    # Data frame with row-level metrics
    data.for.evaluation %>%

      # Renaming variables & tagging "non-transmission" months
      dplyr::mutate(
        forecastValue   = value,
        targetValue     = Observed,
        is_transmission = !lubridate::month(target_end_date) %in% non_transmission_months
      ) %>%

      # Calculating the row-by-row percent agreement
      dplyr::mutate(

        per_agreement = dplyr::case_when(

          # Any data is missing
          is.na(targetValue) | is.na(forecastValue)  ~ NA_real_,

          # Both values correctly predict zero
          targetValue == 0 & forecastValue == 0 ~ 100,

          # Calculating the percent agreement
          TRUE ~ (pmin(targetValue, forecastValue) / pmax(targetValue, forecastValue)) * 100

        )

      )

  #########################################################################
  # Triggered if an error occurs with calculating percent agreement above #
  #########################################################################
  }, error = function(e){

    # Message to show to users
    message(
      "percentAgreementCalculation(): failed while computing row-level percent ",
      "agreement, so metrics are returned as NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - target_end_date class: ", paste(class(data.for.evaluation$target_end_date), collapse = "/"),
      " (must be Date for lubridate::month)\n",
      "  - value class: ", paste(class(data.for.evaluation$value), collapse = "/"),
      "; Observed class: ", paste(class(data.for.evaluation$Observed), collapse = "/"),
      " (both must be numeric for pmin/pmax)\n",
      "  - Rows in: ", nrow(data.for.evaluation)
    )

    # Returning nothing if error occurs
    NULL

  })

  ############################################
  # Triggered if the row-level metric failed #
  ############################################
  if(is.null(percent.agreement)){

    # Returning the NA input
    return(add_missing_as_na(data.for.evaluation))

  }

#------------------------------------------------------------------------------#
# Grouped percent agreement (transmission rows only) ---------------------------
#------------------------------------------------------------------------------#
# About: This section computes the mean, range (min/max), median, and          #
# quantiles per horizon x location and per location, across transmission rows  #
# only, then broadcasts those summaries back onto every row via a join. A      #
# common silent situation is a group where every transmission-row              #
# per_agreement is NA (all observed missing) -> the summaries return NA. That  #
# is expected, not an error; a true error here usually means a grouping column #
# problem.                                                                     #
#------------------------------------------------------------------------------#

  #########################################
  # Trying to build the grouped summaries #
  #########################################
  percent.agreement <- tryCatch({

    # One row per horizon x location
    horizon_summary <- percent.agreement %>%
      dplyr::group_by(horizon, location) %>%
      dplyr::reframe(pa_summary_tbl(per_agreement[is_transmission], "Horizon"))

    # One row per location: Overall summary
    overall_summary <- percent.agreement %>%
      dplyr::group_by(location) %>%
      dplyr::reframe(pa_summary_tbl(per_agreement[is_transmission], "Overall"))

    # Broadcasting both summaries back onto every row
    percent.agreement %>%
      dplyr::left_join(horizon_summary, by = c("horizon", "location")) %>%
      dplyr::left_join(overall_summary, by = "location")

  ##############################################################
  # Triggered if the grouped summaries fail to build correctly #
  ##############################################################
  }, error = function(e){

    # Horizon Groups: Diagnostic group counts (each guarded so the handler can't itself error)
    n_horizon_groups  <- tryCatch(
      dplyr::n_groups(dplyr::group_by(percent.agreement, horizon, location)),
      error = function(e2) NA)

    # Overall Groups: Diagnostic group counts (each guarded so the handler can't itself error)
    n_location_groups <- tryCatch(
      length(unique(percent.agreement$location)), error = function(e2) NA)

    # Message to show to users
    message(
      "percentAgreementCalculation(): row-level metric succeeded but the grouped ",
      "summaries failed, so the summary columns are returned as NA (per_agreement ",
      "is still valid).\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - horizon x location groups: ", n_horizon_groups, "\n",
      "  - distinct locations: ", n_location_groups, "\n",
      "  - Check that 'horizon' and 'location' are present and not all-NA."
    )

    # Returning the row-level frame with NA summary columns added
    add_missing_as_na(percent.agreement)

  })


  ##############################
  # Returning the final result #
  ##############################
  percent.agreement
}
