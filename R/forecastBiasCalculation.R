#' Calculate forecast bias metrics across horizons and overall
#'
#' Computes forecast bias at the row level (raw error and percentage error) and
#' aggregated by horizon and overall within each location. Aggregates are
#' computed on transmission-season rows only; percentage-error summaries are
#' additionally restricted to "stable" rows (observed >= 10) to reduce the
#' influence of low-count instability. Operates on the testing evaluation
#' dataset from `prepare_testing_evaluation_data()`.
#'
#' @param data.for.evaluation Testing evaluation data frame from
#'   `prepare_testing_evaluation_data()`. Must contain `target_end_date`,
#'   `value`, `horizon`, `location`, and `Observed`.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the non-transmission season; excluded from aggregates only.
#'   Default c(5, 6, 7) (May-July).
#' @param stable_threshold Numeric. Observed-count floor at/above which a row is
#'   considered "stable" for percentage-error summaries. Default 10.
#' @param pct_error_cushion Numeric. Percentage-error band (in percentage
#'   points) around zero within which a row counts as "Within Range"; beyond
#'   +/- this it is "Overestimate" / "Underestimate". Default 20.
#'
#' @return The input data frame with row-level and aggregated bias columns
#'   added.
#'
#' @keywords internal
#' @noRd
forecastBiasCalculation <- function(data.for.evaluation,
                                    non_transmission_months,
                                    stable_threshold = 10,
                                    pct_error_cushion = 20) {

#------------------------------------------------------------------------------#
# Confirming function should be run --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the evaluation data exists, is a data frame,    #
# and has at least one row. If not, there is nothing to score, so the input    #
# is returned unchanged and the rest of the report keeps running.              #
#------------------------------------------------------------------------------#

  #######################################################
  # Triggered if there is an issue with evaluation data #
  #######################################################
  if(is.null(data.for.evaluation) ||
     !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0){

    # Returning the (empty / unusable) input unchanged
    return(data.for.evaluation)

  }

  #################################################################
  # Point metrics use the median forecast only (the 0.5 quantile) #
  #################################################################
  data.for.evaluation <- data.for.evaluation %>%
    dplyr::filter(output_type_id == 0.5)

#------------------------------------------------------------------------------#
# Quantile grid + per-metric summary helper ------------------------------------
#------------------------------------------------------------------------------#
# About: The quantile grid and column-name order, plus a one-metric summary    #
# builder (mean, range, median, quantiles) shared by the horizon and overall   #
# stages. Columns are named {stat}{metric}{suffix}, e.g. "q25RawHorizon" or    #
# "meanPctStableOverall". All-NA / empty input yields a one-row NA frame.      #
#------------------------------------------------------------------------------#

  ###########################################
  # List of quantiles to produce for bounds #
  ###########################################
  bias_quantile_probs <- c(0.10, 0.20, 0.25, 0.30, 0.40,
                           0.60, 0.70, 0.75, 0.80, 0.90, 0.95)

  ############################################################
  # Stat order = column order produced by bias_summary_one() #
  ############################################################
  bias_stat_names <- c("mean", "min", "max", "median",
                       paste0("q", bias_quantile_probs * 100))

  ###########################################################
  # Mean / range / median / quantiles for one metric vector #
  ###########################################################
  bias_summary_one <- function(x, metric, suffix){

    # Dropping missing values up front
    x <- x[!is.na(x)]

    # All-NA / empty group -> NA for every stat
    if(length(x) == 0){
      vals <- rep(NA_real_, length(bias_stat_names))

    # Otherwise compute the full set (order must match bias_stat_names)
    }else{

      # Names to compute
      vals <- c(mean(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE), stats::median(x, na.rm = TRUE),
                stats::quantile(x, probs = bias_quantile_probs, na.rm = TRUE, names = FALSE))

    }

    # Naming the values in the data frame
    names(vals) <- paste0(bias_stat_names, metric, suffix)

    # Returning the data frame
    as.data.frame(as.list(vals), check.names = FALSE, stringsAsFactors = FALSE)

  }

#------------------------------------------------------------------------------#
# Expected columns + graceful NA fallback --------------------------------------
#------------------------------------------------------------------------------#
# About: Lists every column this function adds and provides a fallback that    #
# fills any missing ones with a typed NA (numeric / logical / character). If a #
# stage fails, the frame is still returned with the expected columns present   #
# so downstream plotting / tabulation does not choke on a missing column; the  #
# message says why they are NA.                                                #
#------------------------------------------------------------------------------#

  ###############################
  # Row-level columns (by type) #
  ###############################

  # Any columns that contain numbers
  num_row_cols  <- c("forecastValue", "targetValue", "raw_error", "pct_error")

  # Any columns that are boolean
  lgl_row_cols  <- c("is_transmission", "is_stable")

  # Any columns containing strings
  char_row_cols <- c("bias_group")

  ###############################
  # Aggregate (summary) columns #
  ###############################
  num_summary_cols <- c(
    paste0(bias_stat_names, "RawHorizon"),
    paste0(bias_stat_names, "PctAllHorizon"),
    paste0(bias_stat_names, "PctStableHorizon"),
    "n_stable_horizon",
    paste0(bias_stat_names, "RawOverall"),
    paste0(bias_stat_names, "PctAllOverall"),
    paste0(bias_stat_names, "PctStableOverall"),
    "n_stable_overall", "n_total_overall"
  )

  ###################################################
  # Fills any missing expected column with typed NA #
  ###################################################
  add_missing_as_na <- function(df){

    # Numeric columns -> NA_real_
    for(col in c(num_row_cols, num_summary_cols)){
      if(!col %in% names(df)) df[[col]] <- NA_real_
    }

    # Logical columns -> NA
    for(col in lgl_row_cols){
      if(!col %in% names(df)) df[[col]] <- NA
    }

    # Character columns -> NA_character_
    for(col in char_row_cols){
      if(!col %in% names(df)) df[[col]] <- NA_character_
    }

    # Returning the back-filled frame
    df

  }

#------------------------------------------------------------------------------#
# Checking for required columns ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the columns the calculations need are present.  #
# If any are missing, the bias columns are returned as NA (via the fallback)   #
# and a message lists what is missing, so the report keeps running.            #
#------------------------------------------------------------------------------#

  ###############################
  # Columns needed for analysis #
  ###############################
  needed <- c("value", "Observed", "target_end_date", "horizon", "location")

  ############################
  # Flagging missing columns #
  ############################
  missing_cols <- setdiff(needed, names(data.for.evaluation))

  #######################################
  # Triggered if any columns are missing#
  #######################################
  if(length(missing_cols) > 0){

    # Message to show to users
    message(
      "forecastBiasCalculation(): input is missing required column(s), so bias ",
      "metrics are returned as NA.\n",
      "  - Missing: ", paste(missing_cols, collapse = ", "), "\n",
      "  - Columns present: ", paste(names(data.for.evaluation), collapse = ", "), "\n",
      "  - Expected: ", paste(needed, collapse = ", ")
    )

    # Returning the NA-filled frame
    return(add_missing_as_na(data.for.evaluation))

  }

#------------------------------------------------------------------------------#
# Row-level bias measures ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calculates the raw bias error for all rows, after        #
# guarding against missing and zero observations. is_stable marks rows         #
# at/above the stable threshold. bias_group classifies each row, with          #
# off-season and low-count rows labeled distinctly. Off-season rows are kept   #
# (flagged).                                                                   #
#------------------------------------------------------------------------------#

  ########################################
  # Trying to compute the row-level bias #
  ########################################
  forecast.bias <- tryCatch({

    data.for.evaluation %>%

      ###########################################################
      # Renaming columns and flagging 'non-transmission' months #
      ###########################################################
      dplyr::mutate(forecastValue   = value,
                    targetValue     = Observed,
                    is_transmission = !lubridate::month(target_end_date) %in% non_transmission_months) %>%

      #######################################################
      # Row-level errors, stability flag, and bias category #
      #######################################################
      dplyr::mutate(

        # Raw error (forecast - observed)
        raw_error = forecastValue - targetValue,

        # Percentage error, guarded against missing / zero observed
        pct_error = dplyr::if_else(

          # Handling missing values
          is.na(targetValue) | targetValue == 0 | is.na(forecastValue),
          NA_real_,

          # Calculating the percent error
          ((forecastValue - targetValue) / targetValue) * 100
        ),

        # Stable = observed available and at/above the stable threshold
        is_stable = !is.na(targetValue) & targetValue >= stable_threshold,

        # Row category (off-season and low-count rows labeled distinctly)
        bias_group = dplyr::case_when(
          !is_transmission ~ "No Evaluation", # No evaluation model
          !is_stable       ~ "Insufficient Data", # Not enough data
          pct_error >  pct_error_cushion  ~ "Overestimate", # Overestimate with cushion
          pct_error < -pct_error_cushion  ~ "Underestimate", # Underestimate with cushion
          TRUE             ~ "Within Range" # Within range
        )

      )

  #######################################################################
  # Triggered if an error occurs while computing the row-level measures #
  #######################################################################
  }, error = function(e){

    # Message to show to users
    message(
      "forecastBiasCalculation(): failed while computing row-level bias ",
      "measures, so bias metrics are returned as NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - target_end_date class: ",
      paste(class(data.for.evaluation$target_end_date), collapse = "/"),
      " (must be Date for lubridate::month)\n",
      "  - value class: ", paste(class(data.for.evaluation$value), collapse = "/"),
      "; Observed class: ", paste(class(data.for.evaluation$Observed), collapse = "/"),
      " (both must be numeric for the error arithmetic)\n",
      "  - Rows in: ", nrow(data.for.evaluation)
    )

    # Returning NULL so the bail below can fire
    NULL

  })

  ##########################################
  # Bail to NA columns if the stage failed #
  ##########################################
  if(is.null(forecast.bias)) return(add_missing_as_na(data.for.evaluation))

#------------------------------------------------------------------------------#
# Horizon-grouped bias (transmission rows only) --------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a full summary (mean, range, median, quantiles)  #
# of raw and percentage error per horizon x location, over transmission rows   #
# only, plus a 'stable' percentage-error track restricted to rows at/above the #
# stable threshold, and a stable-row count. Computed once per group then       #
# broadcast back via join. A failure keeps the row-level columns and returns   #
# the horizon summaries as NA.                                                 #
#------------------------------------------------------------------------------#

  ###########################################
  # Trying to compute the horizon summaries #
  ###########################################
  forecast.bias <- tryCatch({

    ################################
    # Creating the horizon summary #
    ################################
    horizon_summary <- forecast.bias %>%

      # Grouping by horizon and location
      dplyr::group_by(horizon, location) %>%

      # Creating the new data frame
      dplyr::reframe(cbind(
        bias_summary_one(raw_error[is_transmission],             "Raw",       "Horizon"),
        bias_summary_one(pct_error[is_transmission],             "PctAll",    "Horizon"),
        bias_summary_one(pct_error[is_transmission & is_stable], "PctStable", "Horizon"),
        data.frame(n_stable_horizon = sum(is_stable & is_transmission, na.rm = TRUE))
      ))

    # Broadcasting the horizon summaries back onto every row
    forecast.bias %>%
      dplyr::left_join(horizon_summary, by = c("horizon", "location"))

  #######################################################################
  # Triggered if an error occurs while computing the horizon summaries  #
  #######################################################################
  }, error = function(e){

    # Diagnostic group count (guarded so the handler can't itself error)
    n_h_groups <- tryCatch(
      dplyr::n_groups(dplyr::group_by(forecast.bias, horizon, location)),
      error = function(e2) NA)

    # Message to show to users
    message(
      "forecastBiasCalculation(): row-level measures succeeded but the ",
      "horizon-grouped summaries failed; horizon summary columns are returned ",
      "as NA (row-level columns are still valid).\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - horizon x location groups: ", n_h_groups, "\n",
      "  - Tip: confirm 'horizon' and 'location' are present and not all-NA."
    )

    # Keeping row-level data, filling horizon summaries as NA
    add_missing_as_na(forecast.bias)

  })

#------------------------------------------------------------------------------#
# Overall bias (transmission rows only) ----------------------------------------
#------------------------------------------------------------------------------#
# About: The same full raw / percentage / stable summaries collapsed to one    #
# set per location (agnostic of horizon), plus stable and total transmission-  #
# row counts. Computed once per location then broadcast back via join. A       #
# failure keeps everything computed so far and returns the overall summary     #
# columns as NA.                                                               #
#------------------------------------------------------------------------------#

  ###########################################
  # Trying to compute the overall summaries #
  ###########################################
  forecast.bias <- tryCatch({

    # One row per location: full stat set for each metric + counts
    overall_summary <- forecast.bias %>%

      # Grouping only by location
      dplyr::group_by(location) %>%

      # Creating the data frame with metrics
      dplyr::reframe(cbind(
        bias_summary_one(raw_error[is_transmission],             "Raw",       "Overall"),
        bias_summary_one(pct_error[is_transmission],             "PctAll",    "Overall"),
        bias_summary_one(pct_error[is_transmission & is_stable], "PctStable", "Overall"),
        data.frame(n_stable_overall = sum(is_stable & is_transmission, na.rm = TRUE),
                   n_total_overall  = sum(is_transmission, na.rm = TRUE))
      ))

    # Broadcasting the overall summaries back onto every row
    forecast.bias %>%
      dplyr::left_join(overall_summary, by = "location")

  #######################################################################
  # Triggered if an error occurs while computing the overall summaries  #
  #######################################################################
  }, error = function(e){

    # Diagnostic location count (guarded so the handler can't itself error)
    n_loc <- tryCatch(length(unique(forecast.bias$location)), error = function(e2) NA)

    # Message to show to users
    message(
      "forecastBiasCalculation(): row-level (and horizon) measures succeeded ",
      "but the overall summaries failed; overall summary columns are returned ",
      "as NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - distinct locations: ", n_loc, "\n",
      "  - Tip: confirm 'location' is present and not all-NA."
    )

    # Keeping everything computed so far, filling overall summaries as NA
    add_missing_as_na(forecast.bias)

  })

  #####################################
  # Returning the finished bias frame #
  #####################################
  return(forecast.bias)

}
