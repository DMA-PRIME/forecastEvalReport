#' Build the testing-period evaluation data and metrics
#'
#' Single entry point for the report's testing-period evaluation layer. Joins
#' the evaluation model's testing-period forecasts to the observed outcome
#' values via `prepare_testing_evaluation_data()`, then runs the point-metric
#' helpers (percent agreement, forecast bias), the peak-phase helper, and the
#' traditional scoring-rule helper (WIS, MAE, coverage) on the resulting frame.
#' Every underlying helper guards its own inputs and returns
#' NA-filled / empty output rather than erroring, so this builder is safe to
#' call even when no testing data is present. The prepared frame and each
#' metric result are returned together so the downstream display sections can
#' pull what they need from one object.
#'
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Supplies
#'   `testing_data`, and (when present) `locations` and `time_step`.
#' @param master_data Assembled master data frame from `assemble_report_data()`.
#'   Outcome rows supply the observed (truth) values for the join.
#' @param locations Optional named character vector mapping raw location codes
#'   (names) to display names (values). When `NULL`, falls back to
#'   `eval_meta$locations`, then to the helper's identity map.
#' @param time_step Integer days per step. When `NULL`, taken from
#'   `eval_meta$time_step`, then defaults to 7.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the off-season; passed through to every metric helper. Default
#'   c(5, 6, 7) (May-July).
#' @param season_start_day_month Character "Month DD" passed to the peak-phase
#'   helper for season assignment. Default "August 01".
#' @param peak_window Numeric tolerance around the observed seasonal max, as a
#'   percent of that max, used to define the contiguous peak phase. Default 20.
#' @param stable_threshold Numeric. Observed-count floor at/above which a row is
#'   counted as "stable" for the forecast-bias percentage-error summaries.
#'   Default 10.
#' @param pct_error_cushion Numeric. Percentage-error band (in points) around
#'   zero within which a forecast-bias row counts as "Within Range". Default 20.
#' @param timing_tol_steps Numeric. Tolerance, in time steps, within which a
#'   peak-phase timing miss still counts as "On Time". Default 1.
#' @param mag_tol Numeric in 0-1. Minimum rank-matched agreement at/above which
#'   a peak-phase magnitude counts as "On Target". Default 0.80.
#'
#' @return A named list with `data` (the prepared evaluation frame),
#'   `percentAgreement`, `forecastBias`, `peakPhase`, and `traditional`.
#'
#' @keywords internal
#' @noRd
build_testing_evaluation <- function(eval_meta,
                                     master_data,
                                     locations               = NULL,
                                     time_step               = NULL,
                                     non_transmission_months  = c(5, 6, 7),
                                     season_start_day_month   = "August 01",
                                     peak_window              = 20,
                                     stable_threshold         = 10,
                                     pct_error_cushion        = 20,
                                     timing_tol_steps         = 0L,
                                     mag_tol                  = 0.80) {

#------------------------------------------------------------------------------#
# Resolving inputs from eval_meta ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the inputs included within the evaluation meta  #
# data file. The time step and the location crosswalk are detected upstream    #
# and carried on eval_meta. We honor an explicit argument when one is supplied #
# otherwise we read them off eval_meta, falling back to the same defaults the  #
# helpers use so the builder is safe even on a minimal eval_meta.              #
#------------------------------------------------------------------------------#

  ##############################################
  # Time step: argument -> eval_meta -> 7 days #
  ##############################################
  if(is.null(time_step)){
    time_step <- if(!is.null(eval_meta$time_step)) eval_meta$time_step else 7
  }

  ###########################################################
  # Locations: argument -> eval_meta -> helper identity map #
  ###########################################################
  if(is.null(locations)){
    locations <- if(!is.null(eval_meta$locations)) eval_meta$locations else NULL
  }

#------------------------------------------------------------------------------#
# Prepared evaluation frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section joins the testing-period forecasts to the observed truth #
# values and back-calculates the reference date. This single frame (all        #
# quantile rows) is the input every metric helper consumes; the point-metric   #
# helpers filter to the median quantile internally.                            #
#------------------------------------------------------------------------------#

  ###########################################
  # Forecast-to-truth evaluation data frame #
  ###########################################
  data.for.evaluation <- prepare_testing_evaluation_data(
    eval_meta   = eval_meta,
    master_data = master_data,
    locations   = locations,
    time_step   = time_step
  )

#------------------------------------------------------------------------------#
# Metric calculations ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section runs the four testing-period metric helpers on the       #
# prepared frame. Percent agreement and forecast bias return the frame         #
# augmented with their metric columns; the peak-phase helper returns a         #
# per-forecast indicator frame; the traditional helper scores WIS, MAE, and    #
# coverage from the full quantile rows. Each guards internally, so a missing   #
# or empty input yields NA / empty output rather than an error.                #
#------------------------------------------------------------------------------#

  #################################
  # Calculating percent agreement #
  #################################
  percentAgreement.data <- percentAgreementCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months
  )

  #################################
  # Calculating the forecast bias #
  #################################
  forecastBias.data <- forecastBiasCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months,
    stable_threshold        = stable_threshold,
    pct_error_cushion       = pct_error_cushion
  )

  ##########################################
  # Calculating the peak-phase performance #
  ##########################################
  peakPhase.data <<- calculating_peak_trough_PEAKPHASE(
    data.for.evaluation,
    season_start_day_month  = season_start_day_month,
    peak_window             = peak_window,
    non_transmission_months = non_transmission_months,
    time_step               = time_step,
    timing_tol_steps        = timing_tol_steps,
    mag_tol                 = mag_tol
  )

  ######################################################
  # Calculating traditional scores (WIS, MAE, coverage)#
  ######################################################
  traditional.data <- traditionalMetricsCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months
  )

#------------------------------------------------------------------------------#
# Returning the evaluation bundle ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section produces the list holding the prepared frame and each    #
# metric result, so the downstream display sections can read what they need    #
# from a single object.                                                        #
#------------------------------------------------------------------------------#

  ##################################
  # Bundling the data and metrics  #
  ##################################
  list(
    data             = data.for.evaluation,
    percentAgreement = percentAgreement.data,
    forecastBias     = forecastBias.data,
    peakPhase        = peakPhase.data,
    traditional      = traditional.data
  )

}
