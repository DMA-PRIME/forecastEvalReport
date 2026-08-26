#' Render the testing-period evaluation introduction text
#'
#' Produces the orienting paragraph and the per-metric descriptions shown at the
#' top of the testing-period evaluation section: percent agreement, forecast
#' bias, peak-phase timing, peak-phase magnitude, and the traditional
#' (statistical) scoring metrics. The testing-phase date span is taken from
#' `eval_meta$testing_start` / `eval_meta$testing_end`. When no testing data is
#' present, the function renders nothing so the section disappears from the
#' report entirely.
#'
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Uses
#'   `testing_data` (presence gate) and `testing_start` / `testing_end` (the
#'   displayed date range).
#'
#' @return Rendered HTML via [htmltools::HTML()], or `invisible(NULL)` when no
#'   testing data is available.
#'
#' @keywords internal
#' @noRd
section_testing_intro <- function(eval_meta) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section drives the whole testing-period block. When there is no  #
# testing data (no evaluation model, or an evaluation model with no testing    #
# period), the function returns invisibly so nothing renders -- the intro and  #
# every downstream testing-period section share this same presence signal.     #
#------------------------------------------------------------------------------#

  ###################################
  # Presence of usable testing data #
  ###################################
  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  ###############################################
  # Render nothing when no testing data present #
  ###############################################
  if(!has_testing) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Testing-phase date span ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the start and end of the testing phase for the   #
# intro prose. These bounds are set in extract_evaluation_data() from the      #
# testing-period target dates, so they match the testing-period phase ribbon   #
# on the plots.                                                                #
#------------------------------------------------------------------------------#

  ##########################################
  # First date in the testing phase window #
  ##########################################
  min_ref_date <- if(!is.null(eval_meta$testing_start)){

    # Formatting the start date of the testing window
    format(eval_meta$testing_start, "%B %d, %Y")

  ####################################################
  # Returning UNKNOWN if date could not be formatted #
  ####################################################
  }else{"Unknown"}

  #########################################
  # Last date in the testing phase window #
  #########################################
  max_ref_date <- if(!is.null(eval_meta$testing_end)){

    # Formatting the end date of the testing window
    format(eval_meta$testing_end, "%B %d, %Y")

  ####################################################
  # Returning UNKNOWN if date could not be formatted #
  ####################################################
  }else{"Unknown"}

#------------------------------------------------------------------------------#
# Building the intro HTML ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the intro card -- a lead sentence with the        #
# testing-phase span, followed by one row per metric. Rows are numbered 1-5 in #
# reading order, and each description mirrors what its helper script actually  #
# computes.                                                                    #
#------------------------------------------------------------------------------#

  ################
  # Text to show #
  ################
  htmltools::HTML(paste0('
  <!-- Top-level section header (raw HTML h1), matching the other sections -->
  <h1>Testing Period Evaluation</h1>

  <!-- ============================= Intro card ============================= -->
  <div style="font-family: sans-serif; padding: 1rem 0 1.5rem;">

    <!-- Lead sentence: orients the reader and states the testing-phase span -->
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 2rem 0;">
      The following section includes an exploration of model performance, using five
      key metrics, during the testing phase spanning from
      <strong>', min_ref_date, '</strong> through <strong>', max_ref_date, '</strong>:
    </p>

    <!-- Metric list: one numbered row per metric, in reading order -->
    <div style="display: flex; flex-direction: column; gap: 10px; padding-left: 2rem;">

      <!-- 1. Percent agreement -> percentAgreementCalculation() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">1</span>
        <p style="font-size: 14px; line-height: 1.7; color: #444; margin: 0;">
          <strong>Percent Agreement:</strong> how closely forecasted and observed counts
          align, scored as the ratio of the smaller value to the larger.
        </p>
      </div>

      <!-- 2. Forecast bias -> forecastBiasCalculation() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">2</span>
        <p style="font-size: 14px; line-height: 1.7; color: #444; margin: 0;">
          <strong>Forecast Bias:</strong> the systematic direction of forecast error —
          whether forecasts tend to over- or underestimate observed counts.
        </p>
      </div>

      <!-- 3. Peak-phase timing -> calculating_peak_trough_PEAKPHASE() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">3</span>
        <p style="font-size: 14px; line-height: 1.7; color: #444; margin: 0;">
          <strong>Peak Phase Timing:</strong> how closely the forecasted peak aligned in
          time with the observed seasonal peak phase.
        </p>
      </div>

      <!-- 4. Peak-phase magnitude -> calculating_peak_trough_PEAKPHASE() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">4</span>
        <p style="font-size: 14px; line-height: 1.7; color: #444; margin: 0;">
          <strong>Peak Phase Magnitude:</strong> how closely the forecasted peak counts
          matched the observed counts during the peak phase.
        </p>
      </div>

      <!-- 5. Traditional statistical scores -> traditionalMetricsCalculation() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">5</span>
        <p style="font-size: 14px; line-height: 1.7; color: #444; margin: 0;">
          <strong>Statistical Scoring Metrics:</strong> the Weighted Interval Score (WIS)
          and the absolute error of the median forecast summarize overall accuracy, while
          50% and 95% interval coverage measure how often observed counts fell within the
          corresponding prediction intervals.
        </p>
      </div>

    </div>
  </div>
  '))

}
