#' Render the real-time evaluation introduction text
#'
#' Produces the orienting paragraph and per-metric descriptions shown at the top
#' of the real-time (operational) evaluation section: percent agreement,
#' forecast bias, and the traditional (statistical) scoring metrics. Peak-phase
#' timing/magnitude is intentionally omitted, as it is a testing-period concept.
#' The evaluation window is taken from the realized rows of the real-time
#' evaluation frame (forecast targets that have since been observed). When no
#' realized data is present the function renders nothing so the section
#' disappears from the report entirely.
#'
#' @param realtime_data The `data` element of `build_realtime_evaluation()` --
#'   the prepared real-time evaluation frame with `reference_date`,
#'   `target_end_date`, and `Observed`.
#'
#' @return Rendered HTML via `htmltools::HTML()`, or `invisible(NULL)` when no
#'   realized real-time data is available.
#'
#' @keywords internal
#' @noRd
section_realtime_intro <- function(realtime_data) {

#------------------------------------------------------------------------------#
# Guard: realized real-time data must be present -------------------------------
#------------------------------------------------------------------------------#
# About: This drives the whole real-time block. When the forecast archive has  #
# no rows whose targets have been observed yet (nothing to evaluate), the      #
# function returns invisibly so nothing renders -- the intro and every         #
# downstream real-time section share this same presence signal.                #
#------------------------------------------------------------------------------#

  if(is.null(realtime_data) || !is.data.frame(realtime_data) ||
     nrow(realtime_data) == 0) return(invisible(NULL))

  realized <- realtime_data[!is.na(realtime_data$Observed), , drop = FALSE]
  if(nrow(realized) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Evaluation window + forecast count -------------------------------------------
#------------------------------------------------------------------------------#

  span_dates <- suppressWarnings(anytime::anydate(realized$target_end_date))
  span_dates <- span_dates[!is.na(span_dates)]

  min_date <- if(length(span_dates) > 0) format(min(span_dates), "%B %d, %Y") else "Unknown"
  max_date <- if(length(span_dates) > 0) format(max(span_dates), "%B %d, %Y") else "Unknown"

  n_fc    <- length(unique(realized$reference_date))
  fc_word <- if(n_fc == 1) "forecast" else "forecasts"

#------------------------------------------------------------------------------#
# Building the intro HTML ------------------------------------------------------
#------------------------------------------------------------------------------#

  htmltools::HTML(paste0('
  <!-- Top-level section header (raw HTML h1), matching the other sections -->
  <h1>Real Time Evaluation</h1>

  <!-- ========================= Real-time intro card ========================= -->
  <div style="font-family: sans-serif; padding: 1rem 0 1.5rem;">

    <!-- Lead sentence: orients the reader and states the evaluation window -->
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 2rem 0;">
      This section evaluates the most recent operational (real-time) forecasts
      against the observed counts that have since been reported, using three key
      metrics. It draws on <strong>', n_fc, '</strong> ', fc_word, ' whose targets
      have been observed, spanning <strong>', min_date, '</strong> through
      <strong>', max_date, '</strong>. Unlike the testing-period evaluation, every
      realized week contributes regardless of season:
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

      <!-- 3. Traditional statistical scores -> traditionalMetricsCalculation() -->
      <div style="display: flex; align-items: center; gap: 12px;">
        <span style="min-width: 22px; height: 22px; border-radius: 50%; background-color: #C9B8E8;
                     color: #fff; font-size: 11px; font-weight: 700; display: flex; flex-shrink: 0;
                     align-items: center; justify-content: center;">3</span>
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
