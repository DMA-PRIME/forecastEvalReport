#' Calculate overall and phase-specific trend performance
#'
#' @param traditional.data Testing evaluation data returned by
#'   `traditionalMetricsCalculation()`.
#' @param population Optional custom population crosswalk.
#' @param eval_config Evaluation configuration from
#'   `create_evaluation_config()`.
#' @param week_days Number of days between consecutive target periods.
#'
#' @return A list containing a tidy `summary` and the row-level `data` carrying
#'   the observed trend and phase labels.
#'
#' @keywords internal
#' @noRd
trendPhaseTraditionalCalculation <- function(traditional.data,
                                              population = NULL,
                                              eval_config = NULL,
                                              week_days = 7){

#------------------------------------------------------------------------------#
# Confirming testing forecasts are available ----------------------------------#
#------------------------------------------------------------------------------#

  empty_summary <- data.frame(
    location = character(), horizon = character(), breakdown = character(),
    group = character(),
    agreement_pct = numeric(), agreement_n = integer(),
    trend_mae = numeric(), trend_mae_n = integer(), stringsAsFactors = FALSE
  )

  if(is.null(traditional.data) || !is.data.frame(traditional.data) ||
     nrow(traditional.data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

#------------------------------------------------------------------------------#
# Adding the shared observed trend and epidemic phase labels ------------------#
#------------------------------------------------------------------------------#
# About: The common phase helper keeps the median forecast and applies the same #
# population adjustment, stable threshold, season boundary, and peak window as #
# the Similarity Index and Forecast Bias phase tables.                         #
#------------------------------------------------------------------------------#

  phase_input <- traditional.data
  phase_input$per_agreement <- 0

  phase_bundle <- trendPhasePerformanceCalculation(
    percentAgreement.data = phase_input,
    population = population,
    eval_config = eval_config,
    week_days = week_days
  )

  phase_data <- phase_bundle$data
  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     nrow(phase_data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

#------------------------------------------------------------------------------#
# Preparing the two trend-performance measures --------------------------------#
#------------------------------------------------------------------------------#
# About: Label agreement is the percentage of forecast calls identical to the  #
# observed call for the same target date. Trend MAE is the absolute difference #
# between forecast and observed weekly rate changes per 100,000 population.     #
#------------------------------------------------------------------------------#

  eligible <- phase_data %>%
    dplyr::filter(
      is_transmission %in% TRUE,
      !is.na(phase)
    ) %>%
    dplyr::mutate(
      trend_difference = abs(
        forecast_weekly_rate_change - observed_weekly_rate_change
      )
    )

  if(nrow(eligible) == 0L){
    return(list(summary = empty_summary, data = phase_data))
  }

  safe_mean <- function(values){
    values <- values[!is.na(values) & is.finite(values)]
    if(length(values) == 0L) return(NA_real_)
    mean(values)
  }

  summarize_rows <- function(data, horizon_value, breakdown_value,
                             group_value){
    data %>%
      dplyr::group_by(location) %>%
      dplyr::summarise(
        agreement_pct = safe_mean(as.numeric(trend_agreement)) * 100,
        agreement_n = sum(!is.na(trend_agreement)),
        trend_mae = safe_mean(trend_difference),
        trend_mae_n = sum(!is.na(trend_difference) &
                            is.finite(trend_difference)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        horizon = as.character(horizon_value),
        breakdown = as.character(breakdown_value),
        group = as.character(group_value)
      ) %>%
      dplyr::select(location, horizon, breakdown, group, agreement_pct,
                    agreement_n, trend_mae, trend_mae_n)
  }

#------------------------------------------------------------------------------#
# Creating overall, phase, and observed-trend rows ----------------------------#
#------------------------------------------------------------------------------#

  phase_values <- c("Ascension", "Peak", "Decline")
  trend_values <- c("large increase", "increase", "stable",
                    "decrease", "large decrease")
  summary_parts <- list(
    summarize_rows(eligible, "Overall", "overall", "Overall")
  )

  for(phase_value in phase_values){
    summary_parts[[length(summary_parts) + 1L]] <- summarize_rows(
      eligible[eligible$phase == phase_value, , drop = FALSE],
      "Overall", "phase", phase_value
    )
  }
  for(trend_value in trend_values){
    summary_parts[[length(summary_parts) + 1L]] <- summarize_rows(
      eligible[eligible$observed_trend == trend_value, , drop = FALSE],
      "Overall", "trend", trend_value
    )
  }

  horizons <- unique(as.character(eligible$horizon))
  for(horizon_value in horizons){
    horizon_data <- eligible[
      as.character(eligible$horizon) == horizon_value, , drop = FALSE
    ]
    summary_parts[[length(summary_parts) + 1L]] <- summarize_rows(
      horizon_data, horizon_value, "overall", "Overall"
    )
    for(phase_value in phase_values){
      summary_parts[[length(summary_parts) + 1L]] <- summarize_rows(
        horizon_data[horizon_data$phase == phase_value, , drop = FALSE],
        horizon_value, "phase", phase_value
      )
    }
    for(trend_value in trend_values){
      summary_parts[[length(summary_parts) + 1L]] <- summarize_rows(
        horizon_data[
          horizon_data$observed_trend == trend_value, , drop = FALSE
        ],
        horizon_value, "trend", trend_value
      )
    }
  }

  summary <- dplyr::bind_rows(summary_parts)
  row.names(summary) <- NULL

  list(summary = summary, data = phase_data)
}
