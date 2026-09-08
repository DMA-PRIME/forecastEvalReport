#' Calculate trend- and phase-specific forecast bias
#'
#' Groups the existing row-level forecast-bias measures by observed trend and
#' observed epidemic phase, then summarizes percentage and raw bias by horizon
#' and overall using the Forecast Bias section's median and range convention.
#'
#' @param forecastBias.data Output from `forecastBiasCalculation()`.
#' @param population Optional custom population crosswalk.
#' @param eval_config Evaluation configuration from
#'   `create_evaluation_config()`.
#' @param week_days Number of days between consecutive target periods.
#'
#' @return A list with tidy `summary` and annotated row-level `data`.
#'
#' @keywords internal
#' @noRd
trendPhaseBiasCalculation <- function(forecastBias.data,
                                      population = NULL,
                                      eval_config = NULL,
                                      week_days = 7){

#------------------------------------------------------------------------------#
# Confirming the function should run ------------------------------------------
#------------------------------------------------------------------------------#

  empty_summary <- data.frame(
    location = character(), horizon = character(), observed_trend = character(),
    phase = character(), pct_median = numeric(), pct_minimum = numeric(),
    pct_maximum = numeric(), n_pct = integer(), raw_median = numeric(),
    raw_minimum = numeric(), raw_maximum = numeric(), n_raw = integer(),
    stringsAsFactors = FALSE
  )

  if(is.null(forecastBias.data) || !is.data.frame(forecastBias.data) ||
     nrow(forecastBias.data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

#------------------------------------------------------------------------------#
# Adding the shared observed trend and phase labels ---------------------------
#------------------------------------------------------------------------------#
# About: The Percent Agreement phase helper owns the shared observed-curve     #
# definition. A temporary placeholder lets us reuse that annotation pathway;   #
# the placeholder summary is discarded and only the annotated rows are kept.   #
#------------------------------------------------------------------------------#

  phase_input <- forecastBias.data
  phase_input$per_agreement <- 0

  phase_bundle <- trendPhasePerformanceCalculation(
    percentAgreement.data = phase_input,
    population = population,
    eval_config = eval_config,
    week_days = week_days
  )

  phase_data <- phase_bundle$data
  if(is.null(phase_data) || !is.data.frame(phase_data) || nrow(phase_data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

#------------------------------------------------------------------------------#
# Summarizing percentage and raw bias -----------------------------------------
#------------------------------------------------------------------------------#
# About: Percentage bias uses stable transmission rows only. Raw error uses all #
# transmission rows. This exactly matches the parent Forecast Bias section.    #
#------------------------------------------------------------------------------#

  eligible <- phase_data %>%
    dplyr::filter(
      is_transmission %in% TRUE,
      !is.na(observed_trend),
      !is.na(phase)
    )

  if(nrow(eligible) == 0L){
    return(list(summary = empty_summary, data = phase_data))
  }

  safe_stat <- function(values, fn){
    values <- values[!is.na(values) & is.finite(values)]
    if(length(values) == 0L) return(NA_real_)
    fn(values)
  }

  summarize_groups <- function(data, horizon_value){
    data %>%
      dplyr::group_by(location, observed_trend, phase) %>%
      dplyr::summarise(
        pct_median = safe_stat(pct_error[is_stable %in% TRUE], stats::median),
        pct_minimum = safe_stat(pct_error[is_stable %in% TRUE], min),
        pct_maximum = safe_stat(pct_error[is_stable %in% TRUE], max),
        n_pct = sum(is_stable %in% TRUE & !is.na(pct_error) &
                      is.finite(pct_error)),
        raw_median = safe_stat(raw_error, stats::median),
        raw_minimum = safe_stat(raw_error, min),
        raw_maximum = safe_stat(raw_error, max),
        n_raw = sum(!is.na(raw_error) & is.finite(raw_error)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(horizon = as.character(horizon_value)) %>%
      dplyr::select(
        location, horizon, observed_trend, phase,
        pct_median, pct_minimum, pct_maximum, n_pct,
        raw_median, raw_minimum, raw_maximum, n_raw
      )
  }

  overall <- summarize_groups(eligible, "Overall")
  by_horizon <- eligible %>%
    dplyr::group_split(horizon, .keep = TRUE) %>%
    lapply(function(group_data){
      summarize_groups(group_data, unique(group_data$horizon)[1])
    })

  by_horizon <- if(length(by_horizon) == 0L) empty_summary else
    do.call(rbind, by_horizon)

  summary <- dplyr::bind_rows(overall, by_horizon)
  row.names(summary) <- NULL

  list(summary = summary, data = phase_data)
}
