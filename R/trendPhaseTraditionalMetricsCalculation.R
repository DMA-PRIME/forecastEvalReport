#' Summarize traditional metrics by observed trend and phase
#'
#' @param phase_data Median-forecast data carrying observed trend and phase
#'   labels from `trendPhaseTraditionalCalculation()`.
#'
#' @return A long-format summary by location, horizon, observed trend, phase,
#'   and traditional metric.
#'
#' @keywords internal
#' @noRd
trendPhaseTraditionalMetricsCalculation <- function(phase_data){

#------------------------------------------------------------------------------#
# Confirming labelled forecast scores are available ---------------------------#
#------------------------------------------------------------------------------#

  empty_summary <- data.frame(
    location = character(), horizon = character(), observed_trend = character(),
    phase = character(), metric = character(), mean = numeric(), n = integer(),
    stringsAsFactors = FALSE
  )

  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     nrow(phase_data) == 0L){
    return(empty_summary)
  }

  metric_columns <- c(
    wis = "WIS_Forecast",
    mae = "MAE_Forecast",
    under = "Under_Forecast",
    over = "Over_Forecast",
    cov50 = "Cov50_Forecast",
    cov80 = "Cov80_Forecast",
    cov95 = "Cov95_Forecast"
  )

  for(column in unname(metric_columns)){
    if(!column %in% names(phase_data)) phase_data[[column]] <- NA_real_
  }

  eligible <- phase_data %>%
    dplyr::filter(
      is_transmission %in% TRUE,
      !is.na(observed_trend),
      !is.na(phase)
    )
  if(nrow(eligible) == 0L) return(empty_summary)

#------------------------------------------------------------------------------#
# Summarizing the selected traditional measures -------------------------------#
#------------------------------------------------------------------------------#
# About: The nested table follows the Similarity Index and Forecast Bias      #
# layout: observed trend labels are rows, phases are columns, and users select  #
# Overall or a single horizon. Traditional metrics retain their usual means.    #
#------------------------------------------------------------------------------#

  summarize_groups <- function(data, horizon_value){
    pieces <- lapply(names(metric_columns), function(metric_name){
      column <- unname(metric_columns[metric_name])
      data %>%
        dplyr::group_by(location, observed_trend, phase) %>%
        dplyr::summarise(
          mean = if(all(is.na(.data[[column]]) |
                        !is.finite(.data[[column]]))) NA_real_ else
            mean(.data[[column]][is.finite(.data[[column]])], na.rm = TRUE),
          n = sum(!is.na(.data[[column]]) & is.finite(.data[[column]])),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          horizon = as.character(horizon_value),
          metric = metric_name
        ) %>%
        dplyr::select(location, horizon, observed_trend, phase,
                      metric, mean, n)
    })
    dplyr::bind_rows(pieces)
  }

  overall <- summarize_groups(eligible, "Overall")
  by_horizon <- eligible %>%
    dplyr::group_split(horizon, .keep = TRUE) %>%
    lapply(function(group_data){
      summarize_groups(group_data, unique(group_data$horizon)[1])
    })

  dplyr::bind_rows(overall, by_horizon)
}
