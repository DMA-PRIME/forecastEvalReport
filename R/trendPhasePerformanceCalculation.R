#' Calculate trend- and phase-specific percent agreement
#'
#' Uses population-adjusted observed trend calls to group the existing
#' row-level Percent Agreement measure within the observed Ascension, Peak, and
#' Decline phases. Results are summarized by location, horizon, trend, and phase
#' using the same median and range convention as the Percent Agreement section.
#'
#' @param percentAgreement.data Output from `percentAgreementCalculation()`.
#' @param population Optional custom population crosswalk. Custom rows extend
#'   the package's built-in population reference.
#' @param eval_config Evaluation configuration from
#'   `create_evaluation_config()`.
#' @param week_days Number of days between consecutive target periods. Uses the
#'   cadence detected from the evaluation model.
#'
#' @return A list with `summary`, a tidy summary table, and `data`, the scored
#'   row-level frame with trend and phase labels.
#'
#' @keywords internal
#' @noRd
trendPhasePerformanceCalculation <- function(percentAgreement.data,
                                             population = NULL,
                                             eval_config = NULL,
                                             week_days = 7){

#------------------------------------------------------------------------------#
# Confirming the function should run ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns empty, consistently shaped outputs whenever the  #
# testing Percent Agreement data are unavailable.                              #
#------------------------------------------------------------------------------#

  empty_summary <- data.frame(
    location = character(), horizon = character(), observed_trend = character(),
    phase = character(), median = numeric(), minimum = numeric(),
    maximum = numeric(), n = integer(), stringsAsFactors = FALSE
  )

  if(is.null(percentAgreement.data) ||
     !is.data.frame(percentAgreement.data) ||
     nrow(percentAgreement.data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

#------------------------------------------------------------------------------#
# Calculating population-adjusted trend calls ---------------------------------
#------------------------------------------------------------------------------#
# About: The trend helper retains the existing row-level Percent Agreement     #
# value while adding observed and forecast week-to-week trend labels. Only the #
# observed label is used to define the table rows.                              #
#------------------------------------------------------------------------------#

  trend_locations <- unique(as.character(percentAgreement.data$location))
  display_crosswalk <- NULL
  if("location_display" %in% names(percentAgreement.data)){
    display_pairs <- unique(percentAgreement.data[
      c("location", "location_display")
    ])
    display_pairs <- display_pairs[
      !is.na(display_pairs$location) & !is.na(display_pairs$location_display),
      , drop = FALSE
    ]
    display_crosswalk <- stats::setNames(
      as.character(display_pairs$location_display),
      as.character(display_pairs$location)
    )
  }

  resolved_population <- resolve_population_values(
    trend_locations,
    custom_crosswalk = population,
    location_crosswalk = display_crosswalk
  )

  trend_bundle <- trendCallCalculation(
    percentAgreement.data,
    population = resolved_population,
    stable_threshold = eval_config$stable_threshold,
    week_days = week_days
  )

  trend_data <- trend_bundle$df
  if(is.null(trend_data) || !is.data.frame(trend_data) || nrow(trend_data) == 0L){
    return(list(summary = empty_summary, data = data.frame()))
  }

  # Rebuild one canonical observed-value column from the original Percent
  # Agreement frame. This deliberately does not depend on the suffixes created
  # by an upstream join (`Observed`, `Observed.x`, or `Observed.y`).
  canonical_observed <- percentAgreement.data %>%
    dplyr::group_by(location, target_end_date) %>%
    dplyr::summarise(
      Observed = if(all(is.na(Observed))) NA_real_ else
        dplyr::first(Observed[!is.na(Observed)]),
      .groups = "drop"
    )

  old_observed_columns <- grep(
    "^Observed($|\\.)", names(trend_data), value = TRUE
  )
  if(length(old_observed_columns) > 0L){
    trend_data <- trend_data %>%
      dplyr::select(-dplyr::all_of(old_observed_columns))
  }

  trend_data <- trend_data %>%
    dplyr::left_join(canonical_observed,
                     by = c("location", "target_end_date"))

#------------------------------------------------------------------------------#
# Assigning seasons from observed target dates --------------------------------
#------------------------------------------------------------------------------#
# About: Epidemic phases describe the observed outcome trajectory, so season   #
# assignment uses target dates rather than forecast reference dates.           #
#------------------------------------------------------------------------------#

  season_text <- trimws(as.character(eval_config$season_start_day_month))
  season_date <- suppressWarnings(as.Date(paste0("2000 ", season_text),
                                           format = "%Y %B %d"))
  if(is.na(season_date)){
    season_date <- suppressWarnings(as.Date(paste0("2000 ", season_text),
                                             format = "%Y %b %d"))
  }
  if(is.na(season_date)) season_date <- as.Date("2000-08-01")

  start_month <- as.integer(format(season_date, "%m"))
  start_day <- as.integer(format(season_date, "%d"))

  trend_data <- trend_data %>%
    dplyr::mutate(
      target_end_date = anytime::anydate(target_end_date),
      target_month = as.integer(format(target_end_date, "%m")),
      target_day = as.integer(format(target_end_date, "%d")),
      target_year = as.integer(format(target_end_date, "%Y")),
      season_start_year = dplyr::if_else(
        target_month > start_month |
          (target_month == start_month & target_day >= start_day),
        target_year, target_year - 1L
      ),
      season = paste0(season_start_year, "-", season_start_year + 1L),
      is_transmission = if("is_transmission" %in% names(trend_data)){
        as.logical(is_transmission)
      }else{
        !lubridate::month(target_end_date) %in%
          eval_config$non_transmission_months
      }
    )

#------------------------------------------------------------------------------#
# Finding the observed peak phase ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Within each location and season, the Peak is the contiguous run around #
# the observed maximum whose values remain within `peak_window` percent of the #
# maximum. Dates before that run are Ascension; dates after it are Decline.    #
#------------------------------------------------------------------------------#

  observed_dates <- trend_data %>%
    dplyr::group_by(location, season, target_end_date) %>%
    dplyr::summarise(
      Observed = if(all(is.na(Observed))) NA_real_ else
        dplyr::first(Observed[!is.na(Observed)]),
      is_transmission = any(is_transmission %in% TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(location, season, target_end_date)

  phase_bounds <- observed_dates %>%
    dplyr::group_by(location, season) %>%
    dplyr::group_modify(function(group_data, group_keys){
      usable <- group_data[
        group_data$is_transmission %in% TRUE &
          !is.na(group_data$Observed) & is.finite(group_data$Observed), ,
        drop = FALSE
      ]

      if(nrow(usable) == 0L){
        return(data.frame(peak_start = as.Date(NA), peak_end = as.Date(NA)))
      }

      usable <- usable[order(usable$target_end_date), , drop = FALSE]
      peak_index <- which.max(usable$Observed)
      peak_value <- usable$Observed[peak_index]
      threshold <- peak_value * (1 - eval_config$peak_window / 100)
      within_peak <- usable$Observed >= threshold

      left <- peak_index
      right <- peak_index
      while(left > 1L && isTRUE(within_peak[left - 1L])) left <- left - 1L
      while(right < nrow(usable) && isTRUE(within_peak[right + 1L])){
        right <- right + 1L
      }

      data.frame(
        peak_start = usable$target_end_date[left],
        peak_end = usable$target_end_date[right]
      )
    }) %>%
    dplyr::ungroup()

  trend_data <- trend_data %>%
    dplyr::left_join(phase_bounds, by = c("location", "season")) %>%
    dplyr::mutate(
      phase = dplyr::case_when(
        is.na(peak_start) | is.na(peak_end) ~ NA_character_,
        target_end_date < peak_start ~ "Ascension",
        target_end_date <= peak_end ~ "Peak",
        target_end_date > peak_end ~ "Decline",
        TRUE ~ NA_character_
      )
    )

#------------------------------------------------------------------------------#
# Summarizing the existing Percent Agreement metric ---------------------------
#------------------------------------------------------------------------------#
# About: Horizon summaries use only the selected horizon. Overall summaries    #
# pool every eligible forecast-target pair across horizons, matching the       #
# existing Percent Agreement section's overall calculation.                    #
#------------------------------------------------------------------------------#

  eligible <- trend_data %>%
    dplyr::filter(
      is_transmission %in% TRUE,
      !is.na(observed_trend),
      !is.na(phase),
      !is.na(per_agreement),
      is.finite(per_agreement)
    )

  if(nrow(eligible) == 0L){
    return(list(summary = empty_summary, data = trend_data))
  }

  summarize_groups <- function(data, horizon_value){
    data %>%
      dplyr::group_by(location, observed_trend, phase) %>%
      dplyr::summarise(
        median = stats::median(per_agreement, na.rm = TRUE),
        minimum = min(per_agreement, na.rm = TRUE),
        maximum = max(per_agreement, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(horizon = as.character(horizon_value)) %>%
      dplyr::select(location, horizon, observed_trend, phase,
                    median, minimum, maximum, n)
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

  list(summary = summary, data = trend_data)
}
