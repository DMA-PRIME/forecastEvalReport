#' Calculate population-adjusted testing-period trend calls
#'
#' Replicates the supplied state, region, and county trend analysis for the
#' testing-period forecast evaluation frame. Forecast and observed counts are
#' converted to rates, week-to-week differences are calculated on those rates,
#' and the observed rate-change percentiles within each location define the
#' five trend categories applied to both series.
#'
#' The original analysis uses a hybrid stability rule: a weekly change of fewer
#' than 10 raw counts is called stable before the population-adjusted percentile
#' rules are applied. This function preserves that behavior through
#' `stable_threshold`. Set it to `NULL` to disable the raw-count override.
#'
#' Population can be supplied as one number for a single-location analysis, a
#' named numeric vector keyed by location, or a data frame containing location
#' and population columns. When `population = NULL`, the function resolves
#' locations from [default_population_crosswalk], which includes all Hubverse
#' locations, all South Carolina counties, and South Carolina's four public
#' health regions. A supplied lookup extends and overrides that built-in table.
#'
#' @param data.for.evaluation Testing evaluation data frame, normally
#'   `build_testing_evaluation(...)$data` or
#'   `export_testing_evaluation(...)$testing_eval$data`. Must contain
#'   `location`, `target_end_date`, `horizon`, `value`, and `Observed`.
#' @param population Population denominator. Supply `NULL` for automatic lookup,
#'   a single positive number for one location, a named numeric vector, or a
#'   data frame. Custom lookups only need to contain locations absent from the
#'   built-in table. See `population_location_col` and `population_value_col`.
#' @param population_location_col Character. Location-key column in a population
#'   data frame. Default `"location"`.
#' @param population_value_col Character. Numeric population column in a
#'   population data frame. Default `"population"`.
#' @param rate_multiplier Positive numeric rate denominator. Default `100000`,
#'   producing rates per 100,000 population.
#' @param stable_threshold A non-negative raw-count change below which the call
#'   is forced to `"stable"`, reproducing the regional and county analysis.
#'   Default `10`. Set to `NULL` to use only rate-change percentiles.
#' @param week_days Positive number of days required between consecutive target
#'   dates for a week-to-week comparison. Default `7`.
#'
#' @return A named list with `df`, containing population, raw counts, rates,
#'   raw and rate week-to-week changes, forecast and observed trend calls, and
#'   agreement; and `location_percentiles`, containing each location's
#'   population and observed rate-change cut points.
#'
#' @examples
#' \dontrun{
#' exported <- export_testing_evaluation("report_options.R")
#'
#' trends <- trendCallCalculation(
#'   exported$testing_eval$data,
#'   population = c("Lowcountry" = 1200000, "Midlands" = 1100000)
#' )
#' }
#'
#' @export
trendCallCalculation <- function(data.for.evaluation,
                                 population = NULL,
                                 population_location_col = "location",
                                 population_value_col = "population",
                                 rate_multiplier = 100000,
                                 stable_threshold = 10,
                                 week_days = 7) {

#------------------------------------------------------------------------------#
# Confirming the function should be run ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the evaluation frame and the user-controlled      #
# thresholds before any population matching or trend calculation occurs.       #
#------------------------------------------------------------------------------#

  if(is.null(data.for.evaluation) ||
     !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0L){
    return(list(df = data.frame(), location_percentiles = data.frame()))
  }

  needed <- c("location", "target_end_date", "horizon", "value", "Observed")
  missing_cols <- setdiff(needed, names(data.for.evaluation))
  if(length(missing_cols) > 0L){
    stop("trendCallCalculation(): input is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  if(!is.numeric(rate_multiplier) || length(rate_multiplier) != 1L ||
     is.na(rate_multiplier) || !is.finite(rate_multiplier) ||
     rate_multiplier <= 0){
    stop("`rate_multiplier` must be one positive finite number.",
         call. = FALSE)
  }

  if(!is.null(stable_threshold) &&
     (!is.numeric(stable_threshold) || length(stable_threshold) != 1L ||
      is.na(stable_threshold) || !is.finite(stable_threshold) ||
      stable_threshold < 0)){
    stop("`stable_threshold` must be NULL or one non-negative number.",
         call. = FALSE)
  }

  if(!is.numeric(week_days) || length(week_days) != 1L ||
     is.na(week_days) || !is.finite(week_days) || week_days <= 0){
    stop("`week_days` must be one positive finite number.", call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Preparing the point forecasts ------------------------------------------------
#------------------------------------------------------------------------------#
# About: The trend analysis is a point-forecast comparison, so the median      #
# quantile is retained and all calculation fields are converted consistently. #
#------------------------------------------------------------------------------#

  trend.data <- data.for.evaluation

  if("output_type_id" %in% names(trend.data)){
    ids <- suppressWarnings(as.numeric(as.character(trend.data$output_type_id)))
    trend.data <- trend.data[!is.na(ids) & abs(ids - 0.5) < 1e-8, , drop = FALSE]
    if(nrow(trend.data) == 0L){
      stop("trendCallCalculation(): no median forecast rows ",
           "(output_type_id = 0.5) were found.", call. = FALSE)
    }
  }

  trend.data$location <- as.character(trend.data$location)
  trend.data$target_end_date <- anytime::anydate(trend.data$target_end_date)
  trend.data$value <- suppressWarnings(as.numeric(trend.data$value))
  trend.data$Observed <- suppressWarnings(as.numeric(trend.data$Observed))
  trend.data$horizon <- suppressWarnings(as.numeric(trend.data$horizon))

  key_cols <- intersect(c("model", "location", "horizon", "target_end_date"),
                        names(trend.data))
  duplicate_forecasts <- duplicated(trend.data[key_cols]) |
    duplicated(trend.data[key_cols], fromLast = TRUE)
  if(any(duplicate_forecasts)){
    stop("trendCallCalculation(): more than one point forecast was found for ",
         "a model, location, horizon, and target_end_date.", call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Resolving population denominators -------------------------------------------
#------------------------------------------------------------------------------#
# About: Explicit population inputs take priority while extending the built-in #
# crosswalk. Automatic lookup recognizes codes, abbreviations, clean names,    #
# South Carolina regions, and South Carolina counties.                         #
#------------------------------------------------------------------------------#

  locations <- unique(trend.data$location)
  locations <- locations[!is.na(locations) & nzchar(locations)]

  resolved_population <- if(is.numeric(population) && length(population) == 1L &&
           is.null(names(population))){
    if(length(locations) != 1L){
      stop("An unnamed scalar `population` can only be used when the data ",
           "contain one location. Use a named vector or lookup table for ",
           "multiple locations.", call. = FALSE)
    }
    stats::setNames(as.numeric(population), locations)

  }else{
    # Normalize alternate data-frame column names to the two-column public
    # population crosswalk contract before extending the defaults.
    custom_population <- population
    if(is.data.frame(custom_population)){
      missing_population_cols <- setdiff(
        c(population_location_col, population_value_col),
        names(custom_population)
      )
      if(length(missing_population_cols) > 0L){
        stop("Population lookup is missing required column(s): ",
             paste(missing_population_cols, collapse = ", "), ".",
             call. = FALSE)
      }
      custom_population <- data.frame(
        location = custom_population[[population_location_col]],
        population = custom_population[[population_value_col]],
        stringsAsFactors = FALSE
      )
    }
    resolve_population_values(locations, custom_population)
  }

  bad_population <- is.na(resolved_population) |
    !is.finite(resolved_population) | resolved_population <= 0
  if(any(bad_population)){
    stop_for_missing_population(names(resolved_population)[bad_population])
  }

  trend.data$population <- unname(resolved_population[trend.data$location])

#------------------------------------------------------------------------------#
# Preparing observed rate changes and percentile cut points ------------------
#------------------------------------------------------------------------------#
# About: Truth values repeat across horizons and possibly models, so one       #
# observation per location/week is retained. Percentiles are based on weekly  #
# changes in the rate per `rate_multiplier`, exactly as in the source analysis.#
#------------------------------------------------------------------------------#

  observed.data <- trend.data %>%
    dplyr::group_by(location, target_end_date) %>%
    dplyr::summarise(
      observed_values = dplyr::n_distinct(Observed[!is.na(Observed)]),
      Observed = if(all(is.na(Observed))) NA_real_ else
        dplyr::first(Observed[!is.na(Observed)]),
      population = dplyr::first(population),
      .groups = "drop"
    )

  if(any(observed.data$observed_values > 1L)){
    stop("trendCallCalculation(): conflicting observed values were found for ",
         "the same location and target_end_date.", call. = FALSE)
  }

  observed.data <- observed.data %>%
    dplyr::group_by(location) %>%
    dplyr::arrange(target_end_date, .by_group = TRUE) %>%
    dplyr::mutate(
      observed_rate = Observed / population * rate_multiplier,
      observed_days_between = as.numeric(
        target_end_date - dplyr::lag(target_end_date)),
      observed_weekly_change = dplyr::if_else(
        observed_days_between == week_days,
        Observed - dplyr::lag(Observed), NA_real_),
      observed_weekly_rate_change = dplyr::if_else(
        observed_days_between == week_days,
        observed_rate - dplyr::lag(observed_rate), NA_real_)
    ) %>%
    dplyr::ungroup()

  safe_percentile <- function(x, probability){
    x <- x[!is.na(x) & is.finite(x)]
    if(length(x) == 0L) return(NA_real_)
    as.numeric(stats::quantile(x, probs = probability, na.rm = TRUE,
                               names = FALSE))
  }

  location_percentiles <- observed.data %>%
    dplyr::group_by(location) %>%
    dplyr::summarise(
      population = dplyr::first(population),
      rate_multiplier = rate_multiplier,
      n_weekly_changes = sum(!is.na(observed_weekly_rate_change)),
      p05 = safe_percentile(observed_weekly_rate_change, 0.05),
      p25 = safe_percentile(observed_weekly_rate_change, 0.25),
      p75 = safe_percentile(observed_weekly_rate_change, 0.75),
      p95 = safe_percentile(observed_weekly_rate_change, 0.95),
      .groups = "drop"
    )

  stable_observed <- if(is.null(stable_threshold)){
    rep(FALSE, nrow(observed.data))
  }else{abs(observed.data$observed_weekly_change) < stable_threshold}

  observed.data <- observed.data %>%
    dplyr::left_join(location_percentiles, by = c("location", "population")) %>%
    dplyr::mutate(
      observed_trend = dplyr::case_when(
        stable_observed                              ~ "stable",
        observed_weekly_rate_change <= p05           ~ "large decrease",
        observed_weekly_rate_change > p05 &
          observed_weekly_rate_change <= p25         ~ "decrease",
        observed_weekly_rate_change > p25 &
          observed_weekly_rate_change < p75          ~ "stable",
        observed_weekly_rate_change >= p75 &
          observed_weekly_rate_change < p95          ~ "increase",
        observed_weekly_rate_change >= p95           ~ "large increase",
        TRUE                                         ~ NA_character_
      )
    ) %>%
    dplyr::select(location, target_end_date, population, Observed,
                  observed_rate, observed_weekly_change,
                  observed_weekly_rate_change, observed_trend)

#------------------------------------------------------------------------------#
# Calculating population-adjusted forecast trend calls ------------------------
#------------------------------------------------------------------------------#
# About: The change a forecast asserts for the week ending at each target is  #
# taken within a single submission: the value at horizon h minus the value at #
# horizon h - 1 issued on the same reference date. The smallest horizon in a  #
# submission has no h - 1, so it anchors to the observed value one step       #
# earlier -- the last data point the forecaster had. A gap in the middle of a #
# submission is left NA rather than bridged with information the forecaster   #
# could not have used. The observed rate cut points are then applied so       #
# forecast and truth calls are directly comparable.                           #
#------------------------------------------------------------------------------#

  grouping <- intersect(c("model", "location", "horizon"), names(trend.data))

  # Submission identity: rows sharing a forecast reference date, derived from
  # the target date minus the horizon in steps when no explicit column exists
  horizon_steps <- suppressWarnings(as.numeric(trend.data$horizon))
  trend.data$forecast_anchor_date <-
    trend.data$target_end_date - horizon_steps * week_days

  sub_grouping <- intersect(c("model", "location"), names(trend.data))

  trend.results <- trend.data %>%
    dplyr::mutate(forecast_rate = value / population * rate_multiplier) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(sub_grouping, "forecast_anchor_date")))) %>%
    dplyr::arrange(target_end_date, .by_group = TRUE) %>%
    dplyr::mutate(
      prev_forecast_value = dplyr::lag(value),
      prev_forecast_rate  = dplyr::lag(forecast_rate),
      prev_target_gap     = as.numeric(
        target_end_date - dplyr::lag(target_end_date))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prev_target_date = target_end_date - week_days) %>%
    dplyr::left_join(
      observed.data %>%
        dplyr::select(location,
                      prev_target_date = target_end_date,
                      prev_observed_value = Observed,
                      prev_observed_rate = observed_rate),
      by = c("location", "prev_target_date")
    ) %>%
    dplyr::mutate(

      # Where the anchor for this row's asserted change comes from
      anchor_source = dplyr::case_when(
        !is.na(prev_forecast_value) &
          prev_target_gap == week_days                   ~ "forecast",
        !is.na(prev_observed_value) &
          prev_target_date <= forecast_anchor_date       ~ "observed",
        TRUE                                             ~ NA_character_
      ),
      prev_anchor_value = dplyr::case_when(
        anchor_source == "forecast" ~ prev_forecast_value,
        anchor_source == "observed" ~ prev_observed_value,
        TRUE                        ~ NA_real_
      ),
      prev_anchor_rate = dplyr::case_when(
        anchor_source == "forecast" ~ prev_forecast_rate,
        anchor_source == "observed" ~ prev_observed_rate,
        TRUE                        ~ NA_real_
      ),

      forecast_weekly_change      = value - prev_anchor_value,
      forecast_weekly_rate_change = forecast_rate - prev_anchor_rate
    ) %>%
    dplyr::left_join(location_percentiles, by = c("location", "population"))

  stable_forecast <- if(is.null(stable_threshold)){
    rep(FALSE, nrow(trend.results))
  }else{abs(trend.results$forecast_weekly_change) < stable_threshold}

  # The forecast frame already carries the canonical `Observed` value. Remove
  # the duplicate copy from the derived observed fields before joining so the
  # result keeps `Observed` rather than producing `Observed.x`/`Observed.y`.
  observed.trend.fields <- observed.data %>%
    dplyr::select(-Observed)

  trend.results <- trend.results %>%
    dplyr::mutate(
      forecast_trend = dplyr::case_when(
        stable_forecast                              ~ "stable",
        forecast_weekly_rate_change <= p05           ~ "large decrease",
        forecast_weekly_rate_change > p05 &
          forecast_weekly_rate_change <= p25         ~ "decrease",
        forecast_weekly_rate_change > p25 &
          forecast_weekly_rate_change < p75          ~ "stable",
        forecast_weekly_rate_change >= p75 &
          forecast_weekly_rate_change < p95          ~ "increase",
        forecast_weekly_rate_change >= p95           ~ "large increase",
        TRUE                                         ~ NA_character_
      )
    ) %>%
    dplyr::left_join(observed.trend.fields,
                     by = c("location", "target_end_date", "population")) %>%
    dplyr::mutate(
      trend_agreement = dplyr::if_else(
        is.na(forecast_trend) | is.na(observed_trend),
        NA, forecast_trend == observed_trend)
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(grouping)), target_end_date)

#------------------------------------------------------------------------------#
# Returning the trend-call results --------------------------------------------
#------------------------------------------------------------------------------#

  list(
    df = trend.results,
    location_percentiles = location_percentiles
  )
}
