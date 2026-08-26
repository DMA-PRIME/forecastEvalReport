#' Hubverse U.S. location crosswalk
#'
#' Reference table mapping U.S. location FIPS codes to state abbreviations,
#' names, populations, and per-capita rate-to-count conversion factors used
#' across hubverse forecasting collaborations (FluSight, COVID-19 Forecast
#' Hub, RSV Forecast Hub, and others). Includes all 50 U.S. states, the
#' District of Columbia, Puerto Rico, and a national-level row coded as
#' `"US"`.
#'
#' Used internally by hubverse-format validators (e.g.,
#' [validate_flusight_model()], [validate_covidhub_model()]) to verify that
#' submitted `location` values are members of the official hubverse
#' location set.
#'
#' @format A data frame with rows for each location and the following
#'   columns:
#' \describe{
#'   \item{abbreviation}{Two-letter location abbreviation (e.g., `"SC"`, `"US"`).}
#'   \item{location}{FIPS code as character string (preserves leading zeros).}
#'   \item{location_name}{Full location name.}
#'   \item{population}{Estimated population.}
#'   \item{count_rate0p3, count_rate0p5, count_rate0p7, count_rate1, count_rate1p7, count_rate3, count_rate4, count_rate5}{
#'     Rate-to-count conversion factors at thresholds of 0.3, 0.5, 0.7, 1.0,
#'     1.7, 3.0, 4.0, and 5.0 per 100k population.}
#' }
#'
#' @source FluSight 2024-2025 location reference file. Used unchanged
#'   across hubverse forecasting collaborations.
"hubverse_locations"
