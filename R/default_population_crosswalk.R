#' Built-in population crosswalk
#'
#' Population reference used by population-adjusted forecast evaluation. It
#' contains every Hubverse location, South Carolina's four Department of Public
#' Health regions, and all 46 South Carolina counties. South Carolina county
#' estimates sum to the package's Hubverse South Carolina population so state,
#' region, and county rates use a consistent denominator vintage.
#'
#' @format A data frame with one row per geography and these columns:
#' \describe{
#'   \item{location}{Canonical location name.}
#'   \item{population}{Population denominator.}
#'   \item{geography_type}{National, state, region, or county.}
#'   \item{location_code}{Hubverse or county FIPS code when available.}
#'   \item{abbreviation}{Hubverse abbreviation when available.}
#'   \item{state}{State abbreviation when applicable.}
#'   \item{region}{South Carolina DPH region for regional and county rows.}
#'   \item{population_year}{Population estimate year.}
#'   \item{source}{Population source description.}
#'   \item{aliases}{Pipe-separated additional names accepted during matching.}
#' }
#'
#' @source U.S. Census Bureau Vintage 2023 county population estimates;
#'   South Carolina Department of Public Health four-region map; and the
#'   Hubverse FluSight 2024-2025 location reference distributed with this
#'   package.
"default_population_crosswalk"
