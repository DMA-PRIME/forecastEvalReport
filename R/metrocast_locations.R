#' MetroCast location crosswalk
#'
#' Reference table of locations used by the Flu MetroCast Forecast Hub.
#' Includes Health Service Areas (HSAs), North Carolina flu regions, state
#' aggregates, and NYC. The `location` column contains lowercase, hyphenated
#' identifiers (e.g., `"new-bedford"`, `"south-carolina"`, `"nyc"`) used as
#' the canonical identifier in submission files.
#'
#' Used internally by [validate_metrocast_model()] to verify that submitted
#' `location` values are members of the official MetroCast location set.
#'
#' @format A data frame with rows for each location and the following
#'   columns:
#' \describe{
#'   \item{location}{Lowercase hyphenated identifier; canonical key.}
#'   \item{original_location_code}{The HSA NCI ID, NC flu region ID, or
#'     `"All"` for state aggregates. Stored as character.}
#'   \item{state}{Full state name.}
#'   \item{state_abb}{Two-letter state abbreviation.}
#'   \item{location_name}{Formal display name (e.g., `"Denver, CO"`).}
#'   \item{population}{Population of the location based on 5-year
#'     (2019-2023) census average estimates.}
#'   \item{location_type}{The geography classifier: `"hsa_nci_id"` or
#'     `"nc_flu_region_id"`.}
#'   \item{hsa_counties}{Comma-separated list of counties in the HSA, or
#'     empty string for state-level rows.}
#' }
#'
#' @source Flu MetroCast Hub auxiliary-data/locations.csv.
"metrocast_locations"
