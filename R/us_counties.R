#' U.S. counties and county-equivalents crosswalk
#'
#' Reference table of U.S. counties, parishes, boroughs, census areas, and
#' municipalities, derived from the Census Bureau FIPS master file. Used
#' internally by [validate_general_model()] to validate `location` values
#' when `location_general = "county"`.
#'
#' County names retain their full form (e.g., `"Autauga County"`,
#' `"Aleutians East Borough"`, `"Orleans Parish"`). The validator matches
#' user-supplied county names against a normalized form (lowercase, with
#' the suffix stripped), so users may enter either `"Aiken"` or
#' `"Aiken County"`.
#'
#' @format A data frame with 3,143 rows (one per county/equivalent) and
#'   the following columns:
#' \describe{
#'   \item{fips}{5-digit FIPS county code as character (preserves leading zeros).}
#'   \item{name}{County name in its full form, including the suffix
#'     ("County", "Parish", "Borough", "Census Area", "Municipality").}
#'   \item{state}{Two-letter state abbreviation.}
#' }
#'
#' @source Census Bureau FIPS codes master file.
"us_counties"
