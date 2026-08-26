#' Dartmouth Atlas ZIP-HSA-HRR crosswalk
#'
#' Reference table mapping U.S. ZIP codes to Health Service Areas (HSAs) and
#' Hospital Referral Regions (HRRs) as defined by the Dartmouth Atlas of
#' Health Care. Used by the general-format validator for HSA name lookups
#' when the metrocast crosswalk does not contain the requested HSA.
#'
#' Used internally by [validate_general_model()] as the second-tier HSA
#' lookup. The first tier is [metrocast_locations]; this dataset is the
#' fallback when an HSA is not present in MetroCast.
#'
#' @format A data frame with 40,866 rows (one per U.S. ZIP code) and the
#'   following columns:
#' \describe{
#'   \item{zipcode19}{5-digit ZIP code as character (preserves leading zeros).}
#'   \item{hsanum}{Numeric HSA identifier as character.}
#'   \item{hsacity}{HSA city name (the human-readable HSA label).}
#'   \item{hsastate}{Two-letter state abbreviation of the HSA.}
#'   \item{hrrnum}{Numeric HRR identifier as character.}
#'   \item{hrrcity}{HRR city name.}
#'   \item{hrrstate}{Two-letter state abbreviation of the HRR.}
#' }
#'
#' @source Dartmouth Atlas of Health Care, ZipHsaHrr19.csv (2019).
"dartmouth_hsa_zip"
