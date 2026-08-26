#' Sanitize a location code into its archived-forecast filename tag
#'
#' Reproduces the exact transformation `extract_implementation_data()` applies
#' to a single location when it builds the archived forecast filename
#' (`Forecast-<tag>-<date>.csv`). The writer runs the location through its
#' internal `sanitize_path_component(x, max_chars = 20)` and then title-cases
#' the result, so a raw code such as `"Pee Dee"` is saved as `"Pee_Dee"`.
#'
#' Readers that locate those files (the Forecast Consistency section via
#' `build_forecast_archive()` and the real-time evaluation via
#' `prepare_realtime_evaluation_data()`) must match on this *sanitized* tag
#' rather than the raw code -- otherwise multi-word locations (which pick up
#' underscores) never match and the corresponding sections silently drop out.
#'
#' The returned tag contains only `[a-z0-9_]`, so it is safe to embed directly
#' in a regular expression; matching is done case-insensitively to absorb the
#' writer's title-casing.
#'
#' @param x Character. A raw location code (the *name* of an
#'   `impl_meta$locations` entry).
#' @param max_chars Integer. Maximum tag length, mirroring the writer's
#'   truncation. Default 20.
#'
#' @return A single lowercase character string safe for filename/regex use.
#'
#' @keywords internal
#' @noRd
sanitize_location_tag <- function(x, max_chars = 20){

  # Collapse multiple values if a vector was passed
  x <- paste(x, collapse = "_")

  # Lowercase
  x <- tolower(x)

  # Replace anything that isn't alphanumeric with underscore
  x <- gsub("[^a-z0-9]+", "_", x)

  # Collapse consecutive underscores
  x <- gsub("_+", "_", x)

  # Strip leading and trailing underscores
  x <- gsub("^_|_$", "", x)

  # Truncate to max_chars
  if(nchar(x) > max_chars) x <- substr(x, 1, max_chars)

  # Strip trailing underscore again after truncation
  x <- gsub("_$", "", x)

  # Returning the sanitized tag
  x

}
