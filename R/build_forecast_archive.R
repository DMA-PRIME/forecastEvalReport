#' Locate and order the archived implementation-model forecasts
#'
#' Globs the `Forecasts/` directory that `extract_implementation_data()`
#' writes the current forecast into (the parent directory of
#' `impl_meta$forecast_path`) and returns a tidy table of the most recent
#' forecast CSVs ordered by reference date. Used by the Forecast Consistency
#' section to overlay the last several operational forecasts.
#'
#' Each saved file contains all quantiles for the forward-looking rows and
#' uses raw location codes, so the returned files are suitable for drawing
#' median lines and 50%/95% prediction-interval bands per forecast.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`. Must
#'   contain `forecast_path` (full path to the saved current forecast CSV).
#' @param max_forecasts Integer. Maximum number of most-recent forecasts to
#'   keep. Default 5.
#'
#' @return A data frame with columns `file_path` (character) and
#'   `reference_date` (Date), ordered oldest -> newest, or `NULL` when no
#'   forecast files can be located.
#'
#' @keywords internal
#' @noRd
build_forecast_archive <- function(impl_meta, max_forecasts = 5L){

  ######################################
  # Guard: need a saved forecast path  #
  ######################################
  if(is.null(impl_meta) ||
     is.null(impl_meta$forecast_path) ||
     length(impl_meta$forecast_path) == 0 ||
     is.na(impl_meta$forecast_path[1])){
    return(NULL)
  }

  ######################################
  # Resolve the Forecasts/ directory   #
  ######################################
  forecast_dir <- dirname(impl_meta$forecast_path[1])
  if(!dir.exists(forecast_dir)) return(empty_result)

  if(length(impl_meta[["locations"]]) > 1){

    # Multiple locations: forecasts are saved as Forecast-<date>.csv
    files <- list.files(forecast_dir, pattern = "\\.csv$", full.names = TRUE)

  }else{

    # Single location: forecasts are saved as Forecast-<Code>-<date>.csv.
    # extract_implementation_data() sanitizes the location into that filename
    # (spaces and other non-alphanumerics become underscores, title-cased and
    # truncated to 20 chars), so a raw code like "Pee Dee" is written as
    # "Pee_Dee". Match on the *sanitized* tag -- not the raw code -- otherwise
    # two-word locations never match and the section silently drops out.
    # The tag is [A-Za-z0-9_] only, so it is safe to drop straight into the
    # pattern; ignore.case = TRUE covers the title-casing.
    loc_code <- names(impl_meta[["locations"]])[1]
    loc_tag  <- sanitize_location_tag(loc_code)
    files <- list.files(
      forecast_dir,
      pattern    = paste0("Forecast-", loc_tag, "-.*\\.csv$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  if(length(files) == 0) return(NULL)

  ######################################
  # Reference date per file            #
  ######################################
  ref_dates <- vapply(files, function(fp){
    tryCatch({
      df <- utils::read.csv(fp, stringsAsFactors = FALSE)
      if(!"reference_date" %in% names(df)) return(NA_character_)
      vals <- unique(df$reference_date)
      vals <- vals[!is.na(vals)]
      if(length(vals) == 0) return(NA_character_)
      as.character(max(anytime::anydate(vals)))
    }, error = function(e) NA_character_)
  }, character(1))

  ######################################
  # Assemble + order + keep last N     #
  ######################################
  archive <- data.frame(
    file_path        = files,
    reference_date   = anytime::anydate(ref_dates),
    stringsAsFactors = FALSE
  )

  archive <- archive[!is.na(archive$reference_date), , drop = FALSE]
  if(nrow(archive) == 0) return(NULL)

  archive <- archive[order(archive$reference_date), , drop = FALSE]

  if(nrow(archive) > max_forecasts){
    archive <- archive[(nrow(archive) - max_forecasts + 1L):nrow(archive), ,
                       drop = FALSE]
  }

  rownames(archive) <- NULL
  archive
}
