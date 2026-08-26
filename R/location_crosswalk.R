#' Read and normalize a user-supplied location crosswalk
#'
#' Resolves the optional location crosswalk passed to `generate_report()` into
#' a named character vector mapping each raw location value (the name) to the
#' cleaned display name (the value). The crosswalk lets users keep the raw
#' location codes/names in their source files and archived forecast filenames
#' while showing tidy, human-readable names throughout the report.
#'
#' The input may be:
#' \itemize{
#'   \item `NULL` or a single `NA` -- no crosswalk (returns `NULL`).
#'   \item A path to a two-column CSV with columns `location` (raw value,
#'         exactly as it appears in the model files' `location` column) and
#'         `clean_name` (the display name to show in the report).
#'   \item A `data.frame` with those same two columns.
#'   \item An already-resolved named character vector (returned as-is after
#'         trimming), so the function is safe to call more than once.
#' }
#'
#' Matching against location codes elsewhere is exact after trimming
#' whitespace, so the `location` values must match the raw codes in the model
#' files character-for-character (case-sensitive).
#'
#' @param x A crosswalk file path, data frame, named character vector, `NA`,
#'   or `NULL`.
#'
#' @return A named character vector (raw code -> clean name), or `NULL` when no
#'   crosswalk was supplied.
#'
#' @keywords internal
#' @noRd
read_location_crosswalk <- function(x){

  #############################################
  # No crosswalk supplied -> nothing to apply #
  #############################################
  if(is.null(x)) return(NULL)
  if(is.character(x) && length(x) == 1L && is.na(x)) return(NULL)
  if(length(x) == 1L && is.atomic(x) && is.na(x)) return(NULL)

  ###############################################################
  # Already a named vector: trim, validate names, return as-is  #
  ###############################################################
  if(is.atomic(x) && !is.null(names(x)) && !is.data.frame(x)){

    raw   <- trimws(as.character(names(x)))
    clean <- trimws(as.character(unname(x)))
    return(build_location_crosswalk_vector(raw, clean))

  }

  #########################################################
  # A path to a CSV: read it into a data frame to process #
  #########################################################
  if(is.character(x) && length(x) == 1L){

    if(!file.exists(x)){
      stop(
        "`location_crosswalk` file not found:\n  ", x, "\n\n",
        "Provide a path to a two-column CSV with `location` and `clean_name`,\n",
        "a data.frame with those columns, or NULL.",
        call. = FALSE
      )
    }

    x <- utils::read.csv(
      x, stringsAsFactors = FALSE, check.names = FALSE,
      colClasses = "character", na.strings = c("NA", "")
    )

  }

  ####################################
  # At this point we need a data frame #
  ####################################
  if(!is.data.frame(x)){
    stop(
      "`location_crosswalk` must be a CSV path, a data.frame, a named ",
      "character vector, or NULL.\n",
      "  Received: ", class(x)[1],
      call. = FALSE
    )
  }

  #############################################
  # Required columns: location and clean_name #
  #############################################
  needed  <- c("location", "clean_name")
  missing <- setdiff(needed, names(x))
  if(length(missing) > 0){
    stop(
      "`location_crosswalk` is missing required column(s): ",
      paste(missing, collapse = ", "), ".\n\n",
      "Expected a CSV / data.frame with exactly these columns:\n",
      "  location    - the raw location value, as it appears in your files\n",
      "  clean_name  - the display name to show in the report\n",
      call. = FALSE
    )
  }

  build_location_crosswalk_vector(
    trimws(as.character(x[["location"]])),
    trimws(as.character(x[["clean_name"]]))
  )

}


#' Assemble a validated raw -> clean location vector
#'
#' Shared back-end for `read_location_crosswalk()`. Drops blank rows, warns on
#' and de-duplicates repeated raw codes (first mapping wins), and returns a
#' named character vector or `NULL` when nothing usable remains.
#'
#' @param raw Character vector of raw location values.
#' @param clean Character vector of cleaned display names (same length).
#'
#' @return A named character vector (raw -> clean), or `NULL`.
#'
#' @keywords internal
#' @noRd
build_location_crosswalk_vector <- function(raw, clean){

  ##################################################
  # Drop rows whose raw code or clean name is blank #
  ##################################################
  keep <- !is.na(raw) & nzchar(raw) & !is.na(clean) & nzchar(clean)

  # Warn if any rows were dropped for being incomplete
  if(any(!keep)){
    warning(
      "location crosswalk: dropped ", sum(!keep),
      " row(s) with a blank `location` or `clean_name`.",
      call. = FALSE
    )
  }

  raw   <- raw[keep]
  clean <- clean[keep]

  # Nothing usable left
  if(length(raw) == 0L) return(NULL)

  #####################################################
  # De-duplicate raw codes (first mapping wins), warn #
  #####################################################
  dup <- duplicated(raw)
  if(any(dup)){
    warning(
      "location crosswalk: duplicate `location` value(s) found (",
      paste(unique(raw[dup]), collapse = ", "),
      "); keeping the first mapping for each.",
      call. = FALSE
    )
    raw   <- raw[!dup]
    clean <- clean[!dup]
  }

  # Named vector: raw code -> clean display name
  stats::setNames(clean, raw)

}
