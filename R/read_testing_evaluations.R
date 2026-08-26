#' Read and stack testing-evaluation result CSVs
#'
#' Reads one or more long-format CSV files written by
#' `export_testing_evaluation(save_results = TRUE)` and stacks them into a
#' single tidy data frame. Each input file must carry the fixed export schema
#' (`model`, `location`, `horizon`, `scope`, `family`, `metric`, `value`,
#' `value_chr`). An optional `season` column is carried through when present
#' and filled with `NA` for older files that predate it; a `source_file`
#' column is added so rows can be traced back to the file they came from.
#' This is the input layer for
#' `build_evaluation_table()`, but the stacked frame is useful on its own for
#' custom summaries across models or runs.
#'
#' @param files Either a character vector of CSV file paths, or a single
#'   directory path, in which case every `.csv` file in that directory is
#'   read.
#' @param quiet Logical. When `TRUE` (default), suppresses the per-file
#'   progress messages.
#'
#' @return A single long-format data frame with the export schema plus a
#'   `source_file` column.
#'
#' @export
read_testing_evaluations <- function(files, quiet = TRUE) {

#------------------------------------------------------------------------------#
# Validating and resolving the input files -------------------------------------
#------------------------------------------------------------------------------#
# About: This section turns the files argument into a concrete list of CSV     #
# paths. A single directory is expanded to every CSV it contains; a vector     #
# of paths is used as given. Each resolved path must exist before any          #
# reading is attempted, so problems surface up front with a clear message.     #
#------------------------------------------------------------------------------#

  #################################################
  # A small helper to message only when not quiet #
  #################################################
  say <- function(...) if(!isTRUE(quiet)) message(...)

  #######################################################
  # The fixed export schema every input file must carry #
  #######################################################
  required_cols <- c("model", "location", "horizon", "scope", "family",
                     "metric", "value", "value_chr", "reference_date",
                     "target_end_date")

  ##############################################
  # Files must be a non-empty character vector #
  ##############################################
  if(missing(files) || !is.character(files) || length(files) == 0){

    # Stopping if no files were provided
    stop("`files` must be a character vector of CSV paths or a directory.",
         call. = FALSE)

  }

  ##############################################
  # Expand a single directory to its CSV files #
  ##############################################
  if(length(files) == 1 && dir.exists(files)){

    # Listing every CSV in the directory
    files <- list.files(files, pattern = "\\.csv$", full.names = TRUE,
                        ignore.case = TRUE)

    # Stopping if the directory held no CSVs
    if(length(files) == 0){
      stop("No .csv files were found in the supplied directory.",
           call. = FALSE)
    }

  }

  ##########################################
  # Every resolved path must exist on disk #
  ##########################################
  missing_files <- files[!file.exists(files)]

  # Stopping if any path is missing
  if(length(missing_files) > 0){
    stop("These files could not be found:\n  ",
         paste(missing_files, collapse = "\n  "), call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Reading and validating each file ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads each CSV, confirms it carries the full export      #
# schema, keeps only the schema columns in a fixed order, and tags every       #
# row with the originating file name. Files missing any required column are    #
# rejected with a message naming the offending file and columns.               #
#------------------------------------------------------------------------------#

  ##################################
  # Collecting the per-file frames #
  ##################################
  pieces <- list()

  ################################
  # Looping over each input file #
  ################################
  for(f in files){

    # Reading the file as character-stable data
    df <- utils::read.csv(f, stringsAsFactors = FALSE,
                          na.strings = c("NA", ""))

    # Columns missing relative to the required schema
    missing_cols <- setdiff(required_cols, names(df))

    # Rejecting a file that does not match the schema
    if(length(missing_cols) > 0){
      stop("File does not match the export schema: ", f, "\n",
           "  Missing column(s): ", paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }

    # Filling the optional season indicator when an older file omits it
    if(!"season" %in% names(df)) df$season <- NA_character_

    # Keeping the schema columns plus season, in the canonical order
    df <- df[, c(required_cols, "season"), drop = FALSE]

    # Tagging the rows with their source file name
    df$source_file <- basename(f)

    # Messaging progress and storing the frame
    say("read_testing_evaluations(): read ", nrow(df), " rows from ", f)
    pieces[[length(pieces) + 1L]] <- df

  }

#------------------------------------------------------------------------------#
# Stacking and type-stabilizing the result -------------------------------------
#------------------------------------------------------------------------------#
# About: This section row-binds the per-file frames into one long table and    #
# coerces the value columns to stable types (numeric value, character          #
# label) so downstream pivoting and comparison behave predictably.             #
#------------------------------------------------------------------------------#

  ####################################
  # Row-bind all the per-file frames #
  ####################################
  combined <- do.call(rbind, pieces)

  ##############################################
  # Coercing the value columns to stable types #
  ##############################################
  combined$value     <- suppressWarnings(as.numeric(combined$value))

  # The character label column
  combined$value_chr <- as.character(combined$value_chr)

  ####################################
  # Returning the stacked long frame #
  ####################################
  combined

}
