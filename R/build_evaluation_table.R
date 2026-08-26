#' Build a wide comparison table from testing-evaluation results
#'
#' Takes the long-format results produced by
#' `export_testing_evaluation(save_results = TRUE)` -- either as CSV files or
#' as an already-stacked data frame from `read_testing_evaluations()` -- and
#' reshapes them into a wide comparison table. By default each row is one
#' (family, scope, location, season, horizon, metric) combination and each
#' column is a model, so models sit side by side for easy comparison. Peak
#' rows carry the season they belong to; other families leave it `NA`. The
#' column dimension
#' can be switched to `source_file` when comparing repeated runs of the same
#' model. Optional filters narrow the table to a single metric family, scope,
#' or metric name before pivoting.
#'
#' The cell shown is the numeric `value` when present, otherwise the text
#' `value_chr` label, so numeric metrics and labelled metrics both render in
#' one table.
#'
#' @param x Either a character vector of CSV paths / a directory (passed to
#'   `read_testing_evaluations()`), or a long-format data frame already in the
#'   export schema.
#' @param family Optional character. Keep only this metric family
#'   (`percentAgreement`, `forecastBias`, `peakPhase`, `traditional`).
#' @param scope Optional character. Keep only this scope (`overall` or
#'   `horizon`).
#' @param metric Optional character vector of metric names to keep.
#' @param columns Character naming the column dimension of the wide table.
#'   Either `"model"` (default) or `"source_file"`.
#' @param output_file Optional path. When supplied, the wide table is written
#'   there as a CSV.
#' @param quiet Logical. When `TRUE` (default), suppresses progress messages.
#'
#' @return Invisibly, a named list with `long` (the filtered long frame) and
#'   `table` (the wide comparison data frame).
#'
#' @export
build_evaluation_table <- function(x,
                                   family      = NULL,
                                   scope       = NULL,
                                   metric      = NULL,
                                   columns     = "model",
                                   output_file = NULL,
                                   quiet       = TRUE) {

#------------------------------------------------------------------------------#
# Resolving the long-format input ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section accepts either file paths or an already-stacked data     #
# frame. Paths are read and stacked through read_testing_evaluations();        #
# a data frame is validated against the export schema directly, so the two     #
# entry styles converge on one long frame before any reshaping.                #
#------------------------------------------------------------------------------#

  #################################################
  # A small helper to message only when not quiet #
  #################################################
  say <- function(...) if(!isTRUE(quiet)) message(...)

  #####################################################
  # The fixed export schema, plus the source_file tag #
  #####################################################
  required_cols <- c("model", "location", "horizon", "scope", "family",
                     "metric", "value", "value_chr", "reference_date",
                     "target_end_date")

  ###########################################
  # Resolve x into a long-format data frame #
  ###########################################
  long <- if(is.character(x)){

    # Paths or a directory: read and stack them
    read_testing_evaluations(x, quiet = quiet)

  # Already a data frame: validate the schema directly
  }else if(is.data.frame(x)){

    # Columns missing relative to the required schema
    miss <- setdiff(required_cols, names(x))

    # Rejecting a frame that does not match the schema
    if(length(miss) > 0){
      stop("`x` is missing required column(s): ",
           paste(miss, collapse = ", "), call. = FALSE)
    }

    # Adding a source_file tag when absent
    if(!"source_file" %in% names(x)) x$source_file <- NA_character_

    # Using the frame as given
    x

  # Anything else is unsupported
  }else{
    stop("`x` must be CSV path(s)/a directory or a long-format data frame.",
         call. = FALSE)
  }

  #####################################################
  # Ensure the optional season indicator is available #
  #####################################################
  if(!"season" %in% names(long)) long$season <- NA_character_

#------------------------------------------------------------------------------#
# Validating the column dimension and filters ----------------------------------
#------------------------------------------------------------------------------#
# About: This section checks that the requested column dimension is one of     #
# the two supported keys and applies any family, scope, or metric filters.     #
# Filtering before pivoting keeps the resulting table focused and avoids       #
# sparse, hard-to-read wide output.                                            #
#------------------------------------------------------------------------------#

  #####################################################
  # The column dimension must be model or source_file #
  #####################################################
  if(!is.character(columns) || length(columns) != 1 ||
     !columns %in% c("model", "source_file")){

    # Stopping on an unsupported column dimension
    stop("`columns` must be either \"model\" or \"source_file\".",
         call. = FALSE)

  }

  ####################################
  # Apply the optional family filter #
  ####################################
  if(!is.null(family)) long <- long[long$family %in% family, ]

  ###################################
  # Apply the optional scope filter #
  ###################################
  if(!is.null(scope)) long <- long[long$scope %in% scope, ]

  ####################################
  # Apply the optional metric filter #
  ####################################
  if(!is.null(metric)) long <- long[long$metric %in% metric, ]

  ############################################
  # Stopping if nothing survived the filters #
  ############################################
  if(nrow(long) == 0){
    stop("No rows remain after filtering; check the family / scope / metric ",
         "arguments.", call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Reshaping into the wide comparison table -------------------------------------
#------------------------------------------------------------------------------#
# About: This section collapses value and value_chr into a single display      #
# cell, then pivots the long frame so each row is one descriptive key          #
# combination and each column is a level of the chosen dimension. When the     #
# same key and column appear more than once with differing values, the         #
# first is kept and a warning lists the conflicts.                             #
#------------------------------------------------------------------------------#

  #################################################
  # The descriptive keys that form the table rows #
  #################################################
  id_cols <- c("family", "scope", "location", "season", "horizon",
               "reference_date", "target_end_date", "metric")

  ###########################################################
  # Single display cell: numeric value, else the text label #
  ###########################################################
  long$cell <- ifelse(is.na(long$value), long$value_chr,
                      formatC(long$value, format = "g", digits = 6))

  ###########################################################
  # Stable string key per row-identity and per column level #
  ###########################################################
  id_key  <- do.call(paste, c(long[id_cols], sep = "\u001f"))

  # The column level each row belongs to
  col_val <- as.character(long[[columns]])

  ##################################################
  # Detect and warn on conflicting duplicate cells #
  ##################################################
  dup_key <- paste(id_key, col_val, sep = "\u001f")

  # Conflicts are duplicate id+column pairs with differing cell values
  is_dup  <- duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)

  # Warning the user when genuine conflicts exist
  if(any(is_dup)){

    # The distinct conflicting combinations
    confl <- unique(dup_key[is_dup])

    # Warning listing how many conflicts were collapsed
    warning("build_evaluation_table(): ", length(confl), " duplicate ",
            "key/column cell(s) found; keeping the first of each.",
            call. = FALSE)

  }

  #########################################
  # Ordered, de-duplicated row identities #
  #########################################
  first_row <- !duplicated(id_key)

  # The table of row-identity columns, in first-seen order
  id_tab    <- long[first_row, id_cols, drop = FALSE]

  # The row keys aligned to id_tab
  id_levels <- id_key[first_row]

  ###################################
  # The sorted set of column levels #
  ###################################
  col_levels <- sort(unique(col_val))

  ##################################
  # Empty character matrix to fill #
  ##################################
  mat <- matrix(NA_character_, nrow = nrow(id_tab),
                ncol = length(col_levels),
                dimnames = list(NULL, col_levels))

  ###################################################
  # Row and column indices for every long-frame row #
  ###################################################
  ridx <- match(id_key, id_levels)

  # Column index of each row's column level
  cidx <- match(col_val, col_levels)

  #####################################################
  # Fill the matrix, first value winning on conflicts #
  #####################################################
  for(i in seq_len(nrow(long))){

    # Only writing the first value seen for a cell
    if(is.na(mat[ridx[i], cidx[i]])) mat[ridx[i], cidx[i]] <- long$cell[i]

  }

  ###########################
  # Assemble the wide table #
  ###########################
  wide <- cbind(id_tab, as.data.frame(mat, stringsAsFactors = FALSE),
                stringsAsFactors = FALSE)

  # Resetting the row names for a clean table
  rownames(wide) <- NULL

#------------------------------------------------------------------------------#
# Writing and returning the table ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section writes the wide table to a CSV when a path was given     #
# and returns both the filtered long frame and the wide table invisibly so     #
# the call can feed a larger reporting or comparison pipeline.                 #
#------------------------------------------------------------------------------#

  #################################################
  # Write the wide table when a path was supplied #
  #################################################
  if(!is.null(output_file)){

    # Writing the comparison table to disk
    utils::write.csv(wide, output_file, row.names = FALSE)

    # Messaging the written path
    say("build_evaluation_table(): wrote table to ", output_file)

  }

  ###############################################################
  # Return the filtered long frame and the wide table invisibly #
  ###############################################################
  invisible(list(long = long, table = wide))

}
