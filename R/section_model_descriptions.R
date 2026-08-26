#' Render the per-model description blocks
#'
#' Produces a blockquote description for each model listed in
#' `config$model_descriptions`, showing the model label as a bold heading
#' followed by the user-provided plain-language description. When no model
#' descriptions were provided, nothing is rendered.
#'
#' This version reads the `model` and `description` columns from
#' `config$model_descriptions` (the new column names) and skips rows with no
#' content so empty placeholder rows never render.
#'
#' Rendered as its own section function (via `cat()` with `results='asis'`) so
#' the prose stays modular and out of the report template, matching the pattern
#' of `section_definitions()`.
#'
#' @param config Validated config list from `validate_report_params()`.
#'   Uses `config$model_descriptions`, a data frame with `model` and
#'   `description` columns.
#'
#' @return Called for its side effect of rendering Markdown/HTML via `cat()`.
#'
#' @keywords internal
#' @noRd
section_model_descriptions <- function(config) {

#------------------------------------------------------------------------------#
# Model descriptions must be present and non-empty -----------------------------
#------------------------------------------------------------------------------#
# About: This section acts as an input guard to ensure that the configuration  #
# is provided and model descriptions must be present and non-empty.            #
#------------------------------------------------------------------------------#

  #####################################
  # Extracting the model descriptions #
  #####################################
  md <- config$model_descriptions

  #################################
  # Checks for model descriptions #
  #################################

  # Checks model descriptions are non NULL or missing
  if(is.null(md) || !is.data.frame(md) || nrow(md) == 0) return(invisible(NULL))

  # Checking for mis-specified headers in description
  if(!all(c("model", "description") %in% names(md))) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Dropping rows with no model label or description -----------------------------
#------------------------------------------------------------------------------#
# About: This section keeps only rows that have a non-blank model label and a  #
# non-blank description so empty placeholder rows (model = NA, description =   #
# NA) are never rendered.                                                      #
#------------------------------------------------------------------------------#

  ################################
  # Rows to keep: Non NA & exist #
  ################################
  keep <- !is.na(md$model) &
    nchar(trimws(as.character(md$model))) > 0 &
    !is.na(md$description) &
    nchar(trimws(as.character(md$description))) > 0

  # Filtering model descriptions
  md <- md[keep, , drop = FALSE]

  # Returning NULL if no model descriptions match
  if(nrow(md) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Building and printing the description blocks ---------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the model description call out. Each model and   #
# unique description get their own little highlighted section.                 #
#------------------------------------------------------------------------------#

  ##################################
  # Unique model description lines #
  ##################################
  lines <- sprintf(
    "> #### **%s** : %s\n",
    trimws(as.character(md$model)),
    trimws(as.character(md$description))
  )

  # Collapsing all lines
  cat(paste(lines, collapse = "\n"))

  # Returning NULL warnings/console messages
  invisible(NULL)

}
