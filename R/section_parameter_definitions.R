#' Render the auxiliary variable definitions accordion
#'
#' Produces a collapsible accordion containing definitions for the auxiliary
#' variables used in the model. Each row shows the variable's clean name, its
#' data source clean name, and its definition. The entire accordion (header,
#' intro text, table, and close) is rendered by this single function. When no
#' auxiliary variables are present, nothing is rendered so the section is
#' cleanly omitted.
#'
#' This version reads everything from the validated `variables_crosswalk`:
#' the `aux_variable` rows supply the parameter clean name and definition,
#' and the data source clean name is resolved via `get_aux_source_labels()`.
#'
#' Rendered as its own section function (via `cat()` with `results='asis'`) so
#' the whole accordion stays modular and out of the report template, matching
#' the pattern of `section_definitions()`.
#'
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#'
#' @return Called for its side effect of rendering HTML via `cat()`.
#'
#' @keywords internal
#' @noRd
section_parameter_definitions <- function(variables_crosswalk) {

#------------------------------------------------------------------------------#
# Need a crosswalk with auxiliary variables ------------------------------------
#------------------------------------------------------------------------------#
# About: This section acts as an input guard, making sure that the variables   #
# cross walk is provided by the user, and that auxiliary variables are         #
# included within the cross walk. If either guard is violated, the script      #
# returns NULL.                                                                #
#------------------------------------------------------------------------------#

  #############################################
  # Checking for the variable cross walk file #
  #############################################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0) return(invisible(NULL))

  ###############################
  # Checking for auxiliary rows #
  ###############################

  # Pulling any auxiliary rows
  aux_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "aux_variable", ]

  # Nothing to show if no auxiliary variables are used
  if(nrow(aux_rows) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Building the definitions data frame ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the parameter clean name, data source clean   #
# name and definition for each auxiliary variable row using the usser-provided #
# entries in the variable cross walk file. Rows are de-duplicated by           #
# parameter and data source so repeated entries collapse.                      #
#------------------------------------------------------------------------------#


  #######################################
  # Extracting the clean parameter name #
  #######################################
  param_name <- ifelse(
    !is.na(aux_rows$clean_name_full) &
      nchar(trimws(as.character(aux_rows$clean_name_full))) > 0,
    as.character(aux_rows$clean_name_full),
    as.character(aux_rows$variable)
  )

  #########################################
  # Extracting the clean data source name #
  #########################################
  data_source_name <- vapply(
    as.character(aux_rows$data_source),
    function(ds){
      labels <- get_aux_source_labels(ds, variables_crosswalk)
      labels$legend_group_title
    },
    character(1)
  )

  ##############################
  # Extracting the definitions #
  ##############################

  # Changing definition to character
  definition <- as.character(aux_rows$definition)

  # Replacing NA definition with blank
  definition[is.na(definition)] <- ""

  # Replacing default parameter with blank
  definition[trimws(definition) == "USER: provide a definition"] <- ""

  #########################################
  # Creating the final data table to show #
  #########################################
  defs <- data.frame(
    Parameter      = param_name,
    `Data Source`  = data_source_name,
    Definition     = definition,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  # De-duplicate by Parameter + Data Source, keeping first occurrence
  defs <- defs[!duplicated(defs[c("Parameter", "Data Source")]), , drop = FALSE]

  # Order alphabetically by Parameter for a stable, readable table
  defs <- defs[order(defs$Parameter), , drop = FALSE]

  # Guard if no definitions are provided
  if(nrow(defs) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Rendering the kable table ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the kable table from the provided parameters,    #
# data source, and definitions from above. It then styles the table to match   #
# the rest of the report.                                                      #
#------------------------------------------------------------------------------#

  ############################
  # Creating the kable table #
  ############################
  table_html <- knitr::kable(
    defs,
    format    = "html",
    col.names = c("Parameter", "Data Source", "Definition"),
    align     = c("l", "l", "l"),
    escape    = TRUE
  )

  #####################
  # Styling the table #
  #####################
  table_html <- kableExtra::kable_styling(
    table_html,
    full_width        = FALSE,
    bootstrap_options = c("bordered", "striped"),
    position          = "left"
  )

#------------------------------------------------------------------------------#
# Emitting the full accordion --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the open <details>, summary, intro text, the     #
# table, and the closing tags as one HTML block so the whole section is        #
# self-contained.                                                              #
#------------------------------------------------------------------------------#

  cat(paste0(
    '<details class="accordion">',
    '<summary><strong>Auxiliary Variable Definitions</strong></summary>',
    '<div class="accordion-body">',

    '<p>This section contains the definitions of auxiliary variables used in ',
    'the model. The definitions below describe how each variable is defined ',
    'for analytic use.</p>',

    table_html,

    '</div>',
    '</details>'
  ))

  invisible(NULL)

}
