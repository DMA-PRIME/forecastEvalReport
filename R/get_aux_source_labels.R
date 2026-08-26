#' Look up legend group title and hover label for an auxiliary data source
#'
#' Retrieves the report-facing legend group title and hover label for a
#' given auxiliary data source by looking it up in the variables crosswalk.
#' The crosswalk's `data_source`-type rows carry `clean_name_full` (used
#' as the legend group title) and `clean_name_abb` (used as the hover
#' label). This replaces the old CSV-based `get_parameter_source_labels()`
#' workflow, which required interactive prompts when a data source was
#' missing.
#'
#' If the data source is not found in the crosswalk, the raw data source
#' string is returned for both fields with a warning so rendering never
#' stops unexpectedly.
#'
#' @param data_source Character. The data source identifier to look up.
#'   This should match the `variable` column of a `data_source`-type row
#'   in the crosswalk.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{legend_group_title}{The full display name for the legend group
#'     header (`clean_name_full` from the crosswalk, or the raw data source
#'     string as a fallback).}
#'   \item{hover_label}{The abbreviated display name for hover text
#'     (`clean_name_abb` from the crosswalk, or the raw data source string
#'     as a fallback).}
#' }
#'
#' @keywords internal
#' @noRd
get_aux_source_labels <- function(data_source, variables_crosswalk) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the inputs of the function. If the crosswalk is   #
# absent or the data source string is empty the function falls back to         #
# returning the raw data source string for both fields. This prevents hard     #
# stops during rendering when the crosswalk is incomplete.                     #
#------------------------------------------------------------------------------#

  #########################################
  # Guard: crosswalk must be a data frame #
  #########################################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0){

    # Warning to return to users
    warning(
      "get_aux_source_labels: variables_crosswalk is NULL or empty. ",
      "Using raw data source string as fallback for '", data_source, "'.",
      call. = FALSE
    )

    # Returning empty list
    return(list(
      legend_group_title = data_source,
      hover_label        = data_source
    ))

  }

  #######################################
  # Guard: data_source must be a string #
  #######################################
  if(is.null(data_source) || is.na(data_source) ||
     nchar(trimws(data_source)) == 0){

    # Warning to return to users
    warning(
      "get_aux_source_labels: data_source is NULL, NA, or empty. ",
      "Returning empty strings.",
      call. = FALSE
    )

    # Returning empty list
    return(list(
      legend_group_title = "",
      hover_label        = ""
    ))

  }

#------------------------------------------------------------------------------#
# Looking up the data source in the crosswalk ----------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crosswalk to rows where                      #
# variable_type == "data_source" and the `variable` column matches the         #
# requested data_source string. The match is exact (no case folding) since the #
# data source names in the crosswalk and the aux_variable rows must be         #
# consistent to satisfy the foreign-key constraint enforced by                 #
# validate_variables_crosswalk().                                              #
#------------------------------------------------------------------------------#

  #############################################
  # Filtering to data_source rows for this ds #
  #############################################
  ds_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "data_source" &
      !is.na(variables_crosswalk$variable) &
      variables_crosswalk$variable == data_source, ]

#------------------------------------------------------------------------------#
# Handling no match ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles if there is no matching data source row found.   #
# If no matching data_source row is found, a warning is issued and the         #
# raw data source string is returned for both fields. This is a graceful       #
# fallback that keeps the report rendering rather than stopping it.            #
#------------------------------------------------------------------------------#

  ##########################################
  # Fallback: data source not in crosswalk #
  ##########################################
  if(nrow(ds_rows) == 0){

    # Warning to show to user
    warning(
      "get_aux_source_labels: No data_source row found in the crosswalk for '",
      data_source, "'. Using raw string as fallback. ",
      "Add a data_source row with variable = '", data_source,
      "' to the crosswalk to resolve this.",
      call. = FALSE
    )

    # Returning an empty list
    return(list(
      legend_group_title = data_source,
      hover_label        = data_source
    ))

  }

#------------------------------------------------------------------------------#
# Extracting clean_name_full and clean_name_abb --------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the first matching row's clean_name_full and       #
# clean_name_abb. Multiple rows for the same data source (across different     #
# location groups) should have the same labels, so the first is sufficient.    #
#------------------------------------------------------------------------------#

  #######################################
  # Legend group title: clean_name_full #
  #######################################
  legend_title <- ds_rows$clean_name_full[1]

  # Fall back to raw string if clean_name_full is missing
  if(is.na(legend_title) || nchar(trimws(legend_title)) == 0){legend_title <- data_source}

  ###############################
  # Hover label: clean_name_abb #
  ###############################
  hover_label <- ds_rows$clean_name_abb[1]

  # Fall back to clean_name_full if clean_name_abb is missing
  if(is.na(hover_label) || nchar(trimws(hover_label)) == 0){hover_label <- legend_title}

  #################################
  # Returning the resolved labels #
  #################################
  list(
    legend_group_title = legend_title,
    hover_label        = hover_label
  )

}
