#' Apply percent scaling to a data column using the variables crosswalk
#'
#' Determines whether a given variable's values should be multiplied by 100
#' (percent conversion) by looking up the `convert_percent` flag in the
#' variables crosswalk. If `convert_percent` is TRUE for the variable, all
#' values in the specified column are multiplied by 100 and rounded to three
#' decimal places. A suffix string is also returned for use in hover text
#' and axis labels.
#'
#' @param data A data frame containing the column to be scaled.
#' @param col Character. The name of the column in `data` to scale.
#' @param variable Character. The variable name to look up in the crosswalk.
#'   This should match the `variable` column of an `aux_variable`-type row or
#'   an `outcome`-type row.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`, or `NULL` to skip scaling entirely.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{The input data frame with the specified column scaled (if
#'     applicable) or unchanged.}
#'   \item{suffix}{A character string to append to value labels in hover
#'     text and axis titles: `"%"` if percent conversion was applied,
#'     `""` otherwise.}
#' }
#'
#' @keywords internal
#' @noRd
apply_percent_scaling <- function(data, col, variable,
                                  variables_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure that all are        #
# formatted correctly to apply the percent scaling. It returns data unchanged  #
# when inputs are invalid rather than stopping the render. The suffix is       #
# always "" in fallback cases since no scaling was applied.                    #
#------------------------------------------------------------------------------#

  ####################################
  # Guard: data must be a data frame #
  ####################################
  if(!is.data.frame(data) || nrow(data) == 0){

    # Returning unchanged data
    return(list(data = data, suffix = ""))

  }

  #################################
  # Guard: col must exist in data #
  #################################
  if(!col %in% names(data)){

    # Returning unchanged data
    return(list(data = data, suffix = ""))

  }

  ###########################
  # Guard: crosswalk absent #
  ###########################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0){

    # Returning unchanged data
    return(list(data = data, suffix = ""))
  }

  ####################################
  # Guard: variable must be a string #
  ####################################
  if(is.null(variable) || is.na(variable) || nchar(trimws(variable)) == 0){

    # Returning unchanged data
    return(list(data = data, suffix = ""))

  }

#------------------------------------------------------------------------------#
# Looking up the convert_percent flag ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crosswalk to aux_variable and outcome rows   #
# matching the variable name. The convert_percent column was normalized to     #
# logical TRUE/FALSE/NA by validate_variables_crosswalk(). Only aux_variable   #
# and outcome rows have this flag; data_source rows always have NA.            #
#------------------------------------------------------------------------------#

  ##############################################
  # Filtering to outcome and aux variable rows #
  ##############################################
  aux_outcome_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type %in% c("aux_variable", "outcome") &
      !is.na(variables_crosswalk$variable) &
      variables_crosswalk$variable == variable, ]

  ##############################
  # No match: return unchanged #
  ##############################
  if(nrow(aux_outcome_rows) == 0){

    # Returning unchanged data
    return(list(data = data, suffix = ""))

  }

  ################################################################
  # Extract the convert_percent flag from the first matching row #
  ################################################################
  convert_flag <- aux_outcome_rows$convert_percent[1]

  # NA or FALSE: Return Unchanged
  if(is.na(convert_flag) || !isTRUE(convert_flag)){

    # Returning unchanged data
    return(list(data = data, suffix = ""))

  }

#------------------------------------------------------------------------------#
# Applying percent scaling -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section multiplies the target column by 100 and rounds to three  #
# decimal places. The suffix "%" is returned for use in axis titles and hover  #
# text.                                                                        #
#------------------------------------------------------------------------------#

  ####################################
  # Scale: multiply by 100 and round #
  ####################################
  data[[col]] <- round(as.numeric(data[[col]]) * 100, 3)

  ###############################
  # Return scaled data + suffix #
  ###############################
  list(data = data, suffix = "%")

}
