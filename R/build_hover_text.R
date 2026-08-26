#' Build the hover text label for the target data trace
#'
#' Constructs the hover label shown when a user hovers over the target
#' (truth) data trace on a plot. The label is derived from the abbreviated
#' name (`clean_name_abb`) of the data source row in the variables crosswalk
#' that is referenced by the outcome row for the current geography.
#'
#' @param location Character. The normalized location name for the current
#'   geography being plotted. Used to find the matching crosswalk group.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`.
#' @param config Validated config list from `validate_report_params()`.
#'   Used as a fallback when the crosswalk lookup fails.
#'
#' @return A character string to use as the hover label for the target data
#'   trace. Falls back to `config$outcome_data_label` when the crosswalk
#'   lookup fails.
#'
#' @keywords internal
#' @noRd
build_hover_text <- function(location, variables_crosswalk, config) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure all are available   #
# and will fall back to config$outcome_data_label when inputs are invalid.     #
# This ensures the hover text is never empty even if the crosswalk is          #
# unavailable or incomplete.                                                   #
#------------------------------------------------------------------------------#

  ##############################
  # Fallback label from config #
  ##############################
  fallback <- if(!is.null(config$outcome_data_label) &&
                    !is.na(config$outcome_data_label)){

    # Using the configuration outcome data label
    config$outcome_data_label

  #########################################
  # Using the generic 'Target Data' label #
  #########################################
  }else{"Target Data"}

  #########################################
  # Guard: crosswalk must be a data frame #
  #########################################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0){

    # Returning the fallback label
    return(fallback)

  }

  ####################################
  # Guard: location must be a string #
  ####################################
  if(is.null(location) || is.na(location) || nchar(trimws(location)) == 0){

    # Returning the fallback label
    return(fallback)

  }

#------------------------------------------------------------------------------#
# Finding the outcome row for this location ------------------------------------
#------------------------------------------------------------------------------#
# About: Each location group in the crosswalk has its own outcome row. The     #
# outcome row's `data_source` column points to the variable name of the        #
# corresponding data_source row. We use that link to get the data source's     #
# clean_name_abb which becomes the hover label.                                #
#------------------------------------------------------------------------------#

  ###################################################
  # Filter to outcome rows for the current location #
  ###################################################
  outcome_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "outcome" &
      !is.na(variables_crosswalk$location) &
      variables_crosswalk$location == location, ]

  #########################################
  # Fallback: no outcome row for location #
  #########################################
  if(nrow(outcome_rows) == 0){

    # Try without location filtering (global outcome row)
    outcome_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    # If no outcome row, using fallback label
    if(nrow(outcome_rows) == 0) return(fallback)

  }

  ##########################################################
  # Extract the data_source reference from the outcome row #
  ##########################################################
  ds_variable <- outcome_rows$data_source[1]

  # If no data source, using fallback label
  if(is.na(ds_variable) || nchar(trimws(ds_variable)) == 0){return(fallback)}

#------------------------------------------------------------------------------#
# Looking up the data source clean_name_abb ------------------------------------
#------------------------------------------------------------------------------#
# About: This section Uses the data_source variable name extracted above to    #
# find the corresponding data_source-type row and pull its clean_name_abb.     #
# This is the abbreviated label shown in hover text on the plot.               #
#------------------------------------------------------------------------------#

  ###################################################
  # Filter to data_source rows matching ds_variable #
  ###################################################
  ds_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "data_source" &
      !is.na(variables_crosswalk$variable) &
      variables_crosswalk$variable == ds_variable, ]

  #######################################
  # Fallback: data source row not found #
  #######################################
  if(nrow(ds_rows) == 0) return(fallback)

  # Extract clean_name_abb
  hover_label <- ds_rows$clean_name_abb[1]

  #####################################
  # Fallback: clean_name_abb is empty #
  #####################################
  if(is.na(hover_label) || nchar(trimws(hover_label)) == 0){

    # Try clean_name_full as secondary fallback
    hover_label <- ds_rows$clean_name_full[1]

    # No hover label, using fallback label
    if(is.na(hover_label) || nchar(trimws(hover_label)) == 0){return(fallback)}

  }

  #############################
  # Returning the hover label #
  #############################
  hover_label

}
