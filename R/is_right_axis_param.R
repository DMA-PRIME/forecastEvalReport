#' Determine whether an auxiliary variable should be plotted on the right y-axis
#'
#' Looks up the `on_right_axis` flag for a given auxiliary variable from the
#' variables crosswalk. The crosswalk's `aux_variable`-type rows carry an
#' `on_right_axis` logical column (TRUE/FALSE/NA) that was filled in by the
#' user when completing the crosswalk.
#'
#' This replaces the old `.is_right_axis_param()` CSV-based workflow, which
#' required interactive readline() prompts when a parameter was not found.
#' No prompting is needed here — the crosswalk is the single source of truth.
#'
#' When `on_right_axis` is NA or the variable is not found in the crosswalk,
#' the function defaults to FALSE (left axis) with a warning rather than
#' stopping the render.
#'
#' @param variable Character. The variable name to look up. This should match
#'   the `variable` column of an `aux_variable`-type row in the crosswalk.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`.
#'
#' @return Logical. `TRUE` if the variable should be plotted on the right
#'   y-axis, `FALSE` otherwise (including all fallback cases).
#'
#' @keywords internal
#' @noRd
is_right_axis_param <- function(variable, variables_crosswalk) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns FALSE (left axis) when inputs are invalid rather #
# than stopping the render. A warning is issued so the user knows the fallback #
# was used and can correct the crosswalk if needed.                            #
#------------------------------------------------------------------------------#

  #########################################
  # Guard: crosswalk must be a data frame #
  #########################################
  if(is.null(variables_crosswalk) || !is.data.frame(variables_crosswalk) ||
     nrow(variables_crosswalk) == 0){

    # Warning to show to user
    warning(
      "is_right_axis_param: variables_crosswalk is NULL or empty. ",
      "Defaulting to left axis for '", variable, "'.",
      call. = FALSE
    )

    # Returning a FALSE response
    return(FALSE)

  }

  ####################################
  # Guard: variable must be a string #
  ####################################
  if(is.null(variable) || is.na(variable) || nchar(trimws(variable)) == 0){

    # Returning a FALSE response
    return(FALSE)

  }

#------------------------------------------------------------------------------#
# Looking up the variable in the crosswalk -------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the crosswalk to aux_variable rows matching the  #
# variable name. The on_right_axis column was normalized to logical            #
# TRUE/FALSE/NA by validate_variables_crosswalk() so no string conversion is   #
# needed here.                                                                 #
#------------------------------------------------------------------------------#

  ####################################################
  # Filtering to aux_variable rows for this variable #
  ####################################################
  aux_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "aux_variable" &
      !is.na(variables_crosswalk$variable) &
      variables_crosswalk$variable == variable, ]

#------------------------------------------------------------------------------#
# Handling no match ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section handles when there is no match in the variable cross.    #
# If no matching aux_variable row is found, FALSE is returned with a           #
# warning. This is a graceful fallback since an unknown variable should not    #
# stop the render.                                                             #
#------------------------------------------------------------------------------#

  #######################################
  # Fallback: variable not in crosswalk #
  #######################################
  if(nrow(aux_rows) == 0){

    # Warning to show to user
    warning(
      "is_right_axis_param: No aux_variable row found in the crosswalk for '",
      variable, "'. Defaulting to left axis. ",
      "Add an aux_variable row with variable = '", variable,
      "' to the crosswalk to resolve this.",
      call. = FALSE
    )

    # Returning a FALSE response
    return(FALSE)

  }

#------------------------------------------------------------------------------#
# Returning the on_right_axis value --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section takes the first matching row's on_right_axis value.      #
# Multiple rows for the same variable (across different location groups)       #
# should have the same on_right_axis setting so the first is sufficient. If    #
# the value is NA (user left it blank), defaults to FALSE with a warning.      #
#------------------------------------------------------------------------------#

  #####################################
  # Extracting the on_right_axis flag #
  #####################################
  flag <- aux_rows$on_right_axis[1]

  # Handle NA (user did not fill in the field)
  if(is.na(flag)){

    # Warning to show to user
    warning(
      "is_right_axis_param: `on_right_axis` is NA for variable '", variable,
      "'. Defaulting to left axis. ",
      "Set on_right_axis = TRUE or FALSE in the crosswalk to resolve this.",
      call. = FALSE
    )

    # Returning a FALSE response
    return(FALSE)

  }

  ###############################
  # Returning the resolved flag #
  ###############################
  isTRUE(flag)

}
