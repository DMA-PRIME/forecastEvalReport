#' Render the testing-period transition text
#'
#' Produces the short transitional paragraph that bridges the metric overview
#' and the figures and tables summarizing testing-period performance. When no
#' testing data is present, the function renders nothing so the paragraph drops
#' out of the report along with the rest of the testing-period block.
#'
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Uses
#'   `testing_data` as the presence gate.
#'
#' @return Rendered HTML via [htmltools::HTML()], or `invisible(NULL)` when no
#'   testing data is available.
#'
#' @keywords internal
#' @noRd
section_testing_transition <- function(eval_meta) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to make sure that testing data is available.      #
# Given that the same presence signal used across every testing-period section #
# the transition text appears and disappears as a unit with the rest of the    #
# block.                                                                       #
#------------------------------------------------------------------------------#

  ###################################
  # Presence of usable testing data #
  ###################################
  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  ###############################################
  # Render nothing when no testing data present #
  ###############################################
  if(!has_testing) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Building the transition HTML -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds a single paragraph orienting the reader to the    #
# figures and tables that follow.                                              #
#------------------------------------------------------------------------------#

  ##################################
  # Creating the transitional text #
  ##################################
  htmltools::HTML('
  <!-- Transition paragraph: bridges the metric overview and the figures/tables below -->
  <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1.25rem 0;">
    The figures and tables below summarize model performance across the testing period,
    providing both a visual overview of forecast output alongside observed values and
    a tabular summary of key performance metrics.
  </p>
  ')

}
