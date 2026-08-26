#' Render the "Model Development and Inputs" section heading and intro text
#'
#' Produces the section heading and introductory paragraph that precede the
#' model development and inputs table. Kept as its own section function so the
#' heading text stays modular and out of the report template, matching the
#' pattern used by `section_model_intro()` and `section_overview_text()`.
#'
#' The companion table is rendered separately by `section_model_table()`.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::HTML()].
#'
#' @keywords internal
#' @noRd
section_model_table_intro <- function() {

  #########################################
  # Creating the model table introduction #
  #########################################
  html <- paste0(
    "<h2>Model Development and Inputs</h2>",
    "<p>This section contains information related to model variables and the ",
    "training, validation, and testing periods (if applicable) used during ",
    "model development and evaluation.</p>"
  )

  ##############################
  # Rendering the introduction #
  ##############################
  htmltools::HTML(html)

}
