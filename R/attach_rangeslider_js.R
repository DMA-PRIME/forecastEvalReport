#' Attach custom JavaScript for enhanced Plotly range slider behavior
#'
#' Reads the bundled `plotly_rangeslider.js` file from the package
#' installation using `system.file()` and attaches it to a Plotly widget
#' via `htmlwidgets::onRender()`. The JavaScript enables dynamic y-axis
#' rescaling as the user pans or zooms the range slider, and excludes
#' phase ribbon traces from the y-axis max calculation so they never
#' inflate the visible axis range.
#'
#' The JS file path is resolved at runtime using `system.file()` so this
#' function works correctly regardless of where the package is installed.
#' The old version used a hardcoded absolute path which broke on any
#' machine other than the original development machine.
#'
#' @param p A Plotly object to attach the range slider JS to.
#'
#' @return The Plotly object with the range slider JavaScript attached
#'   via `htmlwidgets::onRender()`.
#'
#' @keywords internal
#' @noRd
attach_rangeslider_js <- function(p) {

#------------------------------------------------------------------------------#
# Locating the JS file ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: system.file() resolves the path to the installed package's            #
# inst/reports/ directory. This works on any machine regardless of where the   #
# package is installed, replacing the old hardcoded absolute path.             #
#------------------------------------------------------------------------------#

  #######################################
  # Finding the JS file via system.file #
  #######################################
  js_path <- system.file(
    "reports", "plotly_rangeslider.js",
    package = "forecastEvalReport"
  )

  ####################################
  # Stopping if JS file is not found #
  ####################################
  if(nchar(js_path) == 0L){

    # Stopping the script if an error occurs
    stop(
      "attach_rangeslider_js: plotly_rangeslider.js could not be found in\n",
      "the forecastEvalReport package installation.\n\n",
      "This is an internal package error. Try re-installing the package:\n",
      "  devtools::install()",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Reading and attaching the JS -------------------------------------------------
#------------------------------------------------------------------------------#
# About: The JS file is read as a single character string and injected into    #
# the Plotly widget via htmlwidgets::onRender(). The JS runs client-side in    #
# the browser after the plot is rendered, binding event listeners for          #
# relayout and restyle events to keep the y-axis and range slider in sync.     #
#------------------------------------------------------------------------------#

  #######################
  # Reading the JS file #
  #######################
  js <- paste(readLines(js_path, encoding = "UTF-8"), collapse = "\n")

  ###########################
  # Attaching to the widget #
  ###########################
  htmlwidgets::onRender(p, htmlwidgets::JS(js))

}
