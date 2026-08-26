#' Add a fullscreen toggle button to a Plotly figure
#'
#' Reads the bundled `plotly_fullscreen.js` file from the package
#' installation using `system.file()`, attaches it to a Plotly widget via
#' `htmlwidgets::onRender()`, and adds a custom expand button to the Plotly
#' modebar. Clicking the button toggles the plot between its normal size and
#' a full-viewport overlay.
#'
#' The JS file path is resolved at runtime using `system.file()` so this
#' function works correctly regardless of where the package is installed.
#' The old version used a hardcoded absolute path which broke on any machine
#' other than the original development machine.
#'
#' @param p A Plotly object to add the fullscreen button to.
#'
#' @return The Plotly object with the fullscreen JavaScript attached and
#'   a custom modebar button added via `plotly::config()`.
#'
#' @keywords internal
#' @noRd
build_fullscreen_button <- function(p) {

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
    "reports", "plotly_fullscreen.js",
    package = "forecastEvalReport"
  )

  ####################################
  # Stopping if JS file is not found #
  ####################################
  if(nchar(js_path) == 0L){

    # Stopping the script if an error occurs
    stop(
      "build_fullscreen_button: plotly_fullscreen.js could not be found in\n",
      "the forecastEvalReport package installation.\n\n",
      "This is an internal package error. Try re-installing the package:\n",
      "  devtools::install()",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Attaching the fullscreen JS --------------------------------------------------
#------------------------------------------------------------------------------#
# About: The JS installs the global toggleFullscreenFromGd() function once     #
# on the page. All fullscreen enter/exit logic lives in this function. It is   #
# attached via onRender() so it runs client-side after the plot is rendered.   #
#------------------------------------------------------------------------------#

  ###########################
  # Reading the JS file     #
  ###########################
  fs_js <- paste(readLines(js_path, encoding = "UTF-8"), collapse = "\n")

  ###########################
  # Attaching to the widget #
  ###########################
  p <- htmlwidgets::onRender(p, htmlwidgets::JS(fs_js))

#------------------------------------------------------------------------------#
# Adding the expand button to the Plotly modebar -------------------------------
#------------------------------------------------------------------------------#
# About: A custom button is added to the Plotly modebar using plotly::config() #
# with modeBarButtonsToAdd. The button icon is an SVG path representing an     #
# expand/fullscreen symbol. Clicking the button calls toggleFullscreenFromGd() #
# which was installed by the JS above.                                         #
#------------------------------------------------------------------------------#

  #####################################
  # SVG icon path for the expand icon #
  #####################################

  # The path string draws a standard fullscreen/expand symbol.
  fs_icon_path <- paste0(
    "M16 3h5v5h-2V6.41L14.41 11 13 9.59 21.59 1 18.59 1 16 3z ",
    "M8 21H3v-5h2v3.59L9.59 13 11 14.41 2.41 23 5.41 23 8 21z"
  )

  ####################################
  # Adding the button to the modebar #
  ####################################
  p %>%
    plotly::config(
      displayModeBar      = TRUE,
      modeBarButtonsToAdd = I(list(
        toggleFullscreen = list(
          name  = "Toggle Fullscreen",
          icon  = list(path = fs_icon_path, width = 24, height = 24),
          click = htmlwidgets::JS(
            "function(gd) { toggleFullscreenFromGd(gd); }"
          )
        )
      ))
    )

}
