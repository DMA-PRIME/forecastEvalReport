#' Build an interactive checkbox legend item for the floating legend
#'
#' Constructs a single legend item consisting of a checkbox, a label,
#' and a styled color swatch. The checkbox controls the visibility of
#' the corresponding Plotly trace via JavaScript. Both `data-trace` and
#' `data-trace-name` attributes are set on the checkbox so the item
#' works with both the syncLegendCheckboxesOnRender.js handler (which
#' reads `data-trace`) and any other handlers that read `data-trace-name`.
#'
#' @param label Character or `htmltools` tag. The display label shown
#'   next to the checkbox.
#' @param trace_name Character. The Plotly trace `legendgroup` name this
#'   item controls. Must match exactly what is set as `legendgroup` on
#'   the corresponding plotly trace(s).
#' @param swatch_class Character. CSS class applied to the swatch span
#'   for color styling.
#' @param checked Logical. Whether the checkbox is checked on initial
#'   load. Default `TRUE`.
#' @param extra_attrs Named list of additional HTML attributes to add to
#'   the checkbox input element. Default `list()`.
#' @param swatch_attrs Named list of additional HTML attributes to add to
#'   the swatch span element. Default `list()`.
#'
#' @return An `htmltools` label tag representing the legend item.
#'
#' @keywords internal
#' @noRd
build_legend_item <- function(label, trace_name, swatch_class,
                              checked      = TRUE,
                              extra_attrs  = list(),
                              swatch_attrs = list()) {

#------------------------------------------------------------------------------#
# Building the checkbox input attributes ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the check-box input attributes. For example, both #
# the data trace and data trace names are set so the JS sync handler works     #
# regardless of which attribute it reads.                                      #
#------------------------------------------------------------------------------#

  #############################
  # Checkbox input attributes #
  #############################
  input_attrs <- c(

    # List of atributes
    list(
      type              = "checkbox",
      class             = "legend-checkbox",
      `data-trace`      = trace_name,
      `data-trace-name` = trace_name
    ),

    # Handling extra attributes
    if(checked) list(checked = NA) else list(),
    extra_attrs)

#------------------------------------------------------------------------------#
# Building swatch span attributes ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Each element of the legend has a swatch next to it to make it clear   #
# how the elements match to the main graph. This section sets up the swatches  #
# dynamically so the handler works regardless of which attribute it reads.     #
#------------------------------------------------------------------------------#

  ################################
  # Build swatch span attributes #
  ################################
  swatch_attrs_full <- c(
    list(class = paste("legend-swatch", swatch_class)),
    swatch_attrs
  )

#------------------------------------------------------------------------------#
# Assembling the legend label --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section assembles the label containing the check box, text, and  #
# swatch for the legend entry.                                                 #
#------------------------------------------------------------------------------#

  ######################################################
  # Assemble label containing checkbox + text + swatch #
  ######################################################
  htmltools::tags$label(
    class = "legend-item",
    do.call(htmltools::tags$input, input_attrs),
    htmltools::span(label),
    do.call(htmltools::tags$span, swatch_attrs_full)
  )

}
