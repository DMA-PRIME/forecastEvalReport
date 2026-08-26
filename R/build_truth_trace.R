#' Build the target data trace for a Plotly figure
#'
#' Adds the observed ("target") data trace to a Plotly object. The trace
#' is styled using the `"Target Data"` entry in `plot_styles` via
#' `resolve_line_style()` and `resolve_marker_style()`.
#'
#' @param p A Plotly object to add the trace to.
#' @param truth_data A data frame of observed outcome data for a single
#'   location. Must contain columns `date` and `value`.
#' @param outcome Character. The outcome display label shown in hover text.
#' @param hover_text Character. The data source abbreviation label for
#'   hover text, produced by `build_hover_text()`.
#' @param value_suffix Character. A suffix appended to the value in hover
#'   text (e.g., `"%"` for percent outcomes, `""` otherwise).
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#'
#' @return The Plotly object with the target data trace added.
#'
#' @keywords internal
#' @noRd
build_truth_trace <- function(p, truth_data, outcome,
                              hover_text, value_suffix, plot_styles){

#------------------------------------------------------------------------------#
# Creating the truth trace -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the truth trace, or the line that shows the      #
# truth data in the figure. The below code creates the line, applies all user  #
# styles and creates the hover template.                                       #
#------------------------------------------------------------------------------#

  ############################
  # Building the truth trace #
  ############################
  p %>%
    plotly::add_trace(
      data             = truth_data,
      x                = ~date,
      y                = ~value,
      type             = "scatter",
      mode             = "lines+markers",
      name             = outcome,
      line             = resolve_line_style("Target Data", plot_styles),
      marker           = resolve_marker_style("Target Data", plot_styles),
      showlegend       = TRUE,
      legendgroup      = "Target Data",
      legendgrouptitle = list(text = "<b>Target Data</b>"),
      visible          = TRUE,
      hovertemplate    = paste0(
        "<b>Target Data</b><br>",
        "Outcome: ", outcome, "<br>",
        "Source: ", hover_text, "<br>",
        "Week: %{x|%Y-%m-%d}<br>",
        "Value: %{y:.3f}", value_suffix, "<br>",
        "<extra></extra>"
      )
    )

}
