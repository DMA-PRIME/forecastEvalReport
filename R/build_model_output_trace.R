#' Build the implementation model current projections trace for a Plotly figure
#'
#' Adds the current projections trace to a Plotly object, representing
#' the implementation model's forward-looking forecasts. The trace is
#' styled using the `"Current Projections"` entry in `plot_styles`.
#'
#' @param p A Plotly object to add the trace to.
#' @param forecast.data A data frame of current projection rows. Must
#'   contain columns `target_end_date` and `value`.
#' @param outcome Character. The outcome display label used in hover text.
#' @param value_suffix Character. Suffix appended to values in hover text.
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#'
#' @return The Plotly object with the current projections trace added.
#'
#' @keywords internal
#' @noRd
build_model_output_trace <- function(p, forecast.data, outcome,
                                     value_suffix, plot_styles) {

#------------------------------------------------------------------------------#
# Building the projections trace -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the projections trace if and only if all inputs   #
# are provided. The projections trace is the forward looking forecasts.        #
#------------------------------------------------------------------------------#

  ##################################
  # Building the projections trace #
  ##################################
  p %>%
    plotly::add_trace(
      data             = forecast.data,
      x                = ~target_end_date,
      y                = ~value,
      type             = "scatter",
      mode             = "lines+markers",
      name             = "Current Projections",
      line             = resolve_line_style("Current Projections", plot_styles),
      marker           = resolve_marker_style("Current Projections", plot_styles),
      legendgrouptitle = list(text = "<b>Implementation Model</b>"),
      showlegend       = TRUE,
      legendrank       = 1,
      legendgroup      = "Implementation Model",
      hovertemplate    = paste0(
        "<b>Current Projections</b><br>",
        "Outcome: ", outcome, "<br>",
        "Week: %{x|%Y-%m-%d}<br>",
        "Value: %{y:.3f}", value_suffix, "<br>",
        "<extra></extra>"
      )
    )

}
