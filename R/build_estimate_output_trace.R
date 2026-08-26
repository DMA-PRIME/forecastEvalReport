#' Build the historical estimates trace for a Plotly figure
#'
#' Adds the historical estimates trace to a Plotly object, representing
#' retrospective model estimates generated prior to the reference date.
#' The trace is styled using the `"Historical Estimates"` entry in
#' `plot_styles`.
#'
#' @param p A Plotly object to add the trace to.
#' @param estimate.data A data frame of historical estimate rows. Must
#'   contain columns `target_end_date` and `value`.
#' @param outcome Character. The outcome display label used in hover text.
#' @param value_suffix Character. Suffix appended to values in hover text.
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#'
#' @return The Plotly object with the historical estimates trace added.
#'
#' @keywords internal
#' @noRd
build_estimate_output_trace <- function(p, estimate.data, outcome,
                                        value_suffix, plot_styles){

#------------------------------------------------------------------------------#
# Building the estimates trace -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the estimates trace that shows the historical     #
# forecasts when available. This general only happens if the training data     #
# was well in the past, and forecasts were being made for some time in the     #
# future.                                                                      #
#------------------------------------------------------------------------------#

  p %>%
    plotly::add_trace(
      data          = estimate.data,
      x             = ~target_end_date,
      y             = ~value,
      type          = "scatter",
      mode          = "lines+markers",
      name          = "Historical Estimates",
      line          = resolve_line_style("Historical Estimates", plot_styles),
      marker        = resolve_marker_style("Historical Estimates", plot_styles),
      showlegend    = TRUE,
      legendrank    = 2,
      legendgroup   = "Implementation Model",
      hovertemplate = paste0(
        "<b>Historical Estimates</b><br>",
        "Outcome: ", outcome, "<br>",
        "Week: %{x|%Y-%m-%d}<br>",
        "Value: %{y:.3f}", value_suffix, "<br>",
        "<extra></extra>"
      )
    )

}
