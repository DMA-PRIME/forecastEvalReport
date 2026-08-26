#' Resolve marker style settings from the plot styles list
#'
#' Looks up the marker style settings (color, size) for a named trace
#' group from the plot styles list produced by `create_plot_styles()`.
#'
#' @param group Character. The trace group name to look up. Must match a
#'   named entry in `plot_styles` (e.g., `"Target Data"`,
#'   `"Current Projections"`, `"Historical Estimates"`).
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#'
#' @return A named list with `color` and `size` elements suitable for
#'   passing to `plotly::add_trace(marker = ...)`.
#'
#' @keywords internal
#' @noRd
resolve_marker_style <- function(group, plot_styles) {

  # Look up the style entry for this group
  style <- plot_styles[[group]]

  # Return the marker style components
  list(
    color = style$color,
    size  = style$marker_size
  )

}
