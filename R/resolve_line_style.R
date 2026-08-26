#' Resolve line style settings from the plot styles list
#'
#' Looks up the line style settings (color, dash, width) for a named
#' trace group from the plot styles list produced by
#' `create_plot_styles()`. Optionally indexes into a split color vector
#' when the group's color is a named vector (e.g., for multi-horizon
#' evaluation model traces).
#'
#' @param group Character. The trace group name to look up. Must match a
#'   named entry in `plot_styles` (e.g., `"Target Data"`,
#'   `"Current Projections"`, `"Historical Estimates"`).
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#' @param split Optional. A named index into the group's color vector
#'   when the color is split by horizon or similar dimension.
#'
#' @return A named list with `color`, `dash`, and `width` elements
#'   suitable for passing to `plotly::add_trace(line = ...)`.
#'
#' @keywords internal
#' @noRd
resolve_line_style <- function(group, plot_styles, split = NULL) {

  # Look up the style entry for this group
  style <- plot_styles[[group]]

  # Return the line style components
  list(
    color = if(!is.null(split) && !is.null(style$color))
      style$color[[split]] else style$color,
    dash  = style$dash,
    width = style$width
  )

}
