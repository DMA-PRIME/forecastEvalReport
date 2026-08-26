#' Build prediction interval bound traces for a Plotly figure
#'
#' Adds filled prediction interval ribbon traces to a Plotly object for
#' each PI level that has non-NULL bounds data. Each PI level is rendered
#' as a pair of lower and upper bound traces with a filled area between
#' them. Levels with NULL bounds (no data for that quantile pair) are
#' skipped automatically.
#'
#' @param p A Plotly object to add the PI traces to.
#' @param bounds A named list of PI bounds produced by
#'   `prepare_pi_bounds()`. Each entry is either a named list with
#'   `lower` and `upper` data frames, or `NULL`.
#' @param pi_styles A named list of PI style settings from
#'   `PLOT_STYLES$pi_styles`. Each entry must have `fill`, `hover_color`,
#'   and `label`.
#' @param outcome Character. The outcome display label for hover text.
#' @param value_suffix Character. Suffix appended to values in hover text.
#'
#' @return The Plotly object with PI bound traces added.
#'
#' @keywords internal
#' @noRd
build_pi_bounds_trace <- function(p, bounds, pi_styles, outcome,
                                  value_suffix) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the figure unchanged when there is               #
# nothing to draw -- no PI bounds, or no PI style definitions.                 #
#------------------------------------------------------------------------------#

  # Returning the figure as-is when there are no bounds to draw
  if(is.null(bounds) || length(bounds) == 0) return(p)

  # Returning the figure as-is when there are no PI styles defined
  if(is.null(pi_styles) || length(pi_styles) == 0) return(p)

#------------------------------------------------------------------------------#
# Looping through each PI level ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adds two traces for each PI level that has both          #
# a style definition and non-NULL bounds: an invisible upper bound and         #
# a lower bound filled up to it, creating the ribbon. Both traces share        #
# a legend group so they toggle together.                                      #
#------------------------------------------------------------------------------#

  ####################################################################
  # Names of the PI levels to draw, taken from the style definitions #
  ####################################################################
  pi_names <- names(pi_styles)

  ###################################
  # Drawing one ribbon per PI level #
  ###################################
  for(pi_name in pi_names){

    # Skipping this level when prepare_pi_bounds() returned no data for it
    if(is.null(bounds[[pi_name]])) next

    # Lower-bound for indexed PI level
    lower_data <- bounds[[pi_name]]$lower

    # Upper-bound for indexed PI level
    upper_data <- bounds[[pi_name]]$upper

    # Skipping when either bound is missing entirely
    if(is.null(lower_data) || is.null(upper_data)) next

    # Skipping when either bound has no rows to plot
    if(nrow(lower_data) == 0 || nrow(upper_data) == 0) next

    #########################################
    # Setting up the style for the PI bound #
    #########################################

    # PI Fill
    pi_fill <- pi_styles[[pi_name]]$fill

    # Hover color
    pi_hover_color <- pi_styles[[pi_name]]$hover_color

    # Label to show
    pi_label <- pi_styles[[pi_name]]$label

    # Shared legend group so the upper/lower pair toggles as one
    legend_group <- paste0("PI-", pi_name)

    #####################
    # Upper bound trace #
    #####################

    # Invisible upper edge of the ribbon; the lower trace fills up to it
    p <- p %>%
      plotly::add_trace(
        data          = upper_data,
        x             = ~target_end_date,
        y             = ~value,
        type          = "scatter",
        mode          = "lines",
        name          = pi_label,
        legendgroup   = legend_group,

        # Hidden from the legend; the lower trace carries the legend entry
        showlegend = FALSE,

        # Transparent line so only the fill (from the lower trace) shows
        line = list(color = "transparent", width = 0),

        # Hover background uses THIS level's ribbon fill (pi_fill)
        hoverlabel    = list(bgcolor = pi_fill,
                             bordercolor = pi_hover_color),

        # Hover template
        hovertemplate = paste0(
          "<b>", pi_label, " Upper</b><br>",
          "Outcome: ", outcome, "<br>",
          "Week: %{x|%Y-%m-%d}<br>",
          "Value: %{y:.3f}", value_suffix, "<br>",
          "<extra></extra>"
        )
      )

    #######################################
    # Lower bound trace (filled to upper) #
    #######################################

    # Visible ribbon: fills from the lower edge up to the upper trace
    p <- p %>%
      plotly::add_trace(
        data          = lower_data,
        x             = ~target_end_date,
        y             = ~value,
        type          = "scatter",
        mode          = "lines",
        name          = pi_label,
        legendgroup   = legend_group,

        # Carries the single legend entry for the upper/lower pair
        showlegend    = TRUE,

        # Fill the area up to the previous (upper) trace
        fill = "tonexty",
        fillcolor = pi_fill,

        # Transparent line so only the fill shows
        line = list(color = "transparent", width = 0),

        # Same hover background as this level's upper trace (the ribbon fill)
        hoverlabel = list(bgcolor = pi_fill,
                          bordercolor = pi_hover_color),

        # Hover template
        hovertemplate = paste0(
          "<b>", pi_label, " Lower</b><br>",
          "Outcome: ", outcome, "<br>",
          "Week: %{x|%Y-%m-%d}<br>",
          "Value: %{y:.3f}", value_suffix, "<br>",
          "<extra></extra>"
        )
      )

  }

  # Returning the figure with all PI ribbons added
  p

}
