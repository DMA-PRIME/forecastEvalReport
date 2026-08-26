#' Add a right y-axis layout to a Plotly figure
#'
#' Appends a secondary (right-side) y-axis to a Plotly figure for use
#' with auxiliary variable traces that are plotted on a different scale
#' than the primary outcome. The axis title and the list of percent-based
#' outcomes are read from `plot_styles$right_axis` (see `create_plot_styles()`),
#' falling back to package defaults when not supplied. Call this after
#' `build_yaxis_layout()`.
#'
#' @param p A Plotly object to add the right y-axis to.
#' @param outcome Character. The outcome display label used to determine
#'   the right axis title.
#' @param autorange Logical. Whether to use plotly autorange for the right
#'   axis. Default `FALSE` so the rangeslider JS controls the range.
#' @param plot_styles A plot styles list from `create_plot_styles()`. Its
#'   `right_axis` element supplies `title_default`, `title_percent`,
#'   `percent_outcomes`, and the title font/standoff. Default `NULL` (the
#'   built-in defaults below are used).
#'
#' @return The Plotly object with the right y-axis layout applied.
#'
#' @keywords internal
#' @noRd
add_right_yaxis <- function(p, outcome, autorange = FALSE,
                            plot_styles = NULL) {

#------------------------------------------------------------------------------#
# Resolve the right-axis labels and styling ------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads the right-axis title text, the list of             #
# percent-based outcomes, and the title font/standoff from                     #
# plot_styles$right_axis when present, falling back to the package             #
# defaults so existing callers keep working unchanged.                         #
#------------------------------------------------------------------------------#

  # Right-axis style block (absent in older style objects -> NULL)
  ra <- if(is.list(plot_styles)) plot_styles$right_axis else NULL

  # Title used when the left axis is already a percentage
  title_percent <- if(!is.null(ra$title_percent)){ra$title_percent

  # Default title
  }else{"Auxiliary Variables"}

  # User supplied title
  title_default <- if(!is.null(ra$title_default)){ra$title_default

  # Default title
  }else{"Weekly Tests"}

  # Outcomes whose left axis is already a percentage
  percent_outcomes <- if(!is.null(ra$percent_outcomes)){ra$percent_outcomes

  # Default outcome list
  }else{
    c(
      "Weekly % Flu-Attributable ED-Visits",
      "Weekly % RSV-Attributable ED-Visits",
      "Weekly % COVID-19-Attributable ED-Visits"
    )
  }

  # Title font size (default 16)
  title_size <- if(!is.null(ra$title_font_size)) ra$title_font_size else 16

  # Title font color (default near-black)
  title_color <- if(!is.null(ra$title_font_color)){ra$title_font_color

  # Default font color
  }else{"#111"}

  # Title standoff from the axis (default 25)
  title_standoff <- if(!is.null(ra$title_standoff)) ra$title_standoff else 25

#------------------------------------------------------------------------------#
# Determining the right axis title ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section picks the right-axis title. Percentage-based             #
# outcomes (whose left axis is already a percent) use the percent              #
# title; all other outcomes use the default title.                             #
#------------------------------------------------------------------------------#

  ###################################
  # Percent-based vs other outcomes #
  ###################################

  # Percent outcomes get the percent title; everything else the default
  right_axis_title <- if(!is.null(outcome) && outcome %in% percent_outcomes){

    # Title provided by user
    title_percent

  # Default title
  }else{title_default}

#------------------------------------------------------------------------------#
# Applying the right y-axis layout ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section overlays yaxis2 on the same plot area, placed on         #
# the right. showgrid = FALSE prevents double grid lines and zeroline =        #
# FALSE removes the overlapping zero line, while tickmode = "sync"             #
# keeps the right ticks aligned with the left axis.                            #
#------------------------------------------------------------------------------#

  #############################################
  # Applying the secondary axis to the figure #
  #############################################
  p %>%
    plotly::layout(
      yaxis2 = list(

        # Axis title (text + styling from plot_styles or the defaults above)
        title = list(
          text     = right_axis_title,
          standoff = title_standoff,
          font     = list(size = title_size, color = title_color)
        ),

        # Overlay on the same plot area as the left axis
        overlaying = "y",
        side       = "right",

        # Range controlled by rangeslider JS after render
        autorange  = autorange,
        automargin = TRUE,
        rangemode  = "tozero",

        # Keep right axis ticks aligned with left axis
        tickmode   = "sync",

        # Suppress double grid lines and zero line overlap
        showgrid   = FALSE,
        zeroline   = FALSE

      )
    )

}
