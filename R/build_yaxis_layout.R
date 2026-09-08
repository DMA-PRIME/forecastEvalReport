#' Build the primary y-axis layout for a Plotly forecast figure
#'
#' Defines and applies the primary (left) y-axis layout for a Plotly
#' figure. The axis title is dynamically constructed from the outcome,
#' disease, and geography labels. For facility-level spatial scales the
#' geography is used as-is (already normalized); for all other scales it
#' is title-cased for display.
#'
#' The geography argument is expected to be already normalized (i.e., the
#' human-readable display name) since `section_forecast_plots()` works
#' with normalized location names throughout the plot loop.
#'
#' @param p A Plotly object to apply the y-axis layout to.
#' @param outcome Character. The outcome display label for the axis title.
#' @param disease Character. The disease display label for the axis title.
#' @param geography Character. The normalized location name for the axis
#'   title.
#' @param spatial.scale Character. The spatial scale of the data (e.g.,
#'   `"state"`, `"national"`, `"facility"`). Facility-level geographies
#'   are displayed as-is; all others are title-cased.
#' @param axis_font_size Optional numeric axis-title font size. When `NULL`,
#'   the standard 16-point title size is used. This value also contributes to
#'   the dynamic figure-height calculation for long or large axis labels.
#' @param plot_height Optional numeric base plot height in pixels. When the
#'   axis label requires extra vertical space, the final figure and its HTML
#'   widget are increased together so the plotting panel is not clipped.
#'
#' @return The Plotly object with the y-axis layout applied.
#'
#' @keywords internal
#' @noRd
build_yaxis_layout <- function(p, outcome, disease, geography,
                               spatial.scale, axis_font_size = NULL,
                               plot_height = NULL) {

#------------------------------------------------------------------------------#
# Building the location label for the y-axis title ----------------------------
#------------------------------------------------------------------------------#
# About: Geography is already normalized (human-readable) coming in from       #
# section_forecast_plots(). For facility-level scales the name is used         #
# as-is since facility names are already properly formatted. For all other     #
# scales the name is title-cased using base R tools::toTitleCase() to          #
# ensure consistent capitalisation in the axis label. stringr is not used      #
# here to avoid an unnecessary dependency.                                     #
#------------------------------------------------------------------------------#

  #####################################
  # Location label for the axis title #
  #####################################
  location_label <- if(!is.null(spatial.scale) &&
                          !is.na(spatial.scale) &&
                          tolower(trimws(spatial.scale)) == "facility"){

    # Facility names are already correctly formatted -- use as-is
    geography

  ###########################################
  # Title case for non-facility geographies #
  ###########################################
  }else{tools::toTitleCase(tolower(geography))}

#------------------------------------------------------------------------------#
# Building the axis title and dynamic figure height ----------------------------
#------------------------------------------------------------------------------#
# About: Plotly rotates the y-axis title, so its rendered length becomes a      #
# vertical space requirement. The estimated title length is compared with the #
# usable figure height. When more room is needed, the figure grows gradually   #
# while the top and bottom margins remain compact. This makes the gridlines    #
# and phase colors grow without adding unnecessary blank space.                #
#------------------------------------------------------------------------------#

  ##########################################
  # Complete text displayed on the y-axis  #
  ##########################################
  axis_title <- paste0(
    outcome,
    " (",
    disease,
    ", ",
    location_label,
    ")"
  )

  ###########################################
  # Resolving the title font used by Plotly #
  ###########################################
  resolved_font_size <- if(
    !is.null(axis_font_size) &&
    is.numeric(axis_font_size) &&
    length(axis_font_size) == 1L &&
    is.finite(axis_font_size) &&
    axis_font_size > 0
  ){

    # User-selected title size
    axis_font_size

  }else{

    # Standard title size
    16

  }

  ###########################################
  # Resolving the base height of the figure #
  ###########################################
  resolved_plot_height <- if(
    !is.null(plot_height) &&
    is.numeric(plot_height) &&
    length(plot_height) == 1L &&
    is.finite(plot_height) &&
    plot_height > 0
  ){

    # Height selected in create_plot_styles()
    plot_height

  }else{

    # Current standard plot height
    800

  }

  ####################################################
  # Reserving space above and below the plotting area #
  ####################################################
  # The top margin protects the modebar and the bottom margin protects the
  # x-axis labels and range slider. Slightly larger fonts receive a matching
  # allowance so neither edge is cropped.
  font_size_extra <- max(0, resolved_font_size - 16)
  top_margin      <- 55 + ceiling(font_size_extra)
  bottom_margin   <- 95 + ceiling(font_size_extra * 2)

  #################################################
  # Estimating the rotated title's rendered length #
  #################################################
  # A typical Plotly sans-serif character is approximately 0.52 em wide. The
  # small buffer keeps the first and final characters away from the plot edges
  # after the title is rotated without overestimating short labels.
  estimated_title_height <- ceiling(
    nchar(axis_title, type = "width") * resolved_font_size * 0.52
  ) + 20

  ###########################################################
  # Growing the figure only when the title nears its edges  #
  ###########################################################
  # The rotated title can use nearly the full SVG height; it does not need to
  # fit wholly inside the grid domain. Comparing it with the full usable canvas
  # avoids making ordinary labels produce an unnecessarily tall figure.
  available_title_height <- max(
    260,
    resolved_plot_height - 80
  )

  extra_domain_height <- max(
    0,
    estimated_title_height - available_title_height
  )

  # Avoiding an excessively tall figure for an unusually long free-text label
  extra_domain_height <- min(extra_domain_height, 260)

  expanded_plot_height <- ceiling(
    resolved_plot_height + extra_domain_height
  )

#------------------------------------------------------------------------------#
# Applying the y-axis layout ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: The axis title combines outcome, disease, and location into a single  #
# descriptive label. autorange is set to FALSE so the rangeslider JS can       #
# control the y-axis range dynamically. rangemode = "tozero" anchors the axis  #
# at zero for consistent display across all geographies.                       #
#------------------------------------------------------------------------------#

  #############################################
  # Preserving any existing horizontal margin #
  #############################################
  existing_margin <- p$x$layout$margin

  if(is.null(existing_margin)){
    existing_margin <- list()
  }

  existing_top_margin <- if(is.null(existing_margin$t)){
    0
  }else{
    existing_margin$t
  }

  existing_bottom_margin <- if(is.null(existing_margin$b)){
    0
  }else{
    existing_margin$b
  }

  existing_margin$t <- max(existing_top_margin, top_margin)
  existing_margin$b <- max(existing_bottom_margin, bottom_margin)

  ################################
  # Formatting the y-axis layout #
  ################################
  p <- p %>%
    plotly::layout(
      yaxis = list(

        # Axis title: "Outcome (Disease, Location)"
        title = list(
          text = axis_title,
          font = list(size = resolved_font_size, color = "#111")
        ),

        # autorange FALSE so JS rangeslider can control the range
        autorange  = FALSE,
        automargin = TRUE,
        side       = "left",

        # Always start from zero for forecast data
        rangemode  = "tozero"

      ),

      # Stable margins protect content at both ends of the figure
      margin = existing_margin,

      # Additional height expands the grid, ticks, and background color bands
      height = expanded_plot_height
    )

  #######################################################
  # Synchronizing the Plotly layout and HTML container  #
  #######################################################
  # plot_ly(height = ...) records a separate htmlwidget height when the plot is
  # first created. Updating only layout$height later leaves that outer element
  # at the old size and clips the lower portion of the enlarged SVG.
  p$height <- expanded_plot_height

  # Returning the completely resized widget
  p

}
