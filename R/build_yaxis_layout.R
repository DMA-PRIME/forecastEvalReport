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
#'
#' @return The Plotly object with the y-axis layout applied.
#'
#' @keywords internal
#' @noRd
build_yaxis_layout <- function(p, outcome, disease, geography,
                               spatial.scale) {

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
# Applying the y-axis layout ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: The axis title combines outcome, disease, and location into a single  #
# descriptive label. autorange is set to FALSE so the rangeslider JS can       #
# control the y-axis range dynamically. rangemode = "tozero" anchors the axis  #
# at zero for consistent display across all geographies.                       #
#------------------------------------------------------------------------------#

  ################################
  # Formatting the y-axis layout #
  ################################
  p %>%
    plotly::layout(
      yaxis = list(

        # Axis title: "Outcome (Disease, Location)"
        title = list(
          text = paste0(outcome, " (", disease, ", ", location_label, ")"),
          font = list(size = 16, color = "#111")
        ),

        # autorange FALSE so JS rangeslider can control the range
        autorange  = FALSE,
        automargin = FALSE,
        side       = "left",

        # Always start from zero for forecast data
        rangemode  = "tozero"

      )
    )

}
