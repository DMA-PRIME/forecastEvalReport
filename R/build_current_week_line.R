#' Add a current reporting week reference line to a Plotly figure
#'
#' Adds a vertical reference line to a Plotly object at the implementation
#' model's reference date. This line provides visual context for
#' distinguishing historical estimates from forward-looking projections
#' within the plot.
#'
#' The line spans the full height of the plotting area using `yref =
#' "paper"` so it displays consistently regardless of the y-axis scale.
#' Existing layout shapes (e.g., phase ribbons added by
#' `build_phase_ribbon()`) are preserved by appending the new line rather
#' than overwriting the full shapes list.
#'
#' If no implementation model is available or it has no rows, the
#' original plot is returned unchanged so the function integrates
#' seamlessly into the plot loop without additional NULL checks.
#'
#' @param p A Plotly object to add the reference line to.
#' @param implementation_model The validated implementation model data
#'   frame, or `NULL`. Must contain a `reference_date` column.
#'
#' @return The Plotly object with the current week reference line added,
#'   or the original plot unchanged if no implementation model is
#'   available.
#'
#' @keywords internal
#' @noRd
build_current_week_line <- function(p, implementation_model) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the plot unchanged when the implementation model #
# is absent or empty. This allows the function to be called unconditionally in #
# the plot loop without the caller needing to check availability first.        #
#------------------------------------------------------------------------------#

  ####################################
  # Guard: NULL implementation model #
  ####################################
  if(is.null(implementation_model)) return(p)

  #####################################
  # Guard: empty implementation model #
  #####################################
  if(!is.data.frame(implementation_model) ||
     nrow(implementation_model) == 0) return(p)

  ########################################
  # Guard: reference_date column missing #
  ########################################
  if(!"reference_date" %in% names(implementation_model)) return(p)

#------------------------------------------------------------------------------#
# Extracting the reference date ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the unique reference date from the implementation  #
# model. anytime::anydate() is used for robust date parsing regardless of how  #
# the column was read in. When multiple reference dates exist (which the       #
# implementation model validator prevents, but we guard anyway), the most      #
# recent is used.                                                              #
#------------------------------------------------------------------------------#

  ###################################
  # Extracting and parsing the date #
  ###################################

  # Pulling the unique reference dates
  ref_dates     <- unique(implementation_model$reference_date)

  # Changing reference date to date format
  vertical_date <- anytime::anydate(ref_dates)

  # Use the most recent reference date when multiple are present
  if(length(vertical_date) > 1){
    vertical_date <- max(vertical_date, na.rm = TRUE)
  }

  # Return unchanged if the date could not be parsed
  if(is.na(vertical_date)) return(p)

#------------------------------------------------------------------------------#
# Adding the vertical reference line -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section build the current week, or vertical reference line       #
# trace. The line is added as a layout shape rather than a trace so it does    #
# not appear in the legend and is not affected by trace visibility toggles.    #
# Existing shapes are preserved by appending to p$x$layout$shapes rather       #
# than replacing the entire shapes list.                                       #
#------------------------------------------------------------------------------#

  ##################################
  # Preserving all existing shapes #
  ##################################
  existing_shapes <- if(!is.null(p$x$layout$shapes)) p$x$layout$shapes else list()

  ###############################
  # Building current week trace #
  ###############################
  p %>%
    plotly::layout(
      shapes = c(

        # Preserve existing shapes (phase ribbons, etc.)
        existing_shapes,

        # New vertical reference line at the reference date
        list(
          list(
            type  = "line",
            x0    = vertical_date,
            x1    = vertical_date,
            y0    = 0,
            y1    = 1,
            xref  = "x",

            # yref = "paper" so the line always spans the full plot height
            # regardless of the y-axis scale
            yref  = "paper",

            line  = list(
              color = "black",
              width = 1,
              dash  = "solid"
            )
          )
        )
      )
    )

}
