#' Add a shaded phase ribbon to a Plotly figure
#'
#' Adds a shaded background ribbon to a Plotly figure to visually highlight
#' a modeling phase (Training Period, Validation Period, or Testing Period)
#' across the timeline. The ribbon spans the full vertical height of the
#' plot using a fixed large y-value so it is clipped naturally by Plotly
#' without requiring dynamic JS updates when traces are toggled.
#'
#' The `training_validation` column in the evaluation model data uses
#' integer codes (0 = Testing, 1 = Training, 2 = Validation). This function
#' converts the integer codes to their string equivalents before filtering
#' so the phase name matches both the data and the legend group names used
#' by the floating legend JavaScript.
#'
#' If no data exists for the requested phase in `evaluation_temp`, the
#' original plot is returned unchanged so the caller does not need to
#' check for data availability before calling.
#'
#' @param p A Plotly object to add the ribbon to.
#' @param evaluation_temp A data frame of evaluation model data for a single
#'   location. Must contain columns `training_validation` (integer: 0/1/2)
#'   and `target_end_date`.
#' @param phase Character. The phase name to highlight. Must be one of
#'   `"Training Period"`, `"Validation Period"`, or `"Testing Period"`.
#' @param color Character. Hex color code for the ribbon fill. Sourced from
#'   `PLOT_STYLES$phase_colors[[phase]]`.
#'
#' @return The Plotly object with the phase ribbon trace added, or the
#'   original plot unchanged if no data exists for the phase.
#'
#' @keywords internal
#' @noRd
build_phase_ribbon <- function(p, evaluation_temp, phase, color) {

#------------------------------------------------------------------------------#
# Converting phase name to integer code ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section converts the phase names provided in the input file to   #
# and integer code. The training_validation column stores integer codes: 1 =   #
# Training, 2 = Validation, 0 = Testing. The phase argument uses the full      #
# string name (e.g., "Training Period") to match the floating legend group     #
# names. This map converts between the two representations.                    #
#------------------------------------------------------------------------------#

  ##################################
  # Phase name to integer code map #
  ##################################
  phase_code_map <- c(
    "Training Period"   = 1L,
    "Validation Period" = 2L,
    "Testing Period"    = 0L
  )

  ############################
  # Resolve the integer code #
  ############################
  phase_code <- phase_code_map[phase]

  ###########################################################
  # If the phase name is not in the map, return p unchanged #
  ###########################################################
  if(is.na(phase_code)){

    # Warning to show to user
    warning(
      "build_phase_ribbon: Unknown phase '", phase, "'. ",
      "Expected one of: Training Period, Validation Period, Testing Period. ",
      "Returning plot unchanged.",
      call. = FALSE
    )

    # Returning an unchanged plot
    return(p)

  }

#------------------------------------------------------------------------------#
# Filtering evaluation data to the requested phase -----------------------------
#------------------------------------------------------------------------------#
# About: This section filters evaluation_temp to rows whose                    #
# training_validation integer code matches the requested phase. If no rows     #
# exist for this phase (e.g., a model with no validation period), the plot is  #
# returned unchanged.                                                          #
#------------------------------------------------------------------------------#

  #################################
  # Filter to the requested phase #
  #################################
  phase_data <- evaluation_temp[
    !is.na(evaluation_temp$training_validation) &
      evaluation_temp$training_validation == phase_code, ]

  # Return unchanged if no data for this phase
  if(nrow(phase_data) == 0) return(p)

#------------------------------------------------------------------------------#
# Extracting the phase start and end dates -------------------------------------
#------------------------------------------------------------------------------#
# About: This section extracts the phase start and end dates. The ribbon       #
# spans from the start of the earliest period to the end of the latest         #
# period in the phase. The cadence (daily, weekly, or monthly) is              #
# determined dynamically from the spacing between target_end_dates, so the     #
# left edge of the ribbon aligns with the first period's start: 0 days         #
# back for daily, step - 1 (e.g. 6) for a Saturday-ending week, and the        #
# first of the month for a monthly cadence.                                    #
#------------------------------------------------------------------------------#

  ##################################
  # Coerce target_end_date to Date #
  ##################################

  # Ensuring the date arithmetic below operates on real Date values
  if(!inherits(phase_data$target_end_date, "Date")){

    # Converting target end date to date
    phase_data$target_end_date <- anytime::anydate(phase_data$target_end_date)

  }

  #############################################
  # Determine the time step (cadence) in days #
  #############################################

  # Unique, sorted, non-missing target dates in the phase
  phase_dates <- sort(unique(phase_data$target_end_date[
    !is.na(phase_data$target_end_date)]))

  # Spacing between consecutive dates; the typical gap is the cadence
  date_gaps <- as.numeric(diff(phase_dates))

  # Median gap in days
  step_days <- if(length(date_gaps) > 0){

    # Determining the median time gap
    round(stats::median(date_gaps, na.rm = TRUE))

  # Default to 7-days
  }else{7}

  # Guard against a zero/negative step (duplicate dates) -- treat as daily
  if(is.na(step_days) || step_days < 1) step_days <- 1

  ###############################
  # Phase span from the cadence #
  ###############################

  # Start date: Month
  start <- if(step_days >= 28){

    # Converting the date
    as.Date(format(min(phase_dates), "%Y-%m-01"))

  # Start Date: Day or Week
  }else{min(phase_dates) - (step_days - 1)}

  # End: last target_end_date in the phase
  end   <- max(phase_dates)

#------------------------------------------------------------------------------#
# Setting a fixed large ribbon height ------------------------------------------
#------------------------------------------------------------------------------#
# About: The ribbon y-values are set to a fixed large number (999999) so the   #
# ribbon always extends beyond the visible plot area and is clipped naturally  #
# by Plotly. This avoids the need for JS to dynamically manage ribbon heights  #
# when traces are toggled. The ribbon traces are excluded from the y-axis max  #
# calculation in the rangeslider JS.                                           #
#------------------------------------------------------------------------------#

  #############################
  # Fixed large ribbon height #
  #############################
  ribbon_height <- 999999

#------------------------------------------------------------------------------#
# Adding the ribbon trace to the plot ------------------------------------------
#------------------------------------------------------------------------------#
# About: The ribbon is added as a filled scatter trace using toself fill mode. #
# The legendgroup matches the phase name so the floating legend checkbox can   #
# toggle it. hoverinfo is set to "skip" so hovering over the ribbon does not   #
# show a tooltip.                                                              #
#------------------------------------------------------------------------------#

  #############################
  # Building the ribbon trace #
  #############################
  p %>%

    plotly::add_trace(
      type        = "scatter",
      mode        = "lines",

      # Four corners of the ribbon rectangle plus closing point
      x           = c(start, end, end, start, start),
      y           = c(0, 0, ribbon_height, ribbon_height, 0),

      fill        = "toself",

      # Semi-transparent fill using scales::alpha for consistent opacity
      fillcolor   = scales::alpha(color, 0.05),

      # No border line on the ribbon
      line        = list(width = 0),

      # Trace name and legend group match the phase string name
      name        = phase,
      legendgroup = phase,

      # Hidden from the plotly legend -- controlled by the floating legend
      showlegend  = FALSE,

      # No hover tooltip for the ribbon.
      hoveron     = "points",
      hoverinfo   = "skip"
    )

}
