#' Create a plot styles configuration for forecast evaluation reports
#'
#' Builds a named list of visual style settings used throughout the
#' forecast evaluation report plots. All arguments have sensible defaults
#' that match the standard report appearance. Users who want to customize
#' the report appearance create their own styles object by calling this
#' function with overrides, then source it in their options file and pass
#' it to [generate_report()].
#'
#' @param colors Character vector of hex color codes used for the primary
#'   color palette (implementation model, general traces). Defaults to a
#'   blue-purple colorblind-friendly palette.
#' @param pi_intervals Named list defining prediction interval levels.
#'   Each entry must have: `lower` (lower quantile), `upper` (upper
#'   quantile), `fill` (rgba fill color string), `hover_color` (hex
#'   color for hover), `label` (display label), `swatch_class` (CSS
#'   class for the legend swatch).
#' @param horizon_styles Named list defining per-horizon line styles for
#'   the evaluation model. Each entry must have: `color`, `dash`, `width`.
#'   Names should match the horizon values in the evaluation model file
#'   (e.g., `"1"`, `"2"`, `"3"`, `"4"`).
#' @param target_data Named list with style settings for the target data
#'   (truth) trace. Must have: `color`, `dash`, `width`, `marker_size`.
#' @param current_projections Named list with style settings for the
#'   current projections trace. Must have: `color`, `dash`, `width`,
#'   `marker_size`.
#' @param historical_estimates Named list with style settings for the
#'   historical estimates trace. Must have: `color`, `dash`, `width`,
#'   `marker_size`.
#' @param phase_colors Named list of hex color codes for the modeling
#'   period phase ribbons. Must have entries named
#'   `"Training Period"`, `"Validation Period"`, `"Testing Period"`.
#' @param plot_width Numeric. Plot width in pixels. Default `900`.
#' @param plot_height Numeric. Plot height in pixels. Default `500`.
#' @param default_view_weeks Integer. Number of weeks shown in the
#'   default range slider window. Default `52`.
#' @param rangeslider_bgcolor Character. Background color of the range
#'   slider. Default `"#ffffff"`.
#' @param eval_legend_title Character. Legend group title for the
#'   evaluation model section. Default `"Evaluation Model"`.
#' @param right_axis Named list configuring the right y-axis used for
#'   auxiliary variable traces. Must have: `title_percent`, `title_default`,
#'   `percent_outcomes`, `title_font_size`, `title_font_color`,
#'   `title_standoff`.
#'
#' @return A named list of plot style settings for use by the report
#'   plotting functions.
#'
#' @export
create_plot_styles <- function(

#------------------------------------------------------------------------------#
# Primary color palette --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the character vector of hex color codes used for #
# the primary color palette. If no user colors are provided, the primary       #
# color palette defaults to a blue-purple colorblind-friendly palette.         #
#------------------------------------------------------------------------------#

  ##############################
  # Character vector of colors #
  ##############################
  # colors = c(
  #   "#648FFF",   # Blue-purple (primary)
  #   "#FE6100",   # Orange
  #   "#DC267F",   # Pink
  #   "#785EF0",   # Purple
  #   "#FFB000"    # Amber
  # ),

colors = c(
  "#898989"   # Blue-purple (primary)
),


#------------------------------------------------------------------------------#
# Prediction intervals ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides a named list defining prediction interval       #
# levels to be included in the figures. Each entry must have a lower quantile  #
# upper quantile, rgba color string, hex code color string for the hover menu  #
# label for the display (hover), and swatch class label for the legend. The    #
# default is to show the 50% and 95% PI intervals.                             #
#------------------------------------------------------------------------------#

  ###########################
  # Creating the named list #
  ###########################
  pi_intervals = list(

    # Default 50% PI
    "50%" = list(
      lower        = 0.25,
      upper        = 0.75,
      fill         = "rgba(100,143,255,0.15)",
      hover_color  = "#648FFF",
      label        = "50% PI",
      swatch_class = "pi-50"
    ),

    # Default 95% PI
    "95%" = list(
      lower        = 0.025,
      upper        = 0.975,
      fill         = "rgba(100,143,255,0.08)",
      hover_color  = "#648FFF",
      label        = "95% PI",
      swatch_class = "pi-95"
    )

  ),

#------------------------------------------------------------------------------#
# Evaluation model horizon styles ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a named list defining per-horizon line styles    #
# for the evaluation model. Each entry must have a color, dash type, and line  #
# width provided. The names should match the horizon values in the evaluation  #
# model file. The default is to have four horizons.                            #
#------------------------------------------------------------------------------#

  #############################################
  # Creating the named list of horizon styles #
  #############################################
  horizon_styles = list(
    "1" = list(color = "#648FFF", dash = "solid",   width = 1.5),
    "2" = list(color = "#FE6100", dash = "dash",    width = 1.5),
    "3" = list(color = "#DC267F", dash = "dot",     width = 1.5),
    "4" = list(color = "#785EF0", dash = "dashdot", width = 1.5)
  ),

#------------------------------------------------------------------------------#
# Named trace styles (used by resolve_line_style / resolve_marker_style) -------
#------------------------------------------------------------------------------#
# About: This section creates the styles for the target data, current          #
# projection, and historical estimates lines. For each, a hex color code, line #
# type, line width and marker size (circles) must be provided. The defaults    #
# are shown below.                                                             #
#------------------------------------------------------------------------------#

  ############################
  # Target data line default #
  ############################
  target_data = list(
    color       = "#000000",
    dash        = "solid",
    width       = 2,
    marker_size = 4
  ),

  ####################################
  # Current projections line default #
  ####################################
  current_projections = list(
    color       = "#648FFF",
    dash        = "solid",
    width       = 2,
    marker_size = 4
  ),

  ################################
  # Historical estimates default #
  ################################
  historical_estimates = list(
    color       = "#648FFF",
    dash        = "dot",
    width       = 1.5,
    marker_size = 3
  ),

#------------------------------------------------------------------------------#
# Phase ribbon colors ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the named list of hex color codes for the        #
# modeling period phase ribbons. The list must have entries named `Training    #
# Period`, `Validation Period`, and `Testing Period`. The colors must be       #
# provided as HEX color codes.                                                 #
#------------------------------------------------------------------------------#

  #################################
  # Creating the phase color list #
  #################################
  phase_colors = list(
    "Training Period"   = "#345FAF",
    "Validation Period" = "#E4572E",
    "Testing Period"    = "#2E8B57"
  ),

#------------------------------------------------------------------------------#
# Plot dimensions --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to determine the size of their plots. They  #
# can select both the width and the height, and the defaults for both can be   #
# found below. The plot dimensions apply to all plots.                         #
#------------------------------------------------------------------------------#

  ############################
  # Selecting the plot width #
  ############################
  plot_width  = 1000,

  #############################
  # Selecting the plot height #
  #############################
  plot_height = 800,

#------------------------------------------------------------------------------#
# Range slider -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section allows users to customize the amount of data shown in    #
# the viewer window for each plot and the shade of the background of the range #
# slide. The default is to show a year of data at the time, and for the        #
# background of the slide to be white.                                         #
#------------------------------------------------------------------------------#

  #################################################
  # Selecting the number of weeks to show in plot #
  #################################################
  default_view_weeks  = 52L,

  ####################################
  # Background color of range slider #
  ####################################
  rangeslider_bgcolor = "#ffffff",

#------------------------------------------------------------------------------#
# Evaluation model legend title ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This allows users to select the title of the evaluation model legend  #
# entry. The default is "Evaluation Model".                                    #
#------------------------------------------------------------------------------#

  #########################################################
  # Legend title for the evaluation model (testing model) #
  #########################################################
  eval_legend_title = "Evaluation Model",

#------------------------------------------------------------------------------#
# Right y-axis (auxiliary variable scale) --------------------------------------
#------------------------------------------------------------------------------#
# About: This section defines the right y-axis used for auxiliary              #
# variables plotted on a different scale than the outcome.                     #
# title_percent is used when the outcome's left axis is already a              #
# percentage; title_default is used for all other (count-based)                #
# outcomes; and percent_outcomes lists which outcomes are percent-based.       #
# The title font size, color, and standoff are also set here.                  #
#------------------------------------------------------------------------------#

  #########################################
  # Right y-axis label and styling config #
  #########################################
  right_axis = list(

    # Title when the outcome's left axis is already a percentage
    title_percent = "Auxiliary Variables",

    # Default title for count-based outcomes
    title_default = "Weekly Tests",

    # Outcomes whose left axis is already a percent (use title_percent)
    percent_outcomes = c(
      "Weekly % Flu-Attributable ED-Visits",
      "Weekly % RSV-Attributable ED-Visits",
      "Weekly % COVID-19-Attributable ED-Visits"
    ),

    # Title font size, color, and standoff from the axis
    title_font_size  = 16,
    title_font_color = "#111",
    title_standoff   = 25
  )

){

# NO USER ENTRIES UNDER THIS LINE

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the required fields are present and        #
# correctly typed before assembling the styles list. It also includes specific #
# errors about what field is wrong, and what the expected values are.          #
#------------------------------------------------------------------------------#

  ########################################
  # Preparing to store and return errors #
  ########################################

  # Empty vector to store errors
  errors <- character()

  # Function to add error to vector
  add_error <- function(msg) errors <<- c(errors, msg)

#------------------------------------------------------------------------------#
# Checking the vectors provided by users ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks each of the user provided vectors to ensure they  #
# are the correct format for the report system. If any violations occurs, the  #
# error is added to the vector and returned to the user at the end.            #
#------------------------------------------------------------------------------#

  ###########################################
  # Checking if colors are character vector #
  ###########################################
  if(!is.character(colors) || length(colors) == 0){

    # Error to show to users
    add_error("`colors` must be a non-empty character vector of hex color codes.")

  }

  ##########################################
  # Checking PI Intervals are a named list #
  ##########################################
  if(!is.list(pi_intervals) || is.null(names(pi_intervals))){

    # Error to show to users
    add_error("`pi_intervals` must be a named list.")

  #################################
  # Checking the PI list elements #
  #################################
  }else{

    # List of required list elements
    required_pi_fields <- c("lower", "upper", "fill", "hover_color",
                            "label", "swatch_class")

    # Checking provided names against required
    for(nm in names(pi_intervals)){

      # Flagging missing names
      missing_fields <- setdiff(required_pi_fields, names(pi_intervals[[nm]]))

      # Triggered if any values are missing
      if(length(missing_fields) > 0){

        # Error to show to users
        add_error(paste0(
          "`pi_intervals$", nm, "` is missing required field(s): ",
          paste(missing_fields, collapse = ", "), "."
        ))

      }
    }
  }

  ##########################################
  # Checking horizons are provided as list #
  ##########################################
  if(!is.list(horizon_styles) || is.null(names(horizon_styles))){

    # Error to show to users
    add_error("`horizon_styles` must be a named list.")

  ##############################
  # Checking the list elements #
  ##############################
  }else{

    # Required list elements
    required_h_fields <- c("color", "dash", "width")

    # Comparing provided and required list elements
    for(nm in names(horizon_styles)){

      # Flagging missing values
      missing_fields <- setdiff(required_h_fields, names(horizon_styles[[nm]]))

      # Triggered if any list elements are missing
      if(length(missing_fields) > 0){

        # Error to show to users
        add_error(paste0(
          "`horizon_styles$", nm, "` is missing required field(s): ",
          paste(missing_fields, collapse = ", "), "."
        ))

      }
    }
  }

  ###################################
  # Checking the named trace styles #
  ###################################

  # Required fields for name traced lists
  required_trace_fields <- c("color", "dash", "width", "marker_size")

  # Checking for missing fields
  for(trace_nm in c("target_data", "current_projections", "historical_estimates")){

    # Getting the trace name
    trace_val <- get(trace_nm)

    # Flagging missing values
    missing_fields <- setdiff(required_trace_fields, names(trace_val))

    # Triggered if any values are missing
    if(length(missing_fields) > 0){

      # Error to show to users
      add_error(paste0(
        "`", trace_nm, "` is missing required field(s): ",
        paste(missing_fields, collapse = ", "), "."
      ))

    }
  }

  ####################################
  # Checking the phase color entries #
  ####################################

  # Required model development phase names
  required_phases <- c("Training Period", "Validation Period", "Testing Period")

  # Checking for missing model development names
  missing_phases <- setdiff(required_phases, names(phase_colors))

  # Triggered if an error occurs
  if(length(missing_phases) > 0){

    # Error to show to user
    add_error(paste0(
      "`phase_colors` is missing required phase(s): ",
      paste(missing_phases, collapse = ", "), "."
    ))

  }

  ####################################
  # Checking the right y-axis config #
  ####################################

  # right_axis must be a list carrying the two title strings
  if(!is.list(right_axis) ||
     is.null(right_axis$title_default) ||
     is.null(right_axis$title_percent)){

    # Error to show to users
    add_error(paste0(
      "`right_axis` must be a list with at least `title_default` and ",
      "`title_percent`."
    ))

  }
  #######################################
  # Checking any numbers provided above #
  #######################################

  # Checking plot width value
  if(!is.numeric(plot_width)  || plot_width  <= 0) add_error("`plot_width` must be a positive number.")

  # Checking plot height value
  if(!is.numeric(plot_height) || plot_height <= 0) add_error("`plot_height` must be a positive number.")

  # Checking number of weeks to show in the range slider
  if(!is.numeric(default_view_weeks) || default_view_weeks <= 0){add_error("`default_view_weeks` must be a positive number.")}

  ##############################
  # Abort script if any errors #
  ##############################
  if(length(errors) > 0){

    # Stopping and error message to show
    stop(
      "create_plot_styles failed with ", length(errors), " error(s):\n  - ",
      paste(errors, collapse = "\n  - "),
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Building the PI styles list for backwards compatibility ----------------------
#------------------------------------------------------------------------------#
# About: The plot functions expect `pi_styles` as a named list of style        #
# entries keyed by PI level label. This is derived from `pi_intervals` so      #
# users only have to specify PI configuration in one place.                    #
#------------------------------------------------------------------------------#

  ###################################################
  # Building the PI style for each user entry above #
  ###################################################
  pi_styles <- lapply(pi_intervals, function(pi){

    # Style list
    list(
      fill         = pi$fill,
      hover_color  = pi$hover_color,
      label        = pi$label,
      swatch_class = pi$swatch_class
    )

  })

  ###################################
  # Creating the pairs of quantiles #
  ###################################
  quantile_pairs <- lapply(pi_intervals, function(pi){
    c(pi$lower, pi$upper)
  })


#------------------------------------------------------------------------------#
# Building the horizon color/dash/width split vectors --------------------------
#------------------------------------------------------------------------------#
# About: This section builds the evaluation model vectors. The evaluation      #
# model trace function indexes colors and line types by horizon numbers. The   #
# below names vectors allow for direct indexing by horizon.                    #
#------------------------------------------------------------------------------#

  #############################
  # Building the color vector #
  #############################
  colors_split    <- setNames(
    vapply(horizon_styles, `[[`, character(1), "color"),
    names(horizon_styles)
  )

  #################################
  # Building the line type vector #
  #################################
  lineType_split  <- setNames(
    vapply(horizon_styles, `[[`, character(1), "dash"),
    names(horizon_styles)
  )

  #################################
  # Building the line size vector #
  #################################
  lineSize_split  <- setNames(
    vapply(horizon_styles, `[[`, numeric(1), "width"),
    names(horizon_styles)
  )

#------------------------------------------------------------------------------#
# Assembling the PLOT_STYLES list ----------------------------------------------
#------------------------------------------------------------------------------#
# About: The returned list uses the same structure as the old PLOT_STYLES so   #
# existing functions (resolve_line_style, resolve_marker_style, etc.) work     #
# without modification. Named trace entries use the exact group names that     #
# resolve_line_style() and resolve_marker_style() look up.                     #
#------------------------------------------------------------------------------#

  list(

    ######################
    # Named trace styles #
    ######################

    # Target Data
    "Target Data" = list(
      color       = target_data$color,
      dash        = target_data$dash,
      width       = target_data$width,
      marker_size = target_data$marker_size
    ),

    # Current Projections
    "Current Projections" = list(
      color       = current_projections$color,
      dash        = current_projections$dash,
      width       = current_projections$width,
      marker_size = current_projections$marker_size
    ),

    # Historical Estimates
    "Historical Estimates" = list(
      color       = historical_estimates$color,
      dash        = historical_estimates$dash,
      width       = historical_estimates$width,
      marker_size = historical_estimates$marker_size
    ),

    # Evaluation Model
    "Evaluation Model" = list(
      legend_title = eval_legend_title
    ),

    #############################
    # Palette and split vectors #
    #############################
    colors          = colors,
    colors_split    = colors_split,
    lineType_split  = lineType_split,
    lineSize_split  = lineSize_split,

    ##############################
    # Prediction interval config #
    ##############################

    # Preparing the intervals
    pi_intervals  = pi_intervals,

    # Preparing the interval styles
    pi_styles     = pi_styles,

    # Preparing the quantile pairs
    quantile_pairs = quantile_pairs,

    #######################
    # Phase ribbon colors #
    #######################
    phase_colors = phase_colors,

    ####################################
    # Plot dimensions and range slider #
    ####################################

    # Plot width
    plot_width          = plot_width,

    # Plot height
    plot_height         = plot_height,

    # View weeks
    default_view_weeks  = as.integer(default_view_weeks),

    # Range slider colors
    rangeslider_bgcolor = rangeslider_bgcolor,

    ###########################################
    # Right y-axis (auxiliary variable scale) #
    ###########################################
    right_axis          = right_axis

  )

}
