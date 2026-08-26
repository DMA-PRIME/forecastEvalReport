#' Build the training data trace for a Plotly figure
#'
#' Adds a single training data trace to a Plotly object as a solid line.
#' Training data is an optional external data source (e.g. Google Trends,
#' wastewater) incorporated during model development. When the user provides
#' training data (via `training.data.file` / `training.variable.name` /
#' `training_data_source` in the options file), it is assembled into
#' `master_data` with `variable_type == "training_data"` and surfaced here.
#'
#' The trace is grouped under the Evaluation Model legend title so it appears
#' in the same legend section as the evaluation horizons, and its legendgroup
#' is the data source clean name so the floating-legend checkbox (whose
#' `data-trace` is that same clean name) can toggle it. The trace is hidden by
#' default; the user reveals it via the legend checkbox.
#'
#' @param p A Plotly object to add the trace to.
#' @param training_data A data frame of training data for a single location.
#'   Must contain columns `date` and `value`.
#' @param data_source Character. The training data source clean name
#'   (`config$training_data_source`). Used as the trace name, legendgroup,
#'   and hover label so it matches the legend checkbox.
#' @param outcome Character. The outcome display label used in hover text.
#' @param value_suffix Character. Suffix appended to values in hover text.
#' @param eval_style A named list of evaluation-model style settings. Used for
#'   `legend_title` (the legend group heading) and, when present, a training
#'   color/width. Populated from PLOT_STYLES in the plot loop.
#'
#' @return The Plotly object with the training data trace added.
#'
#' @keywords internal
#' @noRd
build_training_trace <- function(p, training_data, data_source,
                                 outcome, value_suffix, eval_style) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the figure unchanged when there is no training   #
# data to plot, or no data-source clean name to label and group the trace.     #                                                           #
#------------------------------------------------------------------------------#

  #####################################################################
  # Returning the figure as-is when there is no training data to plot #
  #####################################################################
  if(is.null(training_data) ||
     !is.data.frame(training_data) ||
     nrow(training_data) == 0) return(p)

  ############################################################################
  # Returning the figure as-is without a clean name to label/group the trace #
  ############################################################################
  if(is.null(data_source) ||
     length(data_source) == 0 ||
     all(is.na(data_source)) ||
     all(nchar(trimws(as.character(data_source))) == 0)) return(p)

  #########################################################################
  # Using the first data-source value as the trace name, group, and label #
  #########################################################################
  data_source <- as.character(data_source)[1]

#------------------------------------------------------------------------------#
# Resolve the training line styling --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resolves the line color and width for the training       #
# series, falling back to a neutral dark grey and a width of 2 when eval_style #
# defines no training-specific values, so it stays visually distinct from the  #
# colored evaluation horizons.                                                 #
#------------------------------------------------------------------------------#

  ######################################
  # Line color: Pulling from User File #
  ######################################
  training_color <- if(!is.null(eval_style$training_color)){

    # Pulling the color
    eval_style$training_color

  #################################
  # Line color: Using the Default #
  #################################
  }else{"#444444"}

  ######################################
  # Line width: Pulling from User File #
  ######################################
  training_width <- if(!is.null(eval_style$training_width)){

    # Pulling the line width
    eval_style$training_width

  #################################
  # Line Width: Using the Default #
  #################################
  }else{2}

#------------------------------------------------------------------------------#
# Add the training data trace --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adds the training series as a solid line, grouped under  #
# the Evaluation Model legend heading and keyed by the data-source clean name  #
# so the floating-legend checkbox can toggle it. The trace is hidden by        #
# default; the user reveals it via that checkbox.                              #                                                              #
#------------------------------------------------------------------------------#

  ############################################
  # Adding the training series to the figure #
  ############################################
  p <- p %>%
    plotly::add_trace(
      data             = training_data,
      x                = ~date,
      y                = ~value,
      type             = "scatter",
      mode             = "lines",

      # Trace name is the data source clean name (matches the legend checkbox)
      name             = data_source,

      # Solid line styling
      line             = list(
        color = training_color,
        dash  = "solid",
        width = training_width
      ),

      # Grouped under the Evaluation Model legend heading
      legendgrouptitle = list(text = eval_style$legend_title),
      legendgroup      = data_source,

      # Hidden by default -- user reveals via the floating legend checkbox
      showlegend       = FALSE,
      visible          = FALSE,

      # Hover identifies the training source, outcome, week, and value
      hovertemplate    = paste0(
        "<b>", data_source, " (Training)</b><br>",
        "Outcome: ", outcome, "<br>",
        "Week: %{x|%Y-%m-%d}<br>",
        "Value: %{y:.3f}", value_suffix, "<br>",
        "<extra></extra>"
      )
    )

  ######################################################
  # Returning the figure with the training trace added #
  ######################################################
  p

}
