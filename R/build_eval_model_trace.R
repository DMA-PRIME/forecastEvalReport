#' Build evaluation model traces for multi-horizon Plotly visualization
#'
#' Adds one trace per unique forecast horizon to a Plotly object,
#' representing the evaluation model's performance across multiple forecast
#' lead times. Each horizon is a separate line so users can toggle horizons
#' individually via the floating legend checkboxes.
#'
#' Each horizon's color is resolved from `eval_style$colors`. All evaluation
#' model traces share the same dotted line type so that line style
#' distinguishes the evaluation model from the (solid) implementation model,
#' while color distinguishes the individual horizons.
#'
#' @param p A Plotly object to add the traces to.
#' @param evaluation_temp A data frame of evaluation model data for a single
#'   location. Must contain columns `target_end_date`, `value`, and `horizon`.
#' @param outcome Character. The outcome display label used in hover text.
#' @param value_suffix Character. Suffix appended to values in hover text.
#' @param eval_style A named list of style settings for the evaluation model
#'   traces. Must have: `legend_title`, `colors`, `line_types`, `line_widths`.
#'   Populated from PLOT_STYLES in the plot loop.
#'
#' @return The Plotly object with one trace per unique horizon added.
#'
#' @keywords internal
#' @noRd
build_eval_model_trace <- function(p, evaluation_temp, outcome,
                                   value_suffix, eval_style) {

#------------------------------------------------------------------------------#
# Extracting unique horizons ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls out the unique horizons included within the        #
# evaluation file to be the driver of the loop in the next section. The        #
# evaluation model data contains one row per (location, horizon, date)         #
# combination. Each unique horizon value gets its own trace so users can       #
# toggle horizons individually in the floating legend.                         #
#------------------------------------------------------------------------------#

  #################################
  # Getting unique horizon values #
  #################################

  # Pulling the unique horizons
  horizons <- unique(evaluation_temp$horizon)

  # Sorting the horizons
  horizons <- horizons[order(suppressWarnings(as.numeric(as.character(horizons))))]

#------------------------------------------------------------------------------#
# Looping through each horizon and adding a trace ------------------------------
#------------------------------------------------------------------------------#
# About: Rather than using plotly's color/split arguments which require a      #
# palette function, this loop adds a separate trace for each horizon with      #
# its own color from eval_style. All evaluation traces use a dotted line so    #
# the line style identifies the evaluation model while color identifies the    #
# horizon.                                                                     #
#------------------------------------------------------------------------------#

  ############################
  # Looping through horizons #
  ############################
  for(horizon_idx in seq_along(horizons)){

    # Indexing the horizon
    horizon <- horizons[[horizon_idx]]

    ####################################
    # Filtering to the current horizon #
    ####################################

    # The column name is `horizon` (renamed from `estimate_projected_report`)
    horizon_data <<- evaluation_temp[
      !is.na(evaluation_temp$horizon) &
        evaluation_temp$horizon == horizon, ]

    # Skip if no rows for this horizon
    if(nrow(horizon_data) == 0) next

    ####################################
    # Resolving style for this horizon #
    ####################################

    # Convert horizon to character for named vector look-up
    horizon_key <- as.character(horizon)

    # Look up color: Fall back to cycling through available colors
    horizon_color <- if(!is.null(eval_style$colors) &&
                           horizon_key %in% names(eval_style$colors)){

      # Pulling the designated color from the style file
      eval_style$colors[[horizon_key]]

    # Using the other available colors
    }else{

      # Cycle through available colors using positional indexing
      avail_colors <- unname(eval_style$colors)
      avail_colors[[((horizon_idx - 1L) %% length(avail_colors)) + 1L]]

    }

    # All evaluation model horizons use a dotted line. Line style
    horizon_dash <- "dot"

    # Look up line width -- fall back to 1.5 if not in styles
    horizon_width <- if(!is.null(eval_style$line_widths) &&
                           horizon_key %in% names(eval_style$line_widths)){

      # Pulling the designated line width
      eval_style$line_widths[[horizon_key]]

    # Pulling default line width
    }else{1.5}

    #####################################
    # Adding the trace for this horizon #
    #####################################
    p <- p %>%
      plotly::add_trace(
        data             = horizon_data,
        x                = ~target_end_date,
        y                = ~value,
        type             = "scatter",
        mode             = "lines",

        # Trace name is the horizon value (character)
        name             = horizon_key,

        # Line styling: per-horizon color, shared dotted line type
        line             = list(
          color = horizon_color,
          dash  = horizon_dash,
          width = horizon_width
        ),

        # legendgroup = horizon key so the floating-legend checkbox
        # (data-trace = horizon key) can toggle this trace. The visible group
        # heading still comes from legendgrouptitle.
        legendgrouptitle = list(text = eval_style$legend_title),
        legendgroup      = horizon_key,

        # Hidden by default -- user toggles via floating legend checkboxes
        showlegend       = FALSE,
        visible          = FALSE,

        # Hover template identifies horizon, outcome, week, and value
        hovertemplate    = paste0(
          "<b>", horizon_key, "-Week Horizon</b><br>",
          "Outcome: ", outcome, "<br>",
          "Week: %{x|%Y-%m-%d}<br>",
          "Value: %{y:.3f}", value_suffix, "<br>",
          "<extra></extra>"
        )
      )

  }

  ##############################
  # Returning the updated plot #
  ##############################
  p

}
