#' Build a forecast consistency plot for a single location
#'
#' Renders an interactive Plotly figure for one location showing the target
#' data (truth line) overlaid with the most recent operational forecasts. The
#' newest forecast is drawn as a solid orange line with blue prediction-interval
#' bands; older forecasts are drawn dashed with lighter bands, and forecasts
#' older than the two most recent default to a hidden ("legendonly") state so
#' the plot stays readable. Auxiliary variable traces, a current-week reference
#' line, a range slider, and the fullscreen control match the main forecast
#' plots section.
#'
#' Forecast files are read from disk (raw location codes, all quantiles for the
#' forward-looking rows). Truth data comes from `master_data` (normalized
#' display names).
#'
#' @param loc_display Character. Normalized display name (master_data + labels).
#' @param raw_loc Character. Raw location code (forecast-file filtering).
#' @param archive Data frame from `build_forecast_archive()` with `file_path`
#'   and `reference_date`, ordered oldest -> newest.
#' @param master_data Assembled master data frame.
#' @param implementation_model Validated implementation model data frame.
#' @param impl_meta Metadata list from `extract_implementation_data()`.
#' @param config Validated config list.
#' @param variables_crosswalk Validated crosswalk data frame, or `NULL`.
#' @param plot_styles A named list of plot style settings.
#' @param outcome_display Character outcome label for hover/axis text.
#' @param disease_display Character disease label for the y-axis.
#' @param spatial_scale Character spatial scale label for the y-axis.
#'
#' @return A list with `plot` (a Plotly htmlwidget) and `hover_text`
#'   (the truth-line hover label), or `NULL` when there is no truth data.
#'
#' @keywords internal
#' @noRd
make_forecast_consistency_plot <- function(loc_display,
                                           raw_loc,
                                           archive,
                                           master_data,
                                           implementation_model,
                                           impl_meta,
                                           config,
                                           variables_crosswalk,
                                           plot_styles,
                                           outcome_display,
                                           disease_display,
                                           spatial_scale,
                                           for_export   = FALSE,
                                           font_size    = NULL,
                                           view_start   = NULL,
                                           view_end     = NULL,
                                           tick_size      = NULL,
                                           forecast_size  = NULL,
                                           forecast_color = NULL,
                                           observed_size  = NULL,
                                           observed_color = NULL,
                                           aux_size       = NULL,
                                           aux_color      = NULL,
                                           title_gap      = NULL,
                                           flatten        = FALSE){

#------------------------------------------------------------------------------#
# Truth data -------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Pulls this location's outcome (truth) rows from the master data.      #
# With no truth rows there is nothing to plot, so the function returns NULL.   #
#------------------------------------------------------------------------------#

  # Outcome (truth) rows for this location. The consistency export has to
  # bridge two location systems: archived forecast files usually use raw codes
  # (raw_loc), while master_data often uses cleaned display names (loc_display).
  # Try the display name first, then fall back to the raw code before skipping.
  truth_data <- master_data[
    !is.na(master_data$variable_type) &
      master_data$variable_type == "outcome_data" &
      !is.na(master_data$location) &
      master_data$location == loc_display, ]

  message("[consistency debug] loc_display=", loc_display,
          " | raw_loc=", raw_loc,
          " | n_truth_rows=", nrow(truth_data))
  message("[consistency debug] outcome locations present: ",
          paste(unique(master_data$location[
            !is.na(master_data$variable_type) &
              master_data$variable_type == "outcome_data"]), collapse = " | "))

  if(nrow(truth_data) == 0 && !is.null(raw_loc) && !is.na(raw_loc)){
    truth_data <- master_data[
      !is.na(master_data$variable_type) &
        master_data$variable_type == "outcome_data" &
        !is.na(master_data$location) &
        master_data$location == raw_loc, ]

    # Keep labels consistent with the data actually found.
    if(nrow(truth_data) > 0){
      loc_display <- raw_loc
    }
  }

  # No truth data -> nothing to plot
  if(nrow(truth_data) == 0) return(NULL)

#------------------------------------------------------------------------------#
# Hover text and percent scaling -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Builds the truth-line hover label and percent-scales the truth        #
# values, capturing the value suffix for hover and axis text.                  #
#------------------------------------------------------------------------------#

  # Hover label for the truth line
  hover_text <- build_hover_text(
    location            = loc_display,
    variables_crosswalk = variables_crosswalk,
    config              = config
  )

  # Percent-scale the truth values; capture the value suffix
  scaled_truth <- apply_percent_scaling(
    data                = truth_data,
    col                 = "value",
    variable            = truth_data$variable[1],
    variables_crosswalk = variables_crosswalk
  )
  truth_data   <- scaled_truth$data
  value_suffix <- scaled_truth$suffix

#------------------------------------------------------------------------------#
# Percent flag for the forecast files ------------------------------------------
#------------------------------------------------------------------------------#
# About: Flags whether the forecast files are on a percent scale, so forecast  #
# values can be scaled to match the truth data below.                          #
#------------------------------------------------------------------------------#

  # TRUE when the outcome is a CDC(NSSP) percent measure
  is_pct <- !is.null(config$outcome_data_label) &&
    !is.na(config$outcome_data_label) &&
    config$outcome_data_label == "CDC(NSSP)"

#------------------------------------------------------------------------------#
# Figure and truth trace -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Initializes the Plotly figure with the split palette and configured   #
# dimensions, then draws the target-data (truth) trace.                        #
#------------------------------------------------------------------------------#

  # Empty figure with the configured palette and dimensions
  p <- plotly::plot_ly(
    colors = plot_styles$colors_split,
    width  = plot_styles$plot_width,
    height = plot_styles$plot_height
  )

  # Target-data (truth) line. Observed-line width/color overrides are applied
  # later (see the post-build patch), because trace data is not materialized
  # until the plot is built.
  p <- build_truth_trace(
    p            = p,
    truth_data   = truth_data,
    outcome      = outcome_display,
    hover_text   = hover_text,
    value_suffix = value_suffix,
    plot_styles  = plot_styles
  )

#------------------------------------------------------------------------------#
# Per-forecast color logic -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Returns the line color, dash, band fills, and default visibility for  #
# one forecast. The newest is solid orange with blue bands; older forecasts    #
# are dashed with lighter bands, and any beyond the two most recent start      #
# hidden so the plot stays readable.                                           #
#------------------------------------------------------------------------------#

  consistency_color <- function(reference_date, all_dates, flatten = FALSE){

    # Shared forecast line color
    base_orange <- "#FFB000"

    # Truncated view: no single forecast emphasized. Every forecast uses the
    # lighter (older-forecast) style -- dashed line, lighter bands -- and all
    # are shown.
    if(isTRUE(flatten)){
      return(list(
        line    = base_orange,
        dash    = "dash",
        fill50  = "rgba(180,200,250,0.35)",
        fill95  = "rgba(210,230,255,0.35)",
        visible = TRUE
      ))
    }

    # Rank this date among all forecast dates (newest = position 1)
    sorted   <- sort(all_dates, decreasing = TRUE)
    position <- which(sorted == reference_date)[1]
    if(is.na(position)) position <- length(sorted)

    # Newest forecast: solid, blue bands -- older: dashed, lighter bands
    if(position == 1){
      list(
        line    = base_orange,
        dash    = "solid",
        fill50  = "rgba(60,100,220,0.35)",
        fill95  = "rgba(100,143,255,0.35)",
        visible = TRUE
      )
    }else{
      list(
        line    = base_orange,
        dash    = "dash",
        fill50  = "rgba(180,200,250,0.35)",
        fill95  = "rgba(210,230,255,0.35)",
        visible = if(position <= 2) TRUE else "legendonly"
      )
    }
  }

#------------------------------------------------------------------------------#
# Prediction-interval bands ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Adds one shaded interval as an invisible lower line plus a filled     #
# upper line (fill to next y). Empty bounds are skipped.                       #
#------------------------------------------------------------------------------#

  add_band <- function(p, lower, upper, fillcolor, legendgroup, visible){
    if(nrow(lower) == 0 || nrow(upper) == 0) return(p)

    p <- plotly::add_trace(
      p, data = lower, x = ~target_end_date, y = ~value,
      type = "scatter", mode = "lines",
      line = list(color = "rgba(0,0,0,0)"),
      legendgroup = legendgroup, showlegend = FALSE,
      visible = visible, hoverinfo = "skip"
    )

    plotly::add_trace(
      p, data = upper, x = ~target_end_date, y = ~value,
      type = "scatter", mode = "lines",
      line = list(color = "rgba(0,0,0,0)"),
      fill = "tonexty", fillcolor = fillcolor,
      legendgroup = legendgroup, showlegend = FALSE,
      visible = visible, hoverinfo = "skip"
    )
  }

#------------------------------------------------------------------------------#
# Overlaying archived forecasts ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Reads each archived forecast (oldest first, so the newest draws on    #
# top), filters to this location and the needed quantiles, and adds the 95%    #
# and 50% bands plus the median line. Forecast values and end dates are        #
# accumulated for the y-range and view window.                                 #
#------------------------------------------------------------------------------#

  # Accumulators for the y-range and the view window
  all_ref_dates <- archive$reference_date
  fc_values     <- numeric(0)
  fc_end_dates  <- as.Date(character(0))

  # Recency emphasis (solid line, darker bands on the newest forecast) is
  # flattened when `flatten = TRUE` -- passed by the caller, which knows the
  # last available forecast even after stride mode trims later forecasts. When
  # FALSE the newest forecast is emphasized as usual.

  for(r in seq_len(nrow(archive))){

    raw <- tryCatch(
      utils::read.csv(archive$file_path[r], stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if(is.null(raw) || nrow(raw) == 0) next

    # Normalize key columns
    raw$target_end_date <- anytime::anydate(raw$target_end_date)
    raw$reference_date  <- anytime::anydate(raw$reference_date)
    raw$output_type_id  <- suppressWarnings(
      as.numeric(as.character(raw$output_type_id)))

    if(is_pct) raw$value <- round(raw$value * 100, 3)

    # Filter to this location (raw code)
    raw <- raw[!is.na(raw$location) & raw$location == raw_loc, ]
    if(nrow(raw) == 0) next

    pull_q <- function(q){
      d <- raw[!is.na(raw$output_type_id) &
                 abs(raw$output_type_id - q) < 1e-8, ]
      d[order(d$target_end_date), , drop = FALSE]
    }

    median_df <- pull_q(0.5)
    lower50   <- pull_q(0.25)
    upper50   <- pull_q(0.75)
    lower95   <- pull_q(0.025)
    upper95   <- pull_q(0.975)

    if(nrow(median_df) == 0) next

    d       <- archive$reference_date[r]
    d_lbl   <- format(d, "%Y-%m-%d")
    lgroup  <- paste0("Forecast ", d_lbl)
    colors  <- consistency_color(d, all_ref_dates, flatten = flatten)

    # Bands first (95 underneath 50), then the median line on top
    p <- add_band(p, lower95, upper95, colors$fill95, lgroup, colors$visible)
    p <- add_band(p, lower50, upper50, colors$fill50, lgroup, colors$visible)

    p <- plotly::add_trace(
      p, data = median_df, x = ~target_end_date, y = ~value,
      type = "scatter", mode = "lines+markers",
      name = paste0("Forecast Date: ", d_lbl),
      legendgroup = lgroup, showlegend = FALSE, visible = colors$visible,
      line   = list(
        color = if(!is.null(forecast_color)) forecast_color else colors$line,
        width = if(!is.null(forecast_size))  forecast_size  else 2,
        dash  = colors$dash
      ),
      marker = list(
        color = if(!is.null(forecast_color)) forecast_color else colors$line,
        size  = 6
      ),
      hovertemplate = paste0(
        "<b>Forecast: ", d_lbl, "</b><br>",
        "Week: %{x|%Y-%m-%d}<br>",
        "Value: %{y:.3f}", value_suffix,
        "<extra></extra>"
      )
    )

    fc_values    <- c(fc_values, median_df$value)
    fc_end_dates <- c(fc_end_dates, median_df$target_end_date)
  }

#------------------------------------------------------------------------------#
# Auxiliary variable traces ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Adds this location's auxiliary-variable traces (secondary y-axis)     #
# when any are present.                                                        #
#------------------------------------------------------------------------------#

  # Auxiliary variables for this location, if any. The aux-line width override
  # is applied later (see the post-build patch), once trace data exists.
  param_data <- prepare_parameter_data(master_data, loc_display)
  if(!is.null(param_data)){
    p <- add_parameter_traces_by_disease(
      p                   = p,
      data                = param_data,
      variables_crosswalk = variables_crosswalk,
      outcome             = outcome_display
    )
  }

#------------------------------------------------------------------------------#
# Current-week reference line --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Draws the vertical reference line marking the current                 #
# implementation week.                                                         #
#------------------------------------------------------------------------------#

  # Vertical line at the current implementation week
  p <- build_current_week_line(p, implementation_model)

#------------------------------------------------------------------------------#
# maxPhase for the range-slider JS ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Computes the padded y maximum (forecast + truth) that the             #
# range-slider JS uses to scale the y-axis, with a fallback when empty.        #
#------------------------------------------------------------------------------#

  # Padded y maximum across forecast and truth values
  vals <- c(fc_values, if(nrow(truth_data) > 0) truth_data$value else NULL)
  maxPhase_padded <- if(length(vals) > 0 && any(!is.na(vals))){
    max(vals, na.rm = TRUE) * 1.1
  }else{
    1000
  }
  p$x$maxPhase <- maxPhase_padded

#------------------------------------------------------------------------------#
# Range-slider window ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Sets the initial visible x-window to the configured number of weeks   #
# ending just past the latest forecast (or truth) date; the slider spans the   #
# full series.                                                                 #
#------------------------------------------------------------------------------#

  # End the view one week past the latest forecast (or truth) date
  end_view <- if(length(fc_end_dates) > 0){
    max(fc_end_dates, na.rm = TRUE) + lubridate::weeks(1)
  }else{
    max(truth_data$date, na.rm = TRUE) + lubridate::weeks(1)
  }

  # Start the view default_view_weeks before that end
  start_view <- end_view - lubridate::weeks(plot_styles$default_view_weeks)

  # Initial x-window: an explicit export view overrides the default weeks
  .xrange <- if(!is.null(view_start) && !is.null(view_end)){
    c(as.Date(view_start), as.Date(view_end))
  }else{
    c(start_view, end_view)
  }

  # Apply the slider and tick format. For static exports, do not force the
  # default zoom window unless the user explicitly supplied view_start/view_end;
  # otherwise the screenshot can capture an empty/too-narrow window.
  xaxis_layout <- list(
    rangeslider = list(
      visible = !isTRUE(for_export),
      yaxis   = list(range = c(0, maxPhase_padded)),
      bgcolor = plot_styles$rangeslider_bgcolor
    ),
    title      = "",
    tickformat = "%Y-%m-%d"
  )

  if(!isTRUE(for_export) || (!is.null(view_start) && !is.null(view_end))){
    xaxis_layout$range <- .xrange
  }

  p <- p %>% plotly::layout(xaxis = xaxis_layout)

  # Hide the unselected slider mask only when the slider is shown; restamp
  # maxPhase for the JS/report layout.
  if(!isTRUE(for_export)){
    p$x$layout$xaxis$rangeslider$unselected <- list(opacity = 0)
  }
  p$x$maxPhase <- maxPhase_padded

#------------------------------------------------------------------------------#
# Axes, range-slider JS, and fullscreen ----------------------------------------
#------------------------------------------------------------------------------#
# About: Adds the y-axis layout, the right (auxiliary) axis, the range-slider  #
# behavior JS, fixed sizing, and the fullscreen button, then returns the plot  #
# and its hover text.                                                          #
#------------------------------------------------------------------------------#

  # Left y-axis layout (titles from outcome / disease / geography)
  p <- build_yaxis_layout(
    p             = p,
    outcome       = outcome_display,
    disease       = disease_display,
    geography     = loc_display,
    spatial.scale = spatial_scale
  )

  # Right (auxiliary) y-axis. In a static export, autorange it so aux traces
  # on the secondary axis are not left off-screen.
  p <- add_right_yaxis(p, outcome = outcome_display,
                       autorange = isTRUE(for_export))

  # Range-slider behavior JS. This depends on report-page JavaScript assets
  # that are intentionally omitted from standalone static-export pages.
  if(!isTRUE(for_export)){
    p <- attach_rangeslider_js(p)
  }

  # Fixed sizing (no autosize); legend handled by the floating legend
  p <- p %>%
    plotly::layout(showlegend = FALSE, autosize = FALSE)

#------------------------------------------------------------------------------#
# Static export adjustments ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: For a static export the range-slider JS is gone, so force every trace #
# visible (truth, the loaded forecasts, and aux) and pin the left y-axis to    #
# the data. The archive is already capped to n_forecasts upstream, so "all     #
# visible" shows exactly the requested forecasts. The right axis is autoranged #
# above, the x-window via the view range, and the optional font size below.    #
# These plotly::style()/layout() calls rebuild the widget and drop onRender    #
# hooks, so they run BEFORE build_fullscreen_button(), which attaches the      #
# fullscreen handler the static page's auto-expand depends on.                 #
#------------------------------------------------------------------------------#

  if(isTRUE(for_export)){

    # Show every loaded trace (forecast count already capped to n_forecasts)
    p <- plotly::style(p, visible = TRUE)

    # Pin the left y-axis to the full data range (no slider JS to drive it)
    p <- plotly::layout(p, yaxis = list(range = c(0, maxPhase_padded)))

    # Reserve a modest right margin for the auxiliary axis (not the legend,
    # which the export CSS places in its own column)
    p <- plotly::layout(p, margin = list(r = 110))

  }

  # Whole-figure font size (points): global font plus explicit axis and legend
  # fonts, merged into the existing axes so titles and ranges are preserved.
  # Also before build_fullscreen_button(), for the same onRender-hook reason.
  if(!is.null(font_size)){
    p <- plotly::layout(
      p,
      font   = list(size = font_size),
      legend = list(font = list(size = font_size)),
      xaxis  = list(tickfont = list(size = font_size)),
      yaxis  = list(tickfont = list(size = font_size)),
      yaxis2 = list(tickfont = list(size = font_size))
    )
  }

  # Axis tick-label size, independent of font_size (which also drives the axis
  # titles). Sets the modern tickfont.size key explicitly, merged so the axis
  # titles and ranges are preserved. Like the block above, must precede
  # build_fullscreen_button() so its onRender hook is not stripped.
  if(!is.null(tick_size)){
    p <- plotly::layout(
      p,
      xaxis  = list(tickfont = list(size = tick_size)),
      yaxis  = list(tickfont = list(size = tick_size)),
      yaxis2 = list(tickfont = list(size = tick_size))
    )
  }

  # Post-build patches. Axis titles, the truth line, and aux lines all come from
  # places where the value isn't a deferred attribute we can edit pre-build (the
  # axis title font, and the helper-built truth/aux traces), so build once and
  # edit the materialized object. maxPhase (used by the range-slider JS) is
  # preserved. Runs before build_fullscreen_button() so its onRender hook is not
  # stripped.
  if(!is.null(font_size) || !is.null(title_gap) || !is.null(observed_size) ||
     !is.null(observed_color) || !is.null(aux_size) || !is.null(aux_color)){
    .maxPhase <- p$x$maxPhase
    p   <- plotly::plotly_build(p)
    p$x$maxPhase <- .maxPhase

    # Axis title size and standoff: font_size drives the axis title font, and
    # title_gap sets the standoff (px between the tick labels and the title).
    # The deprecated `titlefont` layout key is ignored by current plotly, so
    # set these on the built layout, preserving each title's text.
    if(!is.null(font_size) || !is.null(title_gap)){
      .set_title <- function(ttl, size, gap){
        if(is.null(ttl))      ttl <- list()
        if(is.character(ttl)) ttl <- list(text = ttl)
        if(!is.null(size)){
          if(is.null(ttl$font)) ttl$font <- list()
          ttl$font$size <- size
        }
        if(!is.null(gap)) ttl$standoff <- gap
        ttl
      }
      for(.ax in c("xaxis", "yaxis", "yaxis2")){
        if(!is.null(p$x$layout[[.ax]])){
          p$x$layout[[.ax]]$title <-
            .set_title(p$x$layout[[.ax]]$title, font_size, title_gap)
          # Let plotly grow the margin to fit the (possibly larger) title and
          # tick labels, so the title and numbers don't overlap or get clipped
          # -- standoff alone can't help when the fixed margin is too narrow.
          p$x$layout[[.ax]]$automargin <- TRUE
        }
      }
    }

    # Observed- and aux-line overrides. Merge width/color into each relevant
    # trace's existing line (preserving dash, hover, etc.). The truth line is
    # the first trace; auxiliary traces are those on the secondary y-axis ("y2").
    # aux_color may be one color or a palette (vector) cycled across aux traces.
    if(!is.null(observed_size) || !is.null(observed_color) ||
       !is.null(aux_size) || !is.null(aux_color)){
      .d     <- p$x$data
      .aux_n <- 0L
      for(.i in seq_along(.d)){
        .tr  <- .d[[.i]]
        .aux <- !is.null(.tr$yaxis) && identical(.tr$yaxis, "y2")

        # Truth line = first trace
        if(.i == 1L && (!is.null(observed_size) || !is.null(observed_color))){
          .ln <- .tr$line
          if(is.null(.ln)) .ln <- list()
          if(!is.null(observed_size))  .ln$width <- observed_size
          if(!is.null(observed_color)) .ln$color <- observed_color
          .d[[.i]]$line <- .ln
          if(!is.null(observed_color) && !is.null(.tr$marker)){
            .d[[.i]]$marker$color <- observed_color
          }
        }

        # Auxiliary lines = secondary-axis traces
        if(isTRUE(.aux) && (!is.null(aux_size) || !is.null(aux_color))){
          .aux_n <- .aux_n + 1L
          .ln <- .tr$line
          if(is.null(.ln)) .ln <- list()
          if(!is.null(aux_size)) .ln$width <- aux_size
          if(!is.null(aux_color)){
            # Cycle the palette across aux traces in order
            .col <- aux_color[((.aux_n - 1L) %% length(aux_color)) + 1L]
            .ln$color <- .col
            if(!is.null(.tr$marker)) .d[[.i]]$marker$color <- .col
          }
          .d[[.i]]$line <- .ln
        }
      }
      p$x$data <- .d
    }
  }

  # Fullscreen control (attaches the onRender hook -- must come AFTER the
  # style()/layout() calls above, which would otherwise strip it)
  p <- build_fullscreen_button(p)

  # Static export: build_fullscreen_button() re-enables the modebar, so hide it
  # again here. config() does not drop onRender hooks, so it is safe after.
  if(isTRUE(for_export)){
    p <- plotly::config(p, displayModeBar = FALSE)
  }

  # Return the plot and its truth-line hover label
  list(plot = p, hover_text = hover_text)
}
