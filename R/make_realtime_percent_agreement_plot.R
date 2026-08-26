#' Build the percent-agreement-over-time plotly for one location
#'
#' For a single location, builds the interactive figure shown in the Percent
#' Agreement section: observed counts as light bars on a right axis, one dashed
#' line per forecast horizon (hidden by default, toggled from the legend), and a
#' solid black overall-median line on the left axis. Non-transmission months
#' (May-July) are broken out of the median and horizon lines and marked with
#' dotted boundary lines and "No Evaluation" annotations. A legend-click handler
#' toggles the right y-axis with the Observed trace, and the fullscreen button
#' is attached before returning.
#'
#' @param data Percent-agreement data frame from
#'   `percentAgreementCalculation()`.
#'   Must contain `location`, `target_end_date`, `Observed`, `horizon`, `value`,
#'   and `per_agreement`.
#' @param loc Character. Raw location code to filter `data` to (matches
#'   `data$location`).
#' @param outcome Character. Outcome label used for the right-axis title and the
#'   hover tooltips.
#'
#' @return A plotly htmlwidget for the location, with the right-axis toggle and
#'   fullscreen button attached.
#'
#' @keywords internal
#' @noRd
make_realtime_percent_agreement_plot <- function(data, loc, outcome) {

#------------------------------------------------------------------------------#
# Preparing the input data for the plot ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the percent agreement data for the selected      #
# location, prepares the observed data for the right y axis, identifies unique #
# horizons, assigns horizon colors, and computes the overall median percent    #
# agreement per target end date for the main line.                             #
#------------------------------------------------------------------------------#

  ##############################
  # Filtering for the location #
  ##############################
  loc_data <- data %>%
    dplyr::filter(location == loc)

  ################################################
  # Preparing observed data for the right y axis #
  ################################################
  observed_data <- loc_data %>%
    dplyr::distinct(target_end_date, .keep_all = TRUE) %>%
    dplyr::select(target_end_date, Observed, is_transmission) %>%
    dplyr::arrange(target_end_date) %>%
    dplyr::mutate(
      is_no_eval = !is_transmission,
      hover_text = dplyr::if_else(
        is_no_eval,
        paste0(
          "<b>Observed Data</b><br>",
          "Date: ", format(target_end_date, "%b %d, %Y"), "<br>",
          outcome, ": ", Observed, "<br>",
          "<br>",
          "\u24d8 <b>No Evaluation Period</b><br>",
          "Target end dates between May 1 and July 31<br>",
          "are excluded from all summary statistics,<br>",
          "corresponding to the non-transmission season<br>",
          "for the pathogens of interest. Row-level values<br>",
          "are retained for plotting."
        ),
        paste0(
          "<b>Observed Data</b><br>",
          "Date: ", format(target_end_date, "%b %d, %Y"), "<br>",
          outcome, ": ", Observed
        )
      )
    )

  ###########################
  # Getting unique horizons #
  ###########################

  # Distinct horizons in ascending order
  horizons   <- sort(unique(loc_data$horizon))

  # Number of horizon traces to draw
  n_horizons <- length(horizons)

  ##########################################################
  # Setting horizon colors using RColorBrewer Set1 palette #
  ##########################################################
  horizon_colors <- setNames(

    # Handling <= 9 Horizons
    if(n_horizons <= 9){RColorBrewer::brewer.pal(max(3, n_horizons), "Set1")[1:n_horizons]

    # Handling >9 Horizons
    }else{colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_horizons)},

    # Color to character
    as.character(horizons)
  )

  ##########################################################
  # Hover value formatter — renders a dash when a value is #
  # missing so tooltips never print NA or null             #
  ##########################################################
  fmt_hover_val <- function(v) {
    ifelse(is.na(v), "\u2014", as.character(v))
  }

#------------------------------------------------------------------------------#
# Computing the overall median line --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section computes the median percent agreement and median         #
# forecasted value across all horizons for each location and target end date   #
# excluding May through July. Both are used in the hover tooltip.              #
#------------------------------------------------------------------------------#

  ########################################################
  # Computing median per date excluding May through July #
  ########################################################
  overall_median <- loc_data %>%
    dplyr::group_by(location, target_end_date) %>%
    dplyr::summarise(
      median_agreement = dplyr::if_else(
        !is_transmission[1],
        NA_real_,
        median(per_agreement, na.rm = TRUE)
      ),
      median_value     = median(value, na.rm = TRUE),
      observed_count   = dplyr::first(Observed),
      .groups          = "drop"
    ) %>%
    dplyr::arrange(target_end_date) %>%
    dplyr::mutate(
      hover_text = dplyr::if_else(
        is.na(median_agreement),
        NA_character_,
        paste0(
          "<b>Overall Median Percent Agreement</b><br>",
          "Date: ", format(target_end_date, "%b %d, %Y"), "<br>",
          "Median Agreement: ", round(median_agreement, 1), "%<br>",
          "Median Forecasted ", outcome, ": ", round(median_value, 1), "<br>",
          "Observed ", outcome, ": ", fmt_hover_val(observed_count)
        )
      )
    )

#------------------------------------------------------------------------------#
# Building the non-transmission period shapes and annotations ------------------
#------------------------------------------------------------------------------#
# About: This section marks each contiguous non-transmission (No Evaluation)   #
# span using the `is_transmission` flag carried in the data rather than any    #
# hardcoded month. It draws dotted boundary lines on either side of each span  #
# and centers a bold No Evaluation label with an info circle inside it.        #
#------------------------------------------------------------------------------#

  ##################################
  # Distinct dates with their flag #
  ##################################

  # One row per target end date, in order, carrying the transmission flag
  date_flags <- loc_data %>%
    dplyr::distinct(target_end_date, is_transmission) %>%
    dplyr::arrange(target_end_date)

  ####################################
  # Contiguous non-transmission runs #
  ####################################

  # Run-length encode the non-transmission flag to find contiguous spans
  nt_rle    <- rle(!date_flags$is_transmission)

  # Start indices of every run
  nt_ends   <- cumsum(nt_rle$lengths)

  # End indices of every run
  nt_starts <- nt_ends - nt_rle$lengths + 1

  # Keep only the runs flagged non-transmission (No Evaluation spans)
  nt_runs   <- which(nt_rle$values)

  ###########################
  # Convenience date vector #
  ###########################

  # Sorted distinct dates
  d   <- date_flags$target_end_date

  # Count of distinct dates
  n_d <- length(d)

  # Half the bar width (tightest date spacing) so lines sit on the bar edges
  half_bar <- if(n_d > 1) as.numeric(min(diff(d))) / 2 else 3.5

  #################################
  # Building vertical line shapes #
  #################################
  all_shapes <- unlist(lapply(nt_runs, function(k){

    # First date index of this span
    i0 <- nt_starts[k]

    # Last date index of this span
    i1 <- nt_ends[k]

    # Left boundary: left edge of the first non-transmission bar
    left  <- d[i0] - half_bar

    # Right boundary: right edge of the last non-transmission bar
    right <- d[i1] + half_bar

    # One dotted vertical line at each boundary of the span
    lapply(c(left, right), function(xb) {
      list(
        type  = "line",
        layer = "above",
        xref  = "x",
        yref  = "paper",
        x0    = format(as.Date(xb), "%Y-%m-%d"),
        x1    = format(as.Date(xb), "%Y-%m-%d"),
        y0    = 0,
        y1    = 1,
        line  = list(
          color = "rgba(150, 150, 150, 0.3)",
          width = 1.2,
          dash  = "dot"
        )
      )
    })
  }), recursive = FALSE)

  ##################################################
  # Building no evaluation labels with info circle #
  ##################################################
  no_eval_annotations <- lapply(nt_runs, function(k){

    # First date index of this span
    i0 <- nt_starts[k]

    # Last date index of this span
    i1 <- nt_ends[k]

    # Center the label within the span
    mid_date <- d[i0] + (d[i1] - d[i0]) / 2

    # Building info circle for non-transmission zone
    list(
      x         = format(as.Date(mid_date), "%Y-%m-%d"),
      y         = 0.97,
      xref      = "x",
      yref      = "paper",
      text      = "<b>No Evaluation</b> \u24d8",
      showarrow = FALSE,
      font      = list(size = 12, color = "rgba(150, 150, 150, 1)"),
      xanchor   = "center",
      yanchor   = "top"
    )
  })

#------------------------------------------------------------------------------#
# Computing right y axis maximum -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the information needed to accuratly get the     #
# right y axis squared away with the left y axis.                              #
#------------------------------------------------------------------------------#

  # Headroom above the largest observed count
  y2_max    <- max(observed_data$Observed, na.rm = TRUE) * 1.1

  # Candidate tick positions across the range
  y2_ticks  <- pretty(c(0, y2_max), n = 5)

  # Drop any tick that sits above the axis maximum
  y2_ticks  <- y2_ticks[y2_ticks <= y2_max]

  # Blank the zero label so it does not collide with the left axis
  y2_labels <- ifelse(y2_ticks == 0, "", as.character(round(y2_ticks)))

#------------------------------------------------------------------------------#
# Building the plotly figure ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the plotly figure in layer order: an invisible    #
# anchor scatter on yaxis to ensure unified hover snaps to every date,         #
# observed bars in light Clemson pastel purple on the right y axis, horizon    #
# dashed lines (hidden by default), and the overall median solid line on top.  #
# Every hover shows observed first, followed by the no-eval message for May    #
# through July dates.                                                          #
#------------------------------------------------------------------------------#

  #########################
  # Initializing the plot #
  #########################
  p <- plotly::plot_ly(height = 700)

  # Adding invisible anchor scatter on yaxis
  p <- p %>%
    plotly::add_trace(
      data       = observed_data,
      x          = ~target_end_date,
      y          = rep(0, nrow(observed_data)),
      type       = "scatter",
      mode       = "markers",
      name       = "anchor",
      yaxis      = "y",
      showlegend = FALSE,
      hoverinfo  = "skip",
      marker     = list(
        color   = "rgba(0, 0, 0, 0)",
        size    = 1,
        opacity = 0
      )
    )

  # Adding observed bars right axis i
  p <- p %>%
    plotly::add_trace(
      data          = observed_data,
      x             = ~target_end_date,
      y             = ~Observed,
      type          = "bar",
      name          = "Observed Data",
      visible       = "legendonly",
      yaxis         = "y2",
      marker        = list(
        color = "rgba(201, 184, 232, 0.2)",
        line  = list(
          color = "rgba(201, 184, 232, 0.2)",
          width = 0.5
        )
      ),
      text          = ~hover_text,
      textposition  = "none",
      hovertemplate = "%{text}<extra></extra>",
      hoverlabel    = list(
        bgcolor     = "#f7f4fc",
        bordercolor = "#C9B8E8",
        font        = list(color = "#522D80")
      )
    )

  ################################################
  # Looping through horizons (hidden by default) #
  ################################################
  for (h in horizons) {

    #################################
    # Filtering for current horizon #
    #################################
    h_data <- loc_data %>%
      dplyr::filter(horizon == h) %>%
      dplyr::mutate(per_agreement = dplyr::if_else(
        !is_transmission,
        NA_real_,
        per_agreement
      )) %>%
      dplyr::arrange(target_end_date)

    # Handling a single horizon point
    h_mode <- if(sum(!is.na(h_data$per_agreement)) <= 1) "markers" else "lines"

    # Forecasted and observed counts paired for the hover tooltip
    h_customdata <- unname(Map(
      c,
      fmt_hover_val(h_data$value),
      fmt_hover_val(h_data$Observed)
    ))

    ##############################
    # Adding horizon dashed line #
    ##############################
    p <- p %>%
      plotly::add_trace(
        data          = h_data,
        x             = ~target_end_date,
        y             = ~per_agreement,
        type          = "scatter",
        mode          = h_mode,
        name          = paste0("Horizon ", h),
        yaxis         = "y",
        connectgaps   = FALSE,
        customdata    = h_customdata,
        line          = list(
          color = horizon_colors[as.character(h)],
          width = 1.0,
          dash  = "dash"
        ),
        marker        = list(
          color = horizon_colors[as.character(h)],
          size  = 7
        ),
        hovertemplate = paste0(
          "<b>Horizon ", h, " Percent Agreement</b><br>",
          "Date: %{x}<br>",
          "Percent Agreement: %{y:.1f}%<br>",
          "Forecasted ", outcome, ": %{customdata[0]}<br>",
          "Observed ", outcome, ": %{customdata[1]}",
          "<extra></extra>"
        )
      )
  }

  ########################################################
  # Adding overall median line (on top, breaks May-July) #
  ########################################################
  p <- p %>%
    plotly::add_trace(
      data          = overall_median,
      x             = ~target_end_date,
      y             = ~median_agreement,
      type          = "scatter",
      mode          = "lines",
      name          = "Overall Median Percent Agreement",
      yaxis         = "y",
      connectgaps   = FALSE,
      customdata    = ~median_value,
      text          = ~hover_text,
      line          = list(
        color = "#333333",
        width = 2.0,
        dash  = "solid"
      ),
      hovertemplate = "%{text}<extra></extra>"
    )

#------------------------------------------------------------------------------#
# Setting the plotly layout ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the layout including axis labels, legend position,  #
# hover mode, background colors, margins, title positioning, vertical line     #
# shapes, and No Evaluation annotations with info circle. The right y axis is  #
# toggled via JS when the Observed trace is clicked in the legend.             #
#------------------------------------------------------------------------------#

  #############################
  # Setting the plotly layout #
  #############################
  p <- p %>%
    plotly::layout(
      barmode     = "overlay",

      # Plot title
      title       = list(
        text    = "",
        font    = list(size = 17),
        x       = 0.5,
        xanchor = "center",
        y       = 0.98,
        yanchor = "top"
      ),

      # X axis
      xaxis       = list(
        title             = "",
        tickformat        = "%b %d, %Y",
        showgrid          = TRUE,
        gridcolor         = "#f8f8f8",
        zeroline          = FALSE,
        automargin        = TRUE,
        ticklabelstandoff = 10,
        range             = list(
          min(loc_data$target_end_date) - 14,
          max(loc_data$target_end_date) + 14
        )
      ),

      # Left Y axis
      yaxis       = list(
        title      = "Percent Agreement (%)",
        titlefont  = list(size = 16),
        showgrid   = TRUE,
        gridcolor  = "#f0f0f0",
        zeroline   = FALSE,
        automargin = TRUE,
        range      = list(-0.5, 105),
        tickvals   = c(0, 20, 40, 60, 80, 100),
        tickfont   = list(size = 12)
      ),

      # Right Y Axis
      yaxis2      = list(
        visible           = FALSE,
        title      = list(
          text     = outcome,
          standoff = 20,
          font     = list(size = 16)
        ),
        overlaying        = "y",
        side              = "right",
        showgrid          = FALSE,
        zeroline          = FALSE,
        automargin        = TRUE,
        ticklabelstandoff = 10,
        range             = list(0, y2_max),
        tickvals          = y2_ticks,
        ticktext          = y2_labels,
        tickfont          = list(size = 12)
      ),

      # Legend
      legend      = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.14
      ),

      # General plot setting
      hovermode     = "closest",
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff",
      font          = list(family = "sans-serif", color = "#555"),
      margin        = list(l = 40, r = 100, t = 40, b = 120),
      shapes        = all_shapes,
      annotations   = no_eval_annotations
    )

#------------------------------------------------------------------------------#
# Adding the fullscreen button and right axis toggle ---------------------------
#------------------------------------------------------------------------------#
# About: This section adds the fullscreen button and a plotly_restyle listener #
# that toggles the right y axis visibility immediately when the Observed trace #
# is clicked in the legend. plotly_restyle fires after the trace has been      #
# updated so the correct new visibility state is read with no delay.           #
#------------------------------------------------------------------------------#

  ############################################################################
  # Wrapping with fs-wrap class and adding right axis toggle on legend click #
  ############################################################################
  p <- htmlwidgets::onRender(p, htmlwidgets::JS("
    function(el, x) {

      // Wrap with fs-wrap class for fullscreen
      var parent = el.closest('.plotly-widget-container') || el.parentElement;
      if (parent && !parent.classList.contains('fs-wrap')) {
        parent.classList.add('fs-wrap');
      }

      // Toggle right y axis visibility with Observed trace
      // plotly_restyle fires after trace update so visibility is correct
      el.on('plotly_restyle', function(data) {
        if (!data || !data[0]) return;
        var traceIdxs = data[1];

        // Only respond when Observed trace (index 1) was restyled
        // Note: index is 1 because anchor trace is index 0
        if (traceIdxs && traceIdxs.indexOf(1) === -1) return;

        // Read new visibility state of Observed trace
        var observedTrace    = el.data[1];
        var currentlyVisible = observedTrace.visible === true ||
                               observedTrace.visible === undefined;
        Plotly.relayout(el, {
          'yaxis2.visible':        currentlyVisible,
          'yaxis2.showticklabels': currentlyVisible
        });
      });
    }
  "))

  ################################
  # Adding the fullscreen button #
  ################################
  p <- build_fullscreen_button(p)

  # Making the plot responsive
  p$x$config$responsive <- TRUE

  # Fullscreen exit restore for a responsive plot
  p <- htmlwidgets::onRender(p, htmlwidgets::JS("
    function(el, x) {
      var gd = el;
      gd.__forceNormalSize = function() {
        if (!gd._fullLayout) return;
        var host = gd.closest('.plot-panel') || gd.parentElement;
        var w = (host && host.clientWidth) ? host.clientWidth : 900;
        gd.style.width  = w + 'px';
        gd.style.height = '700px';
        Plotly.relayout(gd, { autosize: false, width: w, height: 700 }).then(function() {
          if (gd.offsetParent !== null) Plotly.Plots.resize(gd);
        });
      };
    }
  "))

  ######################
  # Returning the plot #
  ######################
  return(p)

}
