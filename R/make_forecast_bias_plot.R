#' Build the forecast bias plot for one location
#'
#' Builds the interactive Plotly forecast bias figure for a single location,
#' with a Bias (%) / Raw Counts toggle. The figure overlays observed-count
#' bars, one dashed line per forecast horizon, and an overall median line, and
#' shades each non-transmission (No Evaluation) span with dotted boundaries and
#' a centered label. Row-level values on non-transmission dates are retained
#' for visual continuity but broken out of the lines with an all-NA scaffold
#' so the series do not connect across them.
#'
#' Parallel percent-bias and raw-error traces back the toggle, and an onRender
#' callback swaps trace visibility and axis titles per mode while preserving
#' legend selections. A fullscreen button is attached before the widget is
#' returned.
#'
#' @param data Forecast bias evaluation frame for the report, carrying
#'   row-level `location`, `target_end_date`, `Observed`, `is_transmission`,
#'   `horizon`, `pct_error`, `raw_error`, `value`, and `is_stable`.
#' @param loc Single `location` code to filter `data` to; one figure is built
#'   per location by the calling section.
#' @param outcome Character label for the outcome, used in the right-axis
#'   title, the Raw Counts axis title, and the hover tooltips.
#'
#' @return A Plotly htmlwidget with the Bias (%) / Raw Counts toggle and the
#'   fullscreen button attached.
#'
#' @keywords internal
#' @noRd
make_forecast_bias_plot <- function(data, loc, outcome) {

#------------------------------------------------------------------------------#
# Preparing the input data for the plot ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the forecast bias data for the selected          #
# location, prepares the observed counts and their hover text, identifies the  #
# unique horizons, and assigns a color to each horizon.                        #
#------------------------------------------------------------------------------#

  ##############################
  # Filtering for the location #
  ##############################
  loc_data <- data %>%
    dplyr::filter(location == loc)

  ########################################################
  # Observed data — all dates retained including no-eval #
  ########################################################
  observed_data <- loc_data %>%
    dplyr::distinct(target_end_date, .keep_all = TRUE) %>%
    dplyr::select(target_end_date, Observed, is_transmission) %>%
    dplyr::arrange(target_end_date) %>%
    dplyr::mutate(
      is_no_eval     = !is_transmission,
      bar_color      = dplyr::if_else(
        is_no_eval,
        "rgba(201, 184, 232, 0.35)",
        "rgba(201, 184, 232, 0.35)"
      ),
      bar_line_color = dplyr::if_else(
        is_no_eval,
        "rgba(201, 184, 232, 0.5)",
        "rgba(201, 184, 232, 0.5)"
      ),
      hover_text     = dplyr::if_else(
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

    # Handling less then or equal to 9 horizons
    if (n_horizons <= 9) {RColorBrewer::brewer.pal(max(3, n_horizons), "Set1")[1:n_horizons]

    # Handling more than 9 horizons
    } else {colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_horizons)},

    # Setting the character for the horzions
    as.character(horizons)
  )

#------------------------------------------------------------------------------#
# Building NA scaffold for no-eval dates ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section collects the non-transmission (No Evaluation) dates and  #
# builds an all-NA scaffold so those dates break the horizon and median lines  #
# instead of connecting across them.                                           #
#------------------------------------------------------------------------------#

  #####################################################
  # Dates flagged as non-transmission (No Evaluation) #
  #####################################################
  no_eval_dates <- observed_data %>%
    dplyr::filter(is_no_eval) %>%
    dplyr::pull(target_end_date)

  ########################################################
  # All-NA rows so the lines break instead of connecting #
  ########################################################
  na_scaffold <- if (length(no_eval_dates) > 0) {
    data.frame(
      target_end_date  = no_eval_dates,
      pct_error        = NA_real_,
      raw_error        = NA_real_,
      value            = NA_real_,
      is_stable        = FALSE,
      is_transmission  = FALSE,
      stringsAsFactors = FALSE
    )

  # No evaluation date present
  } else {NULL}

#------------------------------------------------------------------------------#
# Computing the overall median lines -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section computes the median percent bias and median raw error    #
# across all horizons for each target end date, excluding non-transmission     #
# dates from the summaries. Both feed the overall median hover tooltips.       #
#------------------------------------------------------------------------------#

  #######################################################
  # Computing the overall median: More than one horizon #
  #######################################################
  if (n_horizons > 1) {

    # Median pct and raw error per date, excluding non-transmission
    overall_median <- loc_data %>%
      dplyr::group_by(location, target_end_date) %>%
      dplyr::summarise(
        is_no_eval       = !any(is_transmission),
        median_pct_error = dplyr::if_else(
          !any(is_transmission),
          NA_real_,
          median(pct_error[is_transmission], na.rm = TRUE)
        ),
        median_raw_error = dplyr::if_else(
          !any(is_transmission),
          NA_real_,
          median(raw_error[is_transmission], na.rm = TRUE)
        ),
        median_value     = median(value[is_transmission], na.rm = TRUE),
        observed_count   = dplyr::first(Observed),
        .groups          = "drop"
      ) %>%
      dplyr::arrange(target_end_date) %>%
      dplyr::mutate(
        hover_pct = dplyr::if_else(
          is.na(median_pct_error),
          NA_character_,
          paste0(
            "<b>Overall Median Forecast Bias</b><br>",
            "Date: ", format(target_end_date, "%b %d, %Y"), "<br>",
            "Median Bias: ", round(median_pct_error, 1), "%<br>",
            "Median ", outcome, ": ", round(median_value, 1)
          )
        ),
        hover_raw = dplyr::if_else(
          is.na(median_raw_error),
          NA_character_,
          paste0(
            "<b>Overall Median Forecast Bias</b><br>",
            "Date: ", format(target_end_date, "%b %d, %Y"), "<br>",
            "Median Raw Error: ", round(median_raw_error, 1), "<br>",
            "Median ", outcome, ": ", round(median_value, 1)
          )
        )
      )

    # Only append scaffold rows for dates not already in the summarised output
    if (!is.null(na_scaffold)) {

      # Scaffold dates not already present in the summary
      missing_dates <- na_scaffold$target_end_date[
        !na_scaffold$target_end_date %in% overall_median$target_end_date
      ]

      # Append all-NA rows only when there are missing dates
      if (length(missing_dates) > 0) {

        # Add the all-NA scaffold rows
        overall_median <- overall_median %>%
          dplyr::bind_rows(
            data.frame(
              location         = loc,
              target_end_date  = missing_dates,
              is_no_eval       = TRUE,
              median_pct_error = NA_real_,
              median_raw_error = NA_real_,
              median_value     = NA_real_,
              observed_count   = NA_real_,
              hover_pct        = NA_character_,
              hover_raw        = NA_character_,
              stringsAsFactors = FALSE
            )
          )
      }
    }

    # Keep dates in chronological order
    overall_median <- overall_median %>%
      dplyr::arrange(target_end_date)
  }

#------------------------------------------------------------------------------#
# Non-transmission period shapes and annotations -------------------------------
#------------------------------------------------------------------------------#
# About: This section marks each contiguous non-transmission (No Evaluation)   #
# span using the `is_transmission` flag carried in the data rather than any    #
# hardcoded month. It draws dotted boundary lines on the bar edges of each     #
# span, centers a No Evaluation label in each, and adds a darker zero          #
# reference line.                                                              #
#------------------------------------------------------------------------------#

  #########################################################
  # Distinct dates with their transmission flag, in order #
  #########################################################
  date_flags <- observed_data %>%
    dplyr::distinct(target_end_date, is_transmission) %>%
    dplyr::arrange(target_end_date)

  ####################################################
  # Contiguous non-transmission (No Evaluation) runs #
  ####################################################

  # Run-length encoding of the No Evaluation flag
  nt_rle    <- rle(!date_flags$is_transmission)

  # End index of each run
  nt_ends   <- cumsum(nt_rle$lengths)

  # Start index of each run
  nt_starts <- nt_ends - nt_rle$lengths + 1

  # Which runs are flagged No Evaluation
  nt_runs   <- which(nt_rle$values)

  #######################################################
  # Sorted dates and half-bar offset for the span edges #
  #######################################################

  # Sorted target end dates
  d        <- date_flags$target_end_date

  # Number of dates
  n_d      <- length(d)

  # Half the bar width so the lines sit on the bar edges
  half_bar <- if (n_d > 1) as.numeric(min(diff(d))) / 2 else 3.5

  ###################################################
  # Dotted boundary line at each edge of every span #
  ###################################################
  all_shapes <- unlist(lapply(nt_runs, function(k) {
    i0    <- nt_starts[k]
    i1    <- nt_ends[k]
    left  <- d[i0] - half_bar
    right <- d[i1] + half_bar
    lapply(c(left, right), function(xb) {
      list(
        type  = "line", layer = "above",
        xref  = "x",   yref  = "paper",
        x0    = format(as.Date(xb), "%Y-%m-%d"),
        x1    = format(as.Date(xb), "%Y-%m-%d"),
        y0    = 0, y1 = 1,
        line  = list(
          color = "rgba(150, 150, 150, 0.3)",
          width = 1.2,
          dash  = "dot"
        )
      )
    })
  }), recursive = FALSE)

  #########################################
  # Centered No Evaluation label per span #
  #########################################
  no_eval_annotations <- lapply(nt_runs, function(k) {
    i0       <- nt_starts[k]
    i1       <- nt_ends[k]
    mid_date <- d[i0] + (d[i1] - d[i0]) / 2
    list(
      x = format(as.Date(mid_date), "%Y-%m-%d"), y = 0.97,
      xref = "x", yref = "paper",
      text = "<b>No Evaluation</b> \u24d8",
      showarrow = FALSE,
      font = list(size = 12, color = "rgba(150, 150, 150, 1)"),
      xanchor = "center", yanchor = "top"
    )
  })

  ##########################################
  # Darker zero reference line at bias = 0 #
  ##########################################
  all_shapes <- c(
    all_shapes,
    list(list(
      type  = "line", xref = "paper", yref = "y",
      x0    = 0, x1 = 1, y0 = 0, y1 = 0,
      layer = "above",
      line  = list(color = "rgba(80, 80, 80, 0.75)", width = 1, dash = "solid")
    ))
  )

#------------------------------------------------------------------------------#
# Undefined percent-bias markers (zero / missing observed) ---------------------
#------------------------------------------------------------------------------#
# About: Within the evaluation season, percent bias is undefined whenever the  #
# observed count is 0 (division by zero) or unavailable, so no point is drawn   #
# on the Bias (%) view for that date. Rather than leave an unexplained gap,     #
# each such date is marked with a subtle dotted vertical line, a small info     #
# glyph, and an explanatory hover note, plus one shared footnote. The wording   #
# stays accurate in both Bias (%) and Raw Counts views (the count is 0 either   #
# way), so these are added to the static layout and need no toggle handling.    #
#------------------------------------------------------------------------------#

  #########################################################
  # Evaluation-season dates where percent bias is undefined #
  #########################################################

  # In-season dates whose observed count is 0 or unavailable
  gap_dates <- observed_data %>%
    dplyr::filter(is_transmission & (is.na(Observed) | Observed == 0)) %>%
    dplyr::pull(target_end_date)

  # De-duplicate and order
  gap_dates <- sort(unique(gap_dates))

  ##################################################
  # Only build markers when such a date is present #
  ##################################################
  if (length(gap_dates) > 0) {

    #####################################################
    # Subtle dotted vertical line at each undefined date #
    #####################################################
    gap_shapes <- lapply(gap_dates, function(gap_date) {
      list(
        type  = "line", layer = "below",
        xref  = "x",    yref  = "paper",
        x0    = format(as.Date(gap_date), "%Y-%m-%d"),
        x1    = format(as.Date(gap_date), "%Y-%m-%d"),
        y0    = 0, y1 = 1,
        line  = list(color = "rgba(176, 137, 74, 0.55)", width = 1.1, dash = "dot")
      )
    })
    all_shapes <- c(all_shapes, gap_shapes)

    ############################################################
    # Small info glyph + hover note above each undefined date  #
    ############################################################
    gap_markers <- lapply(gap_dates, function(gap_date) {
      list(
        x = format(as.Date(gap_date), "%Y-%m-%d"), y = 0.90,
        xref = "x", yref = "paper",
        text = "\u24d8",
        hovertext = paste0(
          "<b>Percent bias not shown</b><br>",
          "Observed ", outcome, " is 0 or unavailable on this date, so<br>",
          "percent bias is undefined (it divides by the observed value).<br>",
          "The forecast still appears in the <b>Raw Counts</b> view."
        ),
        showarrow = FALSE,
        captureevents = TRUE,
        font = list(size = 12, color = "rgba(150, 118, 60, 1)"),
        xanchor = "center", yanchor = "middle"
      )
    })

    ######################################################
    # One shared footnote explaining the dotted markers  #
    ######################################################
    gap_footnote <- list(list(
      x = 0, y = -0.22,
      xref = "paper", yref = "paper",
      text = paste0(
        "\u24d8 Dotted marks: weeks with 0 or unavailable observed counts, ",
        "where percent bias is undefined. These weeks still appear in Raw Counts."
      ),
      showarrow = FALSE,
      align = "left",
      font = list(size = 10, color = "rgba(120, 120, 120, 1)"),
      xanchor = "left", yanchor = "top"
    ))

    # Append markers + footnote to the existing annotation set
    no_eval_annotations <- c(no_eval_annotations, gap_markers, gap_footnote)

  }

#------------------------------------------------------------------------------#
# Computing y axis ranges ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section computes the y axis ranges for both display modes:       #
# the symmetric Bias (%) left axis with observed counts on the right, and      #
# the symmetric Raw Counts left axis. It also derives the unique plot ID.      #
#------------------------------------------------------------------------------#

  ##########################################################
  # Bias (%) mode — symmetric left axis, observed on right #
  ##########################################################

  # Right axis headroom above the largest observed count
  y2_max    <- max(observed_data$Observed, na.rm = TRUE) * 1.1

  # Candidate tick positions across the range
  y2_ticks  <- pretty(c(0, y2_max), n = 5)

  # Drop any tick above the maximum
  y2_ticks  <- y2_ticks[y2_ticks <= y2_max]

  # Blank the zero label so it does not collide with the left axis
  y2_labels <- ifelse(y2_ticks == 0, "", as.character(round(y2_ticks)))

  # Symmetric bias-% bound: 1.1x the largest transmission-season magnitude
  y1_pct_abs <- max(abs(loc_data$pct_error[loc_data$is_transmission]), na.rm = TRUE) * 1.1

  # Never tighter than +/- 20%
  y1_pct_abs <- max(y1_pct_abs, 20)

  #####################################################################
  # Raw Counts mode — symmetric left axis fits observed and raw error #
  #####################################################################
  y1_raw_sym <- max(
    max(observed_data$Observed, na.rm = TRUE),
    max(abs(loc_data$raw_error[loc_data$is_transmission]), na.rm = TRUE)
  ) * 1.1

  ############################################
  # Unique plot ID derived from the location #
  ############################################
  plot_id <- paste0("biasPlot_", gsub("[^A-Za-z0-9]", "_", loc))

#------------------------------------------------------------------------------#
# Building the plotly figure ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the figure in layer order: an invisible anchor    #
# scatter so unified hover snaps to every date, observed bars (right axis in   #
# Bias (%) mode, left axis in Raw Counts mode), one dashed line per horizon,   #
# and the overall median line. Parallel pct and raw traces back the Bias (%) / #
# Raw Counts toggle.                                                           #
#------------------------------------------------------------------------------#

  # Initializing the plot
  p <- plotly::plot_ly(height = 700, width = 950)

  ##########################
  # Invisible anchor trace #
  ##########################
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

  ################################################################
  # Observed bars — Bias (%) mode, right axis, hidden by default #
  ################################################################
  p <- p %>%
    plotly::add_trace(
      data          = observed_data,
      x             = ~target_end_date,
      y             = ~Observed,
      type          = "bar",
      name          = "Observed Data",
      yaxis         = "y2",
      visible       = "legendonly",
      legendgroup   = "observed",
      showlegend    = TRUE,
      marker        = list(
        color = observed_data$bar_color,
        line  = list(color = observed_data$bar_line_color, width = 0.5)
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

  #################################################################
  # Observed bars — Raw Counts mode, left axis, hidden by default #
  #################################################################
  p <- p %>%
    plotly::add_trace(
      data          = observed_data,
      x             = ~target_end_date,
      y             = ~Observed,
      type          = "bar",
      name          = "Observed Data",
      yaxis         = "y",
      visible       = FALSE,
      legendgroup   = "observed",
      showlegend    = TRUE,
      marker        = list(
        color = observed_data$bar_color,
        line  = list(color = observed_data$bar_line_color, width = 0.5)
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

  ################################################################
  # Horizon percent-bias lines — Bias (%) mode, shown by default #
  ################################################################
  for (h in horizons) {

    # Per-horizon series with non-transmission set to NA
    h_data <- loc_data %>%
      dplyr::filter(horizon == h) %>%
      dplyr::mutate(
        pct_error = dplyr::if_else(is_transmission, pct_error, NA_real_),
        raw_error = dplyr::if_else(is_transmission, raw_error, NA_real_)
      ) %>%
      dplyr::select(target_end_date, pct_error, raw_error, value, is_stable, is_transmission) %>%
      dplyr::bind_rows(na_scaffold) %>%
      dplyr::arrange(target_end_date)

    # Show all horizons by default
    h_pct_visible <- TRUE

    # Single usable point renders as markers; otherwise a dashed line
    h_mode <- if (sum(!is.na(h_data$pct_error)) <= 1) "markers" else "lines"

    # Adding the horizon percent-bias trace
    p <- p %>%
      plotly::add_trace(
        data          = h_data,
        x             = ~target_end_date,
        y             = ~pct_error,
        type          = "scatter",
        mode          = h_mode,
        name          = paste0("Horizon ", h),
        yaxis         = "y",
        visible       = h_pct_visible,
        connectgaps   = FALSE,
        legendgroup   = paste0("h_", h),
        showlegend    = TRUE,
        customdata    = ~value,
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
          "<b>Horizon ", h, " Forecast Bias</b><br>",
          "Date: %{x}<br>",
          "Bias: %{y:.1f}%<br>",
          outcome, ": %{customdata}",
          "<extra></extra>"
        )
      )
  }

  #####################################################
  # Horizon raw-error lines — Raw Counts mode, hidden #
  #####################################################
  for (h in horizons) {

    # Per-horizon series with non-transmission set to NA
    h_data <- loc_data %>%
      dplyr::filter(horizon == h) %>%
      dplyr::mutate(
        pct_error = dplyr::if_else(is_transmission, pct_error, NA_real_),
        raw_error = dplyr::if_else(is_transmission, raw_error, NA_real_)
      ) %>%
      dplyr::select(target_end_date, pct_error, raw_error, value, is_stable, is_transmission) %>%
      dplyr::bind_rows(na_scaffold) %>%
      dplyr::arrange(target_end_date)

    # Single usable point renders as markers; otherwise a dashed line
    h_mode_raw <- if (sum(!is.na(h_data$raw_error)) <= 1) "markers" else "lines"

    # Adding the horizon raw-error trace
    p <- p %>%
      plotly::add_trace(
        data          = h_data,
        x             = ~target_end_date,
        y             = ~raw_error,
        type          = "scatter",
        mode          = h_mode_raw,
        name          = paste0("Horizon ", h),
        yaxis         = "y",
        visible       = FALSE,
        connectgaps   = FALSE,
        legendgroup   = paste0("h_", h),
        showlegend    = TRUE,
        customdata    = ~value,
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
          "<b>Horizon ", h, " Forecast Bias</b><br>",
          "Date: %{x}<br>",
          "Raw Error: %{y:.1f}<br>",
          outcome, ": %{customdata}",
          "<extra></extra>"
        )
      )
  }

  ###################################################################
  # Overall median percent-bias line — Bias (%) mode, if >1 horizon #
  ###################################################################
  if (n_horizons > 1) {

    # Adding the overall median percent-bias trace
    p <- p %>%
      plotly::add_trace(
        data          = overall_median,
        x             = ~target_end_date,
        y             = ~median_pct_error,
        type          = "scatter",
        mode          = "lines",
        name          = "Overall Median Forecast Bias",
        yaxis         = "y",
        visible       = TRUE,
        connectgaps   = FALSE,
        legendgroup   = "overall",
        showlegend    = TRUE,
        customdata    = ~median_value,
        text          = ~hover_pct,
        line          = list(color = "#333333", width = 2.0, dash = "solid"),
        hovertemplate = "%{text}<extra></extra>"
      )

  ###########################################################
  # Overall median raw-error line — Raw Counts mode, hidden #
  ###########################################################
    p <- p %>%
      plotly::add_trace(
        data          = overall_median,
        x             = ~target_end_date,
        y             = ~median_raw_error,
        type          = "scatter",
        mode          = "lines",
        name          = "Overall Median Forecast Bias",
        yaxis         = "y",
        visible       = FALSE,
        connectgaps   = FALSE,
        legendgroup   = "overall",
        showlegend    = TRUE,
        customdata    = ~median_value,
        text          = ~hover_raw,
        line          = list(color = "#333333", width = 2.0, dash = "solid"),
        hovertemplate = "%{text}<extra></extra>"
      )
  }

#------------------------------------------------------------------------------#
# Setting the plotly layout ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the layout: overlaid bars, a left y axis for bias   #
# or raw error, a hidden right y axis for observed counts, a horizontal        #
# legend, white backgrounds, and the non-transmission shapes and annotations   #
# built above.                                                                 #
#------------------------------------------------------------------------------#

  ##############################
  # Applying the plotly layout #
  ##############################
  p <- p %>%
    plotly::layout(
      barmode     = "overlay",
      title       = list(
        text = "", font = list(size = 17),
        x = 0.5, xanchor = "center",
        y = 0.98, yanchor = "top"
      ),
      xaxis       = list(
        title             = "",
        tickformat        = "%b %d, %Y",
        showgrid          = TRUE,
        gridcolor         = "#f8f8f8",
        zeroline          = FALSE,
        automargin        = TRUE,
        ticklabelstandoff = 10,
        range             = list(
          min(observed_data$target_end_date) - 14,
          max(observed_data$target_end_date) + 14
        )
      ),
      yaxis       = list(
        title      = "Forecast Bias (%)",
        titlefont  = list(size = 16),
        showgrid   = TRUE,
        gridcolor  = "#f0f0f0",
        zeroline   = FALSE,
        automargin = TRUE,
        range      = list(-y1_pct_abs, y1_pct_abs),
        tickfont   = list(size = 12),
        ticksuffix = "%"
      ),
      yaxis2      = list(
        visible    = FALSE,
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
      legend      = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.08
      ),
      hovermode     = "closest",
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff",
      font          = list(family = "sans-serif", color = "#555"),
      margin        = list(l = 40, r = 100, t = 40,
                           b = if (length(gap_dates) > 0) 120 else 80),
      shapes        = all_shapes,
      annotations   = no_eval_annotations
    )

#------------------------------------------------------------------------------#
# Adding fullscreen button and Bias(%) / Raw Counts toggle ---------------------
#------------------------------------------------------------------------------#
# About: This section attaches the onRender JavaScript that builds the Bias    #
# (%) / Raw Counts toggle, swaps trace visibility and axis titles per mode,    #
# syncs the right y axis with the Observed legend entry, and adds the          #
# fullscreen button before returning the widget.                               #
#------------------------------------------------------------------------------#

  # Whether median traces exist affects JS trace indexing
  has_median_js <- if (n_horizons > 1) "true" else "false"

  ############################################
  # Attaching the onRender toggle JavaScript #
  ############################################
  p <- htmlwidgets::onRender(p, htmlwidgets::JS(paste0('
    function(el, x) {

      var parent = el.closest(".plotly-widget-container") || el.parentElement;
      if (parent && !parent.classList.contains("fs-wrap")) {
        parent.classList.add("fs-wrap");
      }

      var gd           = el;
      var plotId       = "', plot_id, '";
      var nHorizons    = ', n_horizons, ';
      var hasMedian    = ', has_median_js, ';
      var currentMode  = "pct";
      var observedShown = false;
      var yPctAbs      = ', y1_pct_abs, ';
      var y2PctMax     = ', y2_max, ';
      var y1RawSym     = ', y1_raw_sym, ';
      var outcomeLabel = "', outcome, '";

      // Trace indices
      // 0:               anchor
      // 1:               observed y2  (Bias (%) mode)
      // 2:               observed y1  (Raw Counts mode)
      // 3..(2+nH):       horizon pct lines y1
      // (3+nH)..(2+2nH): horizon raw lines y1
      // If hasMedian:
      //   (3+2nH):         overall median pct y1
      //   (4+2nH):         overall median raw y1
      var obsPctIdx     = 1;
      var obsRawIdx     = 2;
      var hPctStart     = 3;
      var hPctEnd       = 2 + nHorizons;
      var hRawStart     = 3 + nHorizons;
      var hRawEnd       = 2 + 2 * nHorizons;
      var overallPctIdx = hasMedian ? 3 + 2 * nHorizons : -1;
      var overallRawIdx = hasMedian ? 4 + 2 * nHorizons : -1;

      // Per-horizon shown-state, seeded from the initial pct traces, so a
      // horizon toggled in the legend persists across the Bias(%) / Raw toggle.
      var horizonShown = [];
      for (var hSeed = 0; hSeed < nHorizons; hSeed++) {
        var hv0 = gd.data[hPctStart + hSeed].visible;
        horizonShown.push(hv0 === true || hv0 === undefined);
      }

      // Overall-median shown-state, seeded from its initial pct trace, so the
      // median also persists across the Bias(%) / Raw toggle.
      var medianShown = true;
      if (hasMedian) {
        var mv0 = gd.data[overallPctIdx].visible;
        medianShown = mv0 === true || mv0 === undefined;
      }

      // Track legend toggles so they survive the Bias(%) / Raw switch. For
      // Observed, also sync the right axis in Bias(%) mode.
      el.on("plotly_restyle", function(data) {
        if (!data || !data[0]) return;
        var traceIdxs = data[1];
        if (!traceIdxs) return;

        // Observed: shown-state plus right-axis sync in Bias(%) mode
        var activeObs = currentMode === "pct" ? obsPctIdx : obsRawIdx;
        if (traceIdxs.indexOf(activeObs) !== -1) {
          var observedTrace = el.data[activeObs];
          observedShown = observedTrace.visible === true ||
                          observedTrace.visible === undefined;
          if (currentMode === "pct") {
            Plotly.relayout(el, {
              "yaxis2.visible":        observedShown,
              "yaxis2.showticklabels": observedShown
            });
          }
        }

        // Horizon lines: record each toggled horizon for the active mode
        var hStart = currentMode === "pct" ? hPctStart : hRawStart;
        var hEnd   = currentMode === "pct" ? hPctEnd   : hRawEnd;
        for (var r = 0; r < traceIdxs.length; r++) {
          var ti = traceIdxs[r];
          if (ti >= hStart && ti <= hEnd) {
            var hvr = el.data[ti].visible;
            horizonShown[ti - hStart] = hvr === true || hvr === undefined;
          }
        }

        // Overall median: record its shown-state for the active mode
        if (hasMedian) {
          var activeMed = currentMode === "pct" ? overallPctIdx : overallRawIdx;
          if (traceIdxs.indexOf(activeMed) !== -1) {
            var mvr = el.data[activeMed].visible;
            medianShown = mvr === true || mvr === undefined;
          }
        }
      });

      setTimeout(function() {

        var wrapper = el.closest(".fs-wrap") || el.parentElement;
        wrapper.style.position = "relative";

        if (document.getElementById(plotId + "_toggleBtns")) return;

        var btnContainer = document.createElement("div");
        btnContainer.id  = plotId + "_toggleBtns";
        btnContainer.style.cssText = [
          "position:absolute",
          "top:-42px",
          "right:20px",
          "z-index:9999",
          "display:inline-flex",
          "border:1px solid #ddd",
          "border-radius:6px",
          "overflow:hidden"
        ].join(";");

        var btnPct = document.createElement("button");
        btnPct.id  = plotId + "_btnPct";
        btnPct.textContent = "Bias (%)";
        btnPct.style.cssText = [
          "padding:5px 12px",
          "font-size:12px",
          "font-weight:600",
          "border:none",
          "cursor:pointer",
          "background:#522D80",
          "color:#fff"
        ].join(";");

        var btnRaw = document.createElement("button");
        btnRaw.id  = plotId + "_btnRaw";
        btnRaw.textContent = "Raw Counts";
        btnRaw.style.cssText = [
          "padding:5px 12px",
          "font-size:12px",
          "font-weight:600",
          "border:none",
          "cursor:pointer",
          "background:transparent",
          "color:#555"
        ].join(";");

        btnContainer.appendChild(btnPct);
        btnContainer.appendChild(btnRaw);
        wrapper.appendChild(btnContainer);

        function setMode(mode) {
          currentMode = mode;

          var n      = gd.data.length;
          var visArr = [];
          var obsVis = observedShown ? true : "legendonly";

          for (var i = 0; i < n; i++) {
            if (i === 0) {
              visArr.push(true);
            } else if (i === obsPctIdx) {
              visArr.push(mode === "pct" ? obsVis : false);
            } else if (i === obsRawIdx) {
              visArr.push(mode === "raw" ? obsVis : false);
            } else if (i >= hPctStart && i <= hPctEnd) {
              var onP = horizonShown[i - hPctStart] ? true : "legendonly";
              visArr.push(mode === "pct" ? onP : false);
            } else if (i >= hRawStart && i <= hRawEnd) {
              var onR = horizonShown[i - hRawStart] ? true : "legendonly";
              visArr.push(mode === "raw" ? onR : false);
            } else if (hasMedian && i === overallPctIdx) {
              var onMp = medianShown ? true : "legendonly";
              visArr.push(mode === "pct" ? onMp : false);
            } else if (hasMedian && i === overallRawIdx) {
              var onMr = medianShown ? true : "legendonly";
              visArr.push(mode === "raw" ? onMr : false);
            } else {
              visArr.push(true);
            }
          }

          var layoutUpdate = mode === "pct"
            ? {
                "yaxis.title.text"      : "Forecast Bias (%)",
                "yaxis.range"           : [-yPctAbs, yPctAbs],
                "yaxis.ticksuffix"      : "%",
                "yaxis.rangemode"       : "normal",
                "yaxis2.visible"        : observedShown,
                "yaxis2.showticklabels" : observedShown,
                "yaxis2.range"          : [0, y2PctMax]
              }
            : {
                "yaxis.title.text"      : outcomeLabel,
                "yaxis.range"           : [-y1RawSym, y1RawSym],
                "yaxis.ticksuffix"      : "",
                "yaxis.rangemode"       : "normal",
                "yaxis2.visible"        : false,
                "yaxis2.showticklabels" : false
              };

          Plotly.update(gd, { visible: visArr }, layoutUpdate).then(function() {
            Plotly.relayout(gd, {
              "legend.visible"     : true,
              "legend.orientation" : "h",
              "legend.x"           : 0.5,
              "legend.xanchor"     : "center",
              "legend.y"           : -0.08
            });
          });

          btnPct.style.background = mode === "pct" ? "#522D80" : "transparent";
          btnPct.style.color      = mode === "pct" ? "#fff"    : "#555";
          btnRaw.style.background = mode === "raw" ? "#522D80" : "transparent";
          btnRaw.style.color      = mode === "raw" ? "#fff"    : "#555";

          if (typeof setBiasTableMode === "function") {
            setBiasTableMode(mode);
          }

        }

        btnPct.addEventListener("click", function() { setMode("pct"); });
        btnRaw.addEventListener("click", function() { setMode("raw"); });

      }, 100);
    }
  ')))

  ################################
  # Adding the fullscreen button #
  ################################
  p <- build_fullscreen_button(p)

  #################################
  # Returning the finished widget #
  #################################
  return(p)

}
