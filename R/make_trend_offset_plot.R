#' Build the trend-offset diverging bar figure
#'
#' Renders the signed distance between the forecasted and observed trend labels
#' as diverging stacked bars, one panel per observed epidemic phase and one bar
#' per observed trend. Exact matches are centered on zero, misses that called
#' the trend lower than observed extend to the left, and misses that called it
#' higher extend to the right. Reading across a bar answers the question the
#' capture rate cannot: when the model was wrong, which way was it wrong and by
#' how much.
#'
#' Bars are normalized within each observed trend so every bar spans the same
#' width, which lets a rare trend be compared against a common one directly.
#' Counts stay on hover so a bar built from three target periods is not read as
#' confidently as one built from thirty.
#'
#' The exact-match segment is deliberately drawn in neutral grey while the miss
#' segments carry direction color, so capture reads as the absence of color
#' rather than as another category competing for attention.
#'
#' @param capture_results The list returned by `trendCaptureCalculation()`. Uses
#'   `offset`, `trend_levels`, `phase_levels`, `seasons`, and `horizons`.
#' @param plot_styles Optional styles object from `create_plot_styles()`. Uses
#'   `font_size` and `plot_width` when present.
#' @param height Figure height in pixels. Defaults to `460`.
#'
#' @return A `plotly` object, or `NULL` when no offset rows are available.
#'
#' @keywords internal
#' @noRd
make_trend_offset_plot <- function(capture_results,
                                   plot_styles = NULL,
                                   height      = 460){

#------------------------------------------------------------------------------#
# Guard: scored rows must exist ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Matches make_trend_capture_plot(): an empty offset table means nothing #
# was scorable, and the section renders a note rather than empty axes.          #
#------------------------------------------------------------------------------#

  if(is.null(capture_results) ||
     is.null(capture_results$offset) ||
     !is.data.frame(capture_results$offset) ||
     nrow(capture_results$offset) == 0L){

    return(NULL)

  }

#------------------------------------------------------------------------------#
# Bucket order and colors ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Buckets are stacked left to right in ladder order so the bar reads as a #
# number line: the further from center, the further the label missed. Colors run #
# cool for under-calls and warm for over-calls, with neutral grey at the center, #
# so direction is legible without consulting the legend.                        #
#------------------------------------------------------------------------------#

  ###############################
  # Buckets in stacking order   #
  ###############################
  bucket_order <- c(
    "2 or more too low",
    "1 too low",
    "Exact match",
    "1 too high",
    "2 or more too high"
  )

  ###############################
  # Direction color per bucket  #
  ###############################
  bucket_colors <- c(
    "2 or more too low"  = "#2c6fa8",
    "1 too low"          = "#8fbde0",
    "Exact match"        = "#d9d9d9",
    "1 too high"         = "#f0a882",
    "2 or more too high" = "#c1502e"
  )

  #########################
  # Resolving the font size #
  #########################
  font_size <- if(!is.null(plot_styles$font_size) &&
                  is.numeric(plot_styles$font_size)){

    plot_styles$font_size

  }else{14}

  ############################
  # Canonical label ladders   #
  ############################
  offset_data  <- capture_results$offset
  trend_levels <- capture_results$trend_levels

  # Only phases and buckets that carry rows are drawn
  phase_levels <- capture_results$phase_levels
  phase_levels <- phase_levels[
    phase_levels %in% as.character(offset_data$phase)
  ]

  if(length(phase_levels) == 0L) return(NULL)

  present_buckets <- bucket_order[
    bucket_order %in% as.character(offset_data$bucket)
  ]

  if(length(present_buckets) == 0L) return(NULL)

  ###############################
  # Dropdown values to offer    #
  ###############################
  seasons  <- capture_results$seasons
  horizons <- capture_results$horizons

  combos <- expand.grid(
    season  = seasons,
    horizon = horizons,
    stringsAsFactors = FALSE
  )

#------------------------------------------------------------------------------#
# Building one stacked bar set per phase and filter ----------------------------
#------------------------------------------------------------------------------#
# About: Traces run bucket-within-phase-within-combination, and the visibility  #
# vectors below index by position, so the loop order and those vectors must     #
# stay in step. The legend is shown only for the first panel of the first        #
# combination; every later trace reuses the same legend groups so one legend      #
# controls all panels at once.                                                   #
#------------------------------------------------------------------------------#

  ############################
  # Starting an empty figure #
  ############################
  fig <- plotly::plot_ly(
    width  = if(!is.null(plot_styles$plot_width)) plot_styles$plot_width else NULL,
    height = height
  )

  #####################################
  # Tracking which traces belong where #
  #####################################
  trace_combo <- integer(0)

  ###########################################
  # Adding the stacked bars per combination #
  ###########################################
  for(combo_index in seq_len(nrow(combos))){

    this_season  <- combos$season[combo_index]
    this_horizon <- combos$horizon[combo_index]

    for(phase_index in seq_along(phase_levels)){

      this_phase <- phase_levels[phase_index]

      for(bucket_index in seq_along(present_buckets)){

        this_bucket <- present_buckets[bucket_index]

        ##################################
        # Rows for this bar segment      #
        ##################################
        segment <- offset_data[
          as.character(offset_data$season)  == this_season &
            as.character(offset_data$horizon) == this_horizon &
            as.character(offset_data$phase)   == this_phase &
            as.character(offset_data$bucket)  == this_bucket,
          ,
          drop = FALSE
        ]

        ###############################################
        # Aligning the segment against every trend    #
        ###############################################
        # Each trace carries one value per observed trend, with zero where the
        # bucket never occurred, so the stack stays aligned across panels.
        segment_shares <- rep(0, length(trend_levels))
        segment_counts <- rep(0L, length(trend_levels))

        if(nrow(segment) > 0L){

          matched <- match(as.character(segment$observed_trend), trend_levels)
          keep    <- !is.na(matched)

          segment_shares[matched[keep]] <- segment$share[keep]
          segment_counts[matched[keep]] <- as.integer(segment$n[keep])

        }

        ##############################
        # Hover text for the segment #
        ##############################
        hover_text <- paste0(
          "<b>", this_phase, "</b><br>",
          "Observed: ", trend_levels, "<br>",
          this_bucket, "<br>",
          "Share: ", sprintf("%.0f%%", segment_shares), "<br>",
          "Target periods: ", segment_counts
        )

        ##############################
        # Adding the segment trace   #
        ##############################
        fig <- plotly::add_trace(
          fig,
          type        = "bar",
          orientation = "h",
          x           = segment_shares,
          y           = trend_levels,
          name        = this_bucket,
          legendgroup = this_bucket,
          marker      = list(
            color = unname(bucket_colors[this_bucket]),
            line  = list(color = "#ffffff", width = 1)
          ),
          text        = hover_text,
          hoverinfo   = "text",
          xaxis       = paste0("x", phase_index),
          yaxis       = "y",
          showlegend  = identical(combo_index, 1L) &&
                        identical(phase_index, 1L),
          visible     = identical(combo_index, 1L)
        )

        # Recording which combination this trace belongs to
        trace_combo <- c(trace_combo, combo_index)

      }

    }

  }

#------------------------------------------------------------------------------#
# Laying out the panels --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One x axis per phase with a shared y axis, matching the capture         #
# heatmap so the two figures can be read against each other without             #
# reorienting. barmode is set to stack so the buckets accumulate into a single   #
# full-width bar per observed trend.                                            #
#------------------------------------------------------------------------------#

  #################################
  # Panel widths and x positions  #
  #################################
  panel_count <- length(phase_levels)
  panel_gap   <- 0.05
  panel_width <- (1 - panel_gap * (panel_count - 1)) / panel_count

  ###############################
  # Building each phase's x axis #
  ###############################
  layout_args <- list(fig)

  for(phase_index in seq_along(phase_levels)){

    axis_start <- (phase_index - 1) * (panel_width + panel_gap)

    layout_args[[paste0("xaxis", phase_index)]] <- list(
      domain     = c(axis_start, axis_start + panel_width),
      title      = list(
        text = paste0("<b>", phase_levels[phase_index], "</b>",
                      "<br>Share of target periods (%)"),
        font = list(size = font_size - 1)
      ),
      range      = c(0, 100),
      ticksuffix = "%",
      tickfont   = list(size = font_size - 3),
      gridcolor  = "#eeeeee",
      zeroline   = FALSE
    )

  }

  #############################
  # Shared y axis and framing #
  #############################
  layout_args$yaxis <- list(
    title         = list(text = "Observed trend",
                         font = list(size = font_size - 1)),
    type          = "category",
    categoryorder = "array",
    categoryarray = rev(trend_levels),
    tickfont      = list(size = font_size - 3),
    showgrid      = FALSE,
    zeroline      = FALSE
  )

  layout_args$barmode <- "stack"
  layout_args$bargap  <- 0.3
  layout_args$margin  <- list(l = 130, r = 40, t = 70, b = 110)

  layout_args$legend <- list(
    orientation = "h",
    x = 0, xanchor = "left",
    y = -0.34, yanchor = "top",
    font = list(size = font_size - 3),
    traceorder = "normal"
  )

  layout_args$hoverlabel <- list(
    bgcolor     = "#ffffff",
    bordercolor = "#cbd5e0",
    font        = list(size = font_size - 2)
  )

#------------------------------------------------------------------------------#
# Season and horizon dropdowns -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Built the same way as the capture heatmap's menus, so the two figures  #
# behave identically. Each entry rewrites the whole visibility vector; the      #
# season menu pairs with the first horizon and the horizon menu with the first  #
# season, since Plotly menus cannot read each other's state.                    #
#------------------------------------------------------------------------------#

  ############################################
  # Visibility vector for a given combination #
  ############################################
  visibility_for <- function(combo_index){

    as.list(trace_combo == combo_index)

  }

  ##########################################
  # Title text for a season-horizon pairing #
  ##########################################
  title_for <- function(season_label, horizon_label){

    list(
      text = paste0(
        "<b>Trend offset: direction and size of the miss</b>   ",
        "<span style='font-size:", font_size - 3, "px;color:#666'>",
        season_label, " &middot; horizon ", horizon_label, "</span>"
      ),
      font = list(size = font_size + 3),
      x = 0.02, xanchor = "left"
    )

  }

  ###############################
  # Building the season entries #
  ###############################
  season_buttons <- lapply(seasons, function(this_season){

    combo_index <- which(
      combos$season == this_season & combos$horizon == horizons[1]
    )[1]

    list(
      method = "update",
      label  = this_season,
      args   = list(
        list(visible = visibility_for(combo_index)),
        list(title = title_for(this_season, horizons[1]))
      )
    )

  })

  ################################
  # Building the horizon entries #
  ################################
  horizon_buttons <- lapply(horizons, function(this_horizon){

    combo_index <- which(
      combos$horizon == this_horizon & combos$season == seasons[1]
    )[1]

    list(
      method = "update",
      label  = this_horizon,
      args   = list(
        list(visible = visibility_for(combo_index)),
        list(title = title_for(seasons[1], this_horizon))
      )
    )

  })

  ###############################
  # Attaching the two dropdowns #
  ###############################
  layout_args$updatemenus <- list(

    # Season selector
    list(
      type       = "dropdown",
      direction  = "down",
      showactive = TRUE,
      active     = 0,
      x          = 0,
      xanchor    = "left",
      y          = 1.18,
      yanchor    = "top",
      pad        = list(t = 0, r = 6),
      font       = list(size = font_size - 3),
      bgcolor    = "#ffffff",
      bordercolor = "#cbd5e0",
      buttons    = season_buttons
    ),

    # Horizon selector
    list(
      type       = "dropdown",
      direction  = "down",
      showactive = TRUE,
      active     = 0,
      x          = 0.24,
      xanchor    = "left",
      y          = 1.18,
      yanchor    = "top",
      pad        = list(t = 0, r = 6),
      font       = list(size = font_size - 3),
      bgcolor    = "#ffffff",
      bordercolor = "#cbd5e0",
      buttons    = horizon_buttons
    )

  )

  ###################################
  # Titling the initial view        #
  ###################################
  layout_args$title <- title_for(seasons[1], horizons[1])

#------------------------------------------------------------------------------#
# Returning the figure ---------------------------------------------------------
#------------------------------------------------------------------------------#

  ############################
  # Applying the full layout #
  ############################
  fig <- do.call(plotly::layout, layout_args)

  ###################################
  # Trimming the interactive toolbar #
  ###################################
  fig <- plotly::config(
    fig,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list(
      "lasso2d", "select2d", "autoScale2d", "hoverClosestCartesian",
      "hoverCompareCartesian", "toggleSpikelines"
    )
  )

  fig

}
