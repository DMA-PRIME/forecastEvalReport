#' Build the trend-label capture heatmap
#'
#' Renders the observed-label by forecasted-label confusion matrix as three
#' side-by-side heatmaps, one per observed epidemic phase. The diagonal of each
#' panel is capture -- the share of target periods the model labeled correctly
#' -- and the off-diagonal cells show which label it reached for instead, so a
#' systematic lean toward calling increases during a decline is visible as mass
#' collecting on one side of the diagonal.
#'
#' Cells are shaded by share within each observed label rather than by raw
#' count, so an uncommon trend reads at the same intensity as a common one. Raw
#' counts remain available on hover, which is where the reader needs them: a
#' cell at one hundred percent of two target periods should not be mistaken for
#' a robust result, and the hover text states both.
#'
#' Season and horizon are offered as Plotly dropdowns rather than separate
#' figures. Every combination is drawn as its own hidden trace set and toggled
#' by visibility, which keeps the whole figure self-contained -- no report-level
#' JavaScript is required for the controls to work.
#'
#' @param capture_results The list returned by `trendCaptureCalculation()`. Uses
#'   `confusion`, `trend_levels`, `phase_levels`, `seasons`, and `horizons`.
#' @param plot_styles Optional styles object from `create_plot_styles()`. Uses
#'   `font_size` and `plot_width` when present.
#' @param height Figure height in pixels. Defaults to `520`, which fits three
#'   five-by-five panels without crowding the tick labels.
#'
#' @return A `plotly` object, or `NULL` when no confusion rows are available.
#'
#' @keywords internal
#' @noRd
make_trend_capture_plot <- function(capture_results,
                                    plot_styles = NULL,
                                    height      = 520){

#------------------------------------------------------------------------------#
# Guard: scored rows must exist ------------------------------------------------
#------------------------------------------------------------------------------#
# About: An empty confusion table means the trend call could not be scored at   #
# all. Returning NULL lets the section render its diagnostic note instead of an #
# empty set of axes, which would imply the model scored zero rather than that   #
# nothing was scorable.                                                         #
#------------------------------------------------------------------------------#

  if(is.null(capture_results) ||
     is.null(capture_results$confusion) ||
     !is.data.frame(capture_results$confusion) ||
     nrow(capture_results$confusion) == 0L){

    return(NULL)

  }

#------------------------------------------------------------------------------#
# Resolving styles and label ladders -------------------------------------------
#------------------------------------------------------------------------------#
# About: Pulls the report's font size so this figure matches the others, and    #
# fixes the axis order to the canonical ladders. Axis categories are set        #
# explicitly with categoryarray because Plotly would otherwise order them by    #
# first appearance in the data, which varies with the filter.                   #
#------------------------------------------------------------------------------#

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
  trend_levels <- capture_results$trend_levels
  phase_levels <- capture_results$phase_levels

  # Only phases that actually carry scored rows are drawn
  confusion    <- capture_results$confusion
  phase_levels <- phase_levels[
    phase_levels %in% as.character(confusion$phase)
  ]

  if(length(phase_levels) == 0L) return(NULL)

  ###############################
  # Dropdown values to offer    #
  ###############################
  seasons  <- capture_results$seasons
  horizons <- capture_results$horizons

  # Every season and horizon pairing, in dropdown order
  combos <- expand.grid(
    season  = seasons,
    horizon = horizons,
    stringsAsFactors = FALSE
  )

#------------------------------------------------------------------------------#
# Building one heatmap per phase and filter combination ------------------------
#------------------------------------------------------------------------------#
# About: Traces are added in a strict order -- all phases for combination one,  #
# then all phases for combination two, and so on -- because the dropdown        #
# visibility vectors below are built by position. Changing the loop order        #
# without changing those vectors would silently show the wrong panel.            #
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

  #########################################
  # Adding a heatmap per combination-phase #
  #########################################
  for(combo_index in seq_len(nrow(combos))){

    this_season  <- combos$season[combo_index]
    this_horizon <- combos$horizon[combo_index]

    for(phase_index in seq_along(phase_levels)){

      this_phase <- phase_levels[phase_index]

      ####################################
      # Rows for this panel and filter   #
      ####################################
      panel <- confusion[
        as.character(confusion$season)  == this_season &
          as.character(confusion$horizon) == this_horizon &
          as.character(confusion$phase)   == this_phase,
        ,
        drop = FALSE
      ]

      ############################################
      # Laying the rows onto a full label matrix #
      ############################################
      # A complete matrix is built even when some label pairs never occurred,
      # so every panel shares the same axes and empty cells read as absent
      # rather than shifting the grid.
      share_matrix <- matrix(
        NA_real_,
        nrow = length(trend_levels),
        ncol = length(trend_levels),
        dimnames = list(trend_levels, trend_levels)
      )

      count_matrix <- share_matrix

      if(nrow(panel) > 0L){

        for(row_index in seq_len(nrow(panel))){

          observed_label <- as.character(panel$observed_trend[row_index])
          forecast_label <- as.character(panel$forecast_trend[row_index])

          share_matrix[observed_label, forecast_label] <-
            panel$share[row_index]

          count_matrix[observed_label, forecast_label] <-
            panel$n[row_index]

        }

      }

      ####################################
      # Hover text, one string per cell  #
      ####################################
      hover_matrix <- matrix(
        "",
        nrow = length(trend_levels),
        ncol = length(trend_levels)
      )

      for(observed_index in seq_along(trend_levels)){

        for(forecast_index in seq_along(trend_levels)){

          cell_share <- share_matrix[observed_index, forecast_index]
          cell_count <- count_matrix[observed_index, forecast_index]

          hover_matrix[observed_index, forecast_index] <- if(is.na(cell_share)){

            # No target periods landed in this pairing
            paste0(
              "<b>", this_phase, "</b><br>",
              "Observed: ", trend_levels[observed_index], "<br>",
              "Forecasted: ", trend_levels[forecast_index], "<br>",
              "No target periods"
            )

          }else{

            # Share within the observed label, with the count behind it
            paste0(
              "<b>", this_phase, "</b><br>",
              "Observed: ", trend_levels[observed_index], "<br>",
              "Forecasted: ", trend_levels[forecast_index], "<br>",
              "Share of observed label: ", sprintf("%.0f%%", cell_share), "<br>",
              "Target periods: ", cell_count,
              if(identical(observed_index, forecast_index)){
                "<br><i>Captured</i>"
              }else{""}
            )

          }

        }

      }

      ##############################
      # Adding the heatmap trace   #
      ##############################
      fig <- plotly::add_trace(
        fig,
        type          = "heatmap",
        x             = trend_levels,
        y             = trend_levels,
        z             = share_matrix,
        text          = hover_matrix,
        hoverinfo     = "text",
        xaxis         = paste0("x", phase_index),
        yaxis         = "y",
        zmin          = 0,
        zmax          = 100,
        colorscale    = list(
          list(0,    "#f7f5fb"),
          list(0.25, "#d8cdec"),
          list(0.5,  "#b49ddb"),
          list(0.75, "#8a6cc4"),
          list(1,    "#5b3f96")
        ),
        showscale     = identical(phase_index, length(phase_levels)),
        colorbar      = list(
          title     = list(text = "Share of\nobserved\nlabel (%)",
                           font = list(size = font_size - 2)),
          tickfont  = list(size = font_size - 2),
          thickness = 12,
          len       = 0.85
        ),
        visible       = identical(combo_index, 1L)
      )

      # Recording which combination this trace belongs to
      trace_combo <- c(trace_combo, combo_index)

    }

  }

#------------------------------------------------------------------------------#
# Laying out the panels --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One x axis per phase, spread evenly across the figure with a shared y  #
# axis on the left. The y axis is reversed so "large decrease" sits at the top  #
# and the captured diagonal runs from top-left to bottom-right, which is how a  #
# confusion matrix is conventionally read.                                      #
#------------------------------------------------------------------------------#

  #################################
  # Panel widths and x positions  #
  #################################
  panel_count <- length(phase_levels)
  panel_gap   <- 0.06
  panel_width <- (1 - panel_gap * (panel_count - 1)) / panel_count

  ###############################
  # Building each phase's x axis #
  ###############################
  layout_args <- list(fig)

  for(phase_index in seq_along(phase_levels)){

    axis_start <- (phase_index - 1) * (panel_width + panel_gap)

    layout_args[[paste0("xaxis", phase_index)]] <- list(
      domain        = c(axis_start, axis_start + panel_width),
      title         = list(
        text = paste0("<b>", phase_levels[phase_index], "</b>",
                      "<br>Forecasted trend"),
        font = list(size = font_size - 1)
      ),
      type          = "category",
      categoryorder = "array",
      categoryarray = trend_levels,
      tickangle     = -35,
      tickfont      = list(size = font_size - 3),
      showgrid      = FALSE,
      zeroline      = FALSE
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

  layout_args$margin <- list(l = 130, r = 90, t = 70, b = 120)

  layout_args$hoverlabel <- list(
    bgcolor    = "#ffffff",
    bordercolor = "#cbd5e0",
    font       = list(size = font_size - 2)
  )

#------------------------------------------------------------------------------#
# Season and horizon dropdowns -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Two dropdowns, each rewriting the full visibility vector. Because a    #
# selection has to combine both dimensions, each menu entry carries the other   #
# dimension's current value implicitly: selecting a season shows that season    #
# paired with the first horizon. This is the cost of driving Plotly menus        #
# without callbacks, and it is why the horizon menu is listed second -- the      #
# reader picks the season, then narrows the horizon.                             #
#------------------------------------------------------------------------------#

  ############################################
  # Visibility vector for a given combination #
  ############################################
  visibility_for <- function(combo_index){

    as.list(trace_combo == combo_index)

  }

  ###############################
  # Building the season entries #
  ###############################
  season_buttons <- lapply(seasons, function(this_season){

    # The first horizon paired with this season
    combo_index <- which(
      combos$season == this_season & combos$horizon == horizons[1]
    )[1]

    list(
      method = "update",
      label  = this_season,
      args   = list(
        list(visible = visibility_for(combo_index)),
        list(title = list(
          text = paste0(
            "<b>Trend label capture</b>   ",
            "<span style='font-size:", font_size - 3, "px;color:#666'>",
            this_season, " &middot; horizon ", horizons[1], "</span>"
          ),
          font = list(size = font_size + 3),
          x = 0.02, xanchor = "left"
        ))
      )
    )

  })

  ################################
  # Building the horizon entries #
  ################################
  horizon_buttons <- lapply(horizons, function(this_horizon){

    # This horizon paired with the first season
    combo_index <- which(
      combos$horizon == this_horizon & combos$season == seasons[1]
    )[1]

    list(
      method = "update",
      label  = this_horizon,
      args   = list(
        list(visible = visibility_for(combo_index)),
        list(title = list(
          text = paste0(
            "<b>Trend label capture</b>   ",
            "<span style='font-size:", font_size - 3, "px;color:#666'>",
            seasons[1], " &middot; horizon ", this_horizon, "</span>"
          ),
          font = list(size = font_size + 3),
          x = 0.02, xanchor = "left"
        ))
      )
    )

  })

  ###############################
  # Attaching the two dropdowns #
  ###############################
  layout_args$updatemenus <- list(

    # Season selector
    list(
      type      = "dropdown",
      direction = "down",
      showactive = TRUE,
      active    = 0,
      x         = 0,
      xanchor   = "left",
      y         = 1.16,
      yanchor   = "top",
      pad       = list(t = 0, r = 6),
      font      = list(size = font_size - 3),
      bgcolor   = "#ffffff",
      bordercolor = "#cbd5e0",
      buttons   = season_buttons
    ),

    # Horizon selector
    list(
      type      = "dropdown",
      direction = "down",
      showactive = TRUE,
      active    = 0,
      x         = 0.24,
      xanchor   = "left",
      y         = 1.16,
      yanchor   = "top",
      pad       = list(t = 0, r = 6),
      font      = list(size = font_size - 3),
      bgcolor   = "#ffffff",
      bordercolor = "#cbd5e0",
      buttons   = horizon_buttons
    )

  )

  ###################################
  # Titling the initial view        #
  ###################################
  layout_args$title <- list(
    text = paste0(
      "<b>Trend label capture</b>   ",
      "<span style='font-size:", font_size - 3, "px;color:#666'>",
      seasons[1], " &middot; horizon ", horizons[1], "</span>"
    ),
    font = list(size = font_size + 3),
    x = 0.02, xanchor = "left"
  )

  layout_args$showlegend <- FALSE

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
