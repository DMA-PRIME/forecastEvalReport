#' Build the nested trend-label capture and offset dropdown
#'
#' Wraps the two trend-label figures in a collapsed accordion sized to sit
#' beside `build_trend_phase_traditional_metrics()` inside the Statistical
#' Scoring Metrics section. Two questions are answered in order:
#'
#'   * **Capture** -- how often the forecasted trend label matched the observed
#'     trend label, drawn as an observed-by-forecasted confusion heatmap so the
#'     off-diagonal mass shows which label the model substituted.
#'   * **Offset** -- when the label was wrong, the direction and size of the miss
#'     along the ordered trend ladder, drawn as diverging bars centered on exact
#'     matches.
#'
#' Both figures are faceted by observed epidemic phase and carry their own
#' season and horizon dropdowns. The numbers behind each are held in a further
#' nested accordion, so the dropdown opens onto figures rather than tables.
#'
#' Follows the same structure as the other nested dropdowns in this section: a
#' `details.accordion` wrapper, a `strong` summary label, and an
#' `accordion-body` div, with an explanatory note in the shared 14px body style.
#'
#' @param capture_results The list returned by `trendCaptureCalculation()`, or
#'   `NULL` when the calculation failed.
#' @param plot_styles Optional styles object from `create_plot_styles()`, passed
#'   through to both figures so they inherit the report's font size and width.
#' @param status_message Optional diagnostic string shown in place of the
#'   figures when nothing could be scored.
#'
#' @return An `htmltools` `details` tag, or `NULL` when neither figure could be
#'   built and no status message was supplied.
#'
#' @keywords internal
#' @noRd
build_trend_capture <- function(capture_results,
                                plot_styles    = NULL,
                                status_message = NULL){

#------------------------------------------------------------------------------#
# Handling unavailable results -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Mirrors build_trend_phase_traditional_metrics(): an empty result set    #
# still renders the dropdown, carrying a note that explains why it is empty,     #
# so a reader who expected the figures is not left guessing.                     #
#------------------------------------------------------------------------------#

  ###########################################
  # Detecting the nothing-to-show condition #
  ###########################################
  nothing_scored <- is.null(capture_results) ||
    is.null(capture_results$confusion) ||
    !is.data.frame(capture_results$confusion) ||
    nrow(capture_results$confusion) == 0L

  if(nothing_scored){

    ###################################
    # Resolving the fallback message  #
    ###################################
    if(is.null(status_message) || !nzchar(status_message)){

      status_message <- paste(
        "No trend label capture table could be calculated. Scoring a trend",
        "call requires at least two consecutive in-season target periods with",
        "both an observed and a forecasted value."
      )

    }

    #####################################
    # Returning the dropdown with a note #
    #####################################
    return(htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(
        htmltools::tags$strong("Trend Label Capture and Offset")
      ),
      htmltools::div(
        class = "accordion-body",
        htmltools::tags$p(
          style = "font-size:14px;line-height:1.65;color:#555;margin:0;",
          status_message
        )
      )
    ))

  }

#------------------------------------------------------------------------------#
# Building the two figures -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Each figure carries its season and horizon dropdowns inside the Plotly  #
# object, so the controls work without any report-level JavaScript and without   #
# competing with the location and horizon selectors already on this section.     #
#------------------------------------------------------------------------------#

  ##########################
  # Capture heatmap figure #
  ##########################
  capture_plot <- make_trend_capture_plot(
    capture_results = capture_results,
    plot_styles     = plot_styles
  )

  #######################
  # Offset bars figure  #
  #######################
  offset_plot <- make_trend_offset_plot(
    capture_results = capture_results,
    plot_styles     = plot_styles
  )

#------------------------------------------------------------------------------#
# Rendering the supporting tables ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Percentages are formatted here rather than in the calculation so the    #
# stored values stay exact for any later reuse. row.names is set to FALSE        #
# because the summaries are sorted and subsetted upstream, which leaves stale    #
# row numbers that kable would otherwise print as an unlabeled first column.     #
#------------------------------------------------------------------------------#

  #########################################
  # Table styling shared by both measures #
  #########################################
  render_table <- function(df, column_names, alignment){

    # Guarding against an empty table
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L){

      return(htmltools::tags$p(
        style = "font-size:14px;line-height:1.65;color:#555;margin:0;",
        "No rows were available for this table."
      ))

    }

    # Dropping stale row numbers
    rownames(df) <- NULL

    table_html <- knitr::kable(
      df,
      format    = "html",
      col.names = column_names,
      align     = alignment,
      escape    = TRUE,
      row.names = FALSE
    )

    kableExtra::kable_styling(
      table_html,
      full_width        = FALSE,
      bootstrap_options = c("bordered", "striped"),
      position          = "left"
    )

  }

  ###########################
  # Capture table contents  #
  ###########################
  capture_table <- capture_results$capture

  capture_display <- data.frame(
    Geography = capture_table$location_display,
    Season    = capture_table$season,
    Horizon   = capture_table$horizon,
    Phase     = as.character(capture_table$phase),
    Trend     = as.character(capture_table$observed_trend),
    Periods   = capture_table$n,
    Captured  = capture_table$hits,
    Capture   = sprintf("%.0f%%", capture_table$capture_pct),
    stringsAsFactors = FALSE
  )

  capture_table_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Capture Table")
    ),
    htmltools::div(
      class = "accordion-body",
      htmltools::tags$p(
        style = "font-size:14px;line-height:1.65;color:#444;margin:0 0 1rem;",
        htmltools::HTML(paste(
          "Share of in-season target periods where the forecasted trend label",
          "matched the observed label, by geography, season, horizon, observed",
          "phase, and observed trend. <em>Periods</em> is the denominator",
          "&mdash; a high rate over very few periods is not a stable result."
        ))
      ),
      render_table(
        capture_display,
        c("Geography", "Season", "Horizon", "Phase", "Observed Trend",
          "Periods", "Captured", "Capture"),
        c("l", "l", "l", "l", "l", "r", "r", "r")
      )
    )
  )

  ##########################
  # Offset table contents  #
  ##########################
  offset_table <- capture_results$offset_summary

  offset_display <- data.frame(
    Geography = offset_table$location_display,
    Season    = offset_table$season,
    Horizon   = offset_table$horizon,
    Phase     = as.character(offset_table$phase),
    Trend     = as.character(offset_table$observed_trend),
    Periods   = offset_table$n,
    Exact     = sprintf("%.0f%%", offset_table$exact_pct),
    TooLow    = sprintf("%.0f%%", offset_table$too_low_pct),
    TooHigh   = sprintf("%.0f%%", offset_table$too_high_pct),
    MeanOff   = sprintf("%+.2f", offset_table$mean_offset),
    stringsAsFactors = FALSE
  )

  offset_table_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Offset Table")
    ),
    htmltools::div(
      class = "accordion-body",
      htmltools::tags$p(
        style = "font-size:14px;line-height:1.65;color:#444;margin:0 0 1rem;",
        htmltools::HTML(paste(
          "Direction and size of the trend miss, in categories along the trend",
          "ladder. <em>Mean Offset</em> is signed: positive means the model",
          "called the trend higher than observed on balance. Read it alongside",
          "the too-low and too-high shares, since opposing misses average",
          "toward zero and can make a scattered model look unbiased."
        ))
      ),
      render_table(
        offset_display,
        c("Geography", "Season", "Horizon", "Phase", "Observed Trend",
          "Periods", "Exact", "Too Low", "Too High", "Mean Offset"),
        c("l", "l", "l", "l", "l", "r", "r", "r", "r", "r")
      )
    )
  )

#------------------------------------------------------------------------------#
# Assembling the nested dropdown -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Intro note, then each figure followed by its own table dropdown, so the #
# numbers sit directly beneath the picture they explain rather than collecting    #
# at the end of the body.                                                        #
#------------------------------------------------------------------------------#

  #############################
  # Explanatory lead paragraph #
  #############################
  intro_note <- htmltools::tags$p(
    style = "font-size:14px;line-height:1.65;color:#444;margin:0 0 1rem;",
    htmltools::HTML(paste(
      "Each observed and forecasted target period carries a trend label from",
      "population-adjusted week-over-week change: <strong>large decrease</strong>,",
      "<strong>decrease</strong>, <strong>stable</strong>,",
      "<strong>increase</strong>, or <strong>large increase</strong>.",
      "<strong>Capture</strong> asks how often the forecasted label was the",
      "observed label, and which label the model substituted when it was not.",
      "<strong>Offset</strong> asks, when the label was wrong, whether the model",
      "called the trend higher or lower than it turned out and by how many",
      "categories. Figures pool geographies so each cell rests on enough target",
      "periods to be informative; the tables break the same numbers out by",
      "geography."
    ))
  )

  ############################
  # Returning the dropdown   #
  ############################
  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Trend Label Capture and Offset")
    ),
    htmltools::div(
      class = "accordion-body",
      intro_note,
      capture_plot,
      capture_table_accordion,
      htmltools::div(style = "height:1rem;"),
      offset_plot,
      offset_table_accordion
    )
  )

}
