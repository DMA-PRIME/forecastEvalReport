#' Render the trend- and phase-specific performance section
#'
#' Builds the standalone testing-period section covering how well the model
#' reproduced the observed trend call, cross-cut by observed epidemic phase and
#' observed trend. Two questions are answered in order:
#'
#'   1. **Capture** -- how often the forecasted trend label matched the observed
#'      trend label, shown as an observed-by-forecasted confusion heatmap so the
#'      off-diagonal mass reveals which label the model substituted.
#'   2. **Offset** -- when the label was wrong, the direction and size of the
#'      miss along the ordered trend ladder, shown as diverging bars centered on
#'      exact matches.
#'
#' Each figure is followed by a collapsed accordion holding the numbers behind
#' it, broken out per geography. The figures pool across geographies because the
#' confusion matrix needs enough target periods per cell to be meaningful; the
#' tables carry the per-geography detail.
#'
#' This section replaces the trend/phase accordions that previously lived inside
#' the Percent Agreement, Forecast Bias, and Statistical Scoring Metrics
#' sections. Consolidating them here keeps every trend- and phase-cut view in
#' one place rather than requiring the reader to open three separate
#' subdropdowns to assemble the same picture.
#'
#' @param percentAgreement.data Output from `percentAgreementCalculation()`,
#'   used as the source of the row-level trend calls.
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Uses
#'   `testing_data` as the presence gate and `time_step` as the target cadence.
#' @param eval_config Evaluation configuration from
#'   `create_evaluation_config()`.
#' @param population_crosswalk Optional custom population crosswalk, passed
#'   through to the trend calculation.
#' @param location_crosswalk Optional location crosswalk supplying display
#'   labels for the tables.
#' @param plot_styles Optional styles object from `create_plot_styles()`.
#'
#' @return Rendered HTML via [htmltools::HTML()] and [htmltools::tagList()], or
#'   `invisible(NULL)` when no testing data is available.
#'
#' @keywords internal
#' @noRd
section_trend_phase <- function(percentAgreement.data,
                                eval_meta,
                                eval_config          = NULL,
                                population_crosswalk = NULL,
                                location_crosswalk   = NULL,
                                plot_styles          = NULL){

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#
# About: Shares the presence signal every other testing-period section uses, so #
# a report without a testing period drops this section entirely rather than      #
# rendering an empty heading.                                                    #
#------------------------------------------------------------------------------#

  ###################################
  # Presence of usable testing data #
  ###################################
  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  if(!has_testing) return(invisible(NULL))

  ############################################
  # Presence of usable percent agreement rows #
  ############################################
  if(is.null(percentAgreement.data) ||
     !is.data.frame(percentAgreement.data) ||
     nrow(percentAgreement.data) == 0L){

    return(invisible(NULL))

  }

#------------------------------------------------------------------------------#
# Scoring the trend calls ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Reuses trendPhasePerformanceCalculation() for its phase and season      #
# assignment and its trend calls, then reshapes those rows into the capture and  #
# offset summaries. Both steps are wrapped so an upstream failure produces a     #
# diagnostic note in the report rather than aborting the render -- the same      #
# contract the previous trend/phase accordions honored.                          #
#------------------------------------------------------------------------------#

  ################################
  # Resolving the target cadence #
  ################################
  week_days <- if(!is.null(eval_meta$time_step)){

    eval_meta$time_step

  }else{7}

  #####################################
  # Running the phase/trend labelling #
  #####################################
  trend_phase_error <- NULL

  trend_phase_result <- tryCatch(

    trendPhasePerformanceCalculation(
      percentAgreement.data = percentAgreement.data,
      population            = population_crosswalk,
      eval_config           = eval_config,
      week_days             = week_days
    ),

    error = function(e){

      trend_phase_error <<- paste0(
        "The trend and phase tables could not be calculated: ",
        conditionMessage(e)
      )

      message(
        "Trend and Phase Specific Performance could not be calculated and ",
        "will show a diagnostic note. Reason: ", conditionMessage(e)
      )

      list(summary = data.frame(), data = data.frame())

    }

  )

  ############################################
  # Resolving location codes and display labels #
  ############################################
  location_codes  <- NULL
  location_labels <- NULL

  if(!is.null(location_crosswalk) && is.data.frame(location_crosswalk)){

    code_column <- intersect(
      c("location", "location_code", "code"), names(location_crosswalk)
    )[1]

    label_column <- intersect(
      c("location_display", "location_name", "display", "name"),
      names(location_crosswalk)
    )[1]

    if(!is.na(code_column) && !is.na(label_column)){

      location_codes  <- as.character(location_crosswalk[[code_column]])
      location_labels <- as.character(location_crosswalk[[label_column]])

    }

  }

  #####################################
  # Scoring capture and offset        #
  #####################################
  capture_results <- tryCatch(

    trendCaptureCalculation(
      phase_data      = trend_phase_result$data,
      location_codes  = location_codes,
      location_labels = location_labels
    ),

    error = function(e){

      if(is.null(trend_phase_error)){

        trend_phase_error <<- paste0(
          "The trend capture summaries could not be calculated: ",
          conditionMessage(e)
        )

      }

      NULL

    }

  )

#------------------------------------------------------------------------------#
# Section heading and introduction ---------------------------------------------
#------------------------------------------------------------------------------#
# About: The heading matches the numbered entry in the testing-period intro     #
# list. The introduction states the trend ladder explicitly, because both        #
# measures are defined in terms of positions on it and neither figure means      #
# anything without it.                                                           #
#------------------------------------------------------------------------------#

  #############################
  # Heading and intro prose   #
  #############################
  intro_html <- htmltools::HTML('
  <h2>Trend and Phase Specific Performance</h2>

  <div class="section-intro">
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
      Each observed and forecasted target period is assigned a trend label from
      population-adjusted week-over-week change:
      <strong>large decrease</strong>, <strong>decrease</strong>,
      <strong>stable</strong>, <strong>increase</strong>, or
      <strong>large increase</strong>. Every target period also falls in an
      observed epidemic phase &mdash; <strong>Ascension</strong>,
      <strong>Peak</strong>, or <strong>Decline</strong> &mdash; set by where it
      sits relative to the observed seasonal maximum.
    </p>
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
      The two figures below ask different questions of the same labels.
      <strong>Capture</strong> asks how often the forecasted label was the
      observed label, and which label the model substituted when it was not.
      <strong>Offset</strong> asks, when the label was wrong, whether the model
      called the trend higher or lower than it turned out and by how many
      categories. A model can capture poorly but stay within one category, which
      is a different failure from one that swings across the ladder.
    </p>
    <p style="font-size: 14px; line-height: 1.7; color: #666; margin: 0 0 1.5rem 0;">
      Both figures pool geographies so each cell rests on enough target periods
      to be informative; the tables beneath them break the same numbers out by
      geography. Only in-season target periods with a resolved phase and both
      trend labels present are scored.
    </p>
  </div>
  ')

#------------------------------------------------------------------------------#
# Diagnostic note when nothing scored ------------------------------------------
#------------------------------------------------------------------------------#
# About: Returns the heading plus an explanatory note rather than nothing, so a  #
# reader who expected this section learns why it is empty instead of assuming    #
# the report is truncated.                                                       #
#------------------------------------------------------------------------------#

  ##########################################
  # Detecting the nothing-to-show condition #
  ##########################################
  nothing_scored <- is.null(capture_results) ||
    is.null(capture_results$confusion) ||
    !is.data.frame(capture_results$confusion) ||
    nrow(capture_results$confusion) == 0L

  if(nothing_scored){

    #####################################
    # Resolving the explanatory message #
    #####################################
    note_text <- if(!is.null(trend_phase_error)){

      trend_phase_error

    }else{

      paste(
        "No trend and phase results could be calculated. Scoring a trend call",
        "requires at least two consecutive in-season target periods with both",
        "an observed and a forecasted value, so that week-over-week change can",
        "be derived for each."
      )

    }

    #############################
    # Returning heading plus note #
    #############################
    return(htmltools::tagList(
      intro_html,
      htmltools::HTML(paste0(
        '<p style="font-size: 14px; line-height: 1.7; color: #8a6d3b;',
        ' background: #fcf8e3; border: 1px solid #faebcc; border-radius: 4px;',
        ' padding: 0.75rem 1rem; margin: 0 0 1.5rem 0;">',
        note_text,
        '</p>'
      ))
    ))

  }

#------------------------------------------------------------------------------#
# Building the two figures -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Each figure carries its own season and horizon dropdowns, built into    #
# the Plotly object itself so the controls need no report-level JavaScript.      #
#------------------------------------------------------------------------------#

  #########################
  # Capture heatmap figure #
  #########################
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
# Building the accordion tables ------------------------------------------------
#------------------------------------------------------------------------------#
# About: One table per measure, each collapsed by default so the figures stay    #
# the primary view. Percentages are rounded at render time rather than in the    #
# calculation so the stored values remain exact for any later reuse.             #
#------------------------------------------------------------------------------#

  ###########################################
  # Styling helper shared by both tables    #
  ###########################################
  render_table <- function(df, column_names, alignment){

    # Guarding against an empty table
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L){

      return(htmltools::HTML(
        '<p style="font-size: 14px; color: #666; margin: 0;">
           No rows were available for this table.
         </p>'
      ))

    }

    # Dropping stale row numbers so kable does not print them as a column
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

  ###############################
  # Capture table contents      #
  ###############################
  capture_table <- capture_results$capture

  capture_display <- data.frame(
    Geography  = capture_table$location_display,
    Season     = capture_table$season,
    Horizon    = capture_table$horizon,
    Phase      = as.character(capture_table$phase),
    Trend      = as.character(capture_table$observed_trend),
    Periods    = capture_table$n,
    Captured   = capture_table$hits,
    Capture    = sprintf("%.0f%%", capture_table$capture_pct),
    stringsAsFactors = FALSE
  )

  capture_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Trend Label Capture Table")
    ),
    htmltools::div(
      class = "accordion-body",
      htmltools::HTML(
        '<p style="font-size: 14px; line-height: 1.7; color: #444;
                   margin: 0 0 1rem 0;">
           Share of in-season target periods where the forecasted trend label
           matched the observed label, by geography, season, forecast horizon,
           observed phase, and observed trend. <strong>Periods</strong> is the
           denominator &mdash; a high capture rate over very few periods is not
           a stable result.
         </p>'
      ),
      render_table(
        capture_display,
        c("Geography", "Season", "Horizon", "Phase", "Observed Trend",
          "Periods", "Captured", "Capture"),
        c("l", "l", "l", "l", "l", "r", "r", "r")
      )
    )
  )

  ###############################
  # Offset table contents       #
  ###############################
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

  offset_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Trend Offset Table")
    ),
    htmltools::div(
      class = "accordion-body",
      htmltools::HTML(
        '<p style="font-size: 14px; line-height: 1.7; color: #444;
                   margin: 0 0 1rem 0;">
           Direction and size of the trend miss, measured in categories along the
           trend ladder. <strong>Mean Offset</strong> is signed: positive means
           the model called the trend higher than observed on balance. Read it
           alongside the too-low and too-high shares, since misses in opposing
           directions average toward zero and can make a scattered model look
           unbiased.
         </p>'
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
# Assembling the section -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Figure, then its table, then the second figure and its table, so each   #
# set of numbers sits directly beneath the picture it explains rather than       #
# collecting at the end of the section.                                          #
#------------------------------------------------------------------------------#

  ############################
  # Returning the full section #
  ############################
  htmltools::tagList(

    intro_html,

    # Question one: capture
    capture_plot,
    capture_accordion,

    # Question two: offset
    htmltools::div(style = "height: 1.25rem;"),
    offset_plot,
    offset_accordion

  )

}
