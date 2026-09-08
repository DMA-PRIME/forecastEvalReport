#' Build the testing-period peak and trend performance breakdown
#'
#' Internal comparison-dashboard helper. Reuses the report's own trend- and
#' phase-labelling pathway -- `trendPhasePerformanceCalculation()`,
#' `trendPhaseBiasCalculation()`, `trendPhaseTraditionalCalculation()`, and
#' `trendPhaseTraditionalMetricsCalculation()` -- so the dashboard breakdown is
#' numerically identical to the report's Peak & Trend section. The outputs are
#' reshaped into one long table the dashboard can filter and draw.
#'
#' Three kinds of rows are produced:
#' \itemize{
#'   \item `dimension = "cross"`: one row per location, horizon, observed
#'     trend, and observed epidemic phase (Ascension, Peak, Decline) for each
#'     performance measure -- Percent Agreement (median, minimum, maximum),
#'     Forecast Bias (median percentage and raw error), and the Traditional
#'     Metrics means (WIS, MAE, under/over-prediction, and interval coverage).
#'   \item `dimension = "phase"` / `dimension = "trend"`: the trend-performance
#'     marginals (trend label agreement and the mean absolute weekly
#'     rate-change error) summarized within each phase or observed trend.
#'   \item `dimension = "overall"`: the same two trend-performance measures
#'     pooled across phases and trends.
#' }
#'
#' Every calculation is wrapped so a single failing family degrades to an
#' absent family rather than stopping the dashboard. Locations are returned
#' as raw codes; the caller applies display mapping.
#'
#' @param testing_eval The testing-evaluation bundle produced by
#'   `build_testing_evaluation()`, containing `percentAgreement`,
#'   `forecastBias`, and `traditional` frames.
#' @param eval_config Evaluation configuration from
#'   `create_evaluation_config()`. `NULL` uses the package defaults.
#' @param week_days Number of days between consecutive target periods.
#'
#' @return A data frame with columns `location`, `horizon`, `dimension`,
#'   `observed_trend`, `phase`, `group`, `family`, `metric`, `statistic`,
#'   `value`, `minimum`, `maximum`, and `n`. Zero rows when nothing could be
#'   computed.
#'
#' @keywords internal
#' @noRd
build_comparison_phase_breakdown <- function(testing_eval,
                                             eval_config = NULL,
                                             week_days = 7){

#------------------------------------------------------------------------------#
# Confirming the function should run --------------------------------------------
#------------------------------------------------------------------------------#
# About: The breakdown is testing-period only. Anything missing or malformed   #
# returns the empty, consistently shaped table so the dashboard can simply     #
# hide the section.                                                            #
#------------------------------------------------------------------------------#

  ############################################
  # Empty output with the full fixed schema  #
  ############################################
  empty_breakdown <- function(){
    data.frame(
      location = character(0), horizon = character(0),
      dimension = character(0), observed_trend = character(0),
      phase = character(0), group = character(0),
      family = character(0), metric = character(0),
      statistic = character(0), value = numeric(0),
      minimum = numeric(0), maximum = numeric(0), n = integer(0),
      stringsAsFactors = FALSE
    )
  }

  if(is.null(testing_eval) || !is.list(testing_eval)) return(empty_breakdown())

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  #############################################################
  # Run one report calculation without stopping the dashboard #
  #############################################################
  run_quietly <- function(expr){
    tryCatch(suppressWarnings(suppressMessages(expr)),
             error = function(e) NULL)
  }

  ########################################
  # Consistent numeric and count parsing #
  ########################################
  num_or_na <- function(x) suppressWarnings(as.numeric(x))
  int_or_na <- function(x) suppressWarnings(as.integer(x))

  pieces <- list()

#------------------------------------------------------------------------------#
# Percent Agreement by observed trend and phase ---------------------------------
#------------------------------------------------------------------------------#
# About: The report's median / minimum / maximum convention is preserved so    #
# the dashboard cells match the report's trend-by-phase table exactly.         #
#------------------------------------------------------------------------------#

  pa <- run_quietly(trendPhasePerformanceCalculation(
    percentAgreement.data = testing_eval$percentAgreement,
    eval_config = eval_config,
    week_days = week_days
  ))

  if(!is.null(pa) && is.data.frame(pa$summary) && nrow(pa$summary) > 0L){
    s <- pa$summary
    pieces[[length(pieces) + 1L]] <- data.frame(
      location = as.character(s$location),
      horizon = as.character(s$horizon),
      dimension = "cross",
      observed_trend = as.character(s$observed_trend),
      phase = as.character(s$phase),
      group = NA_character_,
      family = "percentAgreement",
      metric = "per_agreement",
      statistic = "Median",
      value = num_or_na(s$median),
      minimum = num_or_na(s$minimum),
      maximum = num_or_na(s$maximum),
      n = int_or_na(s$n),
      stringsAsFactors = FALSE
    )
  }

#------------------------------------------------------------------------------#
# Forecast Bias by observed trend and phase -------------------------------------
#------------------------------------------------------------------------------#
# About: Percentage bias keeps the report's stable-observation rule and raw    #
# bias keeps all transmission rows -- both arrive pre-computed in the report's #
# own summary, so no aggregation is repeated here.                             #
#------------------------------------------------------------------------------#

  fb <- run_quietly(trendPhaseBiasCalculation(
    forecastBias.data = testing_eval$forecastBias,
    eval_config = eval_config,
    week_days = week_days
  ))

  if(!is.null(fb) && is.data.frame(fb$summary) && nrow(fb$summary) > 0L){
    s <- fb$summary
    bias_base <- data.frame(
      location = as.character(s$location),
      horizon = as.character(s$horizon),
      dimension = "cross",
      observed_trend = as.character(s$observed_trend),
      phase = as.character(s$phase),
      group = NA_character_,
      stringsAsFactors = FALSE
    )

    pieces[[length(pieces) + 1L]] <- cbind(bias_base, data.frame(
      family = "forecastBias", metric = "pct_error", statistic = "Median",
      value = num_or_na(s$pct_median),
      minimum = num_or_na(s$pct_minimum),
      maximum = num_or_na(s$pct_maximum),
      n = int_or_na(s$n_pct),
      stringsAsFactors = FALSE
    ))

    pieces[[length(pieces) + 1L]] <- cbind(bias_base, data.frame(
      family = "forecastBias", metric = "raw_error", statistic = "Median",
      value = num_or_na(s$raw_median),
      minimum = num_or_na(s$raw_minimum),
      maximum = num_or_na(s$raw_maximum),
      n = int_or_na(s$n_raw),
      stringsAsFactors = FALSE
    ))
  }

#------------------------------------------------------------------------------#
# Traditional metrics by observed trend and phase -------------------------------
#------------------------------------------------------------------------------#
# About: The labelled row-level frame feeds the report's traditional-metric    #
# means, and its marginal summary supplies the trend-performance measures      #
# (label agreement and mean absolute weekly rate-change error).                #
#------------------------------------------------------------------------------#

  tr <- run_quietly(trendPhaseTraditionalCalculation(
    traditional.data = testing_eval$traditional,
    eval_config = eval_config,
    week_days = week_days
  ))

  if(!is.null(tr)){

    ##############################################
    # Mean WIS, MAE, components, and coverage    #
    ##############################################
    metrics <- run_quietly(trendPhaseTraditionalMetricsCalculation(tr$data))

    if(is.data.frame(metrics) && nrow(metrics) > 0L){
      metric_names <- c(
        wis = "WIS", mae = "MAE", under = "Under", over = "Over",
        cov50 = "Cov50", cov80 = "Cov80", cov95 = "Cov95"
      )
      mapped <- unname(metric_names[as.character(metrics$metric)])
      keep <- !is.na(mapped)
      metrics <- metrics[keep, , drop = FALSE]
      mapped <- mapped[keep]

      if(nrow(metrics) > 0L){
        pieces[[length(pieces) + 1L]] <- data.frame(
          location = as.character(metrics$location),
          horizon = as.character(metrics$horizon),
          dimension = "cross",
          observed_trend = as.character(metrics$observed_trend),
          phase = as.character(metrics$phase),
          group = NA_character_,
          family = "traditional",
          metric = mapped,
          statistic = "Mean",
          value = num_or_na(metrics$mean),
          minimum = NA_real_,
          maximum = NA_real_,
          n = int_or_na(metrics$n),
          stringsAsFactors = FALSE
        )
      }
    }

    ###################################################
    # Trend-performance marginals by phase and trend  #
    ###################################################
    if(is.data.frame(tr$summary) && nrow(tr$summary) > 0L){
      s <- tr$summary
      dimension <- as.character(s$breakdown)
      group <- as.character(s$group)
      trend_base <- data.frame(
        location = as.character(s$location),
        horizon = as.character(s$horizon),
        dimension = dimension,
        observed_trend = ifelse(dimension == "trend", group, NA_character_),
        phase = ifelse(dimension == "phase", group, NA_character_),
        group = group,
        stringsAsFactors = FALSE
      )

      pieces[[length(pieces) + 1L]] <- cbind(trend_base, data.frame(
        family = "trendPerformance", metric = "trend_agreement",
        statistic = "Percent",
        value = num_or_na(s$agreement_pct),
        minimum = NA_real_, maximum = NA_real_,
        n = int_or_na(s$agreement_n),
        stringsAsFactors = FALSE
      ))

      pieces[[length(pieces) + 1L]] <- cbind(trend_base, data.frame(
        family = "trendPerformance", metric = "trend_mae",
        statistic = "Mean",
        value = num_or_na(s$trend_mae),
        minimum = NA_real_, maximum = NA_real_,
        n = int_or_na(s$trend_mae_n),
        stringsAsFactors = FALSE
      ))
    }
  }

#------------------------------------------------------------------------------#
# Returning the stacked breakdown -----------------------------------------------
#------------------------------------------------------------------------------#

  present <- pieces[vapply(
    pieces, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]

  if(length(present) == 0L) return(empty_breakdown())

  output <- do.call(rbind, present)

  # Empty cells carry no comparison information and would inflate the payload
  output <- output[!is.na(output$value) & is.finite(output$value), ,
                   drop = FALSE]
  rownames(output) <- NULL
  output
}
