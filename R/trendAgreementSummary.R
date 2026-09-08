#' Summarize agreement between two ordered trend-label vectors
#'
#' Reports raw agreement, near-miss agreement, and the label distribution of
#' each side, for a pair of trend calls scored on the same ordered ladder.
#'
#' Raw agreement alone is misleading when the two sides have different label
#' distributions. Forecast trajectories are smoother than observed series, so a
#' model tends to call "stable" more often than the data warrants; a season that
#' is mostly stable will then produce a high agreement rate that reflects the
#' base rate rather than skill. Reporting both label distributions alongside the
#' agreement rate exposes that directly.
#'
#' Near-misses are reported as the share of periods landing on the right label
#' or one step either side, rather than as a weighted score. The ladder is
#' ordinal, so calling "increase" when the truth was "large increase" is not the
#' same failure as calling "large decrease" -- but "right, or nearly right"
#' conveys that without asking the reader to interpret a coefficient.
#'
#' Returning both marginals alongside the scores is deliberate. When a model
#' calls stable far more often than the data does, that gap is the finding, and
#' it should be visible rather than absorbed into a single number.
#'
#' @param observed_labels Character vector of observed trend labels.
#' @param forecast_labels Character vector of forecasted trend labels, parallel
#'   to `observed_labels`.
#' @param trend_levels Ordered trend ladder. Defaults to the five-level scale
#'   `trendCallCalculation()` assigns.
#'
#' @return A list with:
#'   \describe{
#'     \item{`n`}{Number of scorable pairs, meaning both labels present and on
#'       the ladder.}
#'     \item{`agreement_pct`}{Percentage of pairs whose labels matched exactly.}
#'     \item{`within_one_pct`}{Percentage landing on the right label or one
#'       category either side.}
#'     \item{`observed_marginal`, `forecast_marginal`}{Data frames of label,
#'       count, and percentage for each side.}
#'     \item{`mean_abs_offset`}{Mean absolute distance along the ladder.}
#'   }
#'
#' @keywords internal
#' @noRd
trendAgreementSummary <- function(observed_labels,
                                  forecast_labels,
                                  trend_levels = c("large decrease",
                                                   "decrease",
                                                   "stable",
                                                   "increase",
                                                   "large increase")){

#------------------------------------------------------------------------------#
# Empty result -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Defined once and returned from every early exit, so a caller can index #
# the result without first checking whether anything was scorable.              #
#------------------------------------------------------------------------------#

  empty_marginal <- data.frame(
    label = character(), n = integer(), pct = numeric(),
    stringsAsFactors = FALSE
  )

  empty_result <- list(
    n                 = 0L,
    agreement_pct     = NA_real_,
    within_one_pct    = NA_real_,
    observed_marginal = empty_marginal,
    forecast_marginal = empty_marginal,
    mean_abs_offset   = NA_real_
  )

#------------------------------------------------------------------------------#
# Restricting to scorable pairs ------------------------------------------------
#------------------------------------------------------------------------------#
# About: A pair is scorable only when both labels exist and sit on the ladder.  #
# Dropping unrecognised labels rather than appending them keeps the weighting   #
# well defined, since an off-ladder value has no position to measure from.      #
#------------------------------------------------------------------------------#

  observed_labels <- as.character(observed_labels)
  forecast_labels <- as.character(forecast_labels)

  if(length(observed_labels) != length(forecast_labels)){

    stop("trendAgreementSummary(): label vectors must be the same length.",
         call. = FALSE)

  }

  keep <- !is.na(observed_labels) & !is.na(forecast_labels) &
    observed_labels %in% trend_levels & forecast_labels %in% trend_levels

  observed_labels <- observed_labels[keep]
  forecast_labels <- forecast_labels[keep]

  n_pairs <- length(observed_labels)

  if(n_pairs == 0L) return(empty_result)

#------------------------------------------------------------------------------#
# Raw agreement and ladder distance --------------------------------------------
#------------------------------------------------------------------------------#

  observed_index <- match(observed_labels, trend_levels)
  forecast_index <- match(forecast_labels, trend_levels)

  agreement_pct   <- 100 * mean(observed_index == forecast_index)
  mean_abs_offset <- mean(abs(forecast_index - observed_index))

  # Share landing on the right label or one step either side. This carries the
  # near-miss information a weighted score would, in a form that needs no
  # explanation: "right, or nearly right".
  within_one_pct  <- 100 * mean(abs(forecast_index - observed_index) <= 1)

#------------------------------------------------------------------------------#
# Label marginals --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Every ladder level is reported, including those that never occurred,   #
# so the two sides can be read straight down against each other. A level absent #
# from one side is the comparison, not a row to omit.                           #
#------------------------------------------------------------------------------#

  marginal_of <- function(index_vector){

    counts <- vapply(
      seq_along(trend_levels),
      function(k) sum(index_vector == k),
      integer(1)
    )

    data.frame(
      label = trend_levels,
      n     = counts,
      pct   = 100 * counts / n_pairs,
      stringsAsFactors = FALSE
    )

  }

  observed_marginal <- marginal_of(observed_index)
  forecast_marginal <- marginal_of(forecast_index)

#------------------------------------------------------------------------------#
# Returning the summary --------------------------------------------------------
#------------------------------------------------------------------------------#

  list(
    n                 = as.integer(n_pairs),
    agreement_pct     = agreement_pct,
    within_one_pct    = within_one_pct,
    observed_marginal = observed_marginal,
    forecast_marginal = forecast_marginal,
    mean_abs_offset   = mean_abs_offset
  )

}
