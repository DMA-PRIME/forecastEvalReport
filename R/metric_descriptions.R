#' Plain-language descriptions of the testing-period metrics
#'
#' Returns a tidy table of AI-friendly descriptions for the metrics
#' produced by the testing-period evaluation. It is the single source of
#' truth for metric prose: the report template renders it as a metric
#' definitions section, and export_testing_evaluation() can pull from it
#' to annotate the AI summary. Each row carries the metric family, a
#' human-readable label, a case-insensitive regular expression that
#' matches the corresponding result column names, and the description.
#'
#' @return A data frame with columns family, metric, pattern, and
#'   description.
#'
#' @keywords internal
#' @noRd
metric_descriptions <- function() {

#------------------------------------------------------------------------------#
# About: This section lays out one row per metric. The pattern column is a     #
# case-insensitive regular expression that matches the result column names     #
# for that metric (for example '^WIS' matches WIS_Horizon and WIS_Overall),    #
# so a caller can map any exported metric back to its description.             #
#------------------------------------------------------------------------------#

  ############################
  # Metric family per metric #
  ############################
  families <- c(
    "percentAgreement",
    "percentAgreement",
    "forecastBias",
    "forecastBias",
    "forecastBias",
    "traditional",
    "traditional",
    "traditional",
    "traditional",
    "peakPhase",
    "peakPhase",
    "peakPhase",
    "peakPhase",
    "peakPhase"
  )

  ###############################
  # Human-readable metric label #
  ###############################
  labels <- c(
    "Percent agreement (row level)",
    "Percent agreement summaries",
    "Raw error (row level)",
    "Percentage error (row level)",
    "Aggregated bias (stable vs all)",
    "Weighted interval score (WIS)",
    "Median absolute error (MAE)",
    "50% interval coverage",
    "95% interval coverage",
    "Peak timing offset",
    "Peak magnitude offset",
    "Peak matched weeks",
    "Peak hit rate",
    "Directional labels"
  )

  ######################################################
  # Column-name match pattern (case-insensitive regex) #
  ######################################################
  patterns <- c(
    "per_agreement",
    "^(mean|min|max|median)",
    "raw_error",
    "pct_error",
    "^(Raw|PctAll|PctStable)",
    "^WIS",
    "^MAE",
    "Cov50",
    "Cov95",
    "[Tt]ime",
    "[Mm]ag",
    "[Mm]atched",
    "[Hh]it",
    "[Ll]abel"
  )

  ################################
  # AI-friendly prose per metric #
  ################################
  descriptions <- c(
    paste0(
      "Percent agreement is how closely a single forecast matched the ",
      "observed value at that point, expressed as a percentage where 100% is ",
      "an exact match. Higher values mean the forecast landed nearer to what ",
      "actually happened."
    ),
    paste0(
      "The mean, minimum, maximum, and median of percent agreement across the ",
      "transmission-season forecasts. Together they describe both how well ",
      "and how consistently the model agreed with observations. Higher is ",
      "better."
    ),
    paste0(
      "Raw error is the forecast minus the observed value at a single point. ",
      "A positive value means the model over-predicted; a negative value ",
      "means it under-predicted."
    ),
    paste0(
      "Percentage error expresses the raw error as a percent of the observed ",
      "value, so over- and under-prediction stay comparable across weeks of ",
      "very different magnitude."
    ),
    paste0(
      "Aggregated bias summarizes the model's directional tendency: the share ",
      "of forecasts that over-predicted, under-predicted, or fell within an ",
      "acceptable band. The 'stable' variant restricts to periods when the ",
      "observed data had settled; the 'all' variant uses every point."
    ),
    paste0(
      "The weighted interval score rewards a forecast for being both close to ",
      "the truth and well-calibrated across all of its prediction intervals ",
      "at once. It is a proper score reported on the scale of the outcome, ",
      "and lower is better."
    ),
    paste0(
      "Mean absolute error of the median is the average absolute distance ",
      "between the model's median point forecast and the observed value. ",
      "Lower is better."
    ),
    paste0(
      "Fifty-percent coverage is the fraction of observations that fell ",
      "inside the model's 50% prediction interval. A well-calibrated model ",
      "lands near 0.50; much lower means intervals are too narrow, much ",
      "higher means they are too wide."
    ),
    paste0(
      "Ninety-five-percent coverage is the fraction of observations that fell ",
      "inside the model's 95% prediction interval. A well-calibrated model ",
      "lands near 0.95."
    ),
    paste0(
      "The peak timing offset compares the forecast's own near-peak window ",
      "(the run of horizons within the peak-window tolerance of the forecast's ",
      "maximum) to the observed peak-phase window. It is zero when the two ",
      "windows overlap (a hit), negative when the forecast's window falls ",
      "entirely before the observed phase (too early), and positive when it ",
      "falls entirely after (too late); the size is the edge-to-edge gap in ",
      "time steps, and values near zero are best."
    ),
    paste0(
      "The peak magnitude offset measures how far the forecast's values were ",
      "from the observed values at the weeks where the forecast overlaps the ",
      "observed peak phase. Those overlapping forecast and observed values are ",
      "each sorted high-to-low and paired by rank, and the paired differences ",
      "are averaged; sorting each side first keeps magnitude independent of ",
      "timing. The raw version is in outcome units (positive over-, negative ",
      "under-forecast), and the percent-agreement version rescales it to a ",
      "0-100% match where higher is better."
    ),
    paste0(
      "Peak matched weeks is the number of observed peak-phase weeks a ",
      "forecast overlapped, which is how many rank-paired comparisons the ",
      "magnitude offset is built from. It is a context count rather than a ",
      "score: a one-week match and a four-week match summarize different ",
      "amounts of the peak."
    ),
    paste0(
      "The peak hit rate is the fraction of eligible forecasts whose near-peak ",
      "window overlapped the observed peak phase rather than falling entirely ",
      "before or after it. Higher is better."
    ),
    paste0(
      "Directional labels translate the timing and magnitude offsets into ",
      "plain terms such as On Time, Early, or Late, applying the configured ",
      "timing and magnitude tolerances so the result reads at a glance."
    )
  )

  ##########################################
  # Assembling the tidy descriptions table #
  ##########################################
  data.frame(
    family      = families,
    metric      = labels,
    pattern     = patterns,
    description = descriptions,
    stringsAsFactors = FALSE
  )

}

