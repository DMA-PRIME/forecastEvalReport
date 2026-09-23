#' Plain-language definitions for current evaluation metrics
#' @keywords internal
#' @noRd
metric_descriptions <- function() {
  row <- function(family, metric, pattern, description)
    data.frame(family, metric, pattern, description, stringsAsFactors=FALSE)
  dplyr::bind_rows(
    row("percentAgreement", "Percent Accuracy (Similarity Index) (%)", "^per_agreement$",
      "100 times min(forecast, observed) / max(forecast, observed), with two zeros assigned 100. This is a descriptive similarity index, not percent correct or a proper score for median forecasts. This is the primary reported metric; MAE is a secondary point-forecast measure."),
    row("percentAgreement", "Percent Accuracy (Similarity Index) summaries", "^(mean|min|max|median|q[0-9]+)(Horizon|Overall)$",
      "Descriptive summaries of the similarity index over eligible forecasts. Higher means a larger min/max ratio; it does not establish better median-forecast accuracy. Percent Accuracy (Similarity Index) is the primary reported metric; MAE is secondary."),
    row("forecastBias", "Raw error", "^raw_error$",
      "Forecast minus observed count; positive means overprediction and negative means underprediction. Missing or nonfinite pairs are unscorable."),
    row("forecastBias", "Percentage error", "^pct_error$",
      "100 times raw error divided by the observation; unavailable for zero or nonfinite observations or predictions."),
    row("forecastBias", "Bias summaries", "^(mean|min|max|median|q[0-9]+)(Raw|PctAll|PctStable)",
      "Summary statistics of signed errors, not shares of categorical labels. PctStable uses observations at or above the configured count floor; this is a denominator-size filter, not a claim that the time series is stable."),
    row("traditional", "Mean absolute error of the median (MAE)", "^MAE",
      "Secondary point-forecast measure: mean absolute difference between the predicted median and observed count. Lower is better; reported in outcome units."),
    row("traditional", "Weighted interval score (WIS)", "^WIS",
      "A proper probabilistic score on a common symmetric quantile grid within each location. Median-only, incomplete, nonfinite, or crossing quantiles are excluded from WIS. Lower is better; sample sizes can differ from MAE."),
    row("traditional", "Underprediction", "^Under",
      "The underprediction component of WIS, using the same eligible complete-grid forecasts. Lower is better."),
    row("traditional", "Overprediction", "^Over",
      "The overprediction component of WIS, using the same eligible complete-grid forecasts. Lower is better."),
    row("traditional", "50% interval coverage", "^Cov50",
      "Fraction of observations within available valid 0.25 to 0.75 quantile bounds; nominal target 0.50."),
    row("traditional", "80% interval coverage", "^Cov80",
      "Fraction of observations within available valid 0.10 to 0.90 quantile bounds; nominal target 0.80."),
    row("traditional", "95% interval coverage", "^Cov95",
      "Fraction of observations within available valid 0.025 to 0.975 quantile bounds; nominal target 0.95."),
    row("peakPhase", "Peak timing offset", "^predictedPeakTimingOff$",
      "Predicted maximum date minus observed maximum date, divided by the time step, within the testing period for each season and location. Negative is early; positive is late. For ties the earliest date is used. This is not a gap between near-peak windows."),
    row("peakPhase", "Peak timing label", "^predictedPeakTimingLabel$",
      "On Target when the absolute peak-date offset is within the configured timing tolerance; otherwise the offset is labeled early or late."),
    row("peakPhase", "Peak-to-peak magnitude difference", "^predictedPeakMagnitudeOff$",
      "Maximum predicted value at this horizon minus the observed maximum in the testing period for this season/location, regardless of whether their dates match."),
    row("peakPhase", "Magnitude difference at predicted peak", "^sameDayMagnitudeOff$",
      "Predicted maximum minus the observed value on that predicted maximum date."),
    row("peakPhase", "Magnitude difference at observed peak", "^peakWeekMagnitudeOff$",
      "Forecast targeting the observed testing-period maximum date minus the observed maximum. Unavailable when that forecast is missing."),
    row("peakPhase", "Peak-to-peak similarity (proportion)", "^predictedPeakAccuracy$",
      "Min/max ratio of predicted and observed testing-period maxima; stored on 0-1. Descriptive magnitude similarity, not percent correct. Undefined if both are zero."),
    row("peakPhase", "Similarity at predicted peak (proportion)", "^sameDayAccuracy$",
      "Min/max ratio of the predicted maximum and observation on that date; stored on 0-1. Descriptive similarity; undefined for two zeros or missing truth."),
    row("peakPhase", "Similarity at observed peak (proportion)", "^peakWeekAccuracy$",
      "Min/max ratio of the forecast and observed value on the observed testing-period maximum date; stored on 0-1. Descriptive similarity; undefined for two zeros or missing forecasts.")
  )
}
