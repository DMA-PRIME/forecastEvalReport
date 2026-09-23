# Evaluation methods

This version preserves the selected training series as evaluation truth when the existing training-truth coverage rules select it. The observed peak remains the maximum **within each season's testing period**. Observed phase stratification remains retrospective. Cross-model sample matching is not performed automatically.

## Historical trend calibration

Reference: Mathis et al., *A Framework for Classifying Disease Trends Applied to Influenza-Associated Hospital Admissions in the United States* (2025), [Methods](https://doi.org/10.1093/ofid/ofaf460).

The paper calibrated a shared FluSurv-NET reference distribution and applied its thresholds to jurisdictions. This package applies that framework to each location's supplied historical series, as requested; it does not reproduce the paper's original surveillance cohort or guarantee its numerical thresholds.

For each location:

1. Use the chosen truth series strictly before the earliest evaluated reference date **and** target date. Testing reference dates are reconstructed from target dates and horizons under the existing evaluation-file convention; operational forecasts retain their archived reference dates.
2. Separate epidemiological seasons at week 40. Start including differences at the third consecutive, nonzero weekly observation in each season. Only adjacent dates exactly seven days apart form a difference. Once eligibility begins, later zero counts are included; missing weeks never become multiweek differences.
3. Compute weekly differences in rates per 100,000 population. Historical `population` values, if supplied, provide the historical denominators; otherwise the population lookup is used.
4. Calculate empirical 5th, 25th, 75th, and 95th percentiles. The implementation explicitly uses R quantile type 7 and retains full numeric precision by default (`round_digits = NULL`). HTML and AI Markdown format a display copy to eight significant digits; classification and numeric JSON use the stored cutoffs. Explicit integer rounding remains available only as an opt-in convention for legacy reproducibility. Recalibrate previously saved rounded tables to recover precision. The article does not specify an interpolation algorithm; type 7 is a documented implementation convention.
5. Freeze that table throughout the evaluation. Appending later outcomes cannot recalibrate earlier labels. Missing or directionally unsuitable historical calibration is reported as unavailable, rather than estimated from evaluation outcomes. Directional validity requires p05 < 0, p25 <= 0, p75 >= 0, and p95 > 0; this is an implementation safeguard for new datasets.

Apply the paper's boundary rules to the weekly rate difference `d`:

| Category | Rule |
| --- | --- |
| Large decrease | d <= p05 |
| Decrease | p05 < d <= p25 |
| Stable | p25 < d < p75 |
| Increase | p75 <= d < p95 |
| Large increase | d >= p95 |

With valid calibration, absolute raw-count changes **strictly below 10** override the rate category to stable. `trend_count_threshold` controls this separately from the forecast-bias denominator floor. Nonfinite changes are unscorable. Daily or other nonweekly cadence is rejected for this method.

Observed labels use consecutive truth weeks. Forecast labels compare consecutive predicted weeks from the same submission; a preceding truth value can anchor the first predicted week only when its date is no later than the reference date. This is an extension of the paper's observed-trend method to forecasts. It does not verify historical release vintages or when subsequently revised truth became available.

### Configuration

The default uses available pre-evaluation truth automatically:

```r
cfg <- create_evaluation_config()
generate_report("report_options.R", eval_config = cfg)
```

Supply a separate historical calibration dataset when necessary. Location keys must match the raw evaluation location keys. The historical data must have `location`, `target_end_date` (or `date`), and `Observed` (or `value`); `population` is optional.

```r
history <- read.csv("historical_truth.csv", colClasses = c(location = "character"))
cfg <- create_evaluation_config(trend_calibration_data = history)
generate_report("report_options.R", eval_config = cfg)
```

Or save and reuse an explicit table:

```r
frozen <- calibrate_trend_thresholds(
  history,
  cutoff = as.Date("2024-10-01"),  # strictly before every evaluated issue/target
  population = c("45" = 5118425)   # replace with appropriate denominators
)
saveRDS(frozen, "trend_thresholds.rds")
cfg <- create_evaluation_config(trend_thresholds = readRDS("trend_thresholds.rds"))
generate_report("report_options.R", eval_config = cfg)
```

For different location cutoffs, supply a `location`/`cutoff` data frame to `calibrate_trend_thresholds()`. A saved table must include `calibration_end`, which is checked against evaluation dates. Cutoffs are always stored per 100,000. The testing trend section displays calibration dates, sample sizes, cutoffs, and status. Programmatic `trendCallCalculation()` returns these as `location_thresholds` (and the compatible alias `location_percentiles`). Cross-model comparisons require an explicitly shared calibration and matched evaluation cases; they are not supplied automatically here.

## Point and probabilistic scores

**Percent Accuracy (Similarity Index) is the primary reported metric**, `100 * min(predicted, observed) / max(predicted, observed)`, with two zeros assigned 100. It is descriptive, not percent correct and not a consistent score for the predictive median. Nonfinite or negative count inputs are unavailable for this index. **MAE is a secondary point-forecast measure**: the mean absolute error of forecast medians, one contribution per location/reference-date/target-date/horizon case. This presentation preference does not change either calculation. Existing API and CSV column names such as `per_agreement` remain for compatibility. Peak min/max ratios also have descriptive similarity labels; their original two-zero behavior remains undefined.

**WIS** requires a median and at least one symmetric interval, with the same quantile grid across scored cases within a location. The default grid is the union of declared quantile levels over the eligible evaluation rows for that location. Each case must supply all those levels, finite predictions, and noncrossing quantiles. Median-only and incomplete cases no longer receive a misleading reduced-grid WIS. Duplicate quantiles and conflicting truth within a case raise errors. The internal scoring helper also accepts `quantile_grid` to declare a fixed symmetric grid explicitly. WIS and its directional components average only eligible cases; MAE can use additional median/truth pairs. Coverage uses each available, noninverted interval independently. Their sample sizes can differ. Data attributes `wis_quantile_grids` and `wis_diagnostics`, plus report availability text, explain omissions.

Missing/nonfinite forecasts, missing truth, and percentage error with zero truth receive **Unscorable**, not “Within Range.” Percentage-bias sample counts exclude those rows. Real-time report and dashboard paths now pass the configured bias floor and percentage cushion into the calculations.

## Peak definitions

Peak timing measures the difference between predicted and observed maximum dates, in time steps; ties use the earliest date. It is not a distance between near-peak windows. Magnitude measures compare the two maxima, predicted peak versus same-day truth, and the forecast targeting the observed peak. Descriptions now match those fields.
