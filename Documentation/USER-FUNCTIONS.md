# forecastEvalReport — user functions and arguments

Reference for the locally corrected methods-corrections branch, commit 0e8bae333d27ca2c2660480e421e82261acc9916 (23 September 2026).

Covers every public function/operator exported by this build: 23 functions plus the %>% pipe. Two additional documented but unexported helpers are listed separately. Internal implementation helpers and datasets are outside this user-function reference.

Names, argument order, and defaults were extracted from the R source and checked against NAMESPACE. Descriptions explain what the current implementation does. “Required” means you must supply a value; NULL usually means automatic/default behavior or no override, as explained for that argument. TRUE/FALSE are on/off switches. The L suffix in 1L or 5L marks an R integer. For choice vectors such as c("forecast", "consistency"), pass one value; the first is the default.

Load the package with library(forecastEvalReport), then call the exported functions below. An options file defines report_options, which holds model paths and report metadata; eval_config instead controls evaluation rules. This table covers function arguments, not every field inside report_options.

## All public functions

| Function | Arguments | In plain language |
| --- | --- | --- |
| [create_options_template](#create_options_template) | `force` | Create an editable R file containing the report settings and input-file paths. |
| [build_crosswalk_from_options](#build_crosswalk_from_options) | `options_file`, `force` | Create the starter CSV where you describe your data sources, outcomes, and variables. |
| [build_variables_crosswalk](#build_variables_crosswalk) | `config`, `implementation_model`, `evaluation_model`, `force` | Create the same starter crosswalk from already prepared configuration and model data. |
| [create_evaluation_config](#create_evaluation_config) | `non_transmission_months`, `season_start_day_month`, `peak_window`, `stable_threshold`, `pct_error_cushion`, `timing_tol_steps`, `mag_tol`, `trend_count_threshold`, `trend_calibration_data`, `trend_thresholds` | Choose the rules used to score forecasts and classify peaks, bias, and trends. |
| [create_plot_styles](#create_plot_styles) | `colors`, `pi_intervals`, `horizon_styles`, `target_data`, `current_projections`, `historical_estimates`, `phase_colors`, `plot_width`, `plot_height`, `default_view_weeks`, `rangeslider_bgcolor`, `eval_legend_title`, `right_axis` | Choose plot colors, line styles, uncertainty bands, sizes, and labels. |
| [generate_report](#generate_report) | `options_file`, `output_dir`, `output_file`, `quiet`, `plot_styles`, `eval_config`, `plot_export`, `split_by_location`, `split_filename`, `location_crosswalk`, `population_crosswalk` | Build the HTML forecast evaluation report from a completed options file. |
| [export_testing_evaluation](#export_testing_evaluation) | `options_file`, `output_dir`, `ai_form`, `save_results`, `ai_row_detail`, `eval_config`, `file_prefix`, `quiet`, `return_context`, `save_json`, `population_crosswalk` | Score forecasts from the testing period and optionally save AI-ready Markdown, JSON, and CSV. |
| [export_realtime_evaluation](#export_realtime_evaluation) | `options_file`, `output_dir`, `ai_form`, `save_results`, `ai_row_detail`, `eval_config`, `file_prefix`, `quiet`, `return_context`, `save_json`, `archive_current`, `population_crosswalk` | Score archived operational forecasts when observed truth is available, and optionally save Markdown, JSON, and CSV. |
| [calibrate_trend_thresholds](#calibrate_trend_thresholds) | `data`, `cutoff`, `population`, `round_digits` | Calculate reusable trend cutoffs from historical observations before a chosen date. |
| [trendCallCalculation](#trendcallcalculation) | `data.for.evaluation`, `population`, `population_location_col`, `population_value_col`, `rate_multiplier`, `stable_threshold`, `week_days`, `calibration_data`, `thresholds` | Label weekly observed and forecast changes as decreasing, stable, or increasing, using frozen historical cutoffs. |
| [read_testing_evaluations](#read_testing_evaluations) | `files`, `quiet` | Read exported evaluation CSVs and combine them into one long table. |
| [build_evaluation_table](#build_evaluation_table) | `x`, `family`, `scope`, `metric`, `columns`, `output_file`, `quiet` | Turn long evaluation results into a table with a column for each model or input file. |
| [save_forecast_plot](#save_forecast_plot) | `options_file`, `file`, `format`, `plot`, `location`, `width`, `height`, `scale`, `delay`, `n_forecasts`, `forecast_every`, `end_cushion`, `font_size`, `tick_size`, `forecast_size`, `forecast_color`, `observed_size`, `observed_color`, `aux_size`, `aux_color`, `title_gap`, `view_start`, `view_end`, `legend`, `overwrite`, `plot_styles`, `eval_config`, `quiet` | Save the forecast plot, or the forecast-consistency plot, as an interactive HTML file or static image. |
| [save_consistency_plot](#save_consistency_plot) | `options_file`, `...` | Save the plot showing how forecasts changed between issue dates. |
| [extract_implementation_data](#extract_implementation_data) | `implementation_model`, `config`, `save_data` | Pull dates, locations, outcome names, and forecast-period information from validated operational forecasts. |
| [extract_evaluation_data](#extract_evaluation_data) | `evaluation_model`, `config`, `impl_meta` | Split validated evaluation data into training, validation, and testing subsets and collect their metadata. |
| [validate_covidhub_model](#validate_covidhub_model) | `file`, `verbose` | Check a COVIDHub forecast CSV against the format rules implemented in this package. |
| [validate_flusight_model](#validate_flusight_model) | `file`, `verbose` | Check a FluSight forecast CSV against the format rules implemented in this package. |
| [validate_rsvhub_model](#validate_rsvhub_model) | `file`, `verbose` | Check an RSVHub forecast CSV against the format rules implemented in this package. |
| [validate_metrocast_model](#validate_metrocast_model) | `file`, `verbose` | Check a Flu MetroCast forecast CSV against the format rules implemented in this package. |
| [validate_general_model](#validate_general_model) | `file`, `verbose`, `state_context` | Check an operational forecast CSV in the package’s general format, including its geography fields. |
| [validate_eval_model](#validate_eval_model) | `file`, `verbose`, `state_context` | Check a historical evaluation forecast CSV in the package’s evaluation format. |
| [validate_variables_crosswalk](#validate_variables_crosswalk) | `x`, `verbose` | Check that the crosswalk describing your variables and data sources is complete and correctly formatted. |
| [%>%](#pipe) | `lhs`, `rhs` | Pass a value into the next function call so several steps can be chained together. |

## Common starting calls

```r
library(forecastEvalReport)

# First-time setup: edit the generated files between these calls.
create_options_template()
build_crosswalk_from_options("report_options_template.R")
# Complete the crosswalk and set its path in report_options_template.R.

settings <- create_evaluation_config()
generate_report("report_options_template.R", eval_config = settings)

testing <- export_testing_evaluation(
  "report_options_template.R", output_dir = "exports",
  ai_form = TRUE, save_json = TRUE, save_results = TRUE,
  eval_config = settings, file_prefix = "testing"
)

realtime <- export_realtime_evaluation(
  "report_options_template.R", output_dir = "exports",
  ai_form = TRUE, save_json = TRUE, save_results = TRUE,
  eval_config = settings, file_prefix = "realtime",
  archive_current = TRUE
)

testing$paths
realtime$paths
```

<a id="create_options_template"></a>
## create_options_template

Create an editable R file containing the report settings and input-file paths.

**Result / files:** Writes report_options_template.R in the working folder and returns its path.

| Argument | Default | What it means |
| --- | --- | --- |
| `force` | `FALSE` | Allow an existing template or crosswalk file to be overwritten. FALSE stops if it already exists. |

<details>
<summary>Exact call signature</summary>

```r
create_options_template(
  force = FALSE
)
```

</details>

<a id="build_crosswalk_from_options"></a>
## build_crosswalk_from_options

Create the starter CSV where you describe your data sources, outcomes, and variables.

**Result / files:** Writes a variables crosswalk CSV and returns its path. Complete it before generating a report.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to your completed R options file. The variables.crosswalk.file setting can still be blank at this stage. |
| `force` | `FALSE` | Allow an existing template or crosswalk file to be overwritten. FALSE stops if it already exists. |

<details>
<summary>Exact call signature</summary>

```r
build_crosswalk_from_options(
  options_file,
  force = FALSE
)
```

</details>

<a id="build_variables_crosswalk"></a>
## build_variables_crosswalk

Create the same starter crosswalk from already prepared configuration and model data.

**Result / files:** Writes the starter CSV in the working folder and returns its path.

Advanced helper: prefer build_crosswalk_from_options() when starting from a report options file.

| Argument | Default | What it means |
| --- | --- | --- |
| `config` | `Required` | The validated report configuration used internally by the preparation pipeline. This is different from eval_config; normal users can use the options-file entry points instead. |
| `implementation_model` | `NULL` | An already validated operational forecast data frame. NULL, where allowed, means no implementation model is supplied. |
| `evaluation_model` | `NULL` | An already validated historical evaluation data frame. NULL, where allowed, means no evaluation model is supplied. |
| `force` | `FALSE` | Allow an existing template or crosswalk file to be overwritten. FALSE stops if it already exists. |

<details>
<summary>Exact call signature</summary>

```r
build_variables_crosswalk(
  config,
  implementation_model = NULL,
  evaluation_model = NULL,
  force = FALSE
)
```

</details>

<a id="create_evaluation_config"></a>
## create_evaluation_config

Choose the rules used to score forecasts and classify peaks, bias, and trends.

**Result / files:** Returns a settings list to pass as eval_config.

Percent Accuracy (Similarity Index) is the primary reported metric; MAE is a secondary point-forecast measure. Percent Accuracy describes forecast–observation similarity and is not the percentage of forecasts that were correct. Peak summaries use the observed peak within the testing period. The two stable_threshold arguments in this reference have different meanings; use trend_count_threshold here to change the trend override.

| Argument | Default | What it means |
| --- | --- | --- |
| `non_transmission_months` | `c(5, 6, 7)` | Calendar months excluded from testing-period aggregate scores: 5, 6, 7 means May–July. Plots retain these rows. Real-time evaluation includes all realized weeks. |
| `season_start_day_month` | `"August 01"` | Season boundary used for testing peak summaries. Although supplied as "Month DD", only the month is used. This does not change the trend-calibration season. |
| `peak_window` | `20` | Percentage below the observed testing-season maximum allowed inside the contiguous peak phase. 20 means at least 80% of that maximum; it is not a number of weeks. |
| `stable_threshold` | `10` | Minimum observed count for the stable-count percentage-bias summaries and bias-category eligibility. This is an observed-count floor, not a trend-change cutoff. |
| `pct_error_cushion` | `20` | Percentage-error band labeled Within Range. 20 means errors from −20% through +20%. |
| `timing_tol_steps` | `0L` | Allowed peak-timing miss, in time steps, still labeled On Time. Zero requires an exact timing match. |
| `mag_tol` | `0.8` | Minimum smaller/larger peak-magnitude ratio labeled On Target. 0.8 means the smaller magnitude is at least 80% of the larger. |
| `trend_count_threshold` | `10` | Absolute weekly count changes strictly below this value are labeled stable in the trend calculation. Separate from the bias floor stable_threshold. |
| `trend_calibration_data` | `NULL` | Optional historical truth data frame: location, date or target_end_date, and value or Observed; optionally population. NULL uses available selected truth before evaluation. Do not also supply trend_thresholds. |
| `trend_thresholds` | `NULL` | Optional saved table returned by calibrate_trend_thresholds(). Its calibration dates must precede evaluated issue/target dates. Do not also supply trend_calibration_data. |

<details>
<summary>Exact call signature</summary>

```r
create_evaluation_config(
  non_transmission_months = c(5, 6, 7),
  season_start_day_month = "August 01",
  peak_window = 20,
  stable_threshold = 10,
  pct_error_cushion = 20,
  timing_tol_steps = 0L,
  mag_tol = 0.8,
  trend_count_threshold = 10,
  trend_calibration_data = NULL,
  trend_thresholds = NULL
)
```

</details>

<a id="create_plot_styles"></a>
## create_plot_styles

Choose plot colors, line styles, uncertainty bands, sizes, and labels.

**Result / files:** Returns a settings list to pass as plot_styles.

List-valued defaults are summarized in the table for readability. The exact call signature below contains their full values. Actual code defaults are used here, including gray colors and a 1000 × 800 plot size.

| Argument | Default | What it means |
| --- | --- | --- |
| `colors` | `c("#898989")` | Color palette for evaluation-model lines. The current code defaults to gray (#898989). |
| `pi_intervals` | `Built-in list; see exact signature below` | Named list describing prediction bands: lower/upper quantiles, fill, hover color, label, and legend swatch class. Defaults define 50% and 95% intervals. |
| `horizon_styles` | `Built-in list; see exact signature below` | Named list of color, dash pattern, and line width for each forecast horizon; defaults cover horizons 1–4. |
| `target_data` | `list(color = "#000000", dash = "solid", width = 2, marker_size = 4)` | Color, dash pattern, line width, and marker size for observed truth. |
| `current_projections` | `list(color = "#648FFF", dash = "solid", width = 2, marker_size = 4)` | Color, dash pattern, line width, and marker size for current forecasts. |
| `historical_estimates` | `list(color = "#648FFF", dash = "dot", width = 1.5, marker_size = 3)` | Color, dash pattern, line width, and marker size for historical estimates. |
| `phase_colors` | `list(`Training Period` = "#345FAF", `Validation Period` = "#E4572E", `Testing Period` = "#2E8B57")` | Colors used to distinguish training, validation, and testing periods. |
| `plot_width` | `1000` | Plot width in pixels. |
| `plot_height` | `800` | Plot height in pixels. |
| `default_view_weeks` | `52L` | Number of weeks shown in the initial plot view. |
| `rangeslider_bgcolor` | `"#ffffff"` | Background color of the date-range slider. |
| `eval_legend_title` | `"Evaluation Model"` | Heading used for the evaluation-model entries in the legend. |
| `right_axis` | `Built-in list; see exact signature below` | Named settings for the auxiliary-variable axis: titles, outcomes treated as percentages, title font size/color, and title spacing. |

<details>
<summary>Exact call signature</summary>

```r
create_plot_styles(
  colors = c("#898989"),
  pi_intervals = list(`50%` = list(lower = 0.25, upper = 0.75, fill = "rgba(100,143,255,0.15)", hover_color = "#648FFF", label = "50% PI",      swatch_class = "pi-50"), `95%` = list(lower = 0.025, upper = 0.975, fill = "rgba(100,143,255,0.08)", hover_color = "#648FFF",      label = "95% PI", swatch_class = "pi-95")),
  horizon_styles = list(`1` = list(color = "#648FFF", dash = "solid", width = 1.5), `2` = list(color = "#FE6100", dash = "dash", width = 1.5),      `3` = list(color = "#DC267F", dash = "dot", width = 1.5), `4` = list(color = "#785EF0", dash = "dashdot", width = 1.5)),
  target_data = list(color = "#000000", dash = "solid", width = 2, marker_size = 4),
  current_projections = list(color = "#648FFF", dash = "solid", width = 2, marker_size = 4),
  historical_estimates = list(color = "#648FFF", dash = "dot", width = 1.5, marker_size = 3),
  phase_colors = list(`Training Period` = "#345FAF", `Validation Period` = "#E4572E", `Testing Period` = "#2E8B57"),
  plot_width = 1000,
  plot_height = 800,
  default_view_weeks = 52L,
  rangeslider_bgcolor = "#ffffff",
  eval_legend_title = "Evaluation Model",
  right_axis = list(title_percent = "Auxiliary Variables", title_default = "Weekly Tests", percent_outcomes = c("Weekly % Flu-Attributable ED-Visits",      "Weekly % RSV-Attributable ED-Visits", "Weekly % COVID-19-Attributable ED-Visits"), title_font_size = 16, title_font_color = "#111",      title_standoff = 25)
)
```

</details>

<a id="generate_report"></a>
## generate_report

Build the HTML forecast evaluation report from a completed options file.

**Result / files:** Writes HTML and returns the saved path(s); the normal implementation pathway also archives forecasts.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to the completed R options file, defining report_options and the input paths. Report/export calls need a completed variables crosswalk. |
| `output_dir` | `getwd()` | Folder where the requested output files are saved. getwd() means the current R working folder. |
| `output_file` | `NULL` | Optional report filename ending in .html. NULL builds it from the report settings; split reports append each location name. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |
| `plot_styles` | `NULL` | Appearance settings returned by create_plot_styles(). NULL uses the package defaults. |
| `eval_config` | `NULL` | Settings returned by create_evaluation_config(). NULL uses the package defaults. |
| `plot_export` | `NULL` | Advanced list requesting only a plot export. Prefer save_forecast_plot(). Ignored with a warning when split_by_location=TRUE. |
| `split_by_location` | `FALSE` | Create one self-contained HTML report per location when TRUE; otherwise use one report with a location selector. |
| `split_filename` | `NULL` | Optional per-location path template ending in .html, such as "{location}/report.html". Use {location} for a filename-safe label or {location_name} for the raw location value. Used only for split reports. |
| `location_crosswalk` | `NULL` | Optional CSV path or data frame with location and clean_name, overriding displayed location names. NULL uses built-in lookups. |
| `population_crosswalk` | `NULL` | Optional CSV path, location/population data frame, or named numeric vector. Supplies/overrides population denominators for trend calculations; NULL uses built-in lookups. |

<details>
<summary>Exact call signature</summary>

```r
generate_report(
  options_file,
  output_dir = getwd(),
  output_file = NULL,
  quiet = TRUE,
  plot_styles = NULL,
  eval_config = NULL,
  plot_export = NULL,
  split_by_location = FALSE,
  split_filename = NULL,
  location_crosswalk = NULL,
  population_crosswalk = NULL
)
```

</details>

<a id="export_testing_evaluation"></a>
## export_testing_evaluation

Score forecasts from the testing period and optionally save AI-ready Markdown, JSON, and CSV.

**Result / files:** Returns testing_eval, results, payload, paths, and optional context. No HTML rendering or forecast archiving.

Requires an evaluation model; an implementation model is optional. All three save flags default to FALSE: the call still computes and returns results. AI Markdown contains the summary; CSV/JSON include individual forecast results. A supplied training truth series is used as the selected truth source.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to the completed R options file, defining report_options and the input paths. Report/export calls need a completed variables crosswalk. |
| `output_dir` | `getwd()` | Folder where the requested output files are saved. getwd() means the current R working folder. |
| `ai_form` | `FALSE` | Write an AI-ready Markdown summary when TRUE. It prepares text for an AI to read; it does not call an AI service. |
| `save_results` | `FALSE` | Write the complete long-format results CSV, including individual forecast results, when TRUE. |
| `ai_row_detail` | `FALSE` | Include individual forecast tables in the Markdown summary. This has no effect on the full row detail included in CSV or JSON. |
| `eval_config` | `NULL` | Settings returned by create_evaluation_config(). NULL uses the package defaults. |
| `file_prefix` | `NULL` | Filename stem for exported files, without folder separators. NULL builds a name from the report settings. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |
| `return_context` | `FALSE` | Include the prepared inputs, configuration, metadata, crosswalk, and master data in the returned R object for inspection/reuse. |
| `save_json` | `FALSE` | Write structured JSON containing metadata, settings, results, forecast/truth pairs, metric definitions, WIS diagnostics, and trend calibration when TRUE. |
| `population_crosswalk` | `NULL` | Optional named numeric population vector or location/population data frame for trend labels. Unlike generate_report(), this argument is not documented to accept a CSV path. |

<details>
<summary>Exact call signature</summary>

```r
export_testing_evaluation(
  options_file,
  output_dir = getwd(),
  ai_form = FALSE,
  save_results = FALSE,
  ai_row_detail = FALSE,
  eval_config = NULL,
  file_prefix = NULL,
  quiet = TRUE,
  return_context = FALSE,
  save_json = FALSE,
  population_crosswalk = NULL
)
```

</details>

<a id="export_realtime_evaluation"></a>
## export_realtime_evaluation

Score archived operational forecasts when observed truth is available, and optionally save Markdown, JSON, and CSV.

**Result / files:** Returns realtime_eval, results, payload, paths, and optional context. Archives the current forecast by default.

Requires an implementation model. Scores all realized weeks, without testing-only off-season exclusions or peak metrics. If no observed targets are available, output records that status rather than reporting zero error. With save flags FALSE, no evaluation export is saved, but archive_current=TRUE still archives the current forecast.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to a completed R options file with an implementation model and completed variables crosswalk. An evaluation model is optional. |
| `output_dir` | `getwd()` | Folder where the requested output files are saved. getwd() means the current R working folder. |
| `ai_form` | `FALSE` | Write an AI-ready Markdown summary when TRUE. It prepares text for an AI to read; it does not call an AI service. |
| `save_results` | `FALSE` | Write the complete long-format results CSV, including individual forecast results, when TRUE. |
| `ai_row_detail` | `FALSE` | Include individual forecast tables in the Markdown summary. This has no effect on the full row detail included in CSV or JSON. |
| `eval_config` | `NULL` | Settings returned by create_evaluation_config(). NULL uses the package defaults. |
| `file_prefix` | `NULL` | Filename stem for exported files, without folder separators. NULL builds a name from the report settings. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |
| `return_context` | `FALSE` | Include the prepared inputs, configuration, metadata, crosswalk, and master data in the returned R object for inspection/reuse. |
| `save_json` | `FALSE` | Write structured JSON containing metadata, settings, results, forecast/truth pairs, metric definitions, WIS diagnostics, and trend calibration when TRUE. |
| `archive_current` | `TRUE` | Save the current implementation forecast into the archive before scoring. FALSE scores the existing archive without adding the current snapshot. |
| `population_crosswalk` | `NULL` | Optional named numeric population vector or location/population data frame for trend labels; pass data, not a CSV pathname. |

<details>
<summary>Exact call signature</summary>

```r
export_realtime_evaluation(
  options_file,
  output_dir = getwd(),
  ai_form = FALSE,
  save_results = FALSE,
  ai_row_detail = FALSE,
  eval_config = NULL,
  file_prefix = NULL,
  quiet = TRUE,
  return_context = FALSE,
  save_json = FALSE,
  archive_current = TRUE,
  population_crosswalk = NULL
)
```

</details>

<a id="calibrate_trend_thresholds"></a>
## calibrate_trend_thresholds

Calculate reusable trend cutoffs from historical observations before a chosen date.

**Result / files:** Returns a location-level threshold table with calibration dates, sample sizes, and status. Does not save a file by itself.

Calibration uses weekly changes after seasonal eligibility begins at the third consecutive nonzero week, in seasons running epidemiological week 40 through week 39. It calculates the 5th, 25th, 75th, and 95th percentiles separately by location. Insufficient or unsuitable history returns missing thresholds and a status explanation. Save the returned table with standard R saveRDS() and reload it with readRDS() to reuse frozen thresholds.

| Argument | Default | What it means |
| --- | --- | --- |
| `data` | `Required` | Historical weekly counts in a data frame with location, date or target_end_date, and value or Observed. An optional population column supplies denominators for each historical row. |
| `cutoff` | `Required` | Exclusive calibration end: one Date or a data frame with location and cutoff. Observations on or after this date are excluded. |
| `population` | `NULL` | Population lookup used if data has no population column: named numeric vector, location/population data frame, or one positive number for one location. NULL uses built-in lookup values. |
| `round_digits` | `NULL` | NULL (default) keeps full numeric precision. An explicit integer from 0 to 10 rounds the stored cutoffs for legacy reproducibility and changes classification, not just display. Recalibrate older rounded tables to recover precision. Rates are per 100,000 people. |

<details>
<summary>Exact call signature</summary>

```r
calibrate_trend_thresholds(
  data,
  cutoff,
  population = NULL,
  round_digits = NULL
)
```

</details>

<a id="trendcallcalculation"></a>
## trendCallCalculation

Label weekly observed and forecast changes as decreasing, stable, or increasing, using frozen historical cutoffs.

**Result / files:** Returns scored rows in df and calibration tables in location_percentiles/location_thresholds; empty input returns empty df and location_percentiles.

| Argument | Default | What it means |
| --- | --- | --- |
| `data.for.evaluation` | `Required` | Prepared evaluation data containing location, target_end_date, horizon, value (forecast), and Observed (truth). A usual source is export_testing_evaluation(...)$testing_eval$data. |
| `population` | `NULL` | Population denominator: named numeric vector, a data frame, or one positive number for one location. NULL uses the built-in lookup; custom values override it. |
| `population_location_col` | `"location"` | Name of the location-key column if population is a data frame. |
| `population_value_col` | `"population"` | Name of the population-count column if population is a data frame. |
| `rate_multiplier` | `1e+05` | Population rate scale. 100000 expresses counts per 100,000 people; saved calibration cutoffs retain their per-100,000 metadata. |
| `stable_threshold` | `10` | Force a stable trend when the absolute weekly raw-count change is strictly below this number. NULL disables this override. Different meaning from stable_threshold in create_evaluation_config(). |
| `week_days` | `7` | Spacing between weekly observations. This implementation requires 7; other values are rejected. |
| `calibration_data` | `attr(data.for.evaluation, "trend_history")` | Historical truth used to estimate cutoffs. By default, read the trend_history attribute attached to the prepared evaluation data. Evaluation outcomes are not used to fit thresholds. |
| `thresholds` | `NULL` | Optional previously calibrated table from calibrate_trend_thresholds(); uses the saved cutoffs rather than estimating new ones. |

<details>
<summary>Exact call signature</summary>

```r
trendCallCalculation(
  data.for.evaluation,
  population = NULL,
  population_location_col = "location",
  population_value_col = "population",
  rate_multiplier = 1e+05,
  stable_threshold = 10,
  week_days = 7,
  calibration_data = attr(data.for.evaluation, "trend_history"),
  thresholds = NULL
)
```

</details>

<a id="read_testing_evaluations"></a>
## read_testing_evaluations

Read exported evaluation CSVs and combine them into one long table.

**Result / files:** Returns a data frame with source_file identifying where each row came from.

| Argument | Default | What it means |
| --- | --- | --- |
| `files` | `Required` | CSV file paths, or one directory containing the exported results. A directory causes every .csv file in it to be read, so keep unrelated CSVs elsewhere. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |

<details>
<summary>Exact call signature</summary>

```r
read_testing_evaluations(
  files,
  quiet = TRUE
)
```

</details>

<a id="build_evaluation_table"></a>
## build_evaluation_table

Turn long evaluation results into a table with a column for each model or input file.

**Result / files:** Returns long (filtered input) and table (wide display table); optionally writes a CSV.

This reshapes existing scores; it does not recompute or reconcile evaluation periods. Keep testing and real-time results separate unless deliberately filtered. Duplicate row/column keys produce a warning and the first value is retained.

| Argument | Default | What it means |
| --- | --- | --- |
| `x` | `Required` | Exported results CSV paths, a directory of them, or an already loaded long-format results data frame. |
| `family` | `NULL` | Keep selected metric-family names from the results. NULL keeps all; compatibility names include percentAgreement (Similarity Index), forecastBias, peakPhase, and traditional. |
| `scope` | `NULL` | Keep selected values from the scope column, such as overall, horizon, season, or row. NULL keeps all. |
| `metric` | `NULL` | Keep selected metric names exactly as they appear in the results. NULL keeps all. |
| `columns` | `"model"` | Choose what defines each display column: "model" or "source_file". |
| `output_file` | `NULL` | Optional filename/path for saving the wide table as CSV. NULL returns it in R without saving. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |

<details>
<summary>Exact call signature</summary>

```r
build_evaluation_table(
  x,
  family = NULL,
  scope = NULL,
  metric = NULL,
  columns = "model",
  output_file = NULL,
  quiet = TRUE
)
```

</details>

<a id="save_forecast_plot"></a>
## save_forecast_plot

Save the forecast plot, or the forecast-consistency plot, as an interactive HTML file or static image.

**Result / files:** Returns saved file path(s). Static formats require webshot2 and a supported Chrome/Chromium browser; TIFF also uses magick.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to the completed R options file, defining report_options and the input paths. Report/export calls need a completed variables crosswalk. |
| `file` | `NULL` | Output file path or folder. NULL uses the working folder and an automatic name. A recognized file extension determines the format when format is omitted. |
| `format` | `c("html", "png", "jpeg", "jpg", "pdf", "tiff", "tif")` | Choose html, png, jpeg/jpg, pdf, or tiff/tif. Defaults to html unless inferred from the filename. |
| `plot` | `c("forecast", "consistency")` | Choose "forecast" for the forecast plot or "consistency" to show forecasts across issue dates. Defaults to forecast. |
| `location` | `NULL` | Location display name or its positive alphabetical index. NULL includes all: one selectable HTML page or separate static files. |
| `width` | `NULL` | Static capture width in pixels; NULL uses 1200 for static output. |
| `height` | `NULL` | Static capture height in pixels; NULL uses 750 for static output. |
| `scale` | `1` | Image capture zoom/resolution multiplier. A larger value produces more pixels. |
| `delay` | `NULL` | Seconds to wait before capturing the rendered plot. NULL lets the function choose. |
| `n_forecasts` | `5L` | Number of recent forecasts displayed for the consistency plot. Used unless forecast_every selects a different spacing. |
| `forecast_every` | `NULL` | For consistency plots, select every Nth forecast from the chosen starting window. A positive integer overrides n_forecasts. |
| `end_cushion` | `NULL` | For consistency plots with forecast_every, skip this many most-recent forecast dates before selecting the spaced sequence. |
| `font_size` | `NULL` | Optional base plot font size. |
| `tick_size` | `NULL` | Optional axis tick-label font size for the consistency plot. |
| `forecast_size` | `NULL` | Optional forecast median-line width for the consistency plot. |
| `forecast_color` | `NULL` | Optional single color for consistency-plot forecast lines/markers. Does not recolor uncertainty bands. |
| `observed_size` | `NULL` | Optional observed-truth line width for the consistency plot. |
| `observed_color` | `NULL` | Optional observed-truth line/marker color for the consistency plot. |
| `aux_size` | `NULL` | Optional auxiliary-variable line width on the consistency plot’s secondary axis. |
| `aux_color` | `NULL` | One color, or a recycled palette, for auxiliary-variable lines on the consistency plot. |
| `title_gap` | `NULL` | Optional spacing in pixels between axis tick labels and axis titles for the consistency plot. |
| `view_start` | `NULL` | Initial visible start date, as a Date or YYYY-MM-DD string. Supply together with view_end; it changes the view, not the underlying data. |
| `view_end` | `NULL` | Initial visible end date, as a Date or YYYY-MM-DD string. Supply together with view_start. |
| `legend` | `TRUE` | Show a side legend on static exports when TRUE. HTML retains its interactive legend regardless. |
| `overwrite` | `TRUE` | Allow replacement of an existing output file when TRUE. |
| `plot_styles` | `NULL` | Appearance settings returned by create_plot_styles(). NULL uses the package defaults. |
| `eval_config` | `NULL` | Settings returned by create_evaluation_config(). NULL uses the package defaults. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |

<details>
<summary>Exact call signature</summary>

```r
save_forecast_plot(
  options_file,
  file = NULL,
  format = c("html", "png", "jpeg", "jpg", "pdf", "tiff", "tif"),
  plot = c("forecast", "consistency"),
  location = NULL,
  width = NULL,
  height = NULL,
  scale = 1,
  delay = NULL,
  n_forecasts = 5L,
  forecast_every = NULL,
  end_cushion = NULL,
  font_size = NULL,
  tick_size = NULL,
  forecast_size = NULL,
  forecast_color = NULL,
  observed_size = NULL,
  observed_color = NULL,
  aux_size = NULL,
  aux_color = NULL,
  title_gap = NULL,
  view_start = NULL,
  view_end = NULL,
  legend = TRUE,
  overwrite = TRUE,
  plot_styles = NULL,
  eval_config = NULL,
  quiet = TRUE
)
```

</details>

<a id="save_consistency_plot"></a>
## save_consistency_plot

Save the plot showing how forecasts changed between issue dates.

**Result / files:** Calls save_forecast_plot with plot="consistency" and returns saved file path(s).

See save_forecast_plot for the meaning and default of every forwarded argument. The ... argument is optional, not a required value.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `Required` | Path to the completed R options file, defining report_options and the input paths. Report/export calls need a completed variables crosswalk. |
| `...` | `Optional extra arguments` | Optional arguments passed to save_forecast_plot(), with the same defaults: file, format, location, width, height, scale, delay, n_forecasts, forecast_every, end_cushion, font_size, tick_size, forecast_size, forecast_color, observed_size, observed_color, aux_size, aux_color, title_gap, view_start, view_end, legend, overwrite, plot_styles, eval_config, quiet. Do not supply plot; this wrapper sets it to "consistency". |

<details>
<summary>Exact call signature</summary>

```r
save_consistency_plot(
  options_file,
  ...
)
```

</details>

<a id="extract_implementation_data"></a>
## extract_implementation_data

Pull dates, locations, outcome names, and forecast-period information from validated operational forecasts.

**Result / files:** Returns metadata; writes current projections under Forecasts/ unless save_data=FALSE.

Advanced helper: the validated report config is produced by the package’s internal preparation pipeline, not by create_evaluation_config(). Most users should call generate_report() or an export function instead.

| Argument | Default | What it means |
| --- | --- | --- |
| `implementation_model` | `Required` | An already validated operational forecast data frame. NULL, where allowed, means no implementation model is supplied. |
| `config` | `Required` | The validated report configuration used internally by the preparation pipeline. This is different from eval_config; normal users can use the options-file entry points instead. |
| `save_data` | `TRUE` | Write current forecast rows into Forecasts/ when TRUE; FALSE only returns metadata. |

<details>
<summary>Exact call signature</summary>

```r
extract_implementation_data(
  implementation_model,
  config,
  save_data = TRUE
)
```

</details>

<a id="extract_evaluation_data"></a>
## extract_evaluation_data

Split validated evaluation data into training, validation, and testing subsets and collect their metadata.

**Result / files:** Returns the subsets, date ranges, and other report metadata.

Advanced helper: config is the validated report configuration, not the metric settings returned by create_evaluation_config().

| Argument | Default | What it means |
| --- | --- | --- |
| `evaluation_model` | `Required` | An already validated historical evaluation data frame. NULL, where allowed, means no evaluation model is supplied. |
| `config` | `Required` | The validated report configuration used internally by the preparation pipeline. This is different from eval_config; normal users can use the options-file entry points instead. |
| `impl_meta` | `NULL` | Metadata returned by extract_implementation_data(). NULL means derive the shared metadata from the evaluation file itself. |

<details>
<summary>Exact call signature</summary>

```r
extract_evaluation_data(
  evaluation_model,
  config,
  impl_meta = NULL
)
```

</details>

<a id="validate_covidhub_model"></a>
## validate_covidhub_model

Check a COVIDHub forecast CSV against the format rules implemented in this package.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |

<details>
<summary>Exact call signature</summary>

```r
validate_covidhub_model(
  file,
  verbose = FALSE
)
```

</details>

<a id="validate_flusight_model"></a>
## validate_flusight_model

Check a FluSight forecast CSV against the format rules implemented in this package.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |

<details>
<summary>Exact call signature</summary>

```r
validate_flusight_model(
  file,
  verbose = FALSE
)
```

</details>

<a id="validate_rsvhub_model"></a>
## validate_rsvhub_model

Check an RSVHub forecast CSV against the format rules implemented in this package.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |

<details>
<summary>Exact call signature</summary>

```r
validate_rsvhub_model(
  file,
  verbose = FALSE
)
```

</details>

<a id="validate_metrocast_model"></a>
## validate_metrocast_model

Check a Flu MetroCast forecast CSV against the format rules implemented in this package.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |

<details>
<summary>Exact call signature</summary>

```r
validate_metrocast_model(
  file,
  verbose = FALSE
)
```

</details>

<a id="validate_general_model"></a>
## validate_general_model

Check an operational forecast CSV in the package’s general format, including its geography fields.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |
| `state_context` | `NA_character_` | Optional two-letter U.S. state abbreviation to restrict county/HSA matching. NA_character_ means no state restriction. |

<details>
<summary>Exact call signature</summary>

```r
validate_general_model(
  file,
  verbose = FALSE,
  state_context = NA_character_
)
```

</details>

<a id="validate_eval_model"></a>
## validate_eval_model

Check a historical evaluation forecast CSV in the package’s evaluation format.

**Result / files:** Returns the loaded, validated data frame or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `file` | `Required` | Path to the forecast submission CSV in the format named by this validator. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |
| `state_context` | `NA_character_` | Optional two-letter U.S. state abbreviation to restrict county/HSA matching. NA_character_ means no state restriction. |

<details>
<summary>Exact call signature</summary>

```r
validate_eval_model(
  file,
  verbose = FALSE,
  state_context = NA_character_
)
```

</details>

<a id="validate_variables_crosswalk"></a>
## validate_variables_crosswalk

Check that the crosswalk describing your variables and data sources is complete and correctly formatted.

**Result / files:** Returns the validated crosswalk, with logical fields normalized, or stops with input errors.

Validation checks the input rules implemented in this package; it does not establish that the forecasting method is scientifically correct.

| Argument | Default | What it means |
| --- | --- | --- |
| `x` | `Required` | Path to a completed variables-crosswalk CSV, or the already loaded crosswalk data frame. |
| `verbose` | `FALSE` | Print a success summary after validation when TRUE. |

<details>
<summary>Exact call signature</summary>

```r
validate_variables_crosswalk(
  x,
  verbose = FALSE
)
```

</details>

<a id="pipe"></a>
## %>%

Pass a value into the next function call so several steps can be chained together.

**Result / files:** Returns the result of the next step; re-exported from magrittr.

| Argument | Default | What it means |
| --- | --- | --- |
| `lhs` | `Required` | The data/value on the left side of the pipe. |
| `rhs` | `Required` | The function call on the right. The left-side value normally becomes its first argument; use . as the placeholder when needed. |

<details>
<summary>Exact call signature</summary>

```r
lhs %>% rhs
```

</details>

## Documented helpers not exported in this build

These are included for completeness, but are not normal public calls in the delivered package.

<a id="generate_comparison_dashboard"></a>
## generate_comparison_dashboard

Build a local interactive HTML dashboard comparing two or more model reports.

**Result / files:** Returns the saved HTML path. Defined and documented, but absent from this build’s public exports.

Availability note: this function has documentation and an @export annotation, but is missing from NAMESPACE in the delivered build. It is not attached by library(forecastEvalReport) or available through forecastEvalReport::generate_comparison_dashboard.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_files` | `Required` | Paths to at least two completed report options files, one for each model/run. |
| `output_file` | `"forecast-model-comparison.html"` | Path of the self-contained HTML dashboard to write. |
| `title` | `"Model Summaries"` | Title shown in the dashboard. |
| `evaluation` | `c("both", "testing", "realtime")` | Choose "both", "testing", or "realtime". The first choice, both, is the default. |
| `eval_config` | `NULL` | Settings returned by create_evaluation_config(). NULL uses the package defaults. |
| `include_row_metrics` | `TRUE` | Include individual forecast metrics in the dashboard when TRUE; FALSE reduces the output size. |
| `overwrite` | `TRUE` | Allow replacement of an existing output file when TRUE. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |

<details>
<summary>Exact call signature</summary>

```r
generate_comparison_dashboard(
  options_files,
  output_file = "forecast-model-comparison.html",
  title = "Model Summaries",
  evaluation = c("both", "testing", "realtime"),
  eval_config = NULL,
  include_row_metrics = TRUE,
  overwrite = TRUE,
  quiet = TRUE
)
```

</details>

<a id="save_report_pdf"></a>
## save_report_pdf

Print a newly generated or existing HTML report to PDF.

**Result / files:** Returns saved PDF path(s). Requires pagedown and headless Chrome; absent from this build’s public exports.

Availability note: this function has documentation and an @export annotation, but is missing from NAMESPACE in the delivered build. It is not attached by library(forecastEvalReport) or available through forecastEvalReport::save_report_pdf. PDF printing captures the active location; supply separate per-location reports to print every location.

| Argument | Default | What it means |
| --- | --- | --- |
| `options_file` | `NULL` | Options file for generating a report before printing. Supply either this or html_file, but not both. |
| `html_file` | `NULL` | Path(s) to existing HTML report(s) to print. Supply either this or options_file, but not both. |
| `output_dir` | `getwd()` | Folder where the requested output files are saved. getwd() means the current R working folder. |
| `output_file` | `NULL` | Optional PDF filename. NULL generates a name; this setting is ignored when printing several HTML inputs. |
| `paper` | `First choice by default; see exact signature below` | Paper size/orientation. Defaults to tabloid-landscape; other supported sizes are listed in the exact signature below. Use custom with width and height. |
| `width` | `NULL` | Custom paper width in inches, used only with paper="custom". |
| `height` | `NULL` | Custom paper height in inches, used only with paper="custom". |
| `margin` | `0.4` | Page margin on all sides, in inches. |
| `scale` | `1` | Content zoom, from 0.1 to 2. Smaller values fit more content on a page. |
| `wait` | `8` | Seconds to wait after the HTML loads so plots finish drawing. |
| `background` | `TRUE` | Print background colors and images when TRUE. |
| `expand_accordions` | `TRUE` | Open collapsible report sections before printing when TRUE. |
| `expand_legends` | `TRUE` | Expand each plot legend and place it beside the plot when TRUE. |
| `hide_controls` | `TRUE` | Hide interactive-only controls such as plot toolbars and location selectors before printing. |
| `keep_html` | `FALSE` | Keep the intermediate print-ready HTML for inspection when TRUE. |
| `overwrite` | `TRUE` | Allow replacement of an existing output file when TRUE. |
| `quiet` | `TRUE` | Suppress routine progress or rendering messages. Set FALSE to see more detail; this does not disable input checks. |
| `...` | `Optional extra arguments` | Optional additional generate_report() arguments when using options_file, for example plot_styles, eval_config, location_crosswalk, or population_crosswalk. |

<details>
<summary>Exact call signature</summary>

```r
save_report_pdf(
  options_file = NULL,
  html_file = NULL,
  output_dir = getwd(),
  output_file = NULL,
  paper = c("tabloid-landscape", "tabloid", "letter-landscape", "letter", "legal-landscape", "legal", "a3-landscape", "a3", "a4-landscape",      "a4", "custom"),
  width = NULL,
  height = NULL,
  margin = 0.4,
  scale = 1,
  wait = 8,
  background = TRUE,
  expand_accordions = TRUE,
  expand_legends = TRUE,
  hide_controls = TRUE,
  keep_html = FALSE,
  overwrite = TRUE,
  quiet = TRUE,
  ...
)
```

</details>

## Scope and checks

All 24 public entries and 2 documented unexported helpers are included. The tables contain 166 declared argument entries across these functions, with forwarded arguments explained under `save_consistency_plot()` and `save_report_pdf()`.

This reference documents the corrected local package supplied in this conversation. It does not change package code, exports, or GitHub. Call examples are templates: use your completed options and data files.
