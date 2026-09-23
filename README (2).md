# **🚧 IN DEVELOPMENT 🚧**

**This repository is under active development.** Code, input requirements, evaluation methods, outputs, and documentation may change. Review your inputs and independently check important results before relying on them.

# forecastEvalReport

`forecastEvalReport` is an R package for evaluating forecasts against observed outcomes and generating reports. The `main` branch provides report setup tools, input validators, interactive HTML reports, and testing-period evaluation exports.

## Available on `main`

- Create a report-options file and a starter variables crosswalk.
- Validate supported forecast formats and crosswalks.
- Generate interactive HTML reports with forecast plots and testing-period and real-time evaluation sections.
- Export testing-period results as a tidy CSV and an optional AI-ready Markdown summary. These files prepare results for review; the package does not call an AI service or generate AI analysis.
- Customize evaluation settings and plot styles, and save forecast or consistency plots.

## Installation

Install the `main` branch from GitHub:

```r
install.packages("remotes")
remotes::install_github("bleicham/forecastEvalReport", ref = "main")
```

The installed R package is named `forecastEvalReport`:

```r
library(forecastEvalReport)
```

## First report

The package uses an R options file for model paths and report metadata, plus a completed variables crosswalk describing the data sources and variables.

```r
library(forecastEvalReport)

# Create the starter options file, then edit its required fields and paths.
create_options_template()
options_file <- "report_options_template.R"

# After filling in the options file, create a starter crosswalk.
crosswalk_file <- build_crosswalk_from_options(options_file)

# Complete the generated CSV, add its path to the options file, and render.
generate_report(options_file)

# Optional: export testing-period results for review.
export_testing_evaluation(
  options_file,
  ai_form = TRUE,
  save_results = TRUE
)
```

The options template and validator messages describe required fields and accepted input formats. At least one implementation or evaluation model file is required. Review the generated crosswalk and report; successful validation does not establish that the data or mappings are appropriate for your analysis.

## Main branch and beta branch

The [beta branch](https://github.com/bleicham/forecastEvalReport-beta/tree/beta) contains newer experimental work that is not included in this `main` snapshot.

| Capability | `main` | Beta branch additions |
| --- | --- | --- |
| Report generation, model-file validation, testing-period export to Markdown/CSV | Included | Included |
| Historical trend calibration and population-adjusted trend-phase reporting | Not included | Added |
| JSON output for evaluation exports and a standalone real-time evaluation export | Not included | Added |
| Interactive multi-model comparison dashboard | Not included | Added |
| PDF export of a complete report | Not included | Added |
| Evaluation-method updates, including revised WIS eligibility and clearer handling of unscorable results | Not included | Added |

**Beta API note:** In the beta files compared for this README, some new functions are present in source and help files but are not listed in the checked-in `NAMESPACE` for export, including `export_realtime_evaluation()`, `generate_comparison_dashboard()`, `save_report_pdf()`, and `calibrate_trend_thresholds()`. Treat those additions as experimental; they may not be callable as regular functions from an installed package until the exports are updated. Beta also adds options such as JSON output to functions that are already exported.

Results from `main` and beta should not be assumed directly comparable: beta changes some evaluation rules and interpretation. Read the beta method notes and verify the settings when comparing outputs.

## Documentation and examples

Use R help for function arguments and examples, such as `?generate_report` and `?export_testing_evaluation`. The [`Examples/`](Examples/) directory contains example workflows and data.

## License

This project is distributed under the [MIT License](LICENSE.md).
