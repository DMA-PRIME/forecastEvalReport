# **🚧 BETA — IN DEVELOPMENT 🚧**

> **Use with caution.** `forecastEvalReport` is an experimental beta branch and has **not been fully vetted**. Its code, evaluation methods, defaults, outputs, and documentation may contain errors or change without notice. Independently review the input data, configuration, and results with appropriate subject-matter and statistical expertise before relying on them. Do not use this beta as the sole basis for consequential clinical, public-health, operational, or policy decisions.

# forecastEvalReport

`forecastEvalReport` is an R package for preparing forecast-evaluation reports from forecast files, observed outcome data, and a user-completed variables crosswalk. It includes input validators, interactive HTML reporting, and testing-period evaluation exports.

## What this beta includes

- Create an editable report-options file and a starter variables crosswalk.
- Validate supported forecast-file formats and the completed crosswalk.
- Generate an interactive HTML forecast-evaluation report with plots, summaries, and data definitions.
- Export testing-period evaluation results as tidy CSV and JSON, with an optional AI-ready Markdown summary. The export formats prepare results for review; **the package does not call an AI service or generate AI analysis**.
- Configure evaluation settings and plot styles.

The available metrics and report sections depend on the supplied data. Read the [evaluation methods and limitations](inst/METHODS.md) before interpreting results.

## Installation

Install the beta branch from GitHub:

```r
install.packages("remotes")
remotes::install_github("bleicham/forecastEvalReport-beta", ref = "beta")
```

The installed R package is named `forecastEvalReport`:

```r
library(forecastEvalReport)
```

## First report

The package uses a report-options R file to describe your model files, observed outcome data, and report metadata. It also needs a completed variables crosswalk that describes the variables and sources used in the report.

```r
library(forecastEvalReport)

# Create the starter options file in your working directory.
create_options_template()
options_file <- "report_options_template.R"

# Edit the options file first: fill in its required fields and input paths.
# Then create a starter crosswalk from those settings and model files.
crosswalk_file <- build_crosswalk_from_options(options_file)

# Review and complete the generated crosswalk CSV. Add its path to
# variables.crosswalk.file in the options file, then render the report.
generate_report(options_file)
```

The options template and validator messages describe required fields and accepted input formats. At least one implementation or evaluation model file is required. Evaluation model files must contain historical forecast rows in the required format. Review the generated crosswalk carefully; a successful validation cannot establish that the supplied data or mappings are scientifically appropriate.

## Main functions

| Function | Purpose |
| --- | --- |
| `create_options_template()` | Write an editable report-options file. |
| `build_crosswalk_from_options()` | Create a starter variables crosswalk from a completed options file. |
| `generate_report()` | Render an interactive HTML evaluation report. |
| `export_testing_evaluation()` | Score testing-period forecasts and optionally write CSV, JSON, and AI-ready Markdown outputs. |
| `create_evaluation_config()` | Set evaluation options such as season, peak, and trend thresholds. |
| `create_plot_styles()` | Customize report plot styles. |

For arguments and examples, see the [user function reference](Documentation/USER-FUNCTIONS.md). The [`Examples/`](Examples/) directory contains example workflows and data.

## Beta limitations and responsible use

This branch is still being developed. Interfaces, input requirements, calculations, default settings, and report contents may change between commits. Some features or combinations of inputs may be incomplete or insufficiently tested.

Before using an output:

1. Verify the input files, date and location keys, outcome mappings, and crosswalk definitions.
2. Review the method definitions, settings, and any unavailable or unscorable results.
3. Independently check important results against a trusted calculation or review process.
4. Reassess the workflow after updating the package, since results and interfaces may change.

A generated report is an analysis aid; it does not certify forecast quality or replace domain review. See [Evaluation methods](inst/METHODS.md) for metric definitions and known interpretation limits.

## License

This project is distributed under the [MIT License](LICENSE.md).
