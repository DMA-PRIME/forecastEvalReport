# R Source Code

This directory is the authoritative implementation of `forecastEvalReport`. Package installation loads these files into one namespace; filenames group responsibilities but do not create separate R modules.

## Directory contents

The directory is intentionally flat. Files are grouped below by naming convention rather than physical subfolders:

```text
R/
├── README.md
├── generate_report.R
├── generate_comparison_dashboard.R
├── export_testing_evaluation.R
├── export_realtime_evaluation.R
├── evaluation_export_helpers.R
├── create_*.R                  # user configuration/style factories
├── validate_*.R                # input and schema validators
├── extract_*.R                 # input extraction
├── assemble_*.R                # report-data assembly
├── prepare_*.R                 # scoring/plot preparation
├── *Calculation.R              # metric calculations
├── calibrate_trend_thresholds.R
├── build_*.R                   # traces, tables, legends, archives, components
├── make_*_plot.R               # metric-specific plots
├── section_*.R                 # complete report sections
├── save_*.R / write_*.R        # file outputs
└── *_locations.R and crosswalk helpers
```

Use `../NAMESPACE` for the exact exported-function list and `../man/` for public help pages.

## Common public entry points

| Task | Functions |
| --- | --- |
| Configure | `create_options_template()`, `create_evaluation_config()`, `create_plot_styles()` |
| Build metadata | `build_crosswalk_from_options()`, `build_variables_crosswalk()` |
| Validate | `validate_flusight_model()`, `validate_covidhub_model()`, `validate_rsvhub_model()`, `validate_metrocast_model()`, `validate_general_model()`, `validate_eval_model()`, `validate_variables_crosswalk()` |
| Extract | `extract_implementation_data()`, `extract_evaluation_data()`, `read_testing_evaluations()` |
| Evaluate/export | `export_testing_evaluation()`, `export_realtime_evaluation()`, `build_evaluation_table()` |
| Report | `generate_report()`, `save_forecast_plot()`, `save_consistency_plot()` |
| Trend methods | `calibrate_trend_thresholds()`, `trendCallCalculation()` |

The exported API is defined by `../NAMESPACE`; installed help is under `../man/`.

## Source families

| Pattern | Responsibility |
| --- | --- |
| `validate_*` | Schema, type, semantic, crosswalk, and report-option checks. |
| `extract_*`, `assemble_*`, `prepare_*` | Read, normalize, join, and package forecast/truth/report data. |
| `*Calculation.R`, `calibrate_*` | Metric and historical trend-calibration methods. |
| `build_*` | Tables, traces, legends, archives, configuration, and reusable UI/data components. |
| `make_*_plot.R` | Plot construction for individual metrics. |
| `section_*` | HTML report sections used by the installed R Markdown template. |
| `export_*`, `evaluation_export_helpers.R` | Evaluation assembly and Markdown/JSON/CSV serialization. |
| `generate_*` | High-level report and dashboard generation. |
| `write_*`, `save_*` | Standalone HTML/image/PDF outputs. |

## Data flow

```text
options -> validation -> extraction/normalization -> forecast/truth join
        -> calculations -> report sections or export serialization
```

Real-time evaluation can archive the current implementation forecast before reading historical snapshots. Testing evaluation uses the configured evaluation-model file and does not create an archive snapshot.

## Trend precision behavior

`calibrate_trend_thresholds()` now defaults to `round_digits = NULL`, preserving type-7 percentile cutoffs at full numeric precision. Display helpers format a copy to eight significant digits; classification uses stored numeric values. An explicit integer rounding value changes the stored cutoffs for legacy reproducibility. Previously saved rounded tables remain rounded unless recalibrated.

## Contribution rules

- Read `../inst/METHODS.md` before changing metrics or trend classification.
- Add/update roxygen blocks for public functions and regenerate `NAMESPACE`/`man/`.
- Preserve location keys as character values.
- Preserve unavailable states; never coerce invalid/missing scores to zero.
- Keep calibration strictly before evaluated dates to prevent leakage.
- Update calculations, definitions, report text, and exports together when a method changes.
- Keep CSS/JavaScript field contracts synchronized with R payload builders.

This distribution does not include its development regression-test directory. At minimum, build/check the package, syntax-check JavaScript, and validate behavioral changes against controlled fixtures outside the packaged output tree.
