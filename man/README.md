# R Help Pages

This directory contains roxygen-generated `.Rd` help for public functions and packaged datasets.

## Directory contents

The folder is flat and contains one generated help page per documented function or dataset:

```text
man/
├── README.md
├── generate_report.Rd
├── generate_comparison_dashboard.Rd
├── export_testing_evaluation.Rd
├── export_realtime_evaluation.Rd
├── create_*.Rd
├── validate_*.Rd
├── extract_*.Rd
├── save_*.Rd
├── calibrate_trend_thresholds.Rd
├── trendCallCalculation.Rd
└── *_locations.Rd / packaged-data pages
```

Use `../NAMESPACE` for the definitive list of exported functions.

After installation:

```r
library(forecastEvalReport)
?generate_report
?export_realtime_evaluation
?calibrate_trend_thresholds
```

The authoritative help source is the roxygen block in the corresponding file under `../R/`. Do not patch `.Rd` independently. Update source comments, regenerate documentation, inspect `man/` and `NAMESPACE`, rebuild/install, and verify examples and changed behavior.

`calibrate_trend_thresholds.Rd` in this version documents the full-precision default and explicit legacy-rounding option.
