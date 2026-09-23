# Installed Package Resources

Files under `inst/` are copied into the installed R package. They provide methods documentation, setup material, report templates, and dashboard assets needed outside the source checkout.

## Directory contents

```text
inst/
├── README.md
├── METHODS.md
├── forecast-eval-setup.html
├── comparison-dashboard/
│   ├── README.md
│   ├── comparison.css
│   └── comparison.js
└── reports/
    ├── README.md
    ├── forecast_evaluation.Rmd
    ├── accordion.css
    ├── fs-style.css
    ├── styles.css
    ├── float-legend.js
    ├── plot-max-phase.js
    ├── plotly_fullscreen.js
    ├── plotly_rangeslider.js
    ├── sync-geo-dropdowns.js
    └── syncLegendCheckboxesOnRender.js
```

### Directory guide

| Path | Purpose |
| --- | --- |
| `METHODS.md` | Canonical installed evaluation methods and interpretation limitations. |
| `forecast-eval-setup.html` | Rendered setup guide. |
| `reports/` | R Markdown report template plus CSS/JavaScript. |
| `comparison-dashboard/` | CSS/JavaScript for comparison dashboards. |

`METHODS.md` is copied to the repository-level `Evaluation/` directory for distribution with published results. The two copies currently match.

Resolve installed resources with `system.file(..., package = "forecastEvalReport")`, not source-tree assumptions. When changing browser assets, syntax-check JavaScript and render a representative report/dashboard. When changing evaluation logic, update `METHODS.md`, exported notes, and rendered report text together.
