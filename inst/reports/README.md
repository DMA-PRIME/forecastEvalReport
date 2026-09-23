# Report Template and Assets

This directory contains installed resources used by `generate_report()`.

## Directory contents

```text
reports/
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

| File | Role |
| --- | --- |
| `forecast_evaluation.Rmd` | Main parameterized report template. |
| `styles.css`, `fs-style.css`, `accordion.css` | Layout, visual theme, and collapsible sections. |
| `float-legend.js` | Floating legend behavior. |
| `plot-max-phase.js` | Plot/phase interaction helper. |
| `plotly_fullscreen.js` | Full-screen Plotly controls. |
| `plotly_rangeslider.js` | Range-slider behavior. |
| `sync-geo-dropdowns.js` | Synchronizes geography selectors. |
| `syncLegendCheckboxesOnRender.js` | Synchronizes legend checkbox state after rendering. |

`generate_report()` validates options, assembles data/evaluations, and renders the R Markdown template with these assets into a self-contained HTML report.

Maintain IDs/classes in R-generated HTML, CSS, and JavaScript together. Render examples with one/multiple locations, missing truth, median-only forecasts, and full quantile forecasts. Check browser-console errors, controls, keyboard focus, contrast, and non-color cues. Fix reusable issues here and regenerate reports rather than patching final HTML.
