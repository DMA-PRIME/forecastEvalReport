# Comparison Dashboard Assets

This directory provides the browser layer for dashboards produced by `generate_comparison_dashboard()`.

## Directory contents

```text
comparison-dashboard/
├── README.md
├── comparison.css
└── comparison.js
```

| File | Responsibility |
| --- | --- |
| `comparison.css` | Layout, typography, controls, tables, plots, and responsive presentation. |
| `comparison.js` | Client-side filtering, metric displays, forecast/trend transformation, and rendering. |

The R generator supplies the embedded data and HTML shell; these files control offline browser behavior. Keep JavaScript field expectations synchronized with the R payload.

Developer check from the overall repository root:

```bash
node --check forecastEvalReport-BETA/inst/comparison-dashboard/comparison.js
```

Test changed classification/rendering methods with controlled fixtures. Avoid remote dependencies when the dashboard must remain portable and self-contained.
