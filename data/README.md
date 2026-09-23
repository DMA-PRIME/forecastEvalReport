# Packaged R Data

This directory contains serialized lookup objects installed with `forecastEvalReport`.

## Directory contents

```text
data/
├── README.md
├── dartmouth_hsa_zip.rda
├── default_population_crosswalk.rda
├── hubverse_locations.rda
├── metrocast_locations.rda
└── us_counties.rda
```

| File/object | Purpose |
| --- | --- |
| `default_population_crosswalk.rda` | Default population values for population-scaled trend calculations. |
| `hubverse_locations.rda` | Hubverse location lookup. |
| `metrocast_locations.rda` | MetroCast location lookup. |
| `us_counties.rda` | US county lookup. |
| `dartmouth_hsa_zip.rda` | ZIP-to-Dartmouth Hospital Service Area mapping. |

Use these through the installed package (for example, `utils::data(..., package = "forecastEvalReport")`) rather than depending on serialization internals.

The raw source CSVs/build scripts are not included in this trimmed distribution. Do not edit binary `.rda` files. To update them, obtain the authoritative raw-data build sources, verify provenance and location-key handling, regenerate the object, update documentation, and run package/behavior checks.
