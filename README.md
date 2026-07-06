# Reciprocal health dynamics

Rendered vignettes and the full analysis transcript: <https://siardv.github.io/diet-activity-bmi-riclpm/>

Research compendium for **Socioeconomic Variations in the Reciprocal Relationships Between Diet Quality, Physical Activity, and Body Mass Index: A Random-Intercept Cross-Lagged Panel Study** (target journal: *Social Science & Medicine*).

Seven annual waves of the Dutch [LISS panel](https://www.lissdata.nl) (analytic N = 5,676 under full-information maximum likelihood) are analysed with random-intercept cross-lagged panel models (RI-CLPM) in `lavaan`, with household educational attainment as a three-group moderator. Headline results: covariation among BMI, physical activity, and fruit and vegetable consumption is predominantly between persons; the trait-level BMI-PA association is negative, significant, and uniform across socioeconomic strata; within persons, only the BMI-PA pair shows a small, reliable, mutually reinforcing pattern.

The compendium doubles as a worked use case for two packages:

| Package | Role here |
|---|---|
| [`lissr`](https://github.com/siardv/lissr) | archive authentication and download, recipe-driven merging of the health (`ch`) and leisure (`cs`) modules, rule-driven income cleaning, and the `weighted_sqrt` income equivalisation the study uses (verified identical to the pipeline formula at run time) |
| [`weasel`](https://github.com/siardv/weasel) | the minimum-three-of-seven-waves sample rule expressed as a named, comparable scenario, with a tolerance-sensitivity sweep, an attrition-selectivity table, and generated methods-section justification text; the pipeline asserts equality with its own selection at run time |

No microdata are included or may be redistributed; see `data/README.md` for access.

## Layout

```
├── R/           helper functions sourced by the pipeline
├── analysis/    the chunked pipeline, its render driver, and installer
├── scripts/     00 acquire (lissr) · 01 build panel · 02 select sample (weasel) · 03 run analysis
├── vignettes/   quarto walkthroughs of every step (vignette 06 renders without data)
├── tables/      released result tables (derived aggregates, data-free)
├── data/        empty; LISS files go here (or set LISS_DATA_DIR)
└── output/      rendered artefacts
```

## Quick start

```r
install.packages("remotes")
remotes::install_deps()          # reads DESCRIPTION, including the two GitHub packages
```

With LISS data in `data/`:

```sh
make analysis       # installs remaining dependencies, renders analysis/run_all.md
make vignettes      # quarto render vignettes/
```

Without data, `vignettes/06-results-and-reproduction.qmd` still renders every result table from `tables/`, and the executed weasel demonstration in `vignettes/03-sample-selection-with-weasel.qmd` runs on synthetic data.

## Reproducibility notes

The transcript `analysis/run_all.md` is the canonical record: chunk-by-chunk code, output, and a closing `sessionInfo()`. The bootstrap (1,000 resamples) is seeded and cached behind a fit fingerprint, so re-renders reuse it unless the model changes. Two run-time assertions tie the packages to the pipeline: the lissr equivalisation must equal the in-line formula, and the weasel scenario must select exactly the pipeline's respondents.

## Citation and licence

See `CITATION.cff`. MIT licence, © 2026 Siard van den Bosch.
