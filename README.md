# Reciprocal health dynamics

**Published site:** <https://siardv.github.io/diet-activity-bmi-riclpm/>

Research compendium for **Socioeconomic Variations in the Reciprocal
Relationships Between Diet Quality, Physical Activity, and Body Mass Index: A
Random-Intercept Cross-Lagged Panel Study** (target journal: *Social Science &
Medicine*). The compendium grew out of the working project "reciprocal health
dynamics"; the package name (`reciprocal.health.dynamics`) and site title
retain that name, while the repository slug matches the article's constructs.

Seven annual waves of the Dutch [LISS panel](https://www.lissdata.nl) (analytic
N = 5,676 under full-information maximum likelihood) are analysed with
random-intercept cross-lagged panel models (RI-CLPM) in `lavaan`, with
household educational attainment as a three-group moderator. Headline results:
covariation among BMI, physical activity, and fruit and vegetable consumption
is predominantly between persons; the trait-level BMI-PA association is
negative, significant, and uniform across socioeconomic strata; within persons,
only the BMI-PA pair shows a small, reliable, mutually reinforcing pattern.

The compendium doubles as a worked use case for two packages:

| Package | Role here |
|---|---|
| [`lissr`](https://github.com/siardv/lissr) | archive authentication and download, recipe-driven merging of the health (`ch`) and leisure (`cs`) modules, rule-driven income cleaning, and the `weighted_sqrt` income equivalisation the study uses (verified identical to the pipeline formula at run time) |
| [`weasel`](https://github.com/siardv/weasel) | the minimum-three-of-seven-waves sample rule expressed as a named, comparable scenario, with a tolerance-sensitivity sweep, an attrition-selectivity table, and generated methods-section justification text; the pipeline asserts equality with its own selection at run time |

> **Repository history note:** Part of this repository's early publication
> history was reorganized while its GitHub workflow was being established. A
> forthcoming update will add clearer provenance for the recoverable earlier
> state; the current analysis and outputs remain canonical.

## Repository layout

```
├── R/           helper functions sourced by the pipeline
├── analysis/    the chunked pipeline, its render driver, and installer
├── scripts/     00 acquire (lissr) · 01 build panel · 02 select sample (weasel)
│                · 03 run analysis · 04 frve reliability · 05 bmi construction audit
│                · build_liss_merged.R (preserved construction record)
│                · verify_liss_merged.R (cell-by-cell comparison for holders of
│                  the frozen original extracts)
├── vignettes/   quarto walkthroughs of every step (vignette 06 renders without data)
├── tables/      released result tables (derived aggregates, data-free)
├── data/        empty; LISS files go here (or set LISS_DATA_DIR);
│                CHECKSUMS.txt pins the frozen analysis inputs
└── output/      released aggregate outputs and audit artefacts
```

## Data availability

No microdata are included or may be redistributed; see `data/README.md` for
access.

## Reproducing the analysis

```r
install.packages("remotes")
remotes::install_deps()          # reads DESCRIPTION, including the two GitHub packages
```

With LISS data in `data/`:

```sh
make analysis       # installs remaining dependencies, renders analysis/run_all.md
make release-tables # sync the released tables/ with the freshly rendered ones
make vignettes      # quarto render vignettes/
```

Without data, `vignettes/06-results-and-reproduction.qmd` still renders every
result table from `tables/`, and the executed weasel demonstration in
`vignettes/03-sample-selection-with-weasel.qmd` runs on synthetic data.

## Reproducibility notes

The transcript `analysis/run_all.md` is the canonical record: chunk-by-chunk
code, output, and a closing `sessionInfo()`. The bootstrap (1,000 resamples) is
seeded and cached behind a fit fingerprint, so re-renders reuse it unless the
model changes. Two run-time assertions tie the packages to the pipeline: the
lissr equivalisation must equal the in-line formula, and the weasel scenario
must select exactly the pipeline's respondents. `data/CHECKSUMS.txt` records
SHA-256 hashes of the frozen analysis inputs, so any copy can be verified
before use, and `make audit` (scripts/05) quantifies the construction-stage BMI
processing against the raw health modules.

## Licence and citation

See `CITATION.cff`. MIT licence, © 2026 Siard van den Bosch.

## Status

Manuscript under review. Author: Siard van den Bosch.

## Use of generative AI

The research design, analyses, and code in this repository are the author's own
work. Generative AI tools (including ChatGPT, Gemini, Claude, and Grok) were
used, under the author's direction and review, for feedback and language
suggestions on author-written text, and in some cases to help audit, document,
or harden existing code for reproducibility. Every AI-assisted change was
reviewed, tested, and committed by the author, and all reported results derive
from the author's analysis pipeline. References were not generated by AI. The
corresponding disclosure for the manuscript itself appears in the article's
declaration of generative AI and AI-assisted technologies.
