# Hybrid Team Thriving Project

This repository contains reproducible R code for the quantitative analyses for a longitudinal study of thriving in hybrid teams across three measurement waves.

## Project structure

```
thriving-teams/
├── .Rprofile            # activates renv on R startup
├── thriving-teams.Rproj # RStudio project file (open this first)
├── renv.lock            # locked package versions
├── R/
│   └── utils.R          # shared helper functions sourced by every script
├── scripts/             # numbered analysis scripts (run in order)
│   └── _smoke_data/     # synthetic fixture for CI / contributor testing
├── data_raw/            # confidential SPSS dataset (not tracked)
├── data_processed/      # generated panel datasets (not tracked)
└── output/              # generated tables, figures and session logs (not tracked)
```

## Data

Raw data are not included in this repository due to confidentiality and ethics restrictions. The `.gitignore` enforces this — see the *Data safety* section below.

## Reproducing the analyses

Prerequisites: **R ≥ 4.4** and a working internet connection (for the first `renv::restore()`).

```r
# 1. Clone the repo and open thriving-teams.Rproj in RStudio.

# 2. Restore the locked package library (~5–10 min the first time):
renv::restore()

# 3. Place the SPSS file at:
#       data_raw/02_02_26_WIDE_Mergedfiles_LATEST_dataset.sav

# 4. Run the scripts in order:
source(here::here("scripts", "00_import_raw_data.R"))
source(here::here("scripts", "01_build_table1_analytic_sample.R"))
# ... continue through scripts/02 – scripts/10c
source(here::here("scripts", "11_build_panel_dataset.R"))
source(here::here("scripts", "12_main_mlm_models.R"))
```

Each script writes an `output/logs/<timestamp>_<script-name>.log` capturing `sessionInfo()` so the exact package versions that produced any given table are recoverable.

### Reproducing without the real data

A synthetic wide-format fixture mirroring the real schema lives at `scripts/_smoke_data/wide_df_smoke.rds`. Scripts `11_*` onward detect whether `data_raw/wide_df_raw.rds` exists and fall back to the smoke fixture automatically — the same code path runs either way. Useful for CI and contributor onboarding.

To regenerate the fixture:

```r
source(here::here("scripts", "_smoke_data", "generate_smoke_dataset.R"))
```

## Data safety

This repository is **public**. To protect participant data:

- The committed `.gitignore` excludes `data_raw/`, `data_processed/`, `output/`, `*.sav`, `*.rds`, `*.RData` and related patterns from version control.
- Before pushing, `git ls-files | grep -E '(data_raw|\.sav$|\.rds$|\.RData$)'` should print nothing.
- If a sensitive file ever lands in a commit, do **not** rely on a follow-up "delete" commit — the file remains in the git history. Use `git filter-repo` (or contact a maintainer) to scrub it.

## Analytic sample rule

For team-level analyses, teams with fewer than three responding members at a given wave are excluded. This yields:

| Wave | Respondents | Teams |
| --- | --- | --- |
| T1 | 308 | 58 |
| T2 | 210 | 44 |
| T3 | 121 | 31 |

## Scripts

### Data import and sample construction
- `scripts/00_import_raw_data.R` — Imports the raw SPSS dataset and saves an R working file.
- `scripts/01_build_table1_analytic_sample.R` — Builds Table 1 sample characteristics by wave for the analytic sample.

### Descriptives and sample transparency
- `scripts/02_supplementary_role_based_demographics.R` — Role-based descriptive statistics by wave.
- `scripts/03_attrition_full_sample.R` — Full-sample summary by wave and full-sample attrition.
- `scripts/04_sample_comparison_and_attrition.R` — Compares full and analytic samples; summarises attrition across waves.
- `scripts/05_compare_full_vs_analytic_and_attrition_bias.R` — Tests whether the analytic sample differs from the full sample and assesses attrition bias.
- `scripts/06_build_supplementary_table_s3_attrition_retention.R` — Produces Supplementary Table S3 summarising sample comparison and attrition. *(Requires `officer` and `flextable` — install with `renv::install(c("officer","flextable"))` if not already present.)*
- `scripts/07_geography_by_wave_and_sample_comparison.R` — Examines geographic composition by wave and compares geography across samples.
- `scripts/08_geography_chisquare_tests.R` — Chi-square tests for geographic composition and geography-related attrition.

### Psychometrics and aggregation
- `scripts/09_rebuild_table2_aggregation_reliability.R` — Aggregation and reliability table for team-level constructs across waves: mean item composites, Cronbach's α, McDonald's ω, ICC(1), ICC(2), median and mean rwg(j) for team engagement, control over work time and team psychological safety.

### Means, standard deviations and correlations
- `scripts/10_descriptives_reliability_correlations_by_wave.R` — Descriptive statistics, internal consistency (α, ω) and correlations for the main T1–T2 panel sample used in hypothesis testing.
- `scripts/10b_descriptives_t1_full_sample.R` — Same as above for the full T1 sample (Table S1 robustness check).

### Missing data diagnostics
- `scripts/10c_missing_data_diagnostics.R` — Missing data patterns and attrition prior to multilevel modelling; logistic regression tests for MAR justification.

### Main multilevel analyses
- `scripts/11_build_panel_dataset.R` — Canonical T1–T2 (and T1–T3) panel datasets; writes individual- and team-level forms to `data_processed/`.
- `scripts/12_main_mlm_models.R` — Headline lagged multilevel models for T2 team work engagement and T2 team performance.

### Planned (forthcoming PRs)
- `scripts/13_robustness_brms_bayesian.R` — Bayesian replication of the headline MLM via `brms`.
- `scripts/14_power_simulation.R` — `simr`-based post-hoc power for the headline MLM.
- `scripts/15_publication_figures.R` — Forest plots of fixed effects and caterpillar plots of random intercepts.

## Model registry

| Model | Outcome | Predictors | Random structure | Inference | Script |
| --- | --- | --- | --- | --- | --- |
| Headline 1 | T2 Team work engagement | T1 AR, T1 psych safety, T1 control, T1 disconnected, T1 overload, controls | `(1 \| TeamRef_T1)` | KR (lmerTest + pbkrtest), profile CIs | `12_main_mlm_models.R` |
| Headline 2 | T2 Team performance | (same) | `(1 \| TeamRef_T1)` | (same) | `12_main_mlm_models.R` |

## Continuous integration

`.github/workflows/r-lint.yml` parses every script and runs `lintr` on every push and PR. Soft-fail for the moment so findings surface without blocking merges; will tighten to hard-fail once the baseline is clean.

## AI Statement

Sections of the analysis code in this repository were drafted or refactored with the assistance of GitHub Copilot (GitHub Inc., San Francisco, CA), an AI-powered code completion tool. The underlying language model is selected by Copilot at the time of use and may rotate over time; no model output was accepted without human review. The author reviewed and edited all generated code, validated it against the project specification and takes full responsibility for the final repository. AI was not used to generate or interpret empirical results.

Additional code review, statistical refactoring and analytic-plan contributions were made with the assistance of Anthropic's Claude (Anthropic PBC, San Francisco, CA). All such contributions appear as pull requests authored by [@ricardotwumasi](https://github.com/ricardotwumasi) and were reviewed by the thesis author before merging.
