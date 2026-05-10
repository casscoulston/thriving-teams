# Hybrid Team Thriving Project

This repository contains reproducible R code for the quantitative analyses for a longitudinal study of thriving in hybrid teams across three measurement waves.

## Project structure

- `scripts/` contains analysis scripts in run order
- `data_raw/` contains confidential raw data files and is not tracked publicly
- `output/` contains locally generated tables and other outputs and is not tracked publicly

## Data

Raw data are not included in this repository due to confidentiality and ethics restrictions.

## Current workflow

1. `scripts/00_import_raw_data.R`
   - imports the raw SPSS `.sav` dataset into R
   - saves an R working file as `wide_df_raw.rds`

2. `scripts/01_build_table1_analytic_sample.R`
   - identifies the analytic sample using the rule that teams must have at least 3 responding members at each wave
   - generates Table 1 sample characteristics by measurement wave
   - exports a local CSV version of Table 1

## Analytic sample rule

For team-level analyses, teams with fewer than three responding members at a given wave are excluded.

This yielded the following analytic sample sizes:
- T1: 308 respondents across 58 teams
- T2: 210 respondents across 44 teams
- T3: 121 respondents across 31 teams

## Reproducibility

This project is being developed in RStudio using scripted, reproducible workflows. Additional analysis scripts and outputs will be added as the project progresses.
