# Hybrid Team Thriving Project

This repository contains reproducible R code for the quantitative analyses for a longitudinal study of thriving in hybrid teams across three measurement waves.

## Project structure

- `scripts/` contains analysis scripts in run order
- `data_raw/` contains confidential raw data files and is not tracked publicly
- `output/` contains locally generated tables and other outputs and is not tracked publicly

## Data

Raw data are not included in this repository due to confidentiality and ethics restrictions.

## Current workflow

## Scripts

### Data import and sample construction
- `scripts/00_import_raw_data.R`  
  Imports the raw SPSS dataset and saves an R working file.

- `scripts/01_build_table1_analytic_sample.R`  
  Builds Table 1 sample characteristics by wave for the analytic sample.

### Descriptives and sample transparency
- `scripts/02_supplementary_role_based_demographics.R`  
  Generates role-based descriptive statistics by wave.

- `scripts/03_attrition_full_sample.R`  
  Summarises the full sample by wave and calculates full-sample attrition.

- `scripts/04_sample_comparison_and_attrition.R`  
  Compares full and analytic samples and summarises attrition across waves.

- `scripts/05_compare_full_vs_analytic_and_attrition_bias.R`  
  Tests whether the analytic sample differs from the full sample and assesses attrition bias.

- `scripts/06_build_supplementary_table_s3_attrition_retention.R`  
  Produces Supplementary Table S3 summarising sample comparison and attrition.

- `scripts/07_geography_by_wave_and_sample_comparison.R`  
  Examines geographic composition by wave and compares geography across samples.

- `scripts/08_geography_chisquare_tests.R`  
  Runs chi-square tests for geographic composition and geography-related attrition.

  ### Psychometrics and aggregation

- `scripts/09_rebuild_table2_aggregation_reliability.R`  
  Rebuilds the aggregation and reliability table for team-level constructs across waves. The script computes mean item composites, Cronbach’s alpha, McDonald’s omega, ICC(1), ICC(2), and median/mean rwg(j) for team engagement, control over work time, and team psychological safety.

 ###  Means, standard deviations, and correlations

 -  scripts/10_descriptives_reliability_correlations_by_wave.R
Computes descriptive statistics, internal consistency (alpha, omega), and correlations for the main T1–T2 panel sample used in hypothesis testing. Includes composite variables for all focal constructs and generates the main correlation table with significance testing.
  
### Next planned section
- Main multilevel analyses
- Supplementary robustness analyses

## Analytic sample rule

For team-level analyses, teams with fewer than three responding members at a given wave are excluded.

This yielded the following analytic sample sizes:
- T1: 308 respondents across 58 teams
- T2: 210 respondents across 44 teams
- T3: 121 respondents across 31 teams

## Reproducibility

This project is being developed in RStudio using scripted, reproducible workflows. Additional analysis scripts and outputs will be added as the project progresses.
