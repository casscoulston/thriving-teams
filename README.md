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
  Rebuilds the aggregation and reliability table for team-level constructs across waves. The script computes mean item composites, Cronbach’s alpha, McDonald’s omega, ICC(1), ICC(2), and median/mean rwg(j) for control over work time and team psychological safety.

  

 ###  Means, standard deviations, and correlations

- `scripts/10_descriptives_reliability_correlations_by_wave.R`  
    Computes descriptive statistics, internal consistency (alpha, omega), and correlations for the main T1–T2 panel sample used in hypothesis testing. Includes composite variables for all focal constructs and generates the main correlation table with significance testing.

- `scripts/10b_descriptives_t1_full_sample.R`  
    Computes descriptives, reliability (Cronbach’s alpha, McDonald’s omega), and correlations for the full T1 sample (teams with ≥3 members). This serves as a robustness check against the reduced T1–T2 panel sample used in the main analyses. Results are consistent with the main findings and are reported in Table S1.

###  Missing data diagnostics
- `scripts/10c_missing_data_diagnostics.R`  
     Examines missing data patterns and attrition prior to multilevel modelling. Computes overall missingness, visualises missing data patterns (naniar), and conducts logistic regression analyses to test whether dropout at T2 and T3 is associated with baseline variables (feeling disconnected, connection overload, and team work engagement). Results support a missing at random (MAR) assumption. Primary multilevel models use available complete observations for the variables included in each analysis.
  
###  Main multilevel analyses
- `scripts/11_main_modesl_outputs.R`  
  Estimates multilevel models testing whether relational demands (feeling disconnected, connection overload) and resources (psychological safety, control over work time) at T1 predict team work engagement and team performance at T2. Variables are group-mean (CWC) and grand-mean (CGM) centred as appropriate. Models include random intercepts for teams (TeamRef_T1) and control for baseline outcomes and team tenure. Results form the main analyses reported in the paper

- `scripts/12_exploratory_stress_pathway.R`  
   Examines occupational stress as a potential strain-related pathway linking hybrid-work demands and resources to perceived team performance. Tests whether Time 1 demands and resources predict Time 2 occupational stress after accounting for baseline stress, and whether Time 2 stress is associated with Time 2 performance after controlling for prior performance. 

- `scripts/13_exploratory_stress_model.R`  
  This is work in progress still 
  
### Next planned section
- Supplementary robustness analyses T1-T3

## Analytic sample rule

For team-level analyses, teams with fewer than three responding members at a given wave are excluded.

This yielded the following analytic sample sizes:
- T1: 308 respondents across 58 teams
- T2: 210 respondents across 44 teams
- T3: 121 respondents across 31 teams

## Reproducibility

This project is being developed in RStudio using scripted, reproducible workflows. Additional analysis scripts and outputs will be added as the project progresses.

## AI Statement

This code was edited with the assistance of GitHub CoPilot (Powered by GPT 5.5, OpenAI, San Francisco: CA)

After using this tool, the authors reviewed and edited the code as needed and take full responsibility for the final repo.
