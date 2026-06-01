
# ============================================================
# SCRIPT 11: PRIMARY MULTILEVEL MODELS (T1 -> T2)
# ============================================================

# Run once only if packages are not already installed:
# install.packages(c(
#   "tidyverse",
#   "lme4",
#   "lmerTest",
#   "sjPlot",
#   "performance",
#   "ggeffects",
#   "ggplot2"
# ))

# -------------------------
# LOAD LIBRARIES
# -------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(performance)
library(ggeffects)
library(ggplot2)

# -------------------------
# LOAD CORRECTED DATASET
# -------------------------
#
# Script 09b created:
# - Level 2 team means
# - team-level grand-mean-centred resource variables
# - respondent-level predictors and outcomes

model_data <- readRDS(
  "data_processed/multilevel_modelling_dataset_t1_t2.rds"
)

# -------------------------
# VERIFY LEVEL 2 VARIABLES
# -------------------------
#
# Each team should carry only one value for each Level 2
# resource. Expected values: 1, 1, 0, 0.

level2_check <- model_data %>%
  group_by(TeamRef_T1) %>%
  summarise(
    psych_safety_values_within_team =
      n_distinct(
        t1_team_psychological_safety,
        na.rm = TRUE
      ),
    
    control_values_within_team =
      n_distinct(
        t1_control_over_work_time,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  summarise(
    max_psych_safety_values_within_team =
      max(psych_safety_values_within_team),
    
    max_control_values_within_team =
      max(control_values_within_team),
    
    teams_with_multiple_psych_safety_values =
      sum(psych_safety_values_within_team > 1),
    
    teams_with_multiple_control_values =
      sum(control_values_within_team > 1)
  )

print(
  level2_check,
  width = Inf
)

# -------------------------
# WITHIN-TEAM CENTRING:
# LEVEL 1 HINDRANCE DEMANDS
# -------------------------

# Retain this step.
# It isolates whether an individual reports more or less
# disconnection or overload than other members of their team.

model_data <- model_data %>%
  group_by(TeamRef_T1) %>%
  mutate(
    disconnected_cwc =
      t1_feeling_disconnected -
      mean(
        t1_feeling_disconnected,
        na.rm = TRUE
      ),
    
    overload_cwc =
      t1_connection_overload -
      mean(
        t1_connection_overload,
        na.rm = TRUE
      )
  ) %>%
  ungroup()

# -------------------------
# CREATE CONSISTENT MODEL SAMPLES
# -------------------------

twe_data <- model_data %>%
  filter(
    !is.na(TeamRef_T1),
    !is.na(t2_twe),
    !is.na(t1_twe),
    !is.na(team_tenure),
    !is.na(psych_safety_cgm),
    !is.na(control_cgm),
    !is.na(disconnected_cwc),
    !is.na(overload_cwc)
  ) %>%
  group_by(TeamRef_T1) %>%
  filter(n() >= 3) %>%
  ungroup()

perf_data <- model_data %>%
  filter(
    !is.na(TeamRef_T1),
    !is.na(t2_team_performance),
    !is.na(t1_team_performance),
    !is.na(team_tenure),
    !is.na(psych_safety_cgm),
    !is.na(control_cgm),
    !is.na(disconnected_cwc),
    !is.na(overload_cwc)
  ) %>%
  group_by(TeamRef_T1) %>%
  filter(n() >= 3) %>%
  ungroup()

# -------------------------
# PRINT FINAL MODEL SAMPLE SIZES
# -------------------------

sample_summary <- tibble(
  Outcome = c(
    "T2 team work engagement",
    "T2 team performance"
  ),
  
  Respondents = c(
    nrow(twe_data),
    nrow(perf_data)
  ),
  
  Teams = c(
    n_distinct(twe_data$TeamRef_T1),
    n_distinct(perf_data$TeamRef_T1)
  )
)

print(
  sample_summary
)

# ============================================================
# MODEL SET A: T2 TEAM WORK ENGAGEMENT
# ============================================================

model_twe_1 <- lmer(
  t2_twe ~
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_data,
  REML = TRUE
)

model_twe_2 <- lmer(
  t2_twe ~
    psych_safety_cgm +
    control_cgm +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_data,
  REML = TRUE
)

model_twe_3 <- lmer(
  t2_twe ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_data,
  REML = TRUE
)

model_twe_4 <- lmer(
  t2_twe ~
    psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_data,
  REML = TRUE
)

# ============================================================
# MODEL SET B: T2 TEAM PERFORMANCE
# ============================================================

model_perf_1 <- lmer(
  t2_team_performance ~
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_data,
  REML = TRUE
)

model_perf_2 <- lmer(
  t2_team_performance ~
    psych_safety_cgm +
    control_cgm +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_data,
  REML = TRUE
)

model_perf_3 <- lmer(
  t2_team_performance ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_data,
  REML = TRUE
)

model_perf_4 <- lmer(
  t2_team_performance ~
    psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_data,
  REML = TRUE
)

# -------------------------
# OUTPUT MODEL SUMMARIES
# -------------------------

summary(model_twe_1)
summary(model_twe_2)
summary(model_twe_3)
summary(model_twe_4)

summary(model_perf_1)
summary(model_perf_2)
summary(model_perf_3)
summary(model_perf_4)

# ============================================================
# ROBUSTNESS CHECK:
# MODELS WITHOUT BASELINE OUTCOME CONTROL
# ============================================================

model_twe_no_baseline <- lmer(
  t2_twe ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_data,
  REML = TRUE
)

model_perf_no_baseline <- lmer(
  t2_team_performance ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_data,
  REML = TRUE
)

summary(model_twe_no_baseline)
summary(model_perf_no_baseline)

# -------------------------
# CALCULATE R-SQUARED VALUES
# -------------------------

r2_results <- bind_rows(
  tibble(
    Model = "TWE M1",
    as.data.frame(
      performance::r2_nakagawa(model_twe_1)
    )
  ),
  
  tibble(
    Model = "TWE M2",
    as.data.frame(
      performance::r2_nakagawa(model_twe_2)
    )
  ),
  
  tibble(
    Model = "TWE M3",
    as.data.frame(
      performance::r2_nakagawa(model_twe_3)
    )
  ),
  
  tibble(
    Model = "TWE M4",
    as.data.frame(
      performance::r2_nakagawa(model_twe_4)
    )
  ),
  
  tibble(
    Model = "Performance M1",
    as.data.frame(
      performance::r2_nakagawa(model_perf_1)
    )
  ),
  
  tibble(
    Model = "Performance M2",
    as.data.frame(
      performance::r2_nakagawa(model_perf_2)
    )
  ),
  
  tibble(
    Model = "Performance M3",
    as.data.frame(
      performance::r2_nakagawa(model_perf_3)
    )
  ),
  
  tibble(
    Model = "Performance M4",
    as.data.frame(
      performance::r2_nakagawa(model_perf_4)
    )
  )
)

print(
  r2_results,
  n = 20,
  width = Inf
)

# -------------------------
# SAVE MODELS
# -------------------------

dir.create(
  "output/models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  model_twe_1,
  "output/models/corrected_model_twe_1.rds"
)

saveRDS(
  model_twe_2,
  "output/models/corrected_model_twe_2.rds"
)

saveRDS(
  model_twe_3,
  "output/models/corrected_model_twe_3.rds"
)

saveRDS(
  model_twe_4,
  "output/models/corrected_model_twe_4.rds"
)

saveRDS(
  model_perf_1,
  "output/models/corrected_model_perf_1.rds"
)

saveRDS(
  model_perf_2,
  "output/models/corrected_model_perf_2.rds"
)

saveRDS(
  model_perf_3,
  "output/models/corrected_model_perf_3.rds"
)

saveRDS(
  model_perf_4,
  "output/models/corrected_model_perf_4.rds"
)

saveRDS(
  model_twe_no_baseline,
  "output/models/corrected_model_twe_no_baseline.rds"
)

saveRDS(
  model_perf_no_baseline,
  "output/models/corrected_model_perf_no_baseline.rds"
)

# -------------------------
# SAVE MODEL TABLES
# -------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

sjPlot::tab_model(
  model_twe_1,
  model_twe_2,
  model_twe_3,
  model_twe_4,
  show.se = TRUE,
  show.p = TRUE,
  show.ci = FALSE,
  dv.labels = c(
    "M1",
    "M2",
    "M3",
    "M4"
  ),
  file =
    "output/tables/table_twe_full_models_corrected.html"
)

sjPlot::tab_model(
  model_perf_1,
  model_perf_2,
  model_perf_3,
  model_perf_4,
  show.se = TRUE,
  show.p = TRUE,
  show.ci = FALSE,
  dv.labels = c(
    "M1",
    "M2",
    "M3",
    "M4"
  ),
  file =
    "output/tables/table_performance_full_models_corrected.html"
)

sjPlot::tab_model(
  model_twe_no_baseline,
  model_perf_no_baseline,
  show.se = TRUE,
  show.p = TRUE,
  show.ci = FALSE,
  file =
    "output/tables/table_robustness_models_corrected.html"
)

# -------------------------
# SAVE SAMPLE AND R-SQUARED OUTPUTS
# -------------------------

write_csv(
  sample_summary,
  "output/tables/corrected_main_model_sample_summary.csv"
)

write_csv(
  r2_results,
  "output/tables/corrected_main_model_r2.csv"
)

# -------------------------
# SAVE CLEAN DATA FOR LATER SCRIPTS
# -------------------------

dir.create(
  "data_processed",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  model_data,
  "data_processed/model_data_with_centered_demands.rds"
)

saveRDS(
  twe_data,
  "data_processed/model_data_twe_complete.rds"
)

saveRDS(
  perf_data,
  "data_processed/model_data_performance_complete.rds"
)

