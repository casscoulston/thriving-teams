
# ==========================================
# SCRIPT 12: MEDIATION (PSYCHOLOGICAL STRAIN)
# ==========================================

library(lme4)
library(lmerTest)
library(dplyr)

# =========================
# LOAD DATA
# =========================

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

# =========================
# REBUILD MODEL DATA (WITH STRESS)
# =========================

model_data <- wide_df_raw %>%
  mutate(
    # --- T1 variables ---
    t1_team_psychological_safety = MeanScore_TotalPsychologicalSafety_T1,
    t1_control_over_work_time = MeanScore_Totalcontroloverworktime_T1,
    t1_feeling_disconnected = MeanScore_TotalFeelingDisconnected_T1,
    t1_connection_overload = MeanScore_TotalConnectionOverload_T1,
    t1_twe = MeanScore_TotalEngagement_T1,
    t1_team_performance = MeanScore_TotalPerformance_T1,
    team_tenure = Tenure_in_team_T1,
    
    # --- T2 variables ---
    t2_twe = MeanScore_TotalEngagement_T2,
    t2_team_performance = MeanScore_TotalPerformance_T2,
    
    # --- STRESS ---
    stress_t1 = MeanScore_TotalStress_T1,
    stress_t2 = MeanScore_TotalStress_T2
  ) %>%
  
  # =========================
# CENTERING
# =========================

group_by(TeamRef_T1) %>%
  mutate(
    disconnected_cwc = t1_feeling_disconnected - mean(t1_feeling_disconnected, na.rm = TRUE),
    overload_cwc = t1_connection_overload - mean(t1_connection_overload, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    psych_safety_cgm = scale(t1_team_psychological_safety, center = TRUE, scale = FALSE)[,1],
    control_cgm = scale(t1_control_over_work_time, center = TRUE, scale = FALSE)[,1]
  )

# =========================
# MODEL 1: PREDICTORS → STRESS
# =========================

model_strain <- lmer(
  stress_t2 ~ 
    stress_t1 +
    disconnected_cwc +
    overload_cwc +
    psych_safety_cgm +
    control_cgm +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# =========================
# MODEL 2: STRESS → PERFORMANCE
# =========================

model_perf_strain <- lmer(
  t2_team_performance ~ 
    stress_t2 +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# =========================
# MODEL 3: FULL MEDIATION
# =========================

model_full_strain <- lmer(
  t2_team_performance ~ 
    stress_t2 +
    disconnected_cwc +
    overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# =========================
# OUTPUT
# =========================

summary(model_strain)
summary(model_perf_strain)
summary(model_full_strain)
