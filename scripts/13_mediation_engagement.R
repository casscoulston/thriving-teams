# ==========================================
# SCRIPT 13: MEDIATION (ENGAGEMENT)
# ==========================================

# Load required libraries with checks
required_packages <- c("lme4", "lmerTest", "dplyr")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

# =========================
# LOAD DATA
# =========================

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

# =========================
# REBUILD MODEL DATA
# =========================

model_data <- wide_df_raw %>%
  mutate(
    # T1
    t1_twe = MeanScore_TotalEngagement_T1,
    t1_team_performance = MeanScore_TotalPerformance_T1,
    t1_feeling_disconnected = MeanScore_TotalFeelingDisconnected_T1,
    t1_connection_overload = MeanScore_TotalConnectionOverload_T1,
    t1_team_psychological_safety = MeanScore_TotalPsychologicalSafety_T1,
    t1_control_over_work_time = MeanScore_Totalcontroloverworktime_T1,
    team_tenure = Tenure_in_team_T1,
    
    # T2
    t2_twe = MeanScore_TotalEngagement_T2,
    t2_team_performance = MeanScore_TotalPerformance_T2
  ) %>%
  
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
# MODEL 1: PREDICTORS → ENGAGEMENT
# =========================

model_engage <- lmer(
  t2_twe ~ 
    t1_twe +
    disconnected_cwc +
    overload_cwc +
    psych_safety_cgm +
    control_cgm +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# =========================
# MODEL 2: ENGAGEMENT → PERFORMANCE
# =========================

model_perf_engage <- lmer(
  t2_team_performance ~ 
    t2_twe +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# =========================
# MODEL 3: FULL MODEL
# =========================

model_full_engage <- lmer(
  t2_team_performance ~ 
    t2_twe +
    disconnected_cwc +
    overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = model_data

# =========================
# OUTPUT
# =========================

summary(model_engage)
summary(model_perf_engage)
summary(model_full_engage)
  
)

 
 
performance::r2(model_strain)
performance::r2(model_perf_strain)
performance::r2(model_full_strain)
