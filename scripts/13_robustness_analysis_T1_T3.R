# ============================================================
# SUPPLEMENTARY T1 -> T3 ROBUSTNESS MODELS
# ============================================================
#
# Purpose:
# - Mirror the main T1 -> T2 multilevel models using T3 outcomes
# - Retain T1 team resources and T1 individual demands
# - Test whether the main pattern is broadly similar over 6 months
#
# Notes:
# - This is a supplementary robustness analysis
# - Primary models remain T1 -> T2
# ============================================================

library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
library(performance)
library(modelsummary)

# -------------------------
# LOAD RAW DATA
# -------------------------

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

# -------------------------
# HELPER FUNCTIONS
# -------------------------

to_numeric_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.numeric(haven::zap_labels(x)))
  }
  x
}

clean_team_id <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

row_mean_na <- function(items) {
  score <- rowMeans(
    as.data.frame(items),
    na.rm = TRUE
  )
  score[is.nan(score)] <- NA_real_
  score
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

get_valid_teams <- function(df, team_col, min_team_size = 3) {
  df %>%
    filter(!is.na(.data[[team_col]])) %>%
    count(.data[[team_col]], name = "team_size") %>%
    filter(team_size >= min_team_size) %>%
    pull(1)
}

# -------------------------
# CLEAN DATA
# -------------------------

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

# -------------------------
# CREATE RESPONDENT-LEVEL COMPOSITES
# -------------------------

df_composites <- df_num %>%
  mutate(
    # T1 individual ratings used to create level-2 resources
    t1_psychological_safety_individual = row_mean_na(
      select(
        .,
        RecodedPsychologicalsafety_1_T1,
        Psychological_safety_2_T1,
        RecodedPsychologicalSafety_3_T1,
        Psychological_safety_4_T1,
        RecodedPsychologicalSafety_5_T1,
        Psychological_safety_6_T1,
        Psychological_safety_7_T1
      )
    ),
    
    t1_control_over_work_time_individual = row_mean_na(
      select(
        .,
        Choice_remote_work_T1,
        Choice_work_week_T1,
        Choice_vacations_T1,
        Choice_in_office_T1,
        Control_hrs_off_T1
      )
    ),
    
    # T1 level-1 demands
    t1_feeling_disconnected = row_mean_na(
      select(
        .,
        Disconnection_1_T1,
        Disconnection_2_T1,
        Disconnection_3_T1,
        Disconnection_4_T1,
        RecodedDisconnection5_T1,
        RecodedDisconnection6_T1,
        Disconnection_7_T1,
        Disconnection_8_T1,
        Disconnection_9_T1
      )
    ),
    
    t1_connection_overload = row_mean_na(
      select(
        .,
        Connectionoverload_1_T1,
        Connectionoverload_2_T1,
        Connectionoverload_3_T1,
        Connectionoverload_4_T1,
        Connectionoverload_5_T1
      )
    ),
    
    # T1 baseline outcomes
    t1_twe = row_mean_na(
      select(
        .,
        Engagement_1_T1,
        Engagement_2_T1,
        Engagement_3_T1,
        Engagement_4_T1,
        Engagement_5_T1,
        Engagement_6_T1,
        Engagement_7_T1,
        Engagement_8_T1,
        Engagement_9_T1
      )
    ),
    
    t1_team_performance = row_mean_na(
      select(
        .,
        Performance_1_T1,
        Performance_2_T1,
        Recoded_Performance_3_T1,
        Performance_4_T1
      )
    ),
    
    # T3 outcomes
    t3_twe = row_mean_na(
      select(
        .,
        Engagement_1_T3,
        Engagement_2_T3,
        Engagement_3_T3,
        Engagement_4_T3,
        Engagement_5_T3,
        Engagement_6_T3,
        Engagement_7_T3,
        Engagement_8_T3,
        Engagement_9_T3
      )
    ),
    
    t3_team_performance = row_mean_na(
      select(
        .,
        Performance_1_T3,
        Performance_2_T3,
        Recoded_Performance_3_T3,
        Performance_4_T3
      )
    ),
    
    team_tenure = Tenure_in_team_T1
  )

# -------------------------
# CREATE TRUE LEVEL 2 TEAM RESOURCES FROM T1
# -------------------------

team_resources_t1 <- df_composites %>%
  filter(TeamRef_T1 %in% valid_t1_teams) %>%
  group_by(TeamRef_T1) %>%
  summarise(
    n_psych_safety = sum(!is.na(t1_psychological_safety_individual)),
    n_control = sum(!is.na(t1_control_over_work_time_individual)),
    t1_team_psychological_safety = safe_mean(t1_psychological_safety_individual),
    t1_control_over_work_time = safe_mean(t1_control_over_work_time_individual),
    .groups = "drop"
  ) %>%
  filter(
    n_psych_safety >= 3,
    n_control >= 3
  ) %>%
  mutate(
    psych_safety_cgm =
      t1_team_psychological_safety -
      mean(t1_team_psychological_safety, na.rm = TRUE),
    
    control_cgm =
      t1_control_over_work_time -
      mean(t1_control_over_work_time, na.rm = TRUE)
  )

valid_level2_teams <- team_resources_t1$TeamRef_T1

# -------------------------
# CREATE T1-T3 PANEL DATASET
# -------------------------

model_data_t1_t3 <- df_composites %>%
  filter(
    TeamRef_T1 %in% valid_level2_teams,
    TeamRef_T3 %in% valid_t3_teams
  ) %>%
  left_join(
    team_resources_t1,
    by = "TeamRef_T1"
  )

# -------------------------
# WITHIN-TEAM CENTRING OF T1 DEMANDS
# -------------------------

model_data_t1_t3 <- model_data_t1_t3 %>%
  group_by(TeamRef_T1) %>%
  mutate(
    disconnected_cwc =
      t1_feeling_disconnected -
      mean(t1_feeling_disconnected, na.rm = TRUE),
    
    overload_cwc =
      t1_connection_overload -
      mean(t1_connection_overload, na.rm = TRUE)
  ) %>%
  ungroup()

# -------------------------
# CREATE CONSISTENT MODEL SAMPLES
# -------------------------

twe_t3_data <- model_data_t1_t3 %>%
  filter(
    !is.na(TeamRef_T1),
    !is.na(t3_twe),
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

perf_t3_data <- model_data_t1_t3 %>%
  filter(
    !is.na(TeamRef_T1),
    !is.na(t3_team_performance),
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

sample_summary_t3 <- tibble(
  Outcome = c(
    "T3 team work engagement",
    "T3 team performance"
  ),
  Respondents = c(
    nrow(twe_t3_data),
    nrow(perf_t3_data)
  ),
  Teams = c(
    n_distinct(twe_t3_data$TeamRef_T1),
    n_distinct(perf_t3_data$TeamRef_T1)
  )
)

print(sample_summary_t3)

# ============================================================
# MODEL SET A: T3 TEAM WORK ENGAGEMENT
# ============================================================

model_t3_twe_1 <- lmer(
  t3_twe ~
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_t3_data,
  REML = TRUE
)

model_t3_twe_2 <- lmer(
  t3_twe ~
    psych_safety_cgm +
    control_cgm +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_t3_data,
  REML = TRUE
)

model_t3_twe_3 <- lmer(
  t3_twe ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_t3_data,
  REML = TRUE
)

model_t3_twe_4 <- lmer(
  t3_twe ~
    psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_twe +
    team_tenure +
    (1 | TeamRef_T1),
  data = twe_t3_data,
  REML = TRUE
)

# ============================================================
# MODEL SET B: T3 TEAM PERFORMANCE
# ============================================================

model_t3_perf_1 <- lmer(
  t3_team_performance ~
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_t3_data,
  REML = TRUE
)

model_t3_perf_2 <- lmer(
  t3_team_performance ~
    psych_safety_cgm +
    control_cgm +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_t3_data,
  REML = TRUE
)

model_t3_perf_3 <- lmer(
  t3_team_performance ~
    psych_safety_cgm +
    control_cgm +
    disconnected_cwc +
    overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_t3_data,
  REML = TRUE
)

model_t3_perf_4 <- lmer(
  t3_team_performance ~
    psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_team_performance +
    team_tenure +
    (1 | TeamRef_T1),
  data = perf_t3_data,
  REML = TRUE
)

# -------------------------
# OUTPUT MODEL SUMMARIES
# -------------------------

summary(model_t3_twe_1)
summary(model_t3_twe_2)
summary(model_t3_twe_3)
summary(model_t3_twe_4)

summary(model_t3_perf_1)
summary(model_t3_perf_2)
summary(model_t3_perf_3)
summary(model_t3_perf_4)

# -------------------------
# CALCULATE R-SQUARED VALUES
# -------------------------

r2_t3_results <- bind_rows(
  tibble(Model = "T3 TWE M1", as.data.frame(performance::r2_nakagawa(model_t3_twe_1))),
  tibble(Model = "T3 TWE M2", as.data.frame(performance::r2_nakagawa(model_t3_twe_2))),
  tibble(Model = "T3 TWE M3", as.data.frame(performance::r2_nakagawa(model_t3_twe_3))),
  tibble(Model = "T3 TWE M4", as.data.frame(performance::r2_nakagawa(model_t3_twe_4))),
  tibble(Model = "T3 Performance M1", as.data.frame(performance::r2_nakagawa(model_t3_perf_1))),
  tibble(Model = "T3 Performance M2", as.data.frame(performance::r2_nakagawa(model_t3_perf_2))),
  tibble(Model = "T3 Performance M3", as.data.frame(performance::r2_nakagawa(model_t3_perf_3))),
  tibble(Model = "T3 Performance M4", as.data.frame(performance::r2_nakagawa(model_t3_perf_4)))
)

print(r2_t3_results, n = 20, width = Inf)

# -------------------------
# SAVE OUTPUTS
# -------------------------

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

saveRDS(model_t3_twe_4, "output/models/model_t3_twe_4.rds")
saveRDS(model_t3_perf_4, "output/models/model_t3_perf_4.rds")

write_csv(
  sample_summary_t3,
  "output/tables/supplementary_t1_t3_sample_summary.csv"
)

write_csv(
  r2_t3_results,
  "output/tables/supplementary_t1_t3_r2.csv"
)

modelsummary(
  list(
    "M1" = model_t3_twe_1,
    "M2" = model_t3_twe_2,
    "M3" = model_t3_twe_3,
    "M4" = model_t3_twe_4
  ),
  output = "output/tables/Supplementary_Table_5_T1_T3_TWE.docx",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "t1_twe" = "T1 team work engagement",
    "team_tenure" = "Team tenure",
    "psych_safety_cgm" = "T1 team psychological safety",
    "control_cgm" = "T1 control over work time",
    "disconnected_cwc" = "T1 feeling disconnected",
    "overload_cwc" = "T1 connection overload",
    "psych_safety_cgm:disconnected_cwc" = "Psychological safety × feeling disconnected",
    "control_cgm:overload_cwc" = "Control over work time × connection overload"
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik.|RMSE|Std.Errors|ICC"
)

modelsummary(
  list(
    "M1" = model_t3_perf_1,
    "M2" = model_t3_perf_2,
    "M3" = model_t3_perf_3,
    "M4" = model_t3_perf_4
  ),
  output = "output/tables/Supplementary_Table_6_T1_T3_Performance.docx",
  coef_map = c(
    "(Intercept)" = "Intercept",
    "t1_team_performance" = "T1 team performance",
    "team_tenure" = "Team tenure",
    "psych_safety_cgm" = "T1 team psychological safety",
    "control_cgm" = "T1 control over work time",
    "disconnected_cwc" = "T1 feeling disconnected",
    "overload_cwc" = "T1 connection overload",
    "psych_safety_cgm:disconnected_cwc" = "Psychological safety × feeling disconnected",
    "control_cgm:overload_cwc" = "Control over work time × connection overload"
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik.|RMSE|Std.Errors|ICC"
)

n_distinct(perf_t3_data$TeamRef_T1)
