

# ============================================================
# SCRIPT 09B: BUILD MULTILEVEL MODELLING DATASET (T1-T2)
# ============================================================
#
# Purpose:
# - Build respondent-level composite scores
# - Aggregate T1 psychological safety and control over work time
#   to create Level 2 team resources
# - Retain Level 1 demands, outcomes, stress, and team tenure
# - Restrict Level 2 resources to teams with >= 3 usable ratings
# - Save corrected modelling dataset for later analyses
# ============================================================

library(tidyverse)
library(haven)

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

# Calculates mean composites where at least 80% of items are completed.
# Converts scores to NA where too few items, or no items, are available.
row_mean_na <- function(items, min_prop = .80) {
  items <- as.data.frame(items)
  
  required_items <- ceiling(ncol(items) * min_prop)
  completed_items <- rowSums(!is.na(items))
  
  score <- rowMeans(
    items,
    na.rm = TRUE
  )
  
  score[completed_items < required_items] <- NA_real_
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

valid_t1_teams <- get_valid_teams(
  df_num,
  "TeamRef_T1"
)

valid_t2_teams <- get_valid_teams(
  df_num,
  "TeamRef_T2"
)

# -------------------------
# CREATE RESPONDENT-LEVEL COMPOSITES
# -------------------------

df_composites <- df_num %>%
  mutate(
    
    # ========================================================
    # INDIVIDUAL RATINGS USED TO CREATE LEVEL 2 RESOURCES
    # ========================================================
    
    t1_psychological_safety_individual = row_mean_na(
      dplyr::select(
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
      dplyr::select(
        .,
        Choice_remote_work_T1,
        Choice_work_week_T1,
        Choice_vacations_T1,
        Choice_in_office_T1,
        Control_hrs_off_T1
      )
    ),
    
    # ========================================================
    # LEVEL 1 HINDRANCE DEMANDS
    # ========================================================
    
    t1_feeling_disconnected = row_mean_na(
      dplyr::select(
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
      dplyr::select(
        .,
        Connectionoverload_1_T1,
        Connectionoverload_2_T1,
        Connectionoverload_3_T1,
        Connectionoverload_4_T1,
        Connectionoverload_5_T1
      )
    ),
    
    # ========================================================
    # LEVEL 1 OUTCOMES
    # ========================================================
    
    t1_twe = row_mean_na(
      dplyr::select(
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
    
    t2_twe = row_mean_na(
      dplyr::select(
        .,
        Engagement_1_T2,
        Engagement_2_T2,
        Engagement_3_T2,
        Engagement_4_T2,
        Engagement_5_T2,
        Engagement_6_T2,
        Engagement_7_T2,
        Engagement_8_T2,
        Engagement_9_T2
      )
    ),
    
    t1_team_performance = row_mean_na(
      dplyr::select(
        .,
        Performance_1_T1,
        Performance_2_T1,
        Recoded_Performance_3_T1,
        Performance_4_T1
      )
    ),
    
    t2_team_performance = row_mean_na(
      dplyr::select(
        .,
        Performance_1_T2,
        Performance_2_T2,
        Recoded_Performance_3_T2,
        Performance_4_T2
      )
    ),
    
    # ========================================================
    # EXPLORATORY STRAIN VARIABLES AND CONTROL
    # ========================================================
    
    stress_t1 = MeanScore_TotalStress_T1,
    stress_t2 = MeanScore_TotalStress_T2,
    team_tenure = Tenure_in_team_T1
  )

# -------------------------
# CREATE TRUE LEVEL 2 TEAM RESOURCES
# -------------------------
#
# Baseline resources are calculated from all available T1
# respondents before restricting the dataset to the T1-T2 panel.

team_resources_t1 <- df_composites %>%
  filter(
    TeamRef_T1 %in% valid_t1_teams
  ) %>%
  group_by(TeamRef_T1) %>%
  summarise(
    n_psych_safety =
      sum(!is.na(t1_psychological_safety_individual)),
    
    n_control =
      sum(!is.na(t1_control_over_work_time_individual)),
    
    t1_team_psychological_safety =
      safe_mean(t1_psychological_safety_individual),
    
    t1_control_over_work_time =
      safe_mean(t1_control_over_work_time_individual),
    
    .groups = "drop"
  ) %>%
  
  # Retain teams with at least 3 usable ratings for both resources.
  filter(
    n_psych_safety >= 3,
    n_control >= 3
  ) %>%
  
  mutate(
    # Grand-mean centre across teams, not individuals.
    psych_safety_cgm =
      t1_team_psychological_safety -
      mean(t1_team_psychological_safety, na.rm = TRUE),
    
    control_cgm =
      t1_control_over_work_time -
      mean(t1_control_over_work_time, na.rm = TRUE)
  )

valid_level2_teams <- team_resources_t1$TeamRef_T1

# -------------------------
# CREATE T1-T2 PANEL DATASET
# -------------------------

model_data_t1_t2 <- df_composites %>%
  filter(
    TeamRef_T1 %in% valid_level2_teams,
    TeamRef_T2 %in% valid_t2_teams
  ) %>%
  left_join(
    team_resources_t1,
    by = "TeamRef_T1"
  )

# -------------------------
# VERIFY LEVEL 2 VARIABLES
# -------------------------

level2_check <- model_data_t1_t2 %>%
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

print(level2_check)

# Expected result:
# max_psych_safety_values_within_team = 1
# max_control_values_within_team = 1
# teams_with_multiple_psych_safety_values = 0
# teams_with_multiple_control_values = 0

# -------------------------
# SAMPLE OVERVIEW
# -------------------------

model_sample_overview <- tibble(
  n_respondents = nrow(model_data_t1_t2),
  n_t1_teams = n_distinct(model_data_t1_t2$TeamRef_T1),
  n_t2_teams = n_distinct(model_data_t1_t2$TeamRef_T2)
)

print(model_sample_overview)

# -------------------------
# SAVE CORRECTED DATASET
# -------------------------

dir.create(
  "data_processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  model_data_t1_t2,
  "data_processed/multilevel_modelling_dataset_t1_t2.rds"
)

write_csv(
  level2_check,
  "output/tables/level2_resource_check.csv"
)

write_csv(
  model_sample_overview,
  "output/tables/multilevel_modelling_sample_overview_t1_t2.csv"
)

print(level2_check, width = Inf)

