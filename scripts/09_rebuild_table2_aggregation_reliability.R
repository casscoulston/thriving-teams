# scripts/09_rebuild_table2_aggregation_reliability.R
# Aggregation and reliability table for team-level constructs across waves.
# Computes mean composite, Cronbach's alpha, McDonald's omega, ICC(1), ICC(2),
# median and mean rwg(j) for team engagement, control over work time and
# team psychological safety.

source(here::here("R", "utils.R"))
write_session_log("09_rebuild_table2_aggregation_reliability")

library(psych)

wide_df_raw <- readRDS(here::here("data_raw", "wide_df_raw.rds"))

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_num, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

build_construct_stats <- function(
    df,
    wave_label,
    team_col,
    valid_teams,
    construct_label,
    item_cols,
    scale_min = 1,
    scale_max = 7
) {
  df_wave <- df %>%
    filter(.data[[team_col]] %in% valid_teams) %>%
    mutate(composite = rowMeans(select(., all_of(item_cols)), na.rm = TRUE))

  reliability <- calc_alpha_omega(df_wave, item_cols)
  iccs <- calc_icc(df_wave, team_col, "composite")
  rwg <- calc_rwg(df_wave, team_col, item_cols, scale_min = scale_min, scale_max = scale_max)

  tibble(
    Variable = paste(wave_label, construct_label),
    ICC1 = round(iccs$ICC1, 2),
    ICC2 = round(iccs$ICC2, 2),
    Median_rwgj = round(rwg$Median_rwgj, 2),
    Mean_rwgj = round(rwg$Mean_rwgj, 2),
    Alpha = round(reliability$Alpha, 2),
    Omega = round(reliability$Omega, 2)
  )
}

table2_rebuilt <- bind_rows(
  build_construct_stats(
    df_num, "T1", "TeamRef_T1", valid_t1_teams, "Team engagement",
    c("Engagement_1_T1", "Engagement_2_T1", "Engagement_3_T1", "Engagement_4_T1",
      "Engagement_5_T1", "Engagement_6_T1", "Engagement_7_T1", "Engagement_8_T1", "Engagement_9_T1"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T2", "TeamRef_T2", valid_t2_teams, "Team engagement",
    c("Engagement_1_T2", "Engagement_2_T2", "Engagement_3_T2", "Engagement_4_T2",
      "Engagement_5_T2", "Engagement_6_T2", "Engagement_7_T2", "Engagement_8_T2", "Engagement_9_T2"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T3", "TeamRef_T3", valid_t3_teams, "Team engagement",
    c("Engagement_1_T3", "Engagement_2_T3", "Engagement_3_T3", "Engagement_4_T3",
      "Engagement_5_T3", "Engagement_6_T3", "Engagement_7_T3", "Engagement_8_T3", "Engagement_9_T3"),
    scale_min = 1, scale_max = 7
  ),

  build_construct_stats(
    df_num, "T1", "TeamRef_T1", valid_t1_teams, "Control over work time",
    c("Choice_remote_work_T1", "Choice_work_week_T1", "Choice_vacations_T1", "Choice_in_office_T1", "Control_hrs_off_T1"),
    scale_min = 1, scale_max = 5
  ),
  build_construct_stats(
    df_num, "T2", "TeamRef_T2", valid_t2_teams, "Control over work time",
    c("Choice_remote_work_T2", "Choice_work_week_T2", "Choice_vacations_T2", "Choice_in_office_T2", "Control_hrs_off_T2"),
    scale_min = 1, scale_max = 5
  ),
  build_construct_stats(
    df_num, "T3", "TeamRef_T3", valid_t3_teams, "Control over work time",
    c("Choice_remote_work_T3", "Choice_work_week_T3", "Choice_vacations_T3", "Choice_in_office_T3", "Control_hrs_off_T3"),
    scale_min = 1, scale_max = 5
  ),

  build_construct_stats(
    df_num, "T1", "TeamRef_T1", valid_t1_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T1", "Psychological_safety_2_T1", "RecodedPsychologicalSafety_3_T1",
      "Psychological_safety_4_T1", "RecodedPsychologicalSafety_5_T1", "Psychological_safety_6_T1", "Psychological_safety_7_T1"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T2", "TeamRef_T2", valid_t2_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T2", "Psychological_safety_2_T2", "RecodedPsychologicalSafety_3_T2",
      "Psychological_safety_4_T2", "RecodedPsychologicalSafety_5_T2", "Psychological_safety_6_T2", "Psychological_safety_7_T2"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T3", "TeamRef_T3", valid_t3_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T3", "Psychological_safety_2_T3", "RecodedPsychologicalSafety_3_T3",
      "Psychological_safety_4_T3", "RecodedPsychologicalSafety_5_T3", "Psychological_safety_6_T3", "Psychological_safety_7_T3"),
    scale_min = 1, scale_max = 7
  )
)

print(table2_rebuilt, n = 20)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(table2_rebuilt, here::here("output", "tables", "table2_aggregation_reliability_rebuilt.csv"))
