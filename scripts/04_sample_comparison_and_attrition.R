# scripts/04_sample_comparison_and_attrition.R
# Compares full and analytic samples and summarises attrition across waves.

source(here::here("R", "utils.R"))
write_session_log("04_sample_comparison_and_attrition")

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

full_by_wave <- tibble(
  wave = c("T1", "T2", "T3"),
  full_respondents = c(
    sum(!is.na(df_num$TeamRef_T1)),
    sum(!is.na(df_num$TeamRef_T2)),
    sum(!is.na(df_num$TeamRef_T3))
  ),
  full_teams = c(
    n_distinct(df_num$TeamRef_T1[!is.na(df_num$TeamRef_T1)]),
    n_distinct(df_num$TeamRef_T2[!is.na(df_num$TeamRef_T2)]),
    n_distinct(df_num$TeamRef_T3[!is.na(df_num$TeamRef_T3)])
  )
)

analytic_by_wave <- tibble(
  wave = c("T1", "T2", "T3"),
  analytic_respondents = c(
    sum(df_num$TeamRef_T1 %in% valid_t1_teams, na.rm = TRUE),
    sum(df_num$TeamRef_T2 %in% valid_t2_teams, na.rm = TRUE),
    sum(df_num$TeamRef_T3 %in% valid_t3_teams, na.rm = TRUE)
  ),
  analytic_teams = c(
    length(valid_t1_teams),
    length(valid_t2_teams),
    length(valid_t3_teams)
  )
)

sample_comparison <- full_by_wave %>%
  left_join(analytic_by_wave, by = "wave") %>%
  mutate(
    excluded_respondents_below3 = full_respondents - analytic_respondents,
    excluded_teams_below3 = full_teams - analytic_teams,
    respondents_retained_pct = round(100 * analytic_respondents / full_respondents, 1),
    teams_retained_pct = round(100 * analytic_teams / full_teams, 1)
  )

participant_retention_full <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_n = c(sum(!is.na(df_num$TeamRef_T1)), sum(!is.na(df_num$TeamRef_T1))),
  retained_n = c(
    sum(!is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T2)),
    sum(!is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T3))
  )
) %>%
  mutate(retention_rate_pct = round(100 * retained_n / baseline_n, 1))

participant_retention_analytic <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_n = c(
    sum(df_num$TeamRef_T1 %in% valid_t1_teams, na.rm = TRUE),
    sum(df_num$TeamRef_T1 %in% valid_t1_teams, na.rm = TRUE)
  ),
  retained_n = c(
    sum(df_num$TeamRef_T1 %in% valid_t1_teams & df_num$TeamRef_T2 %in% valid_t2_teams, na.rm = TRUE),
    sum(df_num$TeamRef_T1 %in% valid_t1_teams & df_num$TeamRef_T3 %in% valid_t3_teams, na.rm = TRUE)
  )
) %>%
  mutate(retention_rate_pct = round(100 * retained_n / baseline_n, 1))

team_retention_full <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_teams = c(length(unique(na.omit(df_num$TeamRef_T1))), length(unique(na.omit(df_num$TeamRef_T1)))),
  retained_teams = c(
    sum(unique(na.omit(df_num$TeamRef_T1)) %in% unique(na.omit(df_num$TeamRef_T2))),
    sum(unique(na.omit(df_num$TeamRef_T1)) %in% unique(na.omit(df_num$TeamRef_T3)))
  )
) %>%
  mutate(retention_rate_pct = round(100 * retained_teams / baseline_teams, 1))

team_retention_analytic <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_teams = c(length(valid_t1_teams), length(valid_t1_teams)),
  retained_teams = c(
    sum(valid_t1_teams %in% valid_t2_teams),
    sum(valid_t1_teams %in% valid_t3_teams)
  )
) %>%
  mutate(retention_rate_pct = round(100 * retained_teams / baseline_teams, 1))

print(sample_comparison)
print(participant_retention_full)
print(participant_retention_analytic)
print(team_retention_full)
print(team_retention_analytic)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(sample_comparison, here::here("output", "tables", "sample_comparison_full_vs_analytic.csv"))
write_csv(participant_retention_full, here::here("output", "tables", "participant_retention_full.csv"))
write_csv(participant_retention_analytic, here::here("output", "tables", "participant_retention_analytic.csv"))
write_csv(team_retention_full, here::here("output", "tables", "team_retention_full.csv"))
write_csv(team_retention_analytic, here::here("output", "tables", "team_retention_analytic.csv"))
