# scripts/01_build_table1_analytic_sample.R
# Builds Table 1 sample characteristics by wave for the analytic sample.

source(here::here("R", "utils.R"))
write_session_log("01_build_table1_analytic_sample")

wide_df_raw <- readRDS(here::here("data_raw", "wide_df_raw.rds"))

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3),
    Age_T1 = readr::parse_number(as.character(Age_T1), na = c("", "NA", "not answering")),
    Age_T2 = readr::parse_number(as.character(Age_T2), na = c("", "NA", "not answering")),
    Age_T3 = readr::parse_number(as.character(Age_T3), na = c("", "NA", "not answering")),
    Tenure_in_team_T1 = readr::parse_number(as.character(Tenure_in_team_T1), na = c("", "NA")),
    Tenure_in_team_T2 = readr::parse_number(as.character(Tenure_in_team_T2), na = c("", "NA")),
    Tenure_in_team_T3 = readr::parse_number(as.character(Tenure_in_team_T3), na = c("", "NA")),
    Remoteworkingdays_T1_days = Remoteworkingdays_T1 - 1,
    Remoteworkingdays_T2_days = Remoteworkingdays_T2 - 1,
    Remoteworkingdays_T3_days = Remoteworkingdays_T3 - 1
  )

df_chr <- wide_df_raw %>%
  mutate(across(everything(), to_character_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_num, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

build_wave_table1 <- function(wave, team_col, role_num_col, age_col, gender_col, remote_col, tenure_col, industry_col, valid_teams) {
  keep <- df_num[[team_col]] %in% valid_teams

  team_sizes <- df_num %>%
    filter(keep, !is.na(.data[[team_col]])) %>%
    count(.data[[team_col]], name = "team_size")

  tibble(
    wave = wave,
    total_respondents = sum(keep, na.rm = TRUE),
    mean_age = fmt_mean_sd(df_num[[age_col]][keep]),
    pct_women = round(pct_level(df_chr[[gender_col]][keep], "Female"), 1),
    pct_men = round(pct_level(df_chr[[gender_col]][keep], "Male"), 1),
    team_members_n = sum(keep & df_num[[role_num_col]] == 2, na.rm = TRUE),
    team_leaders_n = sum(keep & df_num[[role_num_col]] == 1, na.rm = TRUE),
    teams_represented_n = n_distinct(df_num[[team_col]][keep & !is.na(df_num[[team_col]])]),
    mean_team_size = fmt_mean_sd(team_sizes$team_size),
    team_size_range = fmt_range(team_sizes$team_size),
    mean_remote_days = fmt_mean_sd(df_num[[remote_col]][keep]),
    industries_represented_n = count_industries(df_chr[[industry_col]][keep]),
    mean_team_tenure = fmt_mean_sd(df_num[[tenure_col]][keep])
  )
}

table1_analytic <- bind_rows(
  build_wave_table1(
    "T1", "TeamRef_T1", "Leader_Team_member_T1",
    "Age_T1", "Gender_T1", "Remoteworkingdays_T1_days",
    "Tenure_in_team_T1", "Industry_T1", valid_t1_teams
  ),
  build_wave_table1(
    "T2", "TeamRef_T2", "Leader_Team_member_T2",
    "Age_T2", "Gender_T2", "Remoteworkingdays_T2_days",
    "Tenure_in_team_T2", "Industry_T2", valid_t2_teams
  ),
  build_wave_table1(
    "T3", "TeamRef_T3", "Leader_Team_member_T3",
    "Age_T3", "Gender_T3", "Remoteworkingdays_T3_days",
    "Tenure_in_team_T3", "Industry_T3", valid_t3_teams
  )
)

table1_analytic_long <- tribble(
  ~Characteristic, ~var,
  "Total respondents, n", "total_respondents",
  "Mean age, years (SD)", "mean_age",
  "% women", "pct_women",
  "% men", "pct_men",
  "Team members, n", "team_members_n",
  "Team leaders, n", "team_leaders_n",
  "Teams represented, n", "teams_represented_n",
  "Mean team size (SD)", "mean_team_size",
  "Team size range", "team_size_range",
  "Mean remote days per week (SD)", "mean_remote_days",
  "Industries represented, n", "industries_represented_n",
  "Mean team tenure, years (SD)", "mean_team_tenure"
) %>%
  left_join(
    table1_analytic %>%
      mutate(across(-wave, as.character)) %>%
      pivot_longer(-wave, names_to = "var", values_to = "value") %>%
      pivot_wider(names_from = wave, values_from = value),
    by = "var"
  ) %>%
  select(-var)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(table1_analytic_long, here::here("output", "tables", "table1_analytic_sample_by_wave.csv"))

print(table1_analytic)
print(table1_analytic_long)
