# scripts/02_supplementary_role_based_demographics.R
# Role-based descriptive statistics by wave.

source(here::here("R", "utils.R"))
write_session_log("02_supplementary_role_based_demographics")

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

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_num, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

build_role_wave_summary <- function(wave, team_col, role_col, age_col, remote_col, tenure_col, valid_teams) {
  tibble(
    wave = wave,
    role = c("Team leaders", "Team members")
  ) %>%
    mutate(
      role_code = c(1, 2),
      keep_n = map_int(role_code, ~ sum(df_num[[team_col]] %in% valid_teams & df_num[[role_col]] == .x, na.rm = TRUE)),
      mean_age = map_chr(role_code, ~ fmt_mean_sd(df_num[[age_col]][df_num[[team_col]] %in% valid_teams & df_num[[role_col]] == .x])),
      mean_remote_days = map_chr(role_code, ~ fmt_mean_sd(df_num[[remote_col]][df_num[[team_col]] %in% valid_teams & df_num[[role_col]] == .x])),
      mean_team_tenure = map_chr(role_code, ~ fmt_mean_sd(df_num[[tenure_col]][df_num[[team_col]] %in% valid_teams & df_num[[role_col]] == .x]))
    ) %>%
    select(-role_code)
}

supp_role_table <- bind_rows(
  build_role_wave_summary("T1", "TeamRef_T1", "Leader_Team_member_T1", "Age_T1", "Remoteworkingdays_T1_days", "Tenure_in_team_T1", valid_t1_teams),
  build_role_wave_summary("T2", "TeamRef_T2", "Leader_Team_member_T2", "Age_T2", "Remoteworkingdays_T2_days", "Tenure_in_team_T2", valid_t2_teams),
  build_role_wave_summary("T3", "TeamRef_T3", "Leader_Team_member_T3", "Age_T3", "Remoteworkingdays_T3_days", "Tenure_in_team_T3", valid_t3_teams)
)

print(supp_role_table, n = 20)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(supp_role_table, here::here("output", "tables", "supplementary_role_based_demographics.csv"))
