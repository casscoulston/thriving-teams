# scripts/05_compare_full_vs_analytic_and_attrition_bias.R
# Tests whether the analytic sample differs from the full sample
# and assesses attrition bias on baseline characteristics.

source(here::here("R", "utils.R"))
write_session_log("05_compare_full_vs_analytic_and_attrition_bias")

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

build_full_vs_analytic_wave <- function(
    wave,
    team_col,
    age_col,
    gender_col,
    role_num_col,
    role_chr_col,
    remote_col,
    tenure_col,
    valid_teams
) {
  full_keep <- !is.na(df_num[[team_col]])
  analytic_keep <- df_num[[team_col]] %in% valid_teams

  desc <- tibble(
    wave = wave,
    sample = c("Full", "Analytic"),
    n = c(sum(full_keep), sum(analytic_keep)),
    mean_age = c(
      fmt_mean_sd(df_num[[age_col]][full_keep]),
      fmt_mean_sd(df_num[[age_col]][analytic_keep])
    ),
    pct_women = c(
      round(pct_level(df_chr[[gender_col]][full_keep], "Female"), 1),
      round(pct_level(df_chr[[gender_col]][analytic_keep], "Female"), 1)
    ),
    pct_men = c(
      round(pct_level(df_chr[[gender_col]][full_keep], "Male"), 1),
      round(pct_level(df_chr[[gender_col]][analytic_keep], "Male"), 1)
    ),
    team_leaders_n = c(
      sum(full_keep & df_num[[role_num_col]] == 1, na.rm = TRUE),
      sum(analytic_keep & df_num[[role_num_col]] == 1, na.rm = TRUE)
    ),
    team_members_n = c(
      sum(full_keep & df_num[[role_num_col]] == 2, na.rm = TRUE),
      sum(analytic_keep & df_num[[role_num_col]] == 2, na.rm = TRUE)
    ),
    mean_remote_days = c(
      fmt_mean_sd(df_num[[remote_col]][full_keep]),
      fmt_mean_sd(df_num[[remote_col]][analytic_keep])
    ),
    mean_team_tenure = c(
      fmt_mean_sd(df_num[[tenure_col]][full_keep]),
      fmt_mean_sd(df_num[[tenure_col]][analytic_keep])
    )
  )

  group_indicator <- ifelse(analytic_keep, "Analytic", ifelse(full_keep, "Full_only", NA_character_))
  compare_group <- ifelse(group_indicator == "Analytic", "Analytic", ifelse(group_indicator == "Full_only", "Full_only", NA_character_))

  tests_num <- bind_rows(
    safe_t_test(df_num[[age_col]], compare_group) %>% mutate(variable = "Age"),
    safe_t_test(df_num[[remote_col]], compare_group) %>% mutate(variable = "Remote days"),
    safe_t_test(df_num[[tenure_col]], compare_group) %>% mutate(variable = "Team tenure")
  ) %>%
    mutate(wave = wave, test = "t_test", .before = 1)

  role_comp <- ifelse(df_num[[role_num_col]] == 1, "Team leader", ifelse(df_num[[role_num_col]] == 2, "Team member", NA_character_))
  tests_cat <- bind_rows(
    safe_chisq(role_comp, compare_group) %>% mutate(variable = "Role composition"),
    safe_chisq(df_chr[[gender_col]], compare_group) %>% mutate(variable = "Gender")
  ) %>%
    mutate(wave = wave, test = "chi_square", .before = 1)

  list(desc = desc, tests = bind_rows(tests_num, tests_cat))
}

t1_compare <- build_full_vs_analytic_wave(
  "T1", "TeamRef_T1", "Age_T1", "Gender_T1", "Leader_Team_member_T1", "Leader_Team_member_T1",
  "Remoteworkingdays_T1_days", "Tenure_in_team_T1", valid_t1_teams
)

t2_compare <- build_full_vs_analytic_wave(
  "T2", "TeamRef_T2", "Age_T2", "Gender_T2", "Leader_Team_member_T2", "Leader_Team_member_T2",
  "Remoteworkingdays_T2_days", "Tenure_in_team_T2", valid_t2_teams
)

t3_compare <- build_full_vs_analytic_wave(
  "T3", "TeamRef_T3", "Age_T3", "Gender_T3", "Leader_Team_member_T3", "Leader_Team_member_T3",
  "Remoteworkingdays_T3_days", "Tenure_in_team_T3", valid_t3_teams
)

full_vs_analytic_desc <- bind_rows(
  t1_compare$desc,
  t2_compare$desc,
  t3_compare$desc
)

full_vs_analytic_tests <- bind_rows(
  t1_compare$tests,
  t2_compare$tests,
  t3_compare$tests
) %>%
  select(wave, variable, test, statistic, p_value)

industry_full_vs_analytic <- bind_rows(
  tibble(
    wave = "T1",
    sample = ifelse(df_num$TeamRef_T1 %in% valid_t1_teams, "Analytic", ifelse(!is.na(df_num$TeamRef_T1), "Full_only", NA_character_)),
    industry = df_chr$Industry_T1
  ),
  tibble(
    wave = "T2",
    sample = ifelse(df_num$TeamRef_T2 %in% valid_t2_teams, "Analytic", ifelse(!is.na(df_num$TeamRef_T2), "Full_only", NA_character_)),
    industry = df_chr$Industry_T2
  ),
  tibble(
    wave = "T3",
    sample = ifelse(df_num$TeamRef_T3 %in% valid_t3_teams, "Analytic", ifelse(!is.na(df_num$TeamRef_T3), "Full_only", NA_character_)),
    industry = df_chr$Industry_T3
  )
) %>%
  filter(!is.na(sample), !is.na(industry), industry != "") %>%
  count(wave, sample, industry, name = "n") %>%
  group_by(wave, sample) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  ungroup()

attrition_bias_t1_t2 <- tibble(
  retained_T2 = !is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T2),
  age_T1 = df_num$Age_T1,
  role_T1 = ifelse(df_num$Leader_Team_member_T1 == 1, "Team leader", ifelse(df_num$Leader_Team_member_T1 == 2, "Team member", NA_character_)),
  remote_T1 = df_num$Remoteworkingdays_T1_days,
  tenure_T1 = df_num$Tenure_in_team_T1,
  gender_T1 = df_chr$Gender_T1
) %>%
  mutate(retained_T2 = ifelse(retained_T2, "Retained", "Not retained"))

attrition_bias_t1_t3 <- tibble(
  retained_T3 = !is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T3),
  age_T1 = df_num$Age_T1,
  role_T1 = ifelse(df_num$Leader_Team_member_T1 == 1, "Team leader", ifelse(df_num$Leader_Team_member_T1 == 2, "Team member", NA_character_)),
  remote_T1 = df_num$Remoteworkingdays_T1_days,
  tenure_T1 = df_num$Tenure_in_team_T1,
  gender_T1 = df_chr$Gender_T1
) %>%
  mutate(retained_T3 = ifelse(retained_T3, "Retained", "Not retained"))

attrition_bias_tests <- bind_rows(
  safe_t_test(attrition_bias_t1_t2$age_T1, attrition_bias_t1_t2$retained_T2) %>% mutate(comparison = "T2 retention", variable = "Age at T1"),
  safe_t_test(attrition_bias_t1_t2$remote_T1, attrition_bias_t1_t2$retained_T2) %>% mutate(comparison = "T2 retention", variable = "Remote days at T1"),
  safe_t_test(attrition_bias_t1_t2$tenure_T1, attrition_bias_t1_t2$retained_T2) %>% mutate(comparison = "T2 retention", variable = "Team tenure at T1"),
  safe_chisq(attrition_bias_t1_t2$role_T1, attrition_bias_t1_t2$retained_T2) %>% mutate(comparison = "T2 retention", variable = "Role at T1"),
  safe_chisq(attrition_bias_t1_t2$gender_T1, attrition_bias_t1_t2$retained_T2) %>% mutate(comparison = "T2 retention", variable = "Gender at T1"),

  safe_t_test(attrition_bias_t1_t3$age_T1, attrition_bias_t1_t3$retained_T3) %>% mutate(comparison = "T3 retention", variable = "Age at T1"),
  safe_t_test(attrition_bias_t1_t3$remote_T1, attrition_bias_t1_t3$retained_T3) %>% mutate(comparison = "T3 retention", variable = "Remote days at T1"),
  safe_t_test(attrition_bias_t1_t3$tenure_T1, attrition_bias_t1_t3$retained_T3) %>% mutate(comparison = "T3 retention", variable = "Team tenure at T1"),
  safe_chisq(attrition_bias_t1_t3$role_T1, attrition_bias_t1_t3$retained_T3) %>% mutate(comparison = "T3 retention", variable = "Role at T1"),
  safe_chisq(attrition_bias_t1_t3$gender_T1, attrition_bias_t1_t3$retained_T3) %>% mutate(comparison = "T3 retention", variable = "Gender at T1")
) %>%
  select(comparison, variable, statistic, p_value)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(full_vs_analytic_desc, here::here("output", "tables", "full_vs_analytic_characteristics.csv"))
write_csv(full_vs_analytic_tests, here::here("output", "tables", "full_vs_analytic_tests.csv"))
write_csv(industry_full_vs_analytic, here::here("output", "tables", "industry_full_vs_analytic.csv"))
write_csv(attrition_bias_tests, here::here("output", "tables", "attrition_bias_tests.csv"))

print(full_vs_analytic_desc, n = 20)
print(full_vs_analytic_tests, n = 50)
print(industry_full_vs_analytic, n = 100)
print(attrition_bias_tests, n = 50)
