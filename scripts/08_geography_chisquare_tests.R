# scripts/08_geography_chisquare_tests.R
# Chi-square tests for geographic composition and geography-related attrition.

source(here::here("R", "utils.R"))
write_session_log("08_geography_chisquare_tests")

wide_df_raw <- readRDS(here::here("data_raw", "wide_df_raw.rds"))

df_chr <- wide_df_raw %>%
  mutate(across(everything(), to_character_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

valid_t1_teams <- get_valid_teams(df_chr, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_chr, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_chr, "TeamRef_T3")

geography_full_vs_analytic_tests <- bind_rows(
  safe_chisq(
    df_chr$Geography_T1,
    ifelse(df_chr$TeamRef_T1 %in% valid_t1_teams, "Analytic",
           ifelse(!is.na(df_chr$TeamRef_T1), "Full_only", NA_character_))
  ) %>% mutate(wave = "T1"),
  safe_chisq(
    df_chr$Geography_T2,
    ifelse(df_chr$TeamRef_T2 %in% valid_t2_teams, "Analytic",
           ifelse(!is.na(df_chr$TeamRef_T2), "Full_only", NA_character_))
  ) %>% mutate(wave = "T2"),
  safe_chisq(
    df_chr$Geography_T3,
    ifelse(df_chr$TeamRef_T3 %in% valid_t3_teams, "Analytic",
           ifelse(!is.na(df_chr$TeamRef_T3), "Full_only", NA_character_))
  ) %>% mutate(wave = "T3")
) %>%
  select(wave, statistic, p_value)

geography_attrition_tests <- bind_rows(
  safe_chisq(
    df_chr$Geography_T1,
    ifelse(!is.na(df_chr$TeamRef_T1) & !is.na(df_chr$TeamRef_T2), "Retained", "Not retained")
  ) %>% mutate(comparison = "T2 retention"),
  safe_chisq(
    df_chr$Geography_T1,
    ifelse(!is.na(df_chr$TeamRef_T1) & !is.na(df_chr$TeamRef_T3), "Retained", "Not retained")
  ) %>% mutate(comparison = "T3 retention")
) %>%
  select(comparison, statistic, p_value)

print(geography_full_vs_analytic_tests)
print(geography_attrition_tests)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(geography_full_vs_analytic_tests, here::here("output", "tables", "geography_full_vs_analytic_tests.csv"))
write_csv(geography_attrition_tests, here::here("output", "tables", "geography_attrition_tests.csv"))
