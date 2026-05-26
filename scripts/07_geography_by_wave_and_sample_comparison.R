# scripts/07_geography_by_wave_and_sample_comparison.R
# Geographic composition by wave; comparison across samples.

source(here::here("R", "utils.R"))
write_session_log("07_geography_by_wave_and_sample_comparison")

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

geography_analytic_by_wave <- bind_rows(
  df_chr %>%
    filter(TeamRef_T1 %in% valid_t1_teams) %>%
    count(wave = "T1", geography = Geography_T1, sort = TRUE, name = "n") %>%
    filter(!is.na(geography), geography != ""),
  df_chr %>%
    filter(TeamRef_T2 %in% valid_t2_teams) %>%
    count(wave = "T2", geography = Geography_T2, sort = TRUE, name = "n") %>%
    filter(!is.na(geography), geography != ""),
  df_chr %>%
    filter(TeamRef_T3 %in% valid_t3_teams) %>%
    count(wave = "T3", geography = Geography_T3, sort = TRUE, name = "n") %>%
    filter(!is.na(geography), geography != "")
) %>%
  group_by(wave) %>%
  mutate(
    total_n = sum(n),
    percent = round(100 * n / total_n, 1)
  ) %>%
  ungroup()

geography_counts_summary <- geography_analytic_by_wave %>%
  group_by(wave) %>%
  summarise(geographies_represented_n = n_distinct(geography), .groups = "drop")

print(geography_counts_summary)
print(geography_analytic_by_wave, n = 100)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(geography_counts_summary, here::here("output", "tables", "geography_counts_summary.csv"))
write_csv(geography_analytic_by_wave, here::here("output", "tables", "geography_analytic_by_wave.csv"))
