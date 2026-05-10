library(tidyverse)
library(haven)

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

to_character_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.character(haven::as_factor(x)))
  }
  x
}

clean_team_id <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

df_chr <- wide_df_raw %>%
  mutate(across(everything(), to_character_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

team_size_by_wave <- bind_rows(
  df_chr %>%
    filter(!is.na(TeamRef_T1)) %>%
    count(team_id = TeamRef_T1, name = "team_size") %>%
    mutate(wave = "T1"),
  df_chr %>%
    filter(!is.na(TeamRef_T2)) %>%
    count(team_id = TeamRef_T2, name = "team_size") %>%
    mutate(wave = "T2"),
  df_chr %>%
    filter(!is.na(TeamRef_T3)) %>%
    count(team_id = TeamRef_T3, name = "team_size") %>%
    mutate(wave = "T3")
)

valid_t1_teams <- team_size_by_wave %>% filter(wave == "T1", team_size >= 3) %>% pull(team_id)
valid_t2_teams <- team_size_by_wave %>% filter(wave == "T2", team_size >= 3) %>% pull(team_id)
valid_t3_teams <- team_size_by_wave %>% filter(wave == "T3", team_size >= 3) %>% pull(team_id)

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

write_csv(geography_counts_summary, "output/tables/geography_counts_summary.csv")
write_csv(geography_analytic_by_wave, "output/tables/geography_analytic_by_wave.csv")
