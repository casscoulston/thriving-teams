library(tidyverse)
library(haven)

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

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

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3)
  )

full_by_wave <- tibble(
  wave = c("T1", "T2", "T3"),
  team_col = c("TeamRef_T1", "TeamRef_T2", "TeamRef_T3")
) %>%
  rowwise() %>%
  mutate(
    full_respondents = sum(!is.na(df_num[[team_col]])),
    full_teams = n_distinct(df_num[[team_col]][!is.na(df_num[[team_col]])])
  ) %>%
  ungroup() %>%
  select(wave, full_respondents, full_teams)

participant_retention_full <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_n = c(sum(!is.na(df_num$TeamRef_T1)), sum(!is.na(df_num$TeamRef_T1))),
  retained_n = c(
    sum(!is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T2)),
    sum(!is.na(df_num$TeamRef_T1) & !is.na(df_num$TeamRef_T3))
  )
) %>%
  mutate(retention_rate = round(100 * retained_n / baseline_n, 1))

t1_teams <- unique(na.omit(df_num$TeamRef_T1))
t2_teams <- unique(na.omit(df_num$TeamRef_T2))
t3_teams <- unique(na.omit(df_num$TeamRef_T3))

team_retention_full <- tibble(
  comparison = c("T2_vs_T1", "T3_vs_T1"),
  baseline_teams = c(length(t1_teams), length(t1_teams)),
  retained_teams = c(
    sum(t1_teams %in% t2_teams),
    sum(t1_teams %in% t3_teams)
  )
) %>%
  mutate(retention_rate = round(100 * retained_teams / baseline_teams, 1))

print(full_by_wave)
print(participant_retention_full)
print(team_retention_full)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(full_by_wave, "output/tables/full_sample_by_wave.csv")
write_csv(participant_retention_full, "output/tables/participant_retention_full.csv")
write_csv(team_retention_full, "output/tables/team_retention_full.csv")
