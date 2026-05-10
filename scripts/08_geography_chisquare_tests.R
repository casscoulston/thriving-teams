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

safe_chisq <- function(x, g) {
  keep <- !is.na(x) & !is.na(g) & x != ""
  x <- x[keep]
  g <- g[keep]
  if (length(unique(g)) != 2 || length(unique(x)) < 2) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tab <- table(x, g)
  out <- tryCatch(chisq.test(tab), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tibble(statistic = unname(out$statistic), p_value = out$p.value)
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

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(geography_full_vs_analytic_tests, "output/tables/geography_full_vs_analytic_tests.csv")
write_csv(geography_attrition_tests, "output/tables/geography_attrition_tests.csv")
