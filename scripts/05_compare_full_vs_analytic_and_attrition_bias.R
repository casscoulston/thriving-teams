# scripts/05_compare_full_vs_analytic_and_attrition_bias.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

to_numeric_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.numeric(haven::zap_labels(x)))
  }
  x
}

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

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

safe_t_test <- function(x, g) {
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]
  g <- g[keep]
  if (length(unique(g)) != 2 || length(x) < 3) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }
  out <- tryCatch(t.test(x ~ g), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tibble(statistic = unname(out$statistic), p_value = out$p.value)
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

pct_level <- function(x, level) {
  denom <- sum(!is.na(x) & x != "")
  if (denom == 0) return(NA_real_)
  100 * sum(x == level, na.rm = TRUE) / denom
}

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

team_size_by_wave <- bind_rows(
  df_num %>%
    filter(!is.na(TeamRef_T1)) %>%
    count(team_id = TeamRef_T1, name = "team_size") %>%
    mutate(wave = "T1"),
  df_num %>%
    filter(!is.na(TeamRef_T2)) %>%
    count(team_id = TeamRef_T2, name = "team_size") %>%
    mutate(wave = "T2"),
  df_num %>%
    filter(!is.na(TeamRef_T3)) %>%
    count(team_id = TeamRef_T3, name = "team_size") %>%
    mutate(wave = "T3")
)

valid_t1_teams <- team_size_by_wave %>% filter(wave == "T1", team_size >= 3) %>% pull(team_id)
valid_t2_teams <- team_size_by_wave %>% filter(wave == "T2", team_size >= 3) %>% pull(team_id)
valid_t3_teams <- team_size_by_wave %>% filter(wave == "T3", team_size >= 3) %>% pull(team_id)

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

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(full_vs_analytic_desc, "output/tables/full_vs_analytic_characteristics.csv")
write_csv(full_vs_analytic_tests, "output/tables/full_vs_analytic_tests.csv")
write_csv(industry_full_vs_analytic, "output/tables/industry_full_vs_analytic.csv")
write_csv(attrition_bias_tests, "output/tables/attrition_bias_tests.csv")

print(full_vs_analytic_desc, n = 20)
print(full_vs_analytic_tests, n = 50)
print(industry_full_vs_analytic, n = 100)
print(attrition_bias_tests, n = 50)
