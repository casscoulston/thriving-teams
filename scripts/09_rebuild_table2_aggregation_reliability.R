# install.packages("psych")
# install.packages("multilevel")

library(tidyverse)
library(haven)
library(psych)

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

get_valid_teams <- function(df, team_col) {
  df %>%
    filter(!is.na(.data[[team_col]])) %>%
    count(.data[[team_col]], name = "team_size") %>%
    filter(team_size >= 3) %>%
    pull(1)
}

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_num, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

mean_if_enough <- function(items, min_prop = .80) {
  items <- as.data.frame(items)
  
  required_items <- ceiling(ncol(items) * min_prop)
  completed_items <- rowSums(!is.na(items))
  
  score <- rowMeans(items, na.rm = TRUE)
  score[completed_items < required_items] <- NA_real_
  score[is.nan(score)] <- NA_real_
  
  score
}

calc_icc <- function(df, team_col, score_col) {
  d <- df %>%
    select(team = all_of(team_col), score = all_of(score_col)) %>%
    filter(!is.na(team), !is.na(score))
  
  if (nrow(d) < 3 || dplyr::n_distinct(d$team) < 2) {
    return(tibble(ICC1 = NA_real_, ICC2 = NA_real_))
  }
  
  aov_obj <- aov(score ~ team, data = d)
  aov_tab <- summary(aov_obj)[[1]]
  
  ms_between <- aov_tab["team", "Mean Sq"]
  ms_within <- aov_tab["Residuals", "Mean Sq"]
  
  k <- d %>%
    count(team) %>%
    summarise(mean_n = mean(n), .groups = "drop") %>%
    pull(mean_n)
  
  icc1 <- (ms_between - ms_within) /
    (ms_between + (k - 1) * ms_within)
  
  icc2 <- (ms_between - ms_within) /
    ms_between
  
  tibble(ICC1 = icc1, ICC2 = icc2)
}

calc_rwg <- function(df, team_col, item_cols, scale_min = 1, scale_max = 7) {
  d <- df %>%
    select(all_of(team_col), all_of(item_cols)) %>%
    filter(!is.na(.data[[team_col]]))
  
  expected_var <- ((scale_max - scale_min + 1)^2 - 1) / 12
  
  items <- d %>%
    select(all_of(item_cols)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  rwg_out <- multilevel::rwg.j(
    x = items,
    grpid = d[[team_col]],
    ranvar = expected_var,
    listwise = FALSE
  )
  
  tibble(
    Median_rwgj = median(rwg_out$rwg.j, na.rm = TRUE),
    Mean_rwgj = mean(rwg_out$rwg.j, na.rm = TRUE)
  )
}

calc_alpha_omega <- function(df, item_cols) {
  items <- df %>%
    select(all_of(item_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  # Note: check.keys = FALSE is intentional. Reverse-coded items
  # (RecodedPsychologicalsafety_1_T*, RecodedPsychologicalSafety_3_T*,
  # RecodedPsychologicalSafety_5_T*) have already been reversed at the
  # SPSS preparation stage. Allowing psych::alpha to auto-reverse would
  # double-reverse these items.
  alpha_out <- tryCatch(
    psych::alpha(items, warnings = FALSE, check.keys = FALSE),
    error = function(e) NULL
  )
  
  omega_out <- tryCatch(
    suppressMessages(
      psych::omega(items, plot = FALSE, warnings = FALSE)
    ),
    error = function(e) NULL
  )
  
  tibble(
    Alpha = if (is.null(alpha_out)) {
      NA_real_
    } else {
      alpha_out$total$raw_alpha
    },
    
    Omega = if (is.null(omega_out)) {
      NA_real_
    } else {
      omega_out$omega.tot
    }
  )
}

build_construct_stats <- function(
    df,
    wave_label,
    team_col,
    valid_teams,
    construct_label,
    item_cols,
    scale_min = 1,
    scale_max = 7,
    min_prop = .80
) {
  df_wave <- df %>%
    filter(.data[[team_col]] %in% valid_teams) %>%
    mutate(
      composite = mean_if_enough(
        select(., all_of(item_cols)),
        min_prop = min_prop
      )
    )
  
  valid_teams_for_construct <- df_wave %>%
    filter(!is.na(composite)) %>%
    count(.data[[team_col]], name = "usable_team_size") %>%
    filter(usable_team_size >= 3) %>%
    pull(1)
  
  df_wave <- df_wave %>%
    filter(
      .data[[team_col]] %in% valid_teams_for_construct,
      !is.na(composite)
    )
  
  reliability <- calc_alpha_omega(df_wave, item_cols)
  iccs <- calc_icc(df_wave, team_col, "composite")
  rwg <- calc_rwg(
    df_wave,
    team_col,
    item_cols,
    scale_min = scale_min,
    scale_max = scale_max
  )
  
  tibble(
    Variable = paste(wave_label, construct_label),
    N_respondents = nrow(df_wave),
    N_teams = dplyr::n_distinct(df_wave[[team_col]]),
    ICC1 = round(iccs$ICC1, 2),
    ICC2 = round(iccs$ICC2, 2),
    Median_rwgj = round(rwg$Median_rwgj, 2),
    Mean_rwgj = round(rwg$Mean_rwgj, 2),
    Alpha = round(reliability$Alpha, 2),
    Omega = round(reliability$Omega, 2)
  )
}

table2_rebuilt <- bind_rows(
  build_construct_stats(
    df_num, "T1", "TeamRef_T1", valid_t1_teams, "Control over work time",
    c("Choice_remote_work_T1", "Choice_work_week_T1",
      "Choice_vacations_T1", "Choice_in_office_T1",
      "Control_hrs_off_T1"),
    scale_min = 1, scale_max = 5
  ),
  build_construct_stats(
    df_num, "T2", "TeamRef_T2", valid_t2_teams, "Control over work time",
    c("Choice_remote_work_T2", "Choice_work_week_T2",
      "Choice_vacations_T2", "Choice_in_office_T2",
      "Control_hrs_off_T2"),
    scale_min = 1, scale_max = 5
  ),
  build_construct_stats(
    df_num, "T3", "TeamRef_T3", valid_t3_teams, "Control over work time",
    c("Choice_remote_work_T3", "Choice_work_week_T3",
      "Choice_vacations_T3", "Choice_in_office_T3",
      "Control_hrs_off_T3"),
    scale_min = 1, scale_max = 5
  ),
  
  build_construct_stats(
    df_num, "T1", "TeamRef_T1", valid_t1_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T1", "Psychological_safety_2_T1",
      "RecodedPsychologicalSafety_3_T1", "Psychological_safety_4_T1",
      "RecodedPsychologicalSafety_5_T1", "Psychological_safety_6_T1",
      "Psychological_safety_7_T1"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T2", "TeamRef_T2", valid_t2_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T2", "Psychological_safety_2_T2",
      "RecodedPsychologicalSafety_3_T2", "Psychological_safety_4_T2",
      "RecodedPsychologicalSafety_5_T2", "Psychological_safety_6_T2",
      "Psychological_safety_7_T2"),
    scale_min = 1, scale_max = 7
  ),
  build_construct_stats(
    df_num, "T3", "TeamRef_T3", valid_t3_teams, "Team psychological safety",
    c("RecodedPsychologicalsafety_1_T3", "Psychological_safety_2_T3",
      "RecodedPsychologicalSafety_3_T3", "Psychological_safety_4_T3",
      "RecodedPsychologicalSafety_5_T3", "Psychological_safety_6_T3",
      "Psychological_safety_7_T3"),
    scale_min = 1, scale_max = 7
  )
)

print(table2_rebuilt, n = 20)

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  table2_rebuilt,
  "output/tables/table2_aggregation_reliability_rebuilt.csv"
)

# General teams retained at each wave
length(valid_t1_teams)
length(valid_t2_teams)
length(valid_t3_teams)
