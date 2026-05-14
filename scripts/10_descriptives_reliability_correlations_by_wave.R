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

fmt_mean <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), mean(x))
}

fmt_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), sd(x))
}

calc_alpha_omega <- function(df, item_cols) {
  items <- df %>%
    select(all_of(item_cols)) %>%
    mutate(across(everything(), as.numeric))
  
  alpha_out <- tryCatch(
    psych::alpha(items, warnings = FALSE, check.keys = FALSE),
    error = function(e) NULL
  )
  
  omega_out <- tryCatch(
    suppressMessages(psych::omega(items, plot = FALSE, warnings = FALSE)),
    error = function(e) NULL
  )
  
  tibble(
    Alpha = if (is.null(alpha_out)) NA_real_ else alpha_out$total$raw_alpha,
    Omega = if (is.null(omega_out)) NA_real_ else omega_out$omega.tot
  )
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

df_num_panel <- df_num %>%
  filter(
    TeamRef_T1 %in% valid_t1_teams,
    TeamRef_T2 %in% valid_t2_teams
  )

model_data_t1_t2 <- df_num_panel %>%
  mutate(
    t1_team_psychological_safety = rowMeans(
      select(
        .,
        RecodedPsychologicalsafety_1_T1,
        Psychological_safety_2_T1,
        RecodedPsychologicalSafety_3_T1,
        Psychological_safety_4_T1,
        RecodedPsychologicalSafety_5_T1,
        Psychological_safety_6_T1,
        Psychological_safety_7_T1
      ),
      na.rm = TRUE
    ),
    t1_control_over_work_time = rowMeans(
      select(
        .,
        Choice_remote_work_T1,
        Choice_work_week_T1,
        Choice_vacations_T1,
        Choice_in_office_T1,
        Control_hrs_off_T1
      ),
      na.rm = TRUE
    ),
    t1_feeling_disconnected = rowMeans(
      select(
        .,
        Disconnection_1_T1,
        Disconnection_2_T1,
        Disconnection_3_T1,
        Disconnection_4_T1,
        RecodedDisconnection5_T1,
        RecodedDisconnection6_T1,
        Disconnection_7_T1,
        Disconnection_8_T1,
        Disconnection_9_T1
      ),
      na.rm = TRUE
    ),
    t1_connection_overload = rowMeans(
      select(
        .,
        Connectionoverload_1_T1,
        Connectionoverload_2_T1,
        Connectionoverload_3_T1,
        Connectionoverload_4_T1,
        Connectionoverload_5_T1
      ),
      na.rm = TRUE
    ),
    t1_twe = rowMeans(
      select(
        .,
        Engagement_1_T1,
        Engagement_2_T1,
        Engagement_3_T1,
        Engagement_4_T1,
        Engagement_5_T1,
        Engagement_6_T1,
        Engagement_7_T1,
        Engagement_8_T1,
        Engagement_9_T1
      ),
      na.rm = TRUE
    ),
    t2_twe = rowMeans(
      select(
        .,
        Engagement_1_T2,
        Engagement_2_T2,
        Engagement_3_T2,
        Engagement_4_T2,
        Engagement_5_T2,
        Engagement_6_T2,
        Engagement_7_T2,
        Engagement_8_T2,
        Engagement_9_T2
      ),
      na.rm = TRUE
    ),
    t1_team_performance = rowMeans(
      select(
        .,
        Performance_1_T1,
        Performance_2_T1,
        Recoded_Performance_3_T1,
        Performance_4_T1
      ),
      na.rm = TRUE
    ),
    t2_team_performance = rowMeans(
      select(
        .,
        Performance_1_T2,
        Performance_2_T2,
        Recoded_Performance_3_T2,
        Performance_4_T2
      ),
      na.rm = TRUE
    ),
    team_tenure = Tenure_in_team_T1
  ) %>%
  select(
    TeamRef_T1,
    TeamRef_T2,
    t1_team_psychological_safety,
    t1_control_over_work_time,
    t1_feeling_disconnected,
    t1_connection_overload,
    t1_twe,
    t2_twe,
    t1_team_performance,
    t2_team_performance,
    team_tenure
  )

model_sample_overview <- tibble(
  n_respondents = nrow(model_data_t1_t2),
  n_t1_teams = n_distinct(model_data_t1_t2$TeamRef_T1),
  n_t2_teams = n_distinct(model_data_t1_t2$TeamRef_T2)
)

reliability_main <- bind_rows(
  calc_alpha_omega(
    df_num_panel,
    c(
      "RecodedPsychologicalsafety_1_T1",
      "Psychological_safety_2_T1",
      "RecodedPsychologicalSafety_3_T1",
      "Psychological_safety_4_T1",
      "RecodedPsychologicalSafety_5_T1",
      "Psychological_safety_6_T1",
      "Psychological_safety_7_T1"
    )
  ) %>% mutate(Variable = "T1 Team psychological safety"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Choice_remote_work_T1",
      "Choice_work_week_T1",
      "Choice_vacations_T1",
      "Choice_in_office_T1",
      "Control_hrs_off_T1"
    )
  ) %>% mutate(Variable = "T1 Control over work time"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Disconnection_1_T1",
      "Disconnection_2_T1",
      "Disconnection_3_T1",
      "Disconnection_4_T1",
      "RecodedDisconnection5_T1",
      "RecodedDisconnection6_T1",
      "Disconnection_7_T1",
      "Disconnection_8_T1",
      "Disconnection_9_T1"
    )
  ) %>% mutate(Variable = "T1 Feeling disconnected"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Connectionoverload_1_T1",
      "Connectionoverload_2_T1",
      "Connectionoverload_3_T1",
      "Connectionoverload_4_T1",
      "Connectionoverload_5_T1"
    )
  ) %>% mutate(Variable = "T1 Connection overload"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Engagement_1_T1",
      "Engagement_2_T1",
      "Engagement_3_T1",
      "Engagement_4_T1",
      "Engagement_5_T1",
      "Engagement_6_T1",
      "Engagement_7_T1",
      "Engagement_8_T1",
      "Engagement_9_T1"
    )
  ) %>% mutate(Variable = "T1 Team work engagement"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Engagement_1_T2",
      "Engagement_2_T2",
      "Engagement_3_T2",
      "Engagement_4_T2",
      "Engagement_5_T2",
      "Engagement_6_T2",
      "Engagement_7_T2",
      "Engagement_8_T2",
      "Engagement_9_T2"
    )
  ) %>% mutate(Variable = "T2 Team work engagement"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Performance_1_T1",
      "Performance_2_T1",
      "Recoded_Performance_3_T1",
      "Performance_4_T1"
    )
  ) %>% mutate(Variable = "T1 Team performance"),
  
  calc_alpha_omega(
    df_num_panel,
    c(
      "Performance_1_T2",
      "Performance_2_T2",
      "Recoded_Performance_3_T2",
      "Performance_4_T2"
    )
  ) %>% mutate(Variable = "T2 Team performance")
) %>%
  mutate(
    Omega = ifelse(Variable == "T2 Team performance", NA, Omega)
  )

descriptives_main <- tibble(
  Variable = c(
    "T1 Team psychological safety",
    "T1 Control over work time",
    "T1 Feeling disconnected",
    "T1 Connection overload",
    "T1 Team work engagement",
    "T2 Team work engagement",
    "T1 Team performance",
    "T2 Team performance",
    "Team tenure"
  ),
  Mean = c(
    fmt_mean(model_data_t1_t2$t1_team_psychological_safety),
    fmt_mean(model_data_t1_t2$t1_control_over_work_time),
    fmt_mean(model_data_t1_t2$t1_feeling_disconnected),
    fmt_mean(model_data_t1_t2$t1_connection_overload),
    fmt_mean(model_data_t1_t2$t1_twe),
    fmt_mean(model_data_t1_t2$t2_twe),
    fmt_mean(model_data_t1_t2$t1_team_performance),
    fmt_mean(model_data_t1_t2$t2_team_performance),
    fmt_mean(model_data_t1_t2$team_tenure)
  ),
  SD = c(
    fmt_sd(model_data_t1_t2$t1_team_psychological_safety),
    fmt_sd(model_data_t1_t2$t1_control_over_work_time),
    fmt_sd(model_data_t1_t2$t1_feeling_disconnected),
    fmt_sd(model_data_t1_t2$t1_connection_overload),
    fmt_sd(model_data_t1_t2$t1_twe),
    fmt_sd(model_data_t1_t2$t2_twe),
    fmt_sd(model_data_t1_t2$t1_team_performance),
    fmt_sd(model_data_t1_t2$t2_team_performance),
    fmt_sd(model_data_t1_t2$team_tenure)
  )
) %>%
  left_join(reliability_main, by = "Variable")

cor_matrix_main <- model_data_t1_t2 %>%
  select(
    t1_team_psychological_safety,
    t1_control_over_work_time,
    t1_feeling_disconnected,
    t1_connection_overload,
    t1_twe,
    t2_twe,
    t1_team_performance,
    t2_team_performance,
    team_tenure
  ) %>%
  cor(use = "pairwise.complete.obs")

cor_matrix_main_round <- round(cor_matrix_main, 2)

table_main_corr <- descriptives_main %>%
  mutate(`#` = 1:n()) %>%
  select(`#`, Variable, Mean, SD, Alpha, Omega) %>%
  bind_cols(
    as_tibble(cor_matrix_main_round, .name_repair = "minimal") %>%
      set_names(as.character(1:9))
  )

print(model_sample_overview)
print(descriptives_main, n = 20, width = Inf)
print(as.data.frame(cor_matrix_main_round))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(model_sample_overview, "output/tables/model_sample_overview_t1_t2.csv")
write_csv(descriptives_main, "output/tables/descriptives_main_t1_t2.csv")
write_csv(as.data.frame(cor_matrix_main_round) %>% rownames_to_column("Variable"),
          "output/tables/correlations_main_t1_t2.csv")
write_csv(table_main_corr, "output/tables/table_main_descriptives_correlations_t1_t2.csv")


# =========================
# CORRELATIONS WITH STARS
# =========================

library(psych)

vars_for_corr <- model_data_t1_t2 %>%
  select(
    t1_team_psychological_safety,
    t1_control_over_work_time,
    t1_feeling_disconnected,
    t1_connection_overload,
    t1_twe,
    t2_twe,
    t1_team_performance,
    t2_team_performance,
    team_tenure
  )

cor_test <- psych::corr.test(
  vars_for_corr,
  use = "pairwise",
  adjust = "none"
)

r_mat <- round(cor_test$r, 2)
p_mat <- cor_test$p

add_stars <- function(r, p) {
  stars <- ifelse(p < .01, "**",
                  ifelse(p < .05, "*", ""))
  paste0(formatC(r, format = "f", digits = 2), stars)
}

cor_with_stars <- matrix(
  mapply(add_stars, r_mat, p_mat),
  nrow = nrow(r_mat)
)

colnames(cor_with_stars) <- colnames(r_mat)
rownames(cor_with_stars) <- rownames(r_mat)

cor_with_stars_df <- as.data.frame(cor_with_stars)

# remove upper triangle (clean table)
cor_with_stars_df[upper.tri(cor_with_stars_df)] <- ""

cor_with_stars_df <- cor_with_stars_df %>%
  rownames_to_column("Variable")

# =========================
# FINAL TABLE BUILD
# =========================

table_main_corr <- descriptives_main %>%
  mutate(`#` = 1:n()) %>%
  select(`#`, Variable, Mean, SD, Alpha, Omega) %>%
  bind_cols(
    cor_with_stars_df %>% select(-Variable)
  )

print(table_main_corr)

write_csv(table_main_corr, "output/tables/table_main_descriptives_correlations_t1_t2_FINAL.csv")
