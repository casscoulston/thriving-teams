# scripts/10b_descriptives_t1_full_sample.R
# Descriptives, reliability (alpha, omega) and correlations for the full T1
# sample (teams with at least 3 responding members). Serves as a robustness
# check against the reduced T1-T2 panel used in the main analyses (Table S1).

source(here::here("R", "utils.R"))
write_session_log("10b_descriptives_t1_full_sample")

library(psych)

wide_df_raw <- readRDS(here::here("data_raw", "wide_df_raw.rds"))

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(TeamRef_T1 = clean_team_id(TeamRef_T1))

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")

df_t1 <- df_num %>%
  filter(TeamRef_T1 %in% valid_t1_teams)

# -------------------------
# COMPOSITES
# -------------------------

df_t1 <- df_t1 %>%
  mutate(
    psych_safety = rowMeans(select(.,
                                   RecodedPsychologicalsafety_1_T1, Psychological_safety_2_T1,
                                   RecodedPsychologicalSafety_3_T1, Psychological_safety_4_T1,
                                   RecodedPsychologicalSafety_5_T1, Psychological_safety_6_T1,
                                   Psychological_safety_7_T1), na.rm = TRUE),

    control = rowMeans(select(.,
                              Choice_remote_work_T1, Choice_work_week_T1,
                              Choice_vacations_T1, Choice_in_office_T1,
                              Control_hrs_off_T1), na.rm = TRUE),

    disconnected = rowMeans(select(.,
                                   Disconnection_1_T1, Disconnection_2_T1, Disconnection_3_T1,
                                   Disconnection_4_T1, RecodedDisconnection5_T1,
                                   RecodedDisconnection6_T1, Disconnection_7_T1,
                                   Disconnection_8_T1, Disconnection_9_T1), na.rm = TRUE),

    overload = rowMeans(select(.,
                               Connectionoverload_1_T1, Connectionoverload_2_T1,
                               Connectionoverload_3_T1, Connectionoverload_4_T1,
                               Connectionoverload_5_T1), na.rm = TRUE),

    twe = rowMeans(select(., Engagement_1_T1:Engagement_9_T1), na.rm = TRUE),

    performance = rowMeans(select(.,
                                  Performance_1_T1, Performance_2_T1,
                                  Recoded_Performance_3_T1, Performance_4_T1), na.rm = TRUE),

    tenure = Tenure_in_team_T1
  )

# -------------------------
# RELIABILITY
# -------------------------

reliability <- bind_rows(
  calc_alpha_omega(df_t1, c(
    "RecodedPsychologicalsafety_1_T1", "Psychological_safety_2_T1",
    "RecodedPsychologicalSafety_3_T1", "Psychological_safety_4_T1",
    "RecodedPsychologicalSafety_5_T1", "Psychological_safety_6_T1",
    "Psychological_safety_7_T1")) %>% mutate(Variable = "Psych safety"),

  calc_alpha_omega(df_t1, c(
    "Choice_remote_work_T1", "Choice_work_week_T1",
    "Choice_vacations_T1", "Choice_in_office_T1",
    "Control_hrs_off_T1")) %>% mutate(Variable = "Control"),

  calc_alpha_omega(df_t1, c(
    "Disconnection_1_T1", "Disconnection_2_T1", "Disconnection_3_T1",
    "Disconnection_4_T1", "RecodedDisconnection5_T1",
    "RecodedDisconnection6_T1", "Disconnection_7_T1",
    "Disconnection_8_T1", "Disconnection_9_T1")) %>% mutate(Variable = "Disconnected"),

  calc_alpha_omega(df_t1, c(
    "Connectionoverload_1_T1", "Connectionoverload_2_T1",
    "Connectionoverload_3_T1", "Connectionoverload_4_T1",
    "Connectionoverload_5_T1")) %>% mutate(Variable = "Overload"),

  calc_alpha_omega(df_t1, paste0("Engagement_", 1:9, "_T1")) %>% mutate(Variable = "TWE"),

  calc_alpha_omega(df_t1, c(
    "Performance_1_T1", "Performance_2_T1",
    "Recoded_Performance_3_T1", "Performance_4_T1")) %>% mutate(Variable = "Performance")
)

print(reliability)

# -------------------------
# DESCRIPTIVES
# -------------------------

descriptives <- tibble(
  Variable = c("Psych safety", "Control", "Disconnected", "Overload", "TWE", "Performance", "Tenure"),
  Mean = c(
    fmt_mean(df_t1$psych_safety),
    fmt_mean(df_t1$control),
    fmt_mean(df_t1$disconnected),
    fmt_mean(df_t1$overload),
    fmt_mean(df_t1$twe),
    fmt_mean(df_t1$performance),
    fmt_mean(df_t1$tenure)
  ),
  SD = c(
    fmt_sd(df_t1$psych_safety),
    fmt_sd(df_t1$control),
    fmt_sd(df_t1$disconnected),
    fmt_sd(df_t1$overload),
    fmt_sd(df_t1$twe),
    fmt_sd(df_t1$performance),
    fmt_sd(df_t1$tenure)
  )
)

print(descriptives)

# -------------------------
# CORRELATIONS WITH STARS
# -------------------------

vars <- df_t1 %>%
  select(psych_safety, control, disconnected, overload, twe, performance, tenure)

cor_test <- psych::corr.test(vars, use = "pairwise")

r <- round(cor_test$r, 2)
p <- cor_test$p

add_stars <- function(r, p) {
  paste0(sprintf("%.2f", r),
         ifelse(p < .01, "**",
                ifelse(p < .05, "*", "")))
}

cor_mat <- matrix(mapply(add_stars, r, p), nrow = nrow(r))

colnames(cor_mat) <- c("Psych safety", "Control", "Disconnected", "Overload", "TWE", "Performance", "Tenure")
rownames(cor_mat) <- colnames(cor_mat)

cor_mat[upper.tri(cor_mat)] <- ""

print(cor_mat)

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
write_csv(reliability, here::here("output", "tables", "reliability_t1_full_sample.csv"))
write_csv(descriptives, here::here("output", "tables", "descriptives_t1_full_sample.csv"))
write.csv(cor_mat, here::here("output", "tables", "correlations_t1_full_sample.csv"))
