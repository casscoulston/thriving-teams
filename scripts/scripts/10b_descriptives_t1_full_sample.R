library(tidyverse)
library(haven)
library(psych)

# -------------------------
# LOAD DATA
# -------------------------

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

# -------------------------
# HELPERS
# -------------------------

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

fmt_mean <- function(x) {
  sprintf("%.2f", mean(x, na.rm = TRUE))
}

fmt_sd <- function(x) {
  sprintf("%.2f", sd(x, na.rm = TRUE))
}

calc_alpha_omega <- function(df, items) {
  items_df <- df %>%
    select(all_of(items)) %>%
    mutate(across(everything(), as.numeric))
  
  alpha_out <- tryCatch(psych::alpha(items_df), error = function(e) NULL)
  omega_out <- tryCatch(psych::omega(items_df, plot = FALSE), error = function(e) NULL)
  
  tibble(
    Alpha = ifelse(is.null(alpha_out), NA, alpha_out$total$raw_alpha),
    Omega = ifelse(is.null(omega_out), NA, omega_out$omega.tot)
  )
}

# -------------------------
# CLEAN DATA
# -------------------------

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(TeamRef_T1 = clean_team_id(TeamRef_T1))

# -------------------------
# VALID TEAMS (>=3)
# -------------------------

get_valid_teams <- function(df, team_col) {
  df %>%
    filter(!is.na(.data[[team_col]])) %>%
    count(.data[[team_col]]) %>%
    filter(n >= 3) %>%
    pull(1)
}

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
    "RecodedPsychologicalsafety_1_T1","Psychological_safety_2_T1",
    "RecodedPsychologicalSafety_3_T1","Psychological_safety_4_T1",
    "RecodedPsychologicalSafety_5_T1","Psychological_safety_6_T1",
    "Psychological_safety_7_T1")) %>% mutate(Variable = "Psych safety"),
  
  calc_alpha_omega(df_t1, c(
    "Choice_remote_work_T1","Choice_work_week_T1",
    "Choice_vacations_T1","Choice_in_office_T1",
    "Control_hrs_off_T1")) %>% mutate(Variable = "Control"),
  
  calc_alpha_omega(df_t1, c(
    "Disconnection_1_T1","Disconnection_2_T1","Disconnection_3_T1",
    "Disconnection_4_T1","RecodedDisconnection5_T1",
    "RecodedDisconnection6_T1","Disconnection_7_T1",
    "Disconnection_8_T1","Disconnection_9_T1")) %>% mutate(Variable = "Disconnected"),
  
  calc_alpha_omega(df_t1, c(
    "Connectionoverload_1_T1","Connectionoverload_2_T1",
    "Connectionoverload_3_T1","Connectionoverload_4_T1",
    "Connectionoverload_5_T1")) %>% mutate(Variable = "Overload"),
  
  calc_alpha_omega(df_t1, paste0("Engagement_",1:9,"_T1")) %>% mutate(Variable = "TWE"),
  
  calc_alpha_omega(df_t1, c(
    "Performance_1_T1","Performance_2_T1",
    "Recoded_Performance_3_T1","Performance_4_T1")) %>% mutate(Variable = "Performance")
)

print(reliability)

# -------------------------
# DESCRIPTIVES
# -------------------------

descriptives <- tibble(
  Variable = c("Psych safety","Control","Disconnected","Overload","TWE","Performance","Tenure"),
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

colnames(cor_mat) <- c("Psych safety","Control","Disconnected","Overload","TWE","Performance","Tenure")
rownames(cor_mat) <- colnames(cor_mat)

cor_mat[upper.tri(cor_mat)] <- ""

print(cor_mat)
