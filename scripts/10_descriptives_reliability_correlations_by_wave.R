library(tidyverse)
library(haven)
library(psych)

# -------------------------
# LOAD CORRECTED DATASET
# -------------------------

model_data_t1_t2 <- readRDS(
  "data_processed/multilevel_modelling_dataset_t1_t2.rds"
)

# -------------------------
# HELPER FUNCTIONS
# -------------------------

fmt_mean <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  sprintf(
    paste0("%.", digits, "f"),
    mean(x)
  )
}

fmt_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  
  if (length(x) <= 1) {
    return(NA_character_)
  }
  
  sprintf(
    paste0("%.", digits, "f"),
    sd(x)
  )
}

calc_alpha_omega <- function(df, item_cols) {
  
  items <- df %>%
    select(all_of(item_cols)) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  # Note: check.keys = FALSE is intentional. Reverse-coded items
  # (RecodedPsychologicalsafety_1_T*, RecodedPsychologicalSafety_3_T*,
  # RecodedPsychologicalSafety_5_T*, RecodedDisconnection5_T*,
  # RecodedDisconnection6_T*, Recoded_Performance_3_T*) have already
  # been reversed at the SPSS preparation stage. Allowing psych::alpha
  # to auto-reverse would double-reverse these items.
  alpha_out <- tryCatch(
    psych::alpha(
      items,
      warnings = FALSE,
      check.keys = FALSE
    ),
    error = function(e) NULL
  )
  
  omega_out <- tryCatch(
    suppressWarnings(
      suppressMessages(
        psych::omega(
          items,
          plot = FALSE,
          warnings = FALSE
        )
      )
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

# -------------------------
# SAMPLE OVERVIEW
# -------------------------

model_sample_overview <- tibble(
  n_respondents = nrow(model_data_t1_t2),
  
  n_t1_teams = n_distinct(
    model_data_t1_t2$TeamRef_T1
  ),
  
  n_t2_teams = n_distinct(
    model_data_t1_t2$TeamRef_T2
  )
)

print(model_sample_overview)

# Expected:
# n_respondents = 182
# n_t1_teams = 43
# n_t2_teams = 43

# -------------------------
# RELIABILITY ESTIMATES
# -------------------------

reliability_main <- bind_rows(
  
  calc_alpha_omega(
    model_data_t1_t2,
    c(
      "RecodedPsychologicalsafety_1_T1",
      "Psychological_safety_2_T1",
      "RecodedPsychologicalSafety_3_T1",
      "Psychological_safety_4_T1",
      "RecodedPsychologicalSafety_5_T1",
      "Psychological_safety_6_T1",
      "Psychological_safety_7_T1"
    )
  ) %>%
    mutate(
      Variable = "T1 Team psychological safety"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
    c(
      "Choice_remote_work_T1",
      "Choice_work_week_T1",
      "Choice_vacations_T1",
      "Choice_in_office_T1",
      "Control_hrs_off_T1"
    )
  ) %>%
    mutate(
      Variable = "T1 Control over work time"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
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
  ) %>%
    mutate(
      Variable = "T1 Feeling disconnected"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
    c(
      "Connectionoverload_1_T1",
      "Connectionoverload_2_T1",
      "Connectionoverload_3_T1",
      "Connectionoverload_4_T1",
      "Connectionoverload_5_T1"
    )
  ) %>%
    mutate(
      Variable = "T1 Connection overload"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
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
  ) %>%
    mutate(
      Variable = "T1 Team work engagement"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
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
  ) %>%
    mutate(
      Variable = "T2 Team work engagement"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
    c(
      "Performance_1_T1",
      "Performance_2_T1",
      "Recoded_Performance_3_T1",
      "Performance_4_T1"
    )
  ) %>%
    mutate(
      Variable = "T1 Team performance"
    ),
  
  calc_alpha_omega(
    model_data_t1_t2,
    c(
      "Performance_1_T2",
      "Performance_2_T2",
      "Recoded_Performance_3_T2",
      "Performance_4_T2"
    )
  ) %>%
    mutate(
      Variable = "T2 Team performance"
    )
) %>%
  
  # Omega estimation was unstable for T2 performance.
  mutate(
    Omega = ifelse(
      Variable == "T2 Team performance",
      NA,
      Omega
    )
  )

# -------------------------
# DESCRIPTIVE STATISTICS
# -------------------------

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
    fmt_mean(
      model_data_t1_t2$t1_psychological_safety_individual
    ),
    
    fmt_mean(
      model_data_t1_t2$t1_control_over_work_time_individual
    ),
    
    fmt_mean(
      model_data_t1_t2$t1_feeling_disconnected
    ),
    
    fmt_mean(
      model_data_t1_t2$t1_connection_overload
    ),
    
    fmt_mean(
      model_data_t1_t2$t1_twe
    ),
    
    fmt_mean(
      model_data_t1_t2$t2_twe
    ),
    
    fmt_mean(
      model_data_t1_t2$t1_team_performance
    ),
    
    fmt_mean(
      model_data_t1_t2$t2_team_performance
    ),
    
    fmt_mean(
      model_data_t1_t2$team_tenure
    )
  ),
  
  SD = c(
    fmt_sd(
      model_data_t1_t2$t1_psychological_safety_individual
    ),
    
    fmt_sd(
      model_data_t1_t2$t1_control_over_work_time_individual
    ),
    
    fmt_sd(
      model_data_t1_t2$t1_feeling_disconnected
    ),
    
    fmt_sd(
      model_data_t1_t2$t1_connection_overload
    ),
    
    fmt_sd(
      model_data_t1_t2$t1_twe
    ),
    
    fmt_sd(
      model_data_t1_t2$t2_twe
    ),
    
    fmt_sd(
      model_data_t1_t2$t1_team_performance
    ),
    
    fmt_sd(
      model_data_t1_t2$t2_team_performance
    ),
    
    fmt_sd(
      model_data_t1_t2$team_tenure
    )
  )
) %>%
  left_join(
    reliability_main,
    by = "Variable"
  )

# -------------------------
# CORRELATIONS WITH STARS
# -------------------------

vars_for_corr <- model_data_t1_t2 %>%
  select(
    t1_psychological_safety_individual,
    t1_control_over_work_time_individual,
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

r_mat <- round(
  cor_test$r,
  2
)

p_mat <- cor_test$p

add_stars <- function(r, p) {
  
  stars <- ifelse(
    p < .01,
    "**",
    ifelse(
      p < .05,
      "*",
      ""
    )
  )
  
  paste0(
    formatC(
      r,
      format = "f",
      digits = 2
    ),
    stars
  )
}

cor_with_stars <- matrix(
  mapply(
    add_stars,
    r_mat,
    p_mat
  ),
  nrow = nrow(
    r_mat
  )
)

colnames(
  cor_with_stars
) <- colnames(
  r_mat
)

rownames(
  cor_with_stars
) <- rownames(
  r_mat
)

cor_with_stars_df <- as.data.frame(
  cor_with_stars
)

# Remove upper triangle for a cleaner table.
cor_with_stars_df[
  upper.tri(
    cor_with_stars_df
  )
] <- ""

cor_with_stars_df <- cor_with_stars_df %>%
  rownames_to_column(
    "Variable"
  )

# -------------------------
# FINAL TABLE BUILD
# -------------------------

table_main_corr <- descriptives_main %>%
  mutate(
    `#` = 1:n()
  ) %>%
  select(
    `#`,
    Variable,
    Mean,
    SD,
    Alpha,
    Omega
  ) %>%
  bind_cols(
    cor_with_stars_df %>%
      select(
        -Variable
      )
  )

# -------------------------
# PRINT OUTPUTS
# -------------------------

print(
  descriptives_main,
  n = 20,
  width = Inf
)

print(
  table_main_corr,
  n = 20,
  width = Inf
)

# -------------------------
# SAVE OUTPUTS
# -------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  model_sample_overview,
  "output/tables/model_sample_overview_t1_t2_corrected.csv"
)

write_csv(
  descriptives_main,
  "output/tables/descriptives_main_t1_t2_corrected.csv"
)

write_csv(
  as.data.frame(r_mat) %>%
    rownames_to_column("Variable"),
  "output/tables/correlations_main_t1_t2_corrected.csv"
)

write_csv(
  table_main_corr,
  "output/tables/table_main_descriptives_correlations_t1_t2_FINAL_corrected.csv"
)


