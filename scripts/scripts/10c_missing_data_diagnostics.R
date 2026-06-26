
# ============================================================
# SCRIPT 10C: MISSING DATA AND ATTRITION DIAGNOSTICS
# ============================================================
#
# Purpose:
# - Summarise missingness across the raw wide dataset
# - Create dropout indicators for T2 and T3
# - Check whether follow-up non-response is predicted by key T1 variables
#
# Notes:
# - The wide file is anchored on T1 participant rows.
# - Attrition analyses are conducted among respondents in the T1 analytic
#   baseline sample, restricted to T1 teams with at least 3 respondents.
# - Dropout is defined as follow-up non-response at T2/T3, using missing
#   TeamRef_T2 / TeamRef_T3.
# - Blank TeamRef values are coded as empty strings ("") in the raw file,
#   so these are first converted to NA.
# - Engagement-item response indicators are retained as diagnostic checks.
# - T1 composites are calculated using an 80% item-completion rule.
# ============================================================

# install.packages("naniar")

library(tidyverse)
library(haven)
library(naniar)

# -------------------------
# LOAD DATA
# -------------------------

wide_df_raw <- readRDS("data_raw/wide_df_raw.rds")

# -------------------------
# HELPER FUNCTIONS
# -------------------------

to_numeric_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.numeric(haven::zap_labels(x)))
  }
  
  x
}

# Calculates mean composites where at least 80% of items are completed.
# Converts scores to NA where too few items, or no items, are available.

row_mean_na <- function(items, min_prop = .80) {
  items <- as.data.frame(items)
  
  required_items <- ceiling(ncol(items) * min_prop)
  completed_items <- rowSums(!is.na(items))
  
  score <- rowMeans(
    items,
    na.rm = TRUE
  )
  
  score[completed_items < required_items] <- NA_real_
  score[is.nan(score)] <- NA_real_
  
  score
}

# -------------------------
# CLEAN DATA
# -------------------------

df_num <- wide_df_raw %>%
  mutate(
    across(
      everything(),
      to_numeric_if_labelled
    ),
    across(
      c(
        TeamRef_T1,
        TeamRef_T2,
        TeamRef_T3
      ),
      ~ na_if(
        as.character(.),
        ""
      )
    )
  )

# -------------------------
# 1. MISSINGNESS SUMMARY
# -------------------------

missing_summary <- df_num %>%
  summarise(
    across(
      everything(),
      ~ mean(is.na(.))
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Proportion"
  ) %>%
  arrange(
    desc(Missing_Proportion)
  )

print(
  missing_summary,
  n = 50
)

# -------------------------
# VISUALISE MISSINGNESS
# -------------------------

vis_miss(
  df_num
)

# -------------------------
# 2. CREATE DROPOUT VARIABLES
# -------------------------
#
# Primary dropout definition:
# - Dropout is defined as follow-up non-response, indicated by missing
#   TeamRef_T2 / TeamRef_T3 after blank strings have been converted to NA.
#
# Diagnostic comparison:
# - Also calculate whether the respondent has no engagement-item responses
#   at the relevant follow-up wave.

df_attrition <- df_num %>%
  mutate(
    dropout_t2 = as.integer(
      is.na(TeamRef_T2)
    ),
    
    dropout_t3 = as.integer(
      is.na(TeamRef_T3)
    ),
    
    dropout_t2_by_engagement = as.integer(
      rowSums(
        !is.na(
          dplyr::select(
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
          )
        )
      ) == 0
    ),
    
    dropout_t3_by_engagement = as.integer(
      rowSums(
        !is.na(
          dplyr::select(
            .,
            Engagement_1_T3,
            Engagement_2_T3,
            Engagement_3_T3,
            Engagement_4_T3,
            Engagement_5_T3,
            Engagement_6_T3,
            Engagement_7_T3,
            Engagement_8_T3,
            Engagement_9_T3
          )
        )
      ) == 0
    )
  )

# -------------------------
# 3. CREATE T1 COMPOSITES
# -------------------------

df_attrition <- df_attrition %>%
  mutate(
    t1_disconnected = row_mean_na(
      dplyr::select(
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
      )
    ),
    
    t1_overload = row_mean_na(
      dplyr::select(
        .,
        Connectionoverload_1_T1,
        Connectionoverload_2_T1,
        Connectionoverload_3_T1,
        Connectionoverload_4_T1,
        Connectionoverload_5_T1
      )
    ),
    
    t1_twe = row_mean_na(
      dplyr::select(
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
      )
    )
  )

# -------------------------
# 4. APPLY T1 ANALYTIC BASELINE TEAM-SIZE FILTER
# -------------------------
#
# Attrition analyses are conducted among respondents in the T1 analytic
# baseline sample, restricted to T1 teams with at least 3 respondents.
# The T2/T3 3+ team rule is not applied to define dropout, because the
# purpose here is to assess follow-up non-response rather than analytic
# exclusion due to team size.

df_attrition_analytic <- df_attrition %>%
  filter(
    !is.na(TeamRef_T1)
  ) %>%
  group_by(
    TeamRef_T1
  ) %>%
  filter(
    n() >= 3
  ) %>%
  ungroup()

cat(
  "Rows before T1 team-size filter:",
  nrow(df_attrition),
  "\n"
)

cat(
  "Rows after T1 team-size filter:",
  nrow(df_attrition_analytic),
  "\n"
)

cat(
  "Number of T1 teams after filter:",
  n_distinct(df_attrition_analytic$TeamRef_T1),
  "\n"
)

# -------------------------
# 5. CHECK DROPOUT DEFINITIONS IN T1 ANALYTIC BASELINE SAMPLE
# -------------------------

dropout_definition_check_t2 <- table(
  by_teamref = df_attrition_analytic$dropout_t2,
  by_engagement = df_attrition_analytic$dropout_t2_by_engagement
)

dropout_definition_check_t3 <- table(
  by_teamref = df_attrition_analytic$dropout_t3,
  by_engagement = df_attrition_analytic$dropout_t3_by_engagement
)

print(
  dropout_definition_check_t2
)

print(
  dropout_definition_check_t3
)

# -------------------------
# 6. CHECK DROPOUT COUNTS
# -------------------------

dropout_summary <- tibble(
  Wave = c(
    "Time 2",
    "Time 3"
  ),
  
  Retained = c(
    sum(df_attrition_analytic$dropout_t2 == 0, na.rm = TRUE),
    sum(df_attrition_analytic$dropout_t3 == 0, na.rm = TRUE)
  ),
  
  Dropped_out = c(
    sum(df_attrition_analytic$dropout_t2 == 1, na.rm = TRUE),
    sum(df_attrition_analytic$dropout_t3 == 1, na.rm = TRUE)
  ),
  
  Total = c(
    nrow(df_attrition_analytic),
    nrow(df_attrition_analytic)
  ),
  
  Retention_Percentage = c(
    mean(df_attrition_analytic$dropout_t2 == 0, na.rm = TRUE) * 100,
    mean(df_attrition_analytic$dropout_t3 == 0, na.rm = TRUE) * 100
  )
)

print(
  dropout_summary
)

# -------------------------
# 7. ATTRITION TESTS
# -------------------------

attrition_t2_model <- glm(
  dropout_t2 ~
    t1_disconnected +
    t1_overload +
    t1_twe,
  data = df_attrition_analytic,
  family = binomial
)

summary(
  attrition_t2_model
)

attrition_t3_model <- glm(
  dropout_t3 ~
    t1_disconnected +
    t1_overload +
    t1_twe,
  data = df_attrition_analytic,
  family = binomial
)

summary(
  attrition_t3_model
)

# -------------------------
# 8. PRINT ODDS RATIOS
# -------------------------

odds_ratios_t2 <- exp(
  coef(
    attrition_t2_model
  )
)

odds_ratios_t3 <- exp(
  coef(
    attrition_t3_model
  )
)

print(
  odds_ratios_t2
)

print(
  odds_ratios_t3
)

# -------------------------
# 9. SAVE OUTPUTS
# -------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  missing_summary,
  "output/tables/missingness_summary.csv"
)

write_csv(
  dropout_summary,
  "output/tables/dropout_summary.csv"
)

write_csv(
  as.data.frame(dropout_definition_check_t2) %>%
    as_tibble(),
  "output/tables/dropout_definition_check_t2.csv"
)

write_csv(
  as.data.frame(dropout_definition_check_t3) %>%
    as_tibble(),
  "output/tables/dropout_definition_check_t3.csv"
)

write_csv(
  tibble(
    Term = names(odds_ratios_t2),
    Odds_Ratio_T2 = as.numeric(odds_ratios_t2)
  ),
  "output/tables/attrition_odds_ratios_t2.csv"
)

write_csv(
  tibble(
    Term = names(odds_ratios_t3),
    Odds_Ratio_T3 = as.numeric(odds_ratios_t3)
  ),
  "output/tables/attrition_odds_ratios_t3.csv"
)

