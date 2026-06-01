install.packages("naniar")

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

# Calculate a mean composite while converting NaN to NA
# where all items are missing.
row_mean_na <- function(items) {
  score <- rowMeans(
    as.data.frame(items),
    na.rm = TRUE
  )
  
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
# Dropout is defined as having no engagement-item responses
# at the relevant follow-up wave.

df_attrition <- df_num %>%
  mutate(
    dropout_t2 = ifelse(
      rowSums(
        !is.na(
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
          )
        )
      ) == 0,
      1,
      0
    ),
    
    dropout_t3 = ifelse(
      rowSums(
        !is.na(
          select(
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
      ) == 0,
      1,
      0
    )
  )

# -------------------------
# 3. CREATE T1 COMPOSITES
# -------------------------

df_attrition <- df_attrition %>%
  mutate(
    t1_disconnected = row_mean_na(
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
      )
    ),
    
    t1_overload = row_mean_na(
      select(
        .,
        Connectionoverload_1_T1,
        Connectionoverload_2_T1,
        Connectionoverload_3_T1,
        Connectionoverload_4_T1,
        Connectionoverload_5_T1
      )
    ),
    
    t1_twe = row_mean_na(
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
      )
    )
  )

# -------------------------
# 4. CHECK DROPOUT COUNTS
# -------------------------

dropout_summary <- tibble(
  Wave = c(
    "Time 2",
    "Time 3"
  ),
  
  Retained = c(
    sum(df_attrition$dropout_t2 == 0, na.rm = TRUE),
    sum(df_attrition$dropout_t3 == 0, na.rm = TRUE)
  ),
  
  Dropped_out = c(
    sum(df_attrition$dropout_t2 == 1, na.rm = TRUE),
    sum(df_attrition$dropout_t3 == 1, na.rm = TRUE)
  )
)

print(
  dropout_summary
)

# -------------------------
# 5. ATTRITION TESTS
# -------------------------

attrition_t2_model <- glm(
  dropout_t2 ~
    t1_disconnected +
    t1_overload +
    t1_twe,
  data = df_attrition,
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
  data = df_attrition,
  family = binomial
)

summary(
  attrition_t3_model
)

# -------------------------
# 6. OPTIONAL: PRINT ODDS RATIOS
# -------------------------

exp(
  coef(
    attrition_t2_model
  )
)

exp(
  coef(
    attrition_t3_model
  )
)

# -------------------------
# 7. SAVE MISSINGNESS SUMMARY
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
