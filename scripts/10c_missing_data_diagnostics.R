# scripts/10c_missing_data_diagnostics.R
# Missing data patterns and attrition prior to multilevel modelling. Computes
# overall missingness, visualises missing data patterns (naniar) and conducts
# logistic regression analyses to test whether dropout at T2 and T3 is
# associated with baseline variables. Results support an MAR assumption and
# justify the use of FIML in subsequent analyses.

source(here::here("R", "utils.R"))
write_session_log("10c_missing_data_diagnostics")

library(naniar)

wide_df_raw <- readRDS(here::here("data_raw", "wide_df_raw.rds"))

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled))

# -------------------------
# 1. MISSINGNESS SUMMARY
# -------------------------

missing_summary <- df_num %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Proportion") %>%
  arrange(desc(Missing_Proportion))

print(missing_summary)

# -------------------------
# VISUALISE MISSINGNESS
# -------------------------

vis_miss_plot <- vis_miss(df_num)
print(vis_miss_plot)

dir.create(here::here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
ggsave(here::here("output", "figures", "missingness_vis_miss.png"),
       vis_miss_plot, width = 10, height = 6, dpi = 200)

# -------------------------
# 2. CREATE DROPOUT VARIABLES
# -------------------------

df_attrition <- df_num %>%
  mutate(
    dropout_t2 = ifelse(is.na(Engagement_1_T2), 1, 0),
    dropout_t3 = ifelse(is.na(Engagement_1_T3), 1, 0)
  )

# -------------------------
# 3. CREATE T1 COMPOSITES
# -------------------------

df_attrition <- df_attrition %>%
  mutate(
    t1_disconnected = rowMeans(select(., starts_with("Disconnection_") & ends_with("_T1")), na.rm = TRUE),
    t1_overload = rowMeans(select(., starts_with("Connectionoverload_") & ends_with("_T1")), na.rm = TRUE),
    t1_twe = rowMeans(select(., starts_with("Engagement_") & ends_with("_T1")), na.rm = TRUE)
  )

# -------------------------
# 4. ATTRITION TESTS (LOGISTIC REGRESSION)
# -------------------------

attrition_t2_model <- glm(
  dropout_t2 ~ t1_disconnected + t1_overload + t1_twe,
  data = df_attrition,
  family = binomial
)

summary(attrition_t2_model)

attrition_t3_model <- glm(
  dropout_t3 ~ t1_disconnected + t1_overload + t1_twe,
  data = df_attrition,
  family = binomial
)

summary(attrition_t3_model)
