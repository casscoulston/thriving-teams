# scripts/11_multilevel_models_t1_t2.R

# -------------------------
# LOAD LIBRARIES
# -------------------------

library(tidyverse)
library(lme4)
library(lmerTest)

# -------------------------
# LOAD CLEAN DATA
# -------------------------

model_data <- readRDS("data_raw/model_data_t1_t2.rds")

# -------------------------
# FILTER TEAMS >= 3
# -------------------------

model_data <- model_data %>%
  group_by(TeamRef_T1) %>%
  mutate(team_size = n()) %>%
  ungroup() %>%
  filter(team_size >= 3)

# -------------------------
# CENTERING
# -------------------------

# Within-team (group-mean centering)
model_data <- model_data %>%
  group_by(TeamRef_T1) %>%
  mutate(
    disconnected_cwc = t1_feeling_disconnected - mean(t1_feeling_disconnected, na.rm = TRUE),
    overload_cwc = t1_connection_overload - mean(t1_connection_overload, na.rm = TRUE)
  ) %>%
  ungroup()

# Grand-mean centering (team-level)
model_data <- model_data %>%
  mutate(
    psych_safety_cgm = scale(t1_team_psychological_safety, center = TRUE, scale = FALSE)[,1],
    control_cgm = scale(t1_control_over_work_time, center = TRUE, scale = FALSE)[,1]
  )

# -------------------------
# MODEL SET A: T2 TWE
# -------------------------

model_twe_1 <- lmer(
  t2_twe ~ t1_twe + team_tenure + (1 | TeamRef_T1),
  data = model_data
)

model_twe_2 <- lmer(
  t2_twe ~ psych_safety_cgm + control_cgm +
    t1_twe + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

model_twe_3 <- lmer(
  t2_twe ~ psych_safety_cgm + control_cgm +
    disconnected_cwc + overload_cwc +
    t1_twe + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

model_twe_4 <- lmer(
  t2_twe ~ psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_twe + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# -------------------------
# MODEL SET B: T2 PERFORMANCE
# -------------------------

model_perf_1 <- lmer(
  t2_team_performance ~ t1_team_performance + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

model_perf_2 <- lmer(
  t2_team_performance ~ psych_safety_cgm + control_cgm +
    t1_team_performance + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

model_perf_3 <- lmer(
  t2_team_performance ~ psych_safety_cgm + control_cgm +
    disconnected_cwc + overload_cwc +
    t1_team_performance + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

model_perf_4 <- lmer(
  t2_team_performance ~ psych_safety_cgm * disconnected_cwc +
    control_cgm * overload_cwc +
    t1_team_performance + team_tenure +
    (1 | TeamRef_T1),
  data = model_data
)

# -------------------------
# OUTPUT 
# -------------------------

summary(model_twe_1)

summary(model_twe_2)
summary(model_twe_3)

summary(model_twe_4)


summary(model_perf_1)
summary(model_perf_2)
summary(model_perf_3)
summary(model_perf_4)

model_twe_no_baseline <- lmer(
  t2_twe ~ psych_safety_cgm + control_cgm +
    disconnected_cwc + overload_cwc +
    team_tenure + (1 | TeamRef_T1),
  data = model_data
)

summary(model_twe_no_baseline)

model_perf_no_baseline <- lmer(
  t2_team_performance ~ psych_safety_cgm + control_cgm +
    disconnected_cwc + overload_cwc +
    team_tenure + (1 | TeamRef_T1),
  data = model_data
)

summary(model_perf_no_baseline)


# =========================
# SAVE MODELS
# =========================

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)

saveRDS(model_twe_1, "output/models/model_twe_1.rds")
saveRDS(model_twe_2, "output/models/model_twe_2.rds")
saveRDS(model_twe_3, "output/models/model_twe_3.rds")
saveRDS(model_twe_4, "output/models/model_twe_4.rds")

saveRDS(model_perf_1, "output/models/model_perf_1.rds")
saveRDS(model_perf_2, "output/models/model_perf_2.rds")
saveRDS(model_perf_3, "output/models/model_perf_3.rds")
saveRDS(model_perf_4, "output/models/model_perf_4.rds")

saveRDS(model_twe_no_baseline, "output/models/model_twe_no_baseline.rds")
saveRDS(model_perf_no_baseline, "output/models/model_perf_no_baseline.rds")

library(sjPlot)

install.packages("sjPlot")
library(sjPlot)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

tab_model(model_twe_1, model_twe_2, model_twe_3, model_twe_4,
          file = "output/tables/table_twe_models.html")

tab_model(model_perf_1, model_perf_2, model_perf_3, model_perf_4,
          file = "output/tables/table_performance_models.html")

tab_model(model_twe_no_baseline, model_perf_no_baseline,
          file = "output/tables/table_robustness_models.html")

library(ggeffects)
library(ggplot2)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

pred_overload <- ggpredict(model_perf_4, terms = "overload_cwc")

plot(pred_overload)

ggsave("output/figures/fig_overload.png", width = 6, height = 4)


pred_disconnect <- ggpredict(model_perf_4, terms = "disconnected_cwc")

plot(pred_disconnect)

ggsave("output/figures/fig_disconnection.png", width = 6, height = 4)

library(sjPlot)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
tab_model(
  model_twe_1,
  model_twe_2,
  model_twe_3,
  model_twe_4,
  show.se = TRUE,
  show.p = TRUE,
  show.ci = FALSE,
  dv.labels = c("M1", "M2", "M3", "M4"),
  file = "output/tables/table_twe_full_models.html"
)

tab_model(
  model_perf_1,
  model_perf_2,
  model_perf_3,
  model_perf_4,
  show.se = TRUE,
  show.p = TRUE,
  show.ci = FALSE,
  dv.labels = c("M1", "M2", "M3", "M4"),
  file = "output/tables/table_performance_full_models.html"
)

# =========================
# SAVE CLEAN DATA FOR OTHER SCRIPTS
# =========================

dir.create("data_processed", showWarnings = FALSE)

saveRDS(model_data, "data_processed/model_data.rds")
