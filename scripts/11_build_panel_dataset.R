# scripts/11_build_panel_dataset.R
# Canonical T1-T2 (and T1-T3) panel dataset construction for the main
# multilevel analyses of Study 3.
#
# Output objects (saved to data_processed/ — ignored by git):
#   * panel_t1_t2_individual.rds   — long-form individual-level dataset with
#                                    T1 predictors and T2 outcomes for
#                                    teams meeting the >=3-member rule at
#                                    both waves.
#   * panel_t1_t2_team.rds         — team-aggregated counterpart (one row
#                                    per team x wave).
#   * panel_t1_t3_individual.rds   — same structure for the T1-T3 sample
#                                    (sensitivity).
#
# The headline lagged-MLM (scripts/12_*) consumes panel_t1_t2_individual.rds.

source(here::here("R", "utils.R"))
write_session_log("11_build_panel_dataset")

# ---------- Toggle real vs synthetic data ----------
# Real run:        readRDS(here::here("data_raw", "wide_df_raw.rds"))
# CI / smoke run:  readRDS(here::here("scripts", "_smoke_data",
#                                     "wide_df_smoke.rds"))

source_path_real  <- here::here("data_raw", "wide_df_raw.rds")
source_path_smoke <- here::here("scripts", "_smoke_data", "wide_df_smoke.rds")

source_path <- if (file.exists(source_path_real)) source_path_real else source_path_smoke
message("Reading from: ", source_path)
if (!file.exists(source_path)) {
  stop("Neither real nor smoke dataset found. Run scripts/00_import_raw_data.R ",
       "or scripts/_smoke_data/generate_smoke_dataset.R first.")
}
wide_df_raw <- readRDS(source_path)

# ---------- Coerce labelled columns ----------

df_num <- wide_df_raw %>%
  mutate(across(everything(), to_numeric_if_labelled)) %>%
  mutate(
    TeamRef_T1 = clean_team_id(TeamRef_T1),
    TeamRef_T2 = clean_team_id(TeamRef_T2),
    TeamRef_T3 = clean_team_id(TeamRef_T3),
    Tenure_in_team_T1 = readr::parse_number(as.character(Tenure_in_team_T1), na = c("", "NA")),
    Remoteworkingdays_T1_days = Remoteworkingdays_T1 - 1,
    Remoteworkingdays_T2_days = Remoteworkingdays_T2 - 1,
    Remoteworkingdays_T3_days = Remoteworkingdays_T3 - 1
  )

# ---------- Analytic-sample teams (>=3 responding members at the wave) ----------

valid_t1_teams <- get_valid_teams(df_num, "TeamRef_T1")
valid_t2_teams <- get_valid_teams(df_num, "TeamRef_T2")
valid_t3_teams <- get_valid_teams(df_num, "TeamRef_T3")

# ---------- Item-set definitions (single source of truth) ----------

items <- list(
  psych_safety_T1 = c("RecodedPsychologicalsafety_1_T1", "Psychological_safety_2_T1",
                      "RecodedPsychologicalSafety_3_T1", "Psychological_safety_4_T1",
                      "RecodedPsychologicalSafety_5_T1", "Psychological_safety_6_T1",
                      "Psychological_safety_7_T1"),
  control_T1       = c("Choice_remote_work_T1", "Choice_work_week_T1",
                       "Choice_vacations_T1", "Choice_in_office_T1",
                       "Control_hrs_off_T1"),
  disconnected_T1  = c("Disconnection_1_T1", "Disconnection_2_T1", "Disconnection_3_T1",
                       "Disconnection_4_T1", "RecodedDisconnection5_T1",
                       "RecodedDisconnection6_T1", "Disconnection_7_T1",
                       "Disconnection_8_T1", "Disconnection_9_T1"),
  overload_T1      = c("Connectionoverload_1_T1", "Connectionoverload_2_T1",
                       "Connectionoverload_3_T1", "Connectionoverload_4_T1",
                       "Connectionoverload_5_T1"),
  twe_T1           = paste0("Engagement_", 1:9, "_T1"),
  twe_T2           = paste0("Engagement_", 1:9, "_T2"),
  twe_T3           = paste0("Engagement_", 1:9, "_T3"),
  performance_T1   = c("Performance_1_T1", "Performance_2_T1",
                       "Recoded_Performance_3_T1", "Performance_4_T1"),
  performance_T2   = c("Performance_1_T2", "Performance_2_T2",
                       "Recoded_Performance_3_T2", "Performance_4_T2"),
  performance_T3   = c("Performance_1_T3", "Performance_2_T3",
                       "Recoded_Performance_3_T3", "Performance_4_T3")
)

# Composite scoring. rowMeans(..., na.rm = TRUE) preserved here for parity
# with the existing pipeline (see issue #4 for the minimum-items-answered
# discussion; the change ships as a focused PR once that issue is resolved).
add_composites <- function(df, item_lists) {
  for (name in names(item_lists)) {
    df[[name]] <- rowMeans(df[, item_lists[[name]], drop = FALSE], na.rm = TRUE)
  }
  df
}

# ---------- Build T1-T2 individual-level panel ----------

df_panel_t1_t2 <- df_num %>%
  filter(TeamRef_T1 %in% valid_t1_teams,
         TeamRef_T2 %in% valid_t2_teams) %>%
  add_composites(items[c("psych_safety_T1", "control_T1", "disconnected_T1",
                         "overload_T1", "twe_T1", "twe_T2",
                         "performance_T1", "performance_T2")]) %>%
  group_by(TeamRef_T1) %>%
  mutate(team_size_T1 = dplyr::n()) %>%
  ungroup() %>%
  mutate(
    team_tenure_T1 = Tenure_in_team_T1,
    remote_days_T1 = Remoteworkingdays_T1_days
  ) %>%
  select(
    TeamRef_T1, TeamRef_T2,
    psych_safety_T1, control_T1, disconnected_T1, overload_T1,
    twe_T1, twe_T2,
    performance_T1, performance_T2,
    team_size_T1, team_tenure_T1, remote_days_T1
  ) %>%
  filter(!is.na(twe_T2) & !is.na(performance_T2))

# ---------- Team-aggregated counterpart ----------

df_panel_t1_t2_team <- df_panel_t1_t2 %>%
  group_by(TeamRef_T1) %>%
  summarise(
    n_respondents      = dplyr::n(),
    psych_safety_T1    = mean(psych_safety_T1, na.rm = TRUE),
    control_T1         = mean(control_T1, na.rm = TRUE),
    disconnected_T1    = mean(disconnected_T1, na.rm = TRUE),
    overload_T1        = mean(overload_T1, na.rm = TRUE),
    twe_T1             = mean(twe_T1, na.rm = TRUE),
    twe_T2             = mean(twe_T2, na.rm = TRUE),
    performance_T1     = mean(performance_T1, na.rm = TRUE),
    performance_T2     = mean(performance_T2, na.rm = TRUE),
    team_size_T1       = first(team_size_T1),
    team_tenure_T1     = mean(team_tenure_T1, na.rm = TRUE),
    remote_days_T1     = mean(remote_days_T1, na.rm = TRUE),
    .groups            = "drop"
  )

# ---------- T1-T3 panel (sensitivity wave) ----------

df_panel_t1_t3 <- df_num %>%
  filter(TeamRef_T1 %in% valid_t1_teams,
         TeamRef_T3 %in% valid_t3_teams) %>%
  add_composites(items[c("psych_safety_T1", "control_T1", "disconnected_T1",
                         "overload_T1", "twe_T1", "twe_T3",
                         "performance_T1", "performance_T3")]) %>%
  group_by(TeamRef_T1) %>%
  mutate(team_size_T1 = dplyr::n()) %>%
  ungroup() %>%
  mutate(
    team_tenure_T1 = Tenure_in_team_T1,
    remote_days_T1 = Remoteworkingdays_T1_days
  ) %>%
  select(
    TeamRef_T1, TeamRef_T3,
    psych_safety_T1, control_T1, disconnected_T1, overload_T1,
    twe_T1, twe_T3,
    performance_T1, performance_T3,
    team_size_T1, team_tenure_T1, remote_days_T1
  ) %>%
  filter(!is.na(twe_T3) & !is.na(performance_T3))

# ---------- Sample sizes report ----------

sizes <- tibble(
  panel = c("T1-T2 individual", "T1-T2 team", "T1-T3 individual"),
  n_rows = c(nrow(df_panel_t1_t2), nrow(df_panel_t1_t2_team), nrow(df_panel_t1_t3)),
  n_teams = c(n_distinct(df_panel_t1_t2$TeamRef_T1),
              n_distinct(df_panel_t1_t2_team$TeamRef_T1),
              n_distinct(df_panel_t1_t3$TeamRef_T1))
)
print(sizes)

# ---------- Save processed panels ----------

processed_dir <- here::here("data_processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(df_panel_t1_t2,      file.path(processed_dir, "panel_t1_t2_individual.rds"))
saveRDS(df_panel_t1_t2_team, file.path(processed_dir, "panel_t1_t2_team.rds"))
saveRDS(df_panel_t1_t3,      file.path(processed_dir, "panel_t1_t3_individual.rds"))

message("Panels written to: ", processed_dir)
