# scripts/_smoke_data/generate_smoke_dataset.R
# Synthetic wide-format dataset matching the schema of the real Thriving
# Teams SPSS file. The purpose is to let the analysis pipeline (panel build,
# MLM, robustness, figures) run end-to-end in CI and on contributor machines
# without the confidential participant data.
#
# IMPORTANT: numbers produced by this dataset are not interpretable as
# results. They are random draws used only to verify the code path.
#
# Usage:
#   source(here::here("scripts", "_smoke_data", "generate_smoke_dataset.R"))
# Output:
#   scripts/_smoke_data/wide_df_smoke.rds  (matches the structure expected
#                                            by scripts/00_import_raw_data.R's
#                                            output, drop in place of
#                                            data_raw/wide_df_raw.rds)

source(here::here("R", "utils.R"))
set.seed(2026)

# ---------- Sample sizes (mirror the real analytic sample) ----------

n_t1_respondents <- 308L
n_t2_respondents <- 210L
n_t3_respondents <- 121L

n_t1_teams <- 58L
n_t2_teams <- 44L
n_t3_teams <- 31L

# Assign respondents to teams so the average team size is plausible
sim_team_assignment <- function(n_resp, n_teams, min_size = 3L, max_size = 12L) {
  base <- sample(rep(seq_len(n_teams), each = min_size))[seq_len(n_resp)]
  remainder <- n_resp - length(base)
  if (remainder > 0) {
    base <- c(base, sample(seq_len(n_teams), remainder, replace = TRUE))
  }
  team_ids <- sprintf("Team_%03d", base)
  # Cap team sizes
  while (any(table(team_ids) > max_size)) {
    over <- which(table(team_ids) > max_size)
    for (t in names(over)) {
      idx <- which(team_ids == t)
      team_ids[sample(idx, 1)] <- sprintf("Team_%03d", sample(seq_len(n_teams), 1))
    }
  }
  team_ids[seq_len(n_resp)]
}

t1_team_ids <- sim_team_assignment(n_t1_respondents, n_t1_teams)

# T2/T3 are subsets of T1 (people who responded at T1 may or may not respond later)
t2_subjects <- sample(seq_len(n_t1_respondents), n_t2_respondents)
t3_subjects <- sample(t2_subjects, n_t3_respondents)

t2_team_ids <- rep(NA_character_, n_t1_respondents)
t3_team_ids <- rep(NA_character_, n_t1_respondents)
t2_team_ids[t2_subjects] <- t1_team_ids[t2_subjects]   # team membership stable
t3_team_ids[t3_subjects] <- t1_team_ids[t3_subjects]

# ---------- Likert item generator ----------

sim_likert_block <- function(n_respondents, n_items, scale_min, scale_max,
                             mean_centre, sd_resp = 1.0, sd_team = 0.6,
                             team_ids, present_mask) {
  team_levels <- unique(na.omit(team_ids))
  team_effects <- setNames(rnorm(length(team_levels), 0, sd_team), team_levels)
  resp_effects <- rnorm(n_respondents, 0, sd_resp)
  resp_team_effect <- ifelse(is.na(team_ids), 0, team_effects[team_ids])

  items <- matrix(NA_real_, nrow = n_respondents, ncol = n_items)
  for (i in seq_len(n_items)) {
    raw <- mean_centre + resp_team_effect + resp_effects + rnorm(n_respondents, 0, 0.8)
    rounded <- round(raw)
    rounded[rounded < scale_min] <- scale_min
    rounded[rounded > scale_max] <- scale_max
    rounded[!present_mask] <- NA_real_
    items[, i] <- rounded
  }
  items
}

build_wave <- function(wave_suffix, team_ids, present_mask, scale_mean_shift = 0) {
  # Engagement (UWES-9), 7-point
  eng <- sim_likert_block(n_t1_respondents, 9, 1, 7, mean_centre = 5 + scale_mean_shift,
                          team_ids = team_ids, present_mask = present_mask)
  colnames(eng) <- paste0("Engagement_", 1:9, wave_suffix)

  # Psychological safety (Edmondson, 7 items), 7-point, items 1/3/5 reverse-coded
  ps <- sim_likert_block(n_t1_respondents, 7, 1, 7, mean_centre = 5 + scale_mean_shift,
                         team_ids = team_ids, present_mask = present_mask)
  ps_names <- c(
    paste0("RecodedPsychologicalsafety_1", wave_suffix),
    paste0("Psychological_safety_2", wave_suffix),
    paste0("RecodedPsychologicalSafety_3", wave_suffix),
    paste0("Psychological_safety_4", wave_suffix),
    paste0("RecodedPsychologicalSafety_5", wave_suffix),
    paste0("Psychological_safety_6", wave_suffix),
    paste0("Psychological_safety_7", wave_suffix)
  )
  colnames(ps) <- ps_names

  # Control over work time (5 items), 5-point
  ctrl <- sim_likert_block(n_t1_respondents, 5, 1, 5, mean_centre = 3 + scale_mean_shift,
                           team_ids = team_ids, present_mask = present_mask)
  colnames(ctrl) <- paste0(c("Choice_remote_work", "Choice_work_week", "Choice_vacations",
                             "Choice_in_office", "Control_hrs_off"), wave_suffix)

  # Feeling disconnected (9 items, 5 and 6 reverse-coded), 7-point
  disc <- sim_likert_block(n_t1_respondents, 9, 1, 7, mean_centre = 3 + scale_mean_shift,
                           team_ids = team_ids, present_mask = present_mask)
  disc_names <- c(
    paste0("Disconnection_", 1:4, wave_suffix),
    paste0("RecodedDisconnection5", wave_suffix),
    paste0("RecodedDisconnection6", wave_suffix),
    paste0("Disconnection_", 7:9, wave_suffix)
  )
  colnames(disc) <- disc_names

  # Connection overload (5 items), 7-point
  ovl <- sim_likert_block(n_t1_respondents, 5, 1, 7, mean_centre = 4 + scale_mean_shift,
                          team_ids = team_ids, present_mask = present_mask)
  colnames(ovl) <- paste0("Connectionoverload_", 1:5, wave_suffix)

  # Team performance (4 items, item 3 reverse-coded), 7-point
  perf <- sim_likert_block(n_t1_respondents, 4, 1, 7, mean_centre = 5 + scale_mean_shift,
                           team_ids = team_ids, present_mask = present_mask)
  perf_names <- c(
    paste0("Performance_1", wave_suffix),
    paste0("Performance_2", wave_suffix),
    paste0("Recoded_Performance_3", wave_suffix),
    paste0("Performance_4", wave_suffix)
  )
  colnames(perf) <- perf_names

  # Demographics / structural
  team_ref <- team_ids
  leader_member <- ifelse(present_mask,
                          sample(c(1, 2), n_t1_respondents, replace = TRUE, prob = c(0.2, 0.8)),
                          NA_real_)
  age <- ifelse(present_mask, round(rnorm(n_t1_respondents, 38, 9)), NA_real_)
  gender <- ifelse(present_mask,
                   sample(c("Female", "Male", "Non-binary", "Prefer not to say"),
                          n_t1_respondents, replace = TRUE,
                          prob = c(0.52, 0.45, 0.02, 0.01)),
                   NA_character_)
  remote_days <- ifelse(present_mask, sample(1:6, n_t1_respondents, replace = TRUE), NA_real_)
  tenure <- ifelse(present_mask, round(rgamma(n_t1_respondents, 2, 0.6), 1), NA_real_)
  industry <- ifelse(present_mask,
                     sample(c("Technology", "Finance", "Healthcare", "Education",
                              "Retail", "Manufacturing", "Public sector", "Consulting"),
                            n_t1_respondents, replace = TRUE),
                     NA_character_)
  geography <- ifelse(present_mask,
                      sample(c("United Kingdom", "United States", "Germany",
                               "France", "Australia", "Canada", "India", "Other"),
                             n_t1_respondents, replace = TRUE,
                             prob = c(0.35, 0.20, 0.10, 0.07, 0.08, 0.08, 0.07, 0.05)),
                      NA_character_)

  structural <- tibble(
    !!paste0("TeamRef", wave_suffix) := team_ref,
    !!paste0("Leader_Team_member", wave_suffix) := leader_member,
    !!paste0("Age", wave_suffix) := age,
    !!paste0("Gender", wave_suffix) := gender,
    !!paste0("Remoteworkingdays", wave_suffix) := remote_days,
    !!paste0("Tenure_in_team", wave_suffix) := tenure,
    !!paste0("Industry", wave_suffix) := industry,
    !!paste0("Geography", wave_suffix) := geography
  )

  bind_cols(structural,
            as_tibble(eng), as_tibble(ps), as_tibble(ctrl),
            as_tibble(disc), as_tibble(ovl), as_tibble(perf))
}

t1_present <- !is.na(t1_team_ids)
t2_present <- !is.na(t2_team_ids)
t3_present <- !is.na(t3_team_ids)

wide_df_smoke <- bind_cols(
  tibble(respondent_id = seq_len(n_t1_respondents)),
  build_wave("_T1", t1_team_ids, t1_present, scale_mean_shift = 0),
  build_wave("_T2", t2_team_ids, t2_present, scale_mean_shift = 0.1),
  build_wave("_T3", t3_team_ids, t3_present, scale_mean_shift = 0.2)
)

cat("Synthetic wide_df dims:", dim(wide_df_smoke), "\n")
cat("T1 teams (>=3):", length(get_valid_teams(wide_df_smoke, "TeamRef_T1")), "\n")
cat("T2 teams (>=3):", length(get_valid_teams(wide_df_smoke, "TeamRef_T2")), "\n")
cat("T3 teams (>=3):", length(get_valid_teams(wide_df_smoke, "TeamRef_T3")), "\n")

dir.create(here::here("scripts", "_smoke_data"), recursive = TRUE, showWarnings = FALSE)
saveRDS(wide_df_smoke, here::here("scripts", "_smoke_data", "wide_df_smoke.rds"))

cat("\nSaved to:", here::here("scripts", "_smoke_data", "wide_df_smoke.rds"), "\n")
cat("To use the smoke fixture instead of real data:\n")
cat("  wide_df_raw <- readRDS(here::here('scripts','_smoke_data','wide_df_smoke.rds'))\n")
