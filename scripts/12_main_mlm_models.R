# scripts/12_main_mlm_models.R
# Headline lagged multilevel models for Study 3.
#
# Specification (per the analytic-plan RFC, issue #13):
#
#   Y_T2 ~ 1 + Y_T1                                  # autoregressive
#                 + psych_safety_T1 + control_T1     # T1 resources
#                 + disconnected_T1 + overload_T1    # T1 demands
#                 + team_size_T1 + remote_days_T1 + team_tenure_T1   # controls
#                 + (1 | TeamRef_T1)                                  # random intercept
#
# Inference: Kenward-Roger small-sample correction via lmerTest + pbkrtest.
# Effect size: marginal and conditional R^2_GLMM via performance::r2().
# Confidence intervals: profile-likelihood.
#
# Outcomes modelled separately:
#   Y in {twe_T2, performance_T2}
#
# Reads:    data_processed/panel_t1_t2_individual.rds
# Writes:   output/tables/mlm_main_t1_t2_<outcome>.csv  (tidied fixed-effects)
#           output/tables/mlm_main_t1_t2_summary.csv    (all outcomes combined)

source(here::here("R", "utils.R"))
write_session_log("12_main_mlm_models")

suppressPackageStartupMessages({
  library(lme4)
  library(lmerTest)
  library(pbkrtest)
  library(broom.mixed)
  library(performance)
})

panel_path <- here::here("data_processed", "panel_t1_t2_individual.rds")
if (!file.exists(panel_path)) {
  stop("Missing ", panel_path, ". Run scripts/11_build_panel_dataset.R first.")
}
panel <- readRDS(panel_path)

# ---------- Pre-modelling: grand-mean centre predictors ----------
# Grand-mean centring makes the intercept interpretable as the outcome for
# an average team at average levels of all predictors. This does not alter
# slopes or their inference; it only re-locates the intercept.

predictors <- c("twe_T1", "performance_T1",
                "psych_safety_T1", "control_T1",
                "disconnected_T1", "overload_T1",
                "team_size_T1", "remote_days_T1", "team_tenure_T1")

panel_c <- panel %>%
  mutate(across(all_of(predictors), ~ as.numeric(scale(.x, center = TRUE, scale = FALSE)),
                .names = "{.col}_c"))

# ---------- Fit headline MLMs ----------

fit_headline_mlm <- function(data, outcome) {
  ar_term <- if (outcome == "twe_T2") "twe_T1_c" else "performance_T1_c"
  rhs <- paste(c(ar_term,
                 "psych_safety_T1_c", "control_T1_c",
                 "disconnected_T1_c", "overload_T1_c",
                 "team_size_T1_c", "remote_days_T1_c", "team_tenure_T1_c",
                 "(1 | TeamRef_T1)"), collapse = " + ")
  f <- as.formula(paste0(outcome, " ~ 1 + ", rhs))
  m <- lmerTest::lmer(f, data = data, REML = TRUE)
  m
}

m_twe   <- fit_headline_mlm(panel_c, "twe_T2")
m_perf  <- fit_headline_mlm(panel_c, "performance_T2")

# ---------- Tidied fixed-effects with KR ----------

tidy_with_kr <- function(model, outcome_label) {
  s <- summary(model, ddf = "Kenward-Roger")
  coefs <- as.data.frame(s$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  ci <- suppressMessages(confint(model, method = "profile", oldNames = FALSE))
  ci <- ci[match(coefs$term, rownames(ci)), , drop = FALSE]
  tibble(
    outcome   = outcome_label,
    term      = coefs$term,
    estimate  = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    df_kr     = coefs[, "df"],
    statistic = coefs[, "t value"],
    p_value   = coefs[, "Pr(>|t|)"],
    ci_lo     = ci[, 1],
    ci_hi     = ci[, 2]
  )
}

fe_twe  <- tidy_with_kr(m_twe,  "T2 Team work engagement")
fe_perf <- tidy_with_kr(m_perf, "T2 Team performance")

# ---------- Variance components, ICC and R^2_GLMM ----------

random_summary <- function(model, outcome_label) {
  vc <- as.data.frame(VarCorr(model))
  tibble(
    outcome   = outcome_label,
    var_team  = vc$vcov[vc$grp == "TeamRef_T1"],
    var_resid = vc$vcov[vc$grp == "Residual"],
    icc       = vc$vcov[vc$grp == "TeamRef_T1"] / sum(vc$vcov),
    r2_marginal    = performance::r2(model)$R2_marginal,
    r2_conditional = performance::r2(model)$R2_conditional
  )
}

rs_twe  <- random_summary(m_twe,  "T2 Team work engagement")
rs_perf <- random_summary(m_perf, "T2 Team performance")

# ---------- Persist tidy outputs ----------

dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(fe_twe,  here::here("output", "tables", "mlm_main_t1_t2_twe.csv"))
write_csv(fe_perf, here::here("output", "tables", "mlm_main_t1_t2_performance.csv"))
write_csv(bind_rows(fe_twe, fe_perf),
          here::here("output", "tables", "mlm_main_t1_t2_summary.csv"))
write_csv(bind_rows(rs_twe, rs_perf),
          here::here("output", "tables", "mlm_main_t1_t2_variance_components.csv"))

cat("\n=== Team work engagement (T2) ===\n")
print(fe_twe, n = 20, width = Inf)
cat("\n=== Team performance (T2) ===\n")
print(fe_perf, n = 20, width = Inf)
cat("\n=== Variance components ===\n")
print(bind_rows(rs_twe, rs_perf), width = Inf)
