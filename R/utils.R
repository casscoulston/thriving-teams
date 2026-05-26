# =============================================================================
# R/utils.R — shared helpers for the thriving-teams analysis pipeline
# =============================================================================
# This file consolidates helper functions that were previously redefined at the
# top of every analysis script in scripts/. Each helper preserves the exact
# behaviour of the originals — this is a pure refactor, not a behaviour change.
#
# Where a helper has an open methodological discussion attached to it, an
# inline `# TODO: see issue #N` comment points to the relevant GitHub issue.
# Any change to a helper's behaviour will ship as a dedicated, separately
# reviewed PR once the issue is resolved.
#
# Usage:
#   source(here::here("R", "utils.R"))
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

# -----------------------------------------------------------------------------
# Variable-type coercion for SPSS-imported (`haven_labelled`) columns
# -----------------------------------------------------------------------------

to_numeric_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.numeric(haven::zap_labels(x)))
  }
  x
}

to_character_if_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(as.character(haven::as_factor(x)))
  }
  x
}

# -----------------------------------------------------------------------------
# Cleaning team identifiers
# -----------------------------------------------------------------------------
# TeamRef columns sometimes contain trailing whitespace, empty strings, the
# literal text "NA" or "NaN" — normalise them all to NA_character_.

clean_team_id <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

# -----------------------------------------------------------------------------
# Sample formatters for descriptive tables
# -----------------------------------------------------------------------------

fmt_mean_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
}

fmt_mean <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), mean(x))
}

fmt_sd <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), sd(x))
}

fmt_range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  paste0(min(x), "–", max(x))
}

pct_level <- function(x, level) {
  denom <- sum(!is.na(x) & x != "")
  if (denom == 0) return(NA_real_)
  100 * sum(x == level, na.rm = TRUE) / denom
}

count_industries <- function(x) {
  dplyr::n_distinct(x[!is.na(x) & x != ""])
}

# -----------------------------------------------------------------------------
# Safe inferential helpers — return NA rather than erroring on edge cases
# -----------------------------------------------------------------------------

safe_t_test <- function(x, g) {
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]
  g <- g[keep]
  if (length(unique(g)) != 2 || length(x) < 3) {
    return(tibble::tibble(statistic = NA_real_, p_value = NA_real_))
  }
  out <- tryCatch(t.test(x ~ g), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble::tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tibble::tibble(statistic = unname(out$statistic), p_value = out$p.value)
}

safe_chisq <- function(x, g) {
  keep <- !is.na(x) & !is.na(g) & x != ""
  x <- x[keep]
  g <- g[keep]
  if (length(unique(g)) != 2 || length(unique(x)) < 2) {
    return(tibble::tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tab <- table(x, g)
  out <- tryCatch(chisq.test(tab), error = function(e) NULL)
  if (is.null(out)) {
    return(tibble::tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tibble::tibble(statistic = unname(out$statistic), p_value = out$p.value)
}

# -----------------------------------------------------------------------------
# Analytic-sample membership: teams with at least three responding members
# -----------------------------------------------------------------------------

get_valid_teams <- function(df, team_col, min_size = 3) {
  df %>%
    dplyr::filter(!is.na(.data[[team_col]])) %>%
    dplyr::count(.data[[team_col]], name = "team_size") %>%
    dplyr::filter(team_size >= min_size) %>%
    dplyr::pull(1)
}

# -----------------------------------------------------------------------------
# Within-group reliability and agreement indices
# -----------------------------------------------------------------------------

# TODO (issue #3): ICC(2) currently uses (MS_B - MS_W) / MS_B, which assumes
# equal team size. For unbalanced teams the harmonic-mean k form is more
# defensible. Behaviour is unchanged in this refactor PR; the fix will land
# as a focused follow-up once the issue is resolved.
calc_icc <- function(df, team_col, score_col) {
  d <- df %>%
    dplyr::select(team = dplyr::all_of(team_col), score = dplyr::all_of(score_col)) %>%
    dplyr::filter(!is.na(team), !is.na(score))

  if (nrow(d) < 3 || dplyr::n_distinct(d$team) < 2) {
    return(tibble::tibble(ICC1 = NA_real_, ICC2 = NA_real_))
  }

  aov_obj <- aov(score ~ team, data = d)
  aov_tab <- summary(aov_obj)[[1]]

  ms_between <- aov_tab["team", "Mean Sq"]
  ms_within <- aov_tab["Residuals", "Mean Sq"]

  k <- d %>%
    dplyr::count(team) %>%
    dplyr::summarise(mean_n = mean(n), .groups = "drop") %>%
    dplyr::pull(mean_n)

  icc1 <- (ms_between - ms_within) / (ms_between + (k - 1) * ms_within)
  icc2 <- (ms_between - ms_within) / ms_between

  tibble::tibble(ICC1 = icc1, ICC2 = icc2)
}

# TODO (issue #7): rwg(j) currently uses a uniform null distribution only.
# Multiple-null reporting (slight skew, moderate skew) and AD(M) will be
# added as a focused follow-up once the issue is resolved.
calc_rwg <- function(df, team_col, item_cols, scale_min = 1, scale_max = 7) {
  d <- df %>%
    dplyr::select(dplyr::all_of(team_col), dplyr::all_of(item_cols)) %>%
    dplyr::filter(!is.na(.data[[team_col]]))

  expected_var <- ((scale_max - scale_min + 1)^2 - 1) / 12

  team_rwgs <- d %>%
    dplyr::group_by(.data[[team_col]]) %>%
    dplyr::group_modify(~ {
      item_vars <- .x %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ var(.x, na.rm = TRUE))) %>%
        unlist(use.names = FALSE)

      item_rwgs <- 1 - (item_vars / expected_var)
      item_rwgs <- item_rwgs[is.finite(item_rwgs)]

      tibble::tibble(
        rwg_j = ifelse(length(item_rwgs) == 0, NA_real_, mean(item_rwgs, na.rm = TRUE))
      )
    }) %>%
    dplyr::ungroup()

  tibble::tibble(
    Median_rwgj = median(team_rwgs$rwg_j, na.rm = TRUE),
    Mean_rwgj = mean(team_rwgs$rwg_j, na.rm = TRUE)
  )
}

# -----------------------------------------------------------------------------
# Reliability: Cronbach's alpha and McDonald's omega
# -----------------------------------------------------------------------------
# Note: check.keys = FALSE is intentional. Reverse-coded items (e.g.
# RecodedPsychologicalsafety_1_T1, RecodedPsychologicalSafety_3_T1,
# RecodedPsychologicalSafety_5_T1, RecodedDisconnection5_T1,
# RecodedDisconnection6_T1, Recoded_Performance_3_T1) have already been
# reversed at the SPSS preparation stage. Allowing psych::alpha() to
# auto-reverse would double-reverse those items.
#
# TODO (issue #6): consider semTools::compRelSEM() for unidimensional omega.

calc_alpha_omega <- function(df, item_cols) {
  items <- df %>%
    dplyr::select(dplyr::all_of(item_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  alpha_out <- tryCatch(
    psych::alpha(items, warnings = FALSE, check.keys = FALSE),
    error = function(e) NULL
  )

  omega_out <- tryCatch(
    suppressMessages(psych::omega(items, plot = FALSE, warnings = FALSE)),
    error = function(e) NULL
  )

  tibble::tibble(
    Alpha = if (is.null(alpha_out)) NA_real_ else alpha_out$total$raw_alpha,
    Omega = if (is.null(omega_out)) NA_real_ else omega_out$omega.tot
  )
}

# -----------------------------------------------------------------------------
# Session info capture — call at the top of each analysis script
# -----------------------------------------------------------------------------

write_session_log <- function(script_name) {
  log_dir <- here::here("output", "logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_path <- file.path(log_dir, paste0(stamp, "_", script_name, ".log"))
  con <- file(log_path, open = "wt")
  on.exit(close(con))
  cat("# Session info for:", script_name, "\n", file = con)
  cat("# Run at:", format(Sys.time(), usetz = TRUE), "\n\n", file = con)
  utils::capture.output(sessionInfo(), file = con, append = TRUE)
  invisible(log_path)
}
