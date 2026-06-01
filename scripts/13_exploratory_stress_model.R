# ============================================================
# SCRIPT 13: EXPLORATORY MODERATED-MEDIATION ANALYSIS
# ============================================================
#
# Purpose:
# - Test whether the indirect associations between Level 1
#   hybrid-work demands and perceived team performance through
#   occupational stress vary according to Level 2 resources
#
# Pathway 1:
# Feeling disconnected -> T2 stress -> T2 performance
# moderated by team psychological safety
#
# Pathway 2:
# Connection overload -> T2 stress -> T2 performance
# moderated by shared control over work time

# ============================================================

# -------------------------
# LOAD LIBRARIES
# -------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(modelsummary)
