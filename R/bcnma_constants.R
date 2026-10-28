# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: default priors, MCMC settings and thresholds of the Bayesian
#          component NMA functions (bcnma_bin(), bcnma_cont()), collected in
#          one place so that every default is named and documented once.
# Inputs:  none
# Outputs: package-internal constants (BCNMA_*)
# Depends: nothing; used by every R/bcnma_*.R file

# ---- Priors on location parameters -----------------------------------------
# Binary outcomes: d[j] and the study baselines u[i] ~ N(0, 10^2) on the
# log-odds scale (precision 0.01, as in NICE DSU TSD2).
BCNMA_EFFECT_PRIOR_SD_BIN   <- 10
BCNMA_BASELINE_PRIOR_SD_BIN <- 10
# Continuous outcomes on the SMD scale: N(0, 1000) (precision 0.001).
BCNMA_EFFECT_PRIOR_SD_SMD   <- sqrt(1000)
BCNMA_BASELINE_PRIOR_SD_SMD <- sqrt(1000)

# Continuous outcomes on the MD scale: priors scale with the outcome, in
# multiples of the median (over studies) within-study pooled SD.
BCNMA_EFFECT_PRIOR_SD_PER_SD_MD   <- 10
BCNMA_BASELINE_PRIOR_SD_PER_SD_MD <- 100

# ---- Heterogeneity priors --------------------------------------------------
# c(meanlog, precision) of a log-normal prior on the variance tau^2.
BCNMA_TAU2_PRIOR_BIN <- c(-1.67, 1 / 1.472^2)
BCNMA_TAU2_PRIOR_SMD <- c(-1.0, 1 / 0.8^2)
# MD: half-normal prior on the SD tau with SD = this x median pooled SD.
BCNMA_TAU_PRIOR_SD_PER_SD_MD <- 1
BCNMA_TAU_PRIOR_FAMILIES <- c("lognormal_tau2", "half_normal_tau")

# ---- SSVS (spike-and-slab) interaction priors -----------------------------
BCNMA_DEFAULT_INCLUSION_PROB <- 0.5
# Slab variance = g x spike variance.
BCNMA_DEFAULT_SLAB_RATIO     <- 100
# eta (spike SD) ~ half-normal with this SD (precision 1000).
BCNMA_DEFAULT_ETA_PRIOR_SD   <- 1 / sqrt(1000)

# ---- MCMC ------------------------------------------------------------------
# JAGS RNG used for every chain; the seed is offset by the chain index.
BCNMA_RNG_NAME <- "base::Mersenne-Twister"
# Upper bound for a seed drawn when seed = NULL (leaves room for the offsets).
BCNMA_MAX_SEED <- 1e8
# Optional (Suggests) packages needed to fit a model.
BCNMA_MCMC_PACKAGES <- c("rjags", "MCMCvis")

# ---- Numerical tolerances --------------------------------------------------
BCNMA_ZERO_TOLERANCE <- 1e-12
