# =============================================================================
# Bayesian component NMA (nmatools; formerly bcnma) — worked example using
# CBTICNMA data. Requires JAGS and the suggested packages rjags and MCMCvis.
# Data source: Furukawa et al. (2024), JAMA Psychiatry
#              https://github.com/ykfrkw/CBTICNMA
# =============================================================================

library(nmatools)
library(ggplot2)

# ---- 1. Load data -----------------------------------------------------------
dat_raw <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
                               package = "nmatools"))
names(dat_raw)[1] <- "study"  # fix BOM character in first column name

# Prepare arm-level data: one row per arm, aggregate duplicates if any
library(dplyr)
dat <- dat_raw |>
  dplyr::select(study, treatment_component, r, n) |>
  dplyr::group_by(study, treatment_component) |>
  dplyr::summarise(r = sum(r), n = sum(n), .groups = "drop")

# 17 components used in the original analysis
components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
                "w",  "ns", "he", "tg", "ind", "gp", "ff", "ae")
# The data also code "dt", which was not a component of the original
# analysis. Tokens that are not components must be declared in `inactive`
# (nmatools >= 0.3.0 stops on unknown tokens instead of ignoring them).
inactive <- "dt"

# ---- 2a. Additive model (no interactions) -----------------------------------
fit_add <- bcnma_bin(
  data         = dat,
  studlab      = study,
  treat        = treatment_component,
  event        = r,
  n            = n,
  components   = components,
  inactive     = inactive,
  interactions = "none",
  n.chains     = 3,
  n.adapt      = 2000,
  n.burnin     = 5000,
  n.iter       = 20000,
  seed         = 42
)
print(fit_add)

# ---- 2b. LASSO model (all pairwise interactions, equiprobable) --------------
fit_lasso <- bcnma_bin(
  data         = dat,
  studlab      = study,
  treat        = treatment_component,
  event        = r,
  n            = n,
  components   = components,
  inactive     = inactive,
  interactions = "lasso",
  lasso_prob   = 0.5,
  g            = 100,
  n.chains     = 4,
  n.adapt      = 2000,
  n.burnin     = 5000,
  n.iter       = 20000,
  seed         = 42
)
print(fit_lasso)

# ---- 2c. LASSO informative: only clinically plausible interactions ----------
active <- c("se", "sd", "cr", "th", "sr", "sc", "re", "ff")

fit_inform <- bcnma_bin(
  data              = dat,
  studlab           = study,
  treat             = treatment_component,
  event             = r,
  n                 = n,
  components        = components,
  inactive          = inactive,
  interactions      = "lasso_informative",
  active_components = active,
  lasso_prob        = 0.8,
  g                 = 100,
  n.chains          = 4,
  n.adapt           = 2000,
  n.burnin          = 5000,
  n.iter            = 20000,
  seed              = 42
)
print(fit_inform)

# ---- 3. Visualise -----------------------------------------------------------

# Forest plot: additive vs. LASSO component OR estimates
forest(fit_lasso, model = "both")

# Interaction overview (gamma vs. inclusion probability)
plot_interactions(fit_lasso, top_n = 10)

# Full interaction table (sorted by |effect|)
inter_tbl <- summarise_interactions(fit_lasso, threshold = 0.5)
print(head(inter_tbl, 15))

# ---- 4. Compare combinations -----------------------------------------------

# Full CBT-I package vs. sleep hygiene only (se)
compare_combinations(fit_lasso,
  combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
  combo2 = c("se"))

# Full package vs. no-component baseline
compare_combinations(fit_lasso,
  combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
  combo2 = character(0))

# Same comparison, additive model only (no interaction adjustment)
compare_combinations(fit_lasso,
  combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
  combo2 = c("se"),
  model  = "additive")

# ---- 5. Raw MCMC access -----------------------------------------------------

# Posterior OR for sleep restriction (sr = component 6)
d_sr <- fit_lasso$samps_lasso[["d[6]"]]
cat("SR vs. no component — OR [95% CrI]:",
    round(quantile(exp(d_sr), c(0.5, 0.025, 0.975)), 2), "\n")

# P(SR is beneficial, i.e. OR > 1)
cat("P(SR is beneficial):", round(mean(exp(d_sr) > 1), 3), "\n")

# Which interaction index corresponds to (sd, th)?
k_sdth <- place("sd", "th", Nc = length(components), components = components)
gamma_sdth <- fit_lasso$samps_lasso[[paste0("gamma[", k_sdth, "]")]]
cat("sd:th interaction OR [95% CrI]:",
    round(quantile(exp(gamma_sdth), c(0.5, 0.025, 0.975)), 3), "\n")

# ---- 6. Arm-level effect modifier (nmatools >= 0.3.0 / bcnma >= 0.2.0) ------
# Does the effect of sleep restriction (sr) depend on programme duration?
# duration_weeks is recorded per arm; the term beta * (duration - center) is
# added for arms containing sr, so d[sr] is the effect at the center (6 weeks).
# beta is identified mainly by between-study differences in duration, so read
# it as an observational (ecological) association, not a randomised effect.
dat_dur <- dat_raw |>
  dplyr::mutate(duration_weeks = suppressWarnings(as.numeric(duration_weeks))) |>
  dplyr::group_by(study) |>
  dplyr::filter(!any(is.na(duration_weeks))) |>
  dplyr::ungroup() |>
  dplyr::select(study, treatment_component, r, n, duration_weeks)

fit_dur <- bcnma_bin(
  data         = dat_dur,
  studlab      = study,
  treat        = treatment_component,
  event        = r,
  n            = n,
  components   = components,
  inactive     = inactive,
  interactions = "none",
  modifiers    = list(list(column = "duration_weeks", components = "sr",
                           type = "continuous", center = 6, label = "weeks")),
  seed         = 42
)
print(fit_dur)                  # includes the "Effect modifiers" table
fit_dur$modifier_effects$additive

# sr in an 8-week vs. a 4-week programme (both vs. no component)
compare_combinations(fit_dur, combo1 = "sr", model = "additive", at = list(weeks = 8))
compare_combinations(fit_dur, combo1 = "sr", model = "additive", at = list(weeks = 4))
