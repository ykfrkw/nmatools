# =============================================================================
# Purpose: worked example of the Bayesian component NMA functions of
#          nmatools (formerly bcnma) on the CBT-I component NMA data:
#          additive and SSVS fits, model checks, comparisons of
#          combinations, reporting aids and an arm-level effect modifier.
# Inputs:  system.file("extdata", "bcnma", "data_CBTICNMA.csv",
#          package = "nmatools") (Furukawa et al. 2024, JAMA Psychiatry;
#          https://github.com/ykfrkw/CBTICNMA)
# Outputs: console output and plots only
# Depends: nmatools, dplyr, ggplot2; JAGS + rjags + MCMCvis (Suggests)
# Usage:   source() it. Set the environment variable BCNMA_EXAMPLE_QUICK=1
#          for a short smoke-test run (results are then NOT usable).
# =============================================================================

library(nmatools)
library(dplyr)
library(ggplot2)

# ---- 0. Settings -------------------------------------------------------------
IS_QUICK_RUN <- nzchar(Sys.getenv("BCNMA_EXAMPLE_QUICK"))
MCMC <- if (IS_QUICK_RUN) {
  list(n.chains = 2, n.adapt = 200, n.burnin = 200, n.iter = 500)
} else {
  list(n.chains = 4, n.adapt = 2000, n.burnin = 5000, n.iter = 20000)
}
SEED <- 42
FULL_CBTI <- c("se", "sd", "cr", "th", "sr", "sc")

# ---- 1. Load data ------------------------------------------------------------
cbti_raw <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
                                 package = "nmatools"))
names(cbti_raw)[1] <- "study"  # fix BOM character in first column name

# One row per arm; aggregate duplicated arms if any
cbti_arms <- cbti_raw %>%
  select(study, treatment_component, r, n) %>%
  group_by(study, treatment_component) %>%
  summarise(r = sum(r), n = sum(n), .groups = "drop")

# 17 components used in the original analysis
components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
                "w",  "ns", "he", "tg", "ind", "gp", "ff", "ae")
# The data also code "dt", which was not a component of the original
# analysis. Tokens that are not components must be declared in `inactive`
# (nmatools >= 0.3.0 stops on unknown tokens instead of ignoring them).
inactive <- "dt"

fit_cbti <- function(interactions, ...) {
  bcnma_bin(
    data = cbti_arms, studlab = study, treat = treatment_component, event = r, n = n,
    components = components, inactive = inactive, interactions = interactions,
    n.chains = MCMC$n.chains, n.adapt = MCMC$n.adapt, n.burnin = MCMC$n.burnin,
    n.iter = MCMC$n.iter, seed = SEED, ...
  )
}

# ---- 2. Fit ------------------------------------------------------------------
# 2a. SSVS: every estimable pair gets a spike-and-slab prior; the additive
#     model is fitted alongside. Non-estimable / aliased pairs are fixed to
#     0 with a warning (see fit_ssvs$identifiability).
fit_ssvs <- fit_cbti("ssvs", inclusion_prob = 0.5, g = 100)
print(fit_ssvs)
summary(fit_ssvs)               # adds the call, MCMC settings and the priors
fit_ssvs$priors

# 2b. SSVS only among clinically plausible interactions
fit_informative <- fit_cbti("ssvs_informative",
                            active_components = c("se", "sd", "cr", "th", "sr", "sc", "re", "ff"),
                            inclusion_prob = 0.8)
print(fit_informative, model = "ssvs")

# The exact JAGS code of each model
bcnma_model_code(fit_ssvs, "additive")
bcnma_model_code(fit_ssvs, "ssvs")

# ---- 3. Visualise ------------------------------------------------------------
forest(fit_ssvs, model = "both")
plot_interactions(fit_ssvs, top_n = 10)
head(summarise_interactions(fit_ssvs, threshold = 0.5), 15)

# ---- 4. Compare combinations -------------------------------------------------
# Each comparison prints the posterior effect, the predictive interval for a
# new study, and notes for combinations / pairs that no arm contains.
compare_combinations(fit_ssvs, combo1 = FULL_CBTI, combo2 = "se")
compare_combinations(fit_ssvs, combo1 = FULL_CBTI, combo2 = character(0))
compare_combinations(fit_ssvs, combo1 = FULL_CBTI, combo2 = "se", model = "additive")

# ---- 5. Model checks and reporting aids -------------------------------------
fit_ssvs$model_fit              # mean residual deviance, pD, DIC per model
fit_ssvs$convergence            # max Rhat / min n.eff per parameter group
additivity <- check_additivity(fit_ssvs)   # vs the standard full-interaction NMA
additivity
coverage <- component_coverage(fit_ssvs)
coverage$components
head(coverage$pairs[order(coverage$pairs$n_studies_contrast), ], 10)

# ---- 6. Raw MCMC access ------------------------------------------------------
sleep_restriction <- match("sr", components)
d_sr <- fit_ssvs$samps_ssvs[[paste0("d[", sleep_restriction, "]")]]
cat("SR vs. no component - OR [95% CrI]:",
    round(quantile(exp(d_sr), c(0.5, 0.025, 0.975)), 2), "\n")
cat("P(SR is beneficial, OR > 1):", round(mean(exp(d_sr) > 1), 3), "\n")

# ---- 7. Arm-level effect modifier --------------------------------------------
# Does the effect of sleep restriction (sr) depend on programme duration?
# duration_weeks is recorded per arm; beta acts on arms containing sr, so
# d[sr] is the effect at the center (6 weeks). beta is identified mainly by
# between-study differences in duration: an observational (ecological)
# association, not a randomised effect.
duration_arms <- cbti_raw %>%
  mutate(duration_weeks = suppressWarnings(as.numeric(duration_weeks))) %>%
  group_by(study) %>%
  filter(!any(is.na(duration_weeks))) %>%
  ungroup() %>%
  select(study, treatment_component, r, n, duration_weeks)

# Studies without a duration in every arm are dropped; report how many.
n_studies_all      <- n_distinct(cbti_raw$study)
n_studies_duration <- n_distinct(duration_arms$study)
cat(sprintf("Modifier analysis: %d of %d studies kept (%d dropped: missing duration).\n",
            n_studies_duration, n_studies_all, n_studies_all - n_studies_duration))

fit_duration <- bcnma_bin(
  data = duration_arms, studlab = study, treat = treatment_component, event = r, n = n,
  components = components, inactive = inactive, interactions = "none",
  modifiers = list(list(column = "duration_weeks", components = "sr",
                        type = "continuous", center = 6, label = "weeks")),
  n.chains = MCMC$n.chains, n.adapt = MCMC$n.adapt, n.burnin = MCMC$n.burnin,
  n.iter = MCMC$n.iter, seed = SEED
)
print(fit_duration)             # includes the effect-modifier table and caveat
fit_duration$modifier_effects$additive

# sr in an 8-week vs. a 4-week programme (both vs. no component)
compare_combinations(fit_duration, combo1 = "sr", model = "additive", at = list(weeks = 8))
compare_combinations(fit_duration, combo1 = "sr", model = "additive", at = list(weeks = 4))
