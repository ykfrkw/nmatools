# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: reporting aids of review item R17: component_coverage() and the
#          table of priors stored in the fit.
# Inputs:  small_bin_data (helper-bcnma-data.R)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

test_that("prior table lists every prior with its source", {
  settings <- list(effect_prior_sd = 10, baseline_prior_sd = 10,
                   tau_prior = .default_tau_prior_bin(),
                   ssvs = list(inclusion_prob = 0.5, slab_ratio = 100, eta_prior_sd = 0.03))
  terms <- data.frame(q = 1, term = "x:C", type = "continuous", prior_sd = 1, monotone = "none")
  priors <- .prior_table(settings, terms, "log-OR")
  expect_equal(nrow(priors), 6)
  expect_match(priors$prior[1], "normal(0, SD 10) on the log-OR scale", fixed = TRUE)
  expect_match(priors$source[3], "not from a verified published category")
  expect_match(priors$prior[6], "per covariate SD")
})

test_that("component_coverage counts studies, arms and contrasts", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_bin(
    small_bin_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "none",
    n.chains = 1, n.adapt = 100, n.burnin = 0, n.iter = 20, seed = 7, quiet = TRUE))
  coverage <- component_coverage(fit)
  # A: S1 arm 1, S2 arms 1-2, S4 arm 2 -> 3 studies, 4 arms, contrast in S1 and S4
  expect_equal(unlist(coverage$components[1, -1]),
               c(n_studies = 3, n_arms = 4, n_studies_contrast = 2))
  expect_equal(coverage$pairs$component_pair, c("A - B", "A - C", "B - C"))
  expect_equal(coverage$pairs$n_arms, c(1, 1, 2))
  expect_equal(nrow(fit$priors), 5)
  expect_output(summary(fit), "Priors:")
})
