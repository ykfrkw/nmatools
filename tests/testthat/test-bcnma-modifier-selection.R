# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: spike-and-slab selection of effect modifiers (review item R19):
#          spec validation, JAGS code, Bayes factor bounds, and recovery of
#          "no modification" (true beta = 0) vs "modification" (beta = 0.5).
# Inputs:  simulate_proficiency_studies() (helper-bcnma-data.R)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting tests (skipped otherwise)

selected_proficiency <- list(column = "proficiency", components = "C", type = "continuous",
                             center = PROFICIENCY_CENTER, selection = "ssvs")

test_that("selection specs are validated; monotone and prior_sd are refused", {
  sim_data <- simulate_proficiency_studies("binary")
  specs <- .normalise_modifier_specs(selected_proficiency, sim_data, SIM_COMPONENTS)
  expect_equal(specs[[1]]$slab_sd, 0.5)
  expect_equal(specs[[1]]$prior_sd, 0.5)
  expect_equal(specs[[1]]$spike_sd, 0.01)
  ordinal <- list(column = "level", components = "C", type = "ordinal",
                  levels = c("low", "mid", "high"), selection = "ssvs", monotone = "increasing")
  expect_error(.normalise_modifier_specs(ordinal, small_bin_data, SIM_COMPONENTS),
               "pushes the posterior away from 0")
  expect_error(.normalise_modifier_specs(modifyList(selected_proficiency, list(prior_sd = 2)),
                                         sim_data, SIM_COMPONENTS), "slab_sd")
  expect_error(.normalise_modifier_specs(modifyList(selected_proficiency, list(spike_sd = 1)),
                                         sim_data, SIM_COMPONENTS), "spike_sd < slab_sd")
})

test_that("ordinal steps of one modifier x component share one indicator", {
  ordinal <- list(column = "level", components = "C", type = "ordinal",
                  levels = c("low", "mid", "high"), selection = "ssvs")
  specs  <- .normalise_modifier_specs(ordinal, small_bin_data, SIM_COMPONENTS)
  design <- suppressMessages(.build_modifier_design(specs, small_bin_data, prepare_small_bin()))
  expect_equal(design$terms$selection_group, c(1L, 1L))
  model_code <- .jags_modifier_block(design$priors, "additive")
  expect_match(model_code, "beta[2] ~ dnorm(0, prec_mod[1])", fixed = TRUE)
  expect_match(model_code, "IndM[1] ~ dbern(0.5)", fixed = TRUE)
  expect_match(model_code, "prec_mod[1] <- IndM[1] * 4 + (1 - IndM[1]) * 10000", fixed = TRUE)
})

test_that("Bayes factors are posterior odds / prior odds, bounded at 0 and 1", {
  expect_equal(.selection_bayes_factor(0.8, 0.5, 1000)$value, 4)
  expect_equal(.selection_bayes_factor(0.5, 0.2, 1000)$value, 4)
  at_zero <- .selection_bayes_factor(0, 0.5, 1000)
  expect_equal(at_zero$bound, "<")
  expect_equal(at_zero$value, (0.0005 / 0.9995))
  expect_equal(.selection_bayes_factor(1, 0.5, 1000)$bound, ">")
})

test_that("selection detects a true modifier and not a null one", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit_selection <- function(true_beta) {
    quietly_fit(bcnma_bin(
      simulate_proficiency_studies("binary", true_beta = true_beta), study, treat, r, n,
      components = SIM_COMPONENTS, interactions = "none", modifiers = selected_proficiency,
      n.chains = SHORT_MCMC$n.chains, n.adapt = SHORT_MCMC$n.adapt,
      n.burnin = SHORT_MCMC$n.burnin, n.iter = SHORT_MCMC$n.iter, seed = SIM_SEED,
      quiet = TRUE))
  }
  fit_null <- fit_selection(true_beta = 0)
  fit_true <- fit_selection(true_beta = TRUE_BETA_BIN)
  null_selection <- fit_null$modifier_selection$additive
  true_selection <- fit_true$modifier_selection$additive
  expect_lt(null_selection$posterior_prob, null_selection$prior_prob)
  expect_gt(true_selection$posterior_prob, 0.9)
  expect_output(print(fit_true), "Lindley")
  expect_match(fit_true$priors$prior[6], "spike-and-slab")
})
