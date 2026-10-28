# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# End-to-end tests with JAGS: recovery of a simulated modifier effect for
# binary and continuous outcomes, monotone ordinal increments in the SSVS
# model, component values in a fit, print/summary/compare_combinations output.
# Each fit is a short single chain on 30 two-arm studies (a few seconds).

proficiency_modifier <- list(list(column = "proficiency", components = "C", type = "continuous",
                                  center = PROFICIENCY_CENTER))

test_that("simulated continuous proficiency effect is recovered (binary outcome)", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  sim_data <- simulate_proficiency_studies("binary")
  fit <- quietly_fit(bcnma_bin(
    sim_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "none",
    modifiers = proficiency_modifier,
    n.chains = SHORT_MCMC$n.chains, n.adapt = SHORT_MCMC$n.adapt,
    n.burnin = SHORT_MCMC$n.burnin, n.iter = SHORT_MCMC$n.iter, seed = SIM_SEED, quiet = TRUE))
  effect <- fit$modifier_effects$additive
  expect_equal(nrow(effect), 1)
  expect_true(effect$lower < TRUE_BETA_BIN && TRUE_BETA_BIN < effect$upper)
  expect_true(all(c("ratio_of_OR", "ratio_of_OR_lower", "ratio_of_OR_upper") %in%
                    names(effect)))
  # R10: beta is sampled per covariate SD and reported per original unit too
  proficiency_sd <- fit$modifier_terms$scale
  expect_equal(effect$median_per_sd, effect$median * proficiency_sd)
  expect_equal(effect$n_studies_contrast, 15)   # designs 1 and 3 contain C in one arm
  expect_output(print(fit), "Caveat: beta is identified mainly by between-study variation")
  expect_output(print(fit), "Effect modifiers")
  expect_output(summary(fit), "Modifier terms")
  capture.output(result <- compare_combinations(
    fit, combo1 = c(A = 1, C = 1), combo2 = c(A = 1), model = "additive",
    exponentiate = FALSE, at = list(proficiency = PROFICIENCY_CENTER + 1)))
  expected_shift <- fit$samps_additive[["d[3]"]] +
    fit$samps_additive[["beta[1]"]] / fit$modifier_terms$scale
  expect_equal(result$effect_samples, expected_shift)
  expect_message(capture.output(compare_combinations(fit, "C", model = "additive")), "center")
})

test_that("monotone increasing ordinal modifier yields non-negative increments", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  sim_data <- simulate_proficiency_studies("binary")
  fit <- quietly_fit(bcnma_bin(
    sim_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "ssvs",
    modifiers = list(list(column = "proficiency_level", components = "C", type = "ordinal",
                          levels = c("low", "mid", "high"), monotone = "increasing",
                          label = "skill")),
    n.chains = SHORT_MCMC$n.chains, n.adapt = SHORT_MCMC$n.adapt,
    n.burnin = SHORT_MCMC$n.burnin, n.iter = SHORT_MCMC$n.iter, seed = SIM_SEED, quiet = TRUE))
  increment_columns <- c("beta[1]", "beta[2]")
  expect_true(all(as.matrix(fit$samps_additive[, increment_columns]) >= 0))
  expect_true(all(as.matrix(fit$samps_ssvs[, increment_columns]) >= 0))
  expect_equal(fit$modifier_terms$level_to, c("mid", "high"))
  expect_false(is.null(fit$modifier_effects$ssvs))
})

test_that("simulated proficiency effect is recovered (continuous outcome, SSVS fitted too)", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  sim_data <- simulate_proficiency_studies("continuous")
  fit <- quietly_fit(bcnma_cont(
    sim_data, study, treat, mean, sd, n, components = SIM_COMPONENTS, sm = "MD",
    interactions = "ssvs", modifiers = proficiency_modifier,
    n.chains = SHORT_MCMC$n.chains, n.adapt = SHORT_MCMC$n.adapt,
    n.burnin = SHORT_MCMC$n.burnin, n.iter = SHORT_MCMC$n.iter, seed = SIM_SEED, quiet = TRUE))
  for (effect in fit$modifier_effects) {
    expect_true(effect$lower < TRUE_BETA_CONT && TRUE_BETA_CONT < effect$upper)
    expect_false("ratio_of_OR" %in% names(effect))
  }
  expect_null(fit$jags_data$Q)
  expect_output(print(fit), "change in component MD")
  expect_output(summary(fit), "Modifier terms")
  capture.output(result <- compare_combinations(
    fit, combo1 = "C", model = "ssvs", at = list(proficiency = PROFICIENCY_CENTER - 1)))
  expected <- fit$samps_ssvs[["d[3]"]] - fit$samps_ssvs[["beta[1]"]] / fit$modifier_terms$scale
  expect_equal(result$effect_samples, expected)
  expect_equal(result$sm, "MD")
})

test_that("component values (doses) enter a continuous fit as data", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_cont(
    small_cont_data, study, treat, mean, sd, n, components = SIM_COMPONENTS,
    interactions = "ssvs", component_values = dose_values,
    n.chains = TINY_MCMC$n.chains, n.adapt = TINY_MCMC$n.adapt,
    n.burnin = TINY_MCMC$n.burnin, n.iter = TINY_MCMC$n.iter, quiet = TRUE))
  expect_equal(fit$prep$x_arm[, 2, 3], c(4, 2.5, 7, 0))
  expect_output(print(fit), "Component values: user-supplied")
  capture.output(result <- compare_combinations(fit, c(C = 2), model = "additive"))
  expect_equal(result$effect_samples, 2 * fit$samps_additive[["d[3]"]])
  expect_warning(capture.output(compare_combinations(fit, "C", at = list(x = 1))), "'at' ignored")
})
