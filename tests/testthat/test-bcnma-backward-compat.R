# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# With component_values = NULL and modifiers = NULL, model strings, JAGS data
# and compare_combinations() results must be identical to bcnma 0.1.0.
# Inputs: fixtures/bcnma_backward_compat.rds (see
# fixtures/make_bcnma_backward_compat_fixture.R).

BACKWARD_COMPAT <- readRDS(test_path("fixtures", "bcnma_backward_compat.rds"))

test_that("binary model strings are unchanged without modifiers", {
  expect_identical(.jags_model_additive(4), BACKWARD_COMPAT$model_additive_bin_4)
  expect_identical(.jags_model_ssvs(4, 6), BACKWARD_COMPAT$model_ssvs_bin_4_6)
  expect_identical(.jags_model_ssvs(4, 6, included = c(1, 6), prob_active = 0.8),
                   BACKWARD_COMPAT$model_ssvs_bin_4_6_partial)
})

test_that("continuous model strings are unchanged without modifiers", {
  expect_identical(.jags_model_additive_cont(4), BACKWARD_COMPAT$model_additive_cont_4)
  expect_identical(.jags_model_ssvs_cont(4, 6), BACKWARD_COMPAT$model_ssvs_cont_4_6)
  expect_identical(.jags_model_ssvs_cont(4, 6, included = c(1, 6), prob_active = 0.8, g = 50),
                   BACKWARD_COMPAT$model_ssvs_cont_4_6_partial)
})

test_that("prepared JAGS data are unchanged (no JAGS needed)", {
  prep_bin <- prepare_small_bin(BACKWARD_COMPAT$small_bin_data)
  expect_identical(prep_bin[c("r", "n", "xA", "xB", "na", "Ns", "Nc")],
                   BACKWARD_COMPAT$jags_data_bin)
  prep_cont <- prepare_small_cont(BACKWARD_COMPAT$small_cont_data)
  expect_identical(list(y = prep_cont$y, prec = prep_cont$prec, xA = prep_cont$xA,
                        xB = prep_cont$xB, na = prep_cont$na, Ns = prep_cont$Ns,
                        Nc = prep_cont$Nc),
                   BACKWARD_COMPAT$jags_data_cont)
})

test_that("fitted objects carry unchanged jags_data and no modifier output", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit_bin <- quietly_fit(bcnma_bin(
    BACKWARD_COMPAT$small_bin_data, study, treat, r, n, components = SIM_COMPONENTS,
    interactions = "none", n.chains = TINY_MCMC$n.chains, n.adapt = TINY_MCMC$n.adapt,
    n.burnin = TINY_MCMC$n.burnin, n.iter = TINY_MCMC$n.iter, quiet = TRUE))
  expect_identical(fit_bin$jags_data, BACKWARD_COMPAT$jags_data_bin)
  expect_null(fit_bin$modifier_terms)
  expect_null(fit_bin$modifier_effects)
  expect_null(fit_bin$component_values)

  fit_cont <- quietly_fit(bcnma_cont(
    BACKWARD_COMPAT$small_cont_data, study, treat, mean, sd, n, components = SIM_COMPONENTS,
    interactions = "none", n.chains = TINY_MCMC$n.chains, n.adapt = TINY_MCMC$n.adapt,
    n.burnin = TINY_MCMC$n.burnin, n.iter = TINY_MCMC$n.iter, quiet = TRUE))
  expect_identical(fit_cont$jags_data, BACKWARD_COMPAT$jags_data_cont)
  expect_null(fit_cont$modifier_terms)
})

test_that("compare_combinations reproduces bcnma 0.1.0 for 0/1 input", {
  cases <- BACKWARD_COMPAT$compare_cases
  for (outcome in c("bin", "cont")) {
    fit <- BACKWARD_COMPAT[[paste0("compare_fit_", outcome)]]
    old_results <- BACKWARD_COMPAT[[paste0("compare_results_", outcome)]]
    for (case_index in seq_len(nrow(cases))) {
      case <- cases[case_index, ]
      capture.output(new_result <- compare_combinations(
        fit, BACKWARD_COMPAT$compare_combos[[case$combo]],
        BACKWARD_COMPAT$compare_against[[case$against]], model = case$model))
      old_result <- old_results[[case_index]]
      expect_identical(new_result[names(old_result)], old_result)
    }
  }
})
