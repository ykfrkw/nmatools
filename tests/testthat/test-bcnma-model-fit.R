# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: residual deviance, pD and DIC (review item R13): the saturated
#          log-likelihood, the per-arm residual deviance derived from
#          rjags::dic.samples(), and a short two-chain fit of each outcome.
# Inputs:  small_bin_data, small_cont_data (helper-bcnma-data.R)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

test_that("saturated log-likelihood handles 0 and n events", {
  prep <- prepare_small_bin(transform(small_bin_data, r = c(0, 12, 50, 9, 14, 18, 11, 16)))
  saturated <- .saturated_loglik(prep)
  expect_equal(saturated[2, 1], 0)   # S2 arm 1: r = 0
  expect_equal(saturated[2, 2], 0)   # S2 arm 2: r = n
  expect_equal(saturated[1, 1], stats::dbinom(12, 50, 12 / 50, log = TRUE))
})

test_that("per-arm residual deviance = node deviance + 2 x saturated log-lik", {
  prep <- prepare_small_bin()
  dic <- list(deviance = c("r[1,1]" = 5, "r[1,2]" = 6), penalty = c("r[1,1]" = 0.5,
                                                                      "r[1,2]" = 0.7))
  arm_deviance <- .arm_residual_deviance(dic, prep)
  expect_equal(arm_deviance$study, c("S1", "S1"))
  expect_equal(arm_deviance$residual_deviance,
               c(5, 6) + 2 * .saturated_loglik(prep)[1, 1:2])
  expect_equal(arm_deviance$pD, c(0.5, 0.7))
})

test_that("fits monitor resdev and report pD and DIC with two chains", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit_bin <- quietly_fit(bcnma_bin(
    small_bin_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "ssvs",
    n.chains = 2, n.adapt = 100, n.burnin = 100, n.iter = 200, seed = 4, quiet = TRUE))
  expect_true("resdev" %in% names(fit_bin$samps_additive))
  expect_equal(fit_bin$model_fit$model, c("additive", "ssvs"))
  expect_equal(fit_bin$model_fit$n_datapoints, c(8, 8))
  expect_false(anyNA(fit_bin$model_fit$DIC))
  expect_equal(nrow(fit_bin$arm_deviance$additive), 8)
  expect_output(print(fit_bin), "Model fit: mean residual deviance")
  fit_cont <- quietly_fit(bcnma_cont(
    small_cont_data, study, treat, mean, sd, n, components = SIM_COMPONENTS,
    interactions = "none", n.chains = 1, n.adapt = 100, n.burnin = 0, n.iter = 50,
    seed = 4, quiet = TRUE))
  expect_true(all(fit_cont$samps_additive$resdev >= 0))
  expect_true(is.na(fit_cont$model_fit$DIC))   # one chain: no DIC
  expect_output(print(fit_cont), "need >= 2 chains")
})
