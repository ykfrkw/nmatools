# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: MCMC convergence summaries and warnings (review item R12).
# Inputs:  synthetic MCMCsummary tables and samples; small_bin_data
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

synthetic_summary <- data.frame(
  Rhat = c(1.00, 1.08, 1.01, NA, NA),
  n.eff = c(5000, 900, 350, 4000, NA),
  row.names = c("d[1]", "d[2]", "tau2", "Ind[1]", "Ind[2]")
)

test_that("worst Rhat and n.eff are reported per parameter group", {
  convergence <- .convergence_by_group(synthetic_summary)
  expect_equal(convergence$group, c("d", "tau2"))
  expect_equal(convergence$max_rhat, c(1.08, 1.01))
  expect_equal(convergence$min_n_eff, c(900, 350))
  expect_warning(.warn_convergence(convergence, "Additive"),
                 "Rhat > 1.05 for d; n.eff < 400 for tau2")
  expect_output(.print_convergence(convergence), "max Rhat 1.080, min n.eff 350")
})

test_that("inclusion probabilities get a Monte Carlo standard error", {
  samples <- data.frame(`Ind[1]` = rep(c(0, 1), each = 50), `Ind[2]` = rep(c(0, 1), 50),
                        check.names = FALSE)
  mcse <- .inclusion_prob_mcse(samples, synthetic_summary, pair_count = 3)
  expect_equal(mcse[1], sqrt(0.5 * 0.5 / 4000))  # n.eff from the summary
  expect_equal(mcse[2], sqrt(0.5 * 0.5 / 100))   # n.eff NA -> number of draws
  expect_true(is.na(mcse[3]))                     # pair not in the model
})

test_that("fits store convergence and print it", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  expect_warning(fit <- muffle_mcmc_warnings_except_convergence(bcnma_bin(
    small_bin_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "none",
    n.chains = 2, n.adapt = 100, n.burnin = 0, n.iter = 30, seed = 2, quiet = TRUE)),
    "Additive model convergence")
  expect_true(all(c("d", "tau2") %in% fit$convergence$additive$group))
  expect_output(print(fit), "Convergence: max Rhat")
})
