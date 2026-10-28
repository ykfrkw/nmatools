# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: compare_combinations() additions of review item R15: predictive
#          interval for a new study and flags for combinations / pairs that
#          no arm contains.
# Inputs:  small_bin_data; a network where A and D never co-occur (below)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

separate_network <- data.frame(
  study = rep(c("S1", "S2", "S3"), each = 2),
  treat = c("A", "A+B", "D", "B+D", "B", "A"),
  r = c(10, 14, 12, 15, 9, 13), n = 50, stringsAsFactors = FALSE
)

test_that("predictive draws add N(0, tau2), are seeded and keep the RNG state", {
  effect <- rep(0.3, 4000)
  set.seed(11)
  state_before <- .Random.seed
  first  <- .predictive_effect_samples(effect, tau2 = rep(0.04, 4000), seed = 3)
  second <- .predictive_effect_samples(effect, tau2 = rep(0.04, 4000), seed = 3)
  expect_identical(.Random.seed, state_before)
  expect_identical(first, second)
  expect_equal(stats::sd(first), 0.2, tolerance = 0.05)
  expect_null(.predictive_effect_samples(effect, tau2 = NULL, seed = 3))
})

test_that("combinations and pairs absent from every arm are flagged", {
  components <- c("A", "B", "D")
  prep <- .bcnma_bin_prepare_data(separate_network, "study", "treat", "r", "n", components)
  fake_fit <- list(prep = prep, components = components, n_components = 3)
  flags <- .combination_flags(fake_fit, values1 = c(A = 1, B = 0, D = 1),
                              values2 = c(A = 1, B = 1, D = 0))
  expect_equal(flags$is_extrapolated, c(combo1 = TRUE, combo2 = FALSE))
  expect_equal(flags$never_copresent_pairs, "A - D")
})

test_that("compare_combinations reports the predictive interval and notes", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_bin(
    small_bin_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "none",
    n.chains = 1, n.adapt = 100, n.burnin = 100, n.iter = 300, seed = 6, quiet = TRUE))
  expect_output(result <- compare_combinations(fit, c("A", "B", "C"), model = "additive",
                                               exponentiate = FALSE),
                "Predictive interval \\(new study\\)")
  expect_true(result$is_extrapolated[["combo1"]])
  expect_gt(stats::sd(result$prediction_samples), stats::sd(result$effect_samples))
  expect_output(compare_combinations(fit, c("A", "B", "C"), model = "additive"),
                "extrapolated under additivity")
})
