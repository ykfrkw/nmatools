# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: check_additivity() and the standard (full-interaction) NMA model
#          (review item R14) on a toy network.
# Inputs:  small_bin_data plus a placebo-controlled study (below)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

placebo_network <- rbind(
  small_bin_data[, c("study", "treat", "r", "n")],
  data.frame(study = c("S5", "S5", "S6", "S6"), treat = c("placebo", "A+C", "placebo", "B"),
             r = c(8, 15, 9, 12), n = 50, stringsAsFactors = FALSE)
)

test_that("observed combinations: the empty combination is the reference", {
  prep <- .bcnma_bin_prepare_data(placebo_network, "study", "treat", "r", "n",
                                  SIM_COMPONENTS, inactive = "placebo")
  combinations <- .observed_combinations(prep)
  expect_equal(combinations$labels[1], "(no component)")
  expect_true(combinations$reference_is_inactive)
  expect_setequal(combinations$labels[-1], c("A", "A+C", "B", "B+C", "A+B"))
  s5 <- match("S5", prep$studies)
  expect_equal(combinations$labels[combinations$arm_index[s5, 1:2]],
               c("(no component)", "A+C"))
  expect_equal(combinations$n_studies[combinations$labels == "A+C"], 2)
})

test_that("without an empty arm the most frequent combination is the reference", {
  combinations <- .observed_combinations(prepare_small_bin())
  expect_false(combinations$reference_is_inactive)
  expect_equal(combinations$n_arms[1], max(combinations$n_arms))
})

test_that("check_additivity compares fit, combinations and arm deviance", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_bin(
    placebo_network, study, treat, r, n, components = SIM_COMPONENTS, inactive = "placebo",
    interactions = "none", n.chains = 2, n.adapt = 100, n.burnin = 100, n.iter = 200,
    seed = 5, quiet = TRUE))
  additivity <- quietly_fit(check_additivity(fit))
  expect_s3_class(additivity, "bcnma_additivity")
  expect_equal(additivity$model_fit$model, c("additive", "standard"))
  expect_equal(nrow(additivity$combinations), 5)
  expect_true(is.logical(additivity$combinations$is_discrepant))
  expect_equal(nrow(additivity$arm_deviance), nrow(placebo_network))
  expect_output(print(additivity), "Reference combination: \\(no component\\)")
  expect_output(bcnma_model_code(fit, "standard"), "dcomb[t[i,k]] - dcomb[t[i,1]]",
                fixed = TRUE)
  fit$modifier_terms <- data.frame(q = 1)
  expect_error(check_additivity(fit), "without effect modifiers")
})
