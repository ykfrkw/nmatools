# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: modifier_equivalence() ROPE summary (review item R20) on fixed
#          draws (deterministic) and one short fit.
# Inputs:  synthetic fit lists (below); simulate_proficiency_studies()
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

DRAW_COUNT <- 1000
ROPE_TERMS <- data.frame(
  q = 1:3, label = c("dose", "skill", "skill"), component = c("C", "A", "A"),
  type = c("continuous", "ordinal", "ordinal"), scale = c(2, 1, 1),
  value_min = c(1, NA, NA), value_max = c(5, NA, NA), stringsAsFactors = FALSE
)

# beta[1] = 0.02 per SD (0.01 per unit; difference over 1..5 = 0.04);
# ordinal steps +0.3 and -0.5 (top - bottom = -0.2, largest difference 0.5)
make_rope_fit <- function(beta_values, sm = "OR") {
  samples <- as.data.frame(matrix(rep(beta_values, each = DRAW_COUNT), nrow = DRAW_COUNT))
  names(samples) <- paste0("beta[", seq_along(beta_values), "]")
  list(modifier_terms = ROPE_TERMS, samps_additive = samples, sm = sm)
}

test_that("continuous difference = beta per unit x observed range", {
  fit <- make_rope_fit(c(0.02, 0.3, -0.5))
  capture.output(result <- modifier_equivalence(fit, "dose", margin = 0.1))
  expect_equal(result$difference_median, 0.04)
  expect_equal(result$p_inside, 1)
  expect_equal(result$verdict, "equivalent")
  capture.output(wide <- modifier_equivalence(fit, "dose", margin = 0.1, from = 0, to = 40))
  expect_equal(wide$difference_median, 0.4)
  expect_equal(wide$verdict, "modification (higher at top)")
})

test_that("ordinal difference sums the increments; largest difference reported", {
  fit <- make_rope_fit(c(0.02, 0.3, -0.5))
  expect_output(result <- modifier_equivalence(fit, "skill", margin = 0.1), "Caveat")
  expect_equal(result$difference_median, -0.2)
  expect_equal(result$max_abs_difference_median, 0.5)
  expect_equal(result$verdict, "modification (lower at top)")
  capture.output(by_ratio <- modifier_equivalence(fit, "skill", margin_ratio = 1.5))
  expect_equal(by_ratio$margin, log(1.5))
  expect_equal(by_ratio$verdict, "equivalent")
})

test_that("the margin is required and validated", {
  fit <- make_rope_fit(c(0.02, 0.3, -0.5))
  expect_error(modifier_equivalence(fit, "dose"), "pre-specify")
  expect_error(modifier_equivalence(fit, "dose", margin = 0.1, margin_ratio = 1.2), "not both")
  expect_error(modifier_equivalence(make_rope_fit(c(0, 0, 0), sm = "SMD"), "dose",
                                    margin_ratio = 1.2), "binary outcomes")
  expect_error(modifier_equivalence(fit, "nope", margin = 0.1), "modifier labels")
})

test_that("modifier_equivalence works on a fitted model", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_bin(
    simulate_proficiency_studies("binary"), study, treat, r, n, components = SIM_COMPONENTS,
    interactions = "none",
    modifiers = list(column = "proficiency", components = "C", type = "continuous"),
    n.chains = 1, n.adapt = 200, n.burnin = 200, n.iter = 1000, seed = SIM_SEED,
    quiet = TRUE))
  capture.output(result <- modifier_equivalence(fit, "proficiency", margin_ratio = 1.25))
  # true beta 0.5 per unit over a range of about 4 units: clearly not equivalent
  expect_equal(result$verdict, "modification (higher at top)")
})
