# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: check that `seed` makes bcnma fits reproducible (review item R1):
#          per-chain JAGS RNG initial values are built from the seed, stored
#          in the fit, and identical seeds give identical posterior samples.
# Inputs:  small_bin_data (helper-bcnma-data.R)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

test_that("per-chain inits fix the JAGS RNG with seed + chain index", {
  inits <- .build_chain_inits(seed = 7L, n_chains = 3)
  expect_length(inits, 3)
  expect_equal(vapply(inits, function(chain) chain$.RNG.seed, numeric(1)), 8:10)
  expect_true(all(vapply(inits, function(chain) chain$.RNG.name, character(1)) ==
                    "base::Mersenne-Twister"))
  expect_equal(.resolve_seed(12), 12L)
  expect_true(is.integer(.resolve_seed(NULL)))
  expect_error(.resolve_seed("a"), "single integer")
})

test_that("identical seeds give identical samples; the seed is stored", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit_twice <- lapply(1:2, function(run) {
    quietly_fit(bcnma_bin(
      small_bin_data, study, treat, r, n, components = SIM_COMPONENTS,
      interactions = "none", n.chains = 2, n.adapt = 100, n.burnin = 50,
      n.iter = 50, seed = 99, quiet = TRUE))
  })
  expect_identical(fit_twice[[1]]$samps_additive, fit_twice[[2]]$samps_additive)
  expect_equal(fit_twice[[1]]$seed, 99L)
  expect_length(fit_twice[[1]]$inits, 2)
})
