# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: numerical equivalence of the rewritten bcnma code (review item
#          R3) with bcnma 0.1.0 and with netmeta. The byte-identity tests of
#          the old model strings are retired: the models are now written
#          with inprod() over within-study contrasts. Instead we check that
#          (1) the prepared data, (2) the compare_combinations() arithmetic
#          on fixed draws match the bcnma 0.1.0 fixture, (3) the printed
#          model is short and TSD2-like, and (4) an additive fit agrees with
#          netmeta::discomb() on a simulated network.
# Inputs:  fixtures/bcnma_backward_compat.rds (see
#          fixtures/make_bcnma_backward_compat_fixture.R), helper data
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting tests (skipped otherwise)

BACKWARD_COMPAT <- readRDS(test_path("fixtures", "bcnma_backward_compat.rds"))
MAX_MODEL_CODE_LINES  <- 60
NETMETA_TOLERANCE     <- 0.1   # log-OR; MCMC error + prior on tau2

test_that("prepared binary data equal bcnma 0.1.0", {
  old <- BACKWARD_COMPAT$jags_data_bin
  prep <- prepare_small_bin(BACKWARD_COMPAT$small_bin_data)
  expect_equal(prep$outcome$r, old$r, ignore_attr = TRUE)
  expect_equal(prep$outcome$n, old$n, ignore_attr = TRUE)
  expect_equal(prep$x_arm, old$xA, ignore_attr = TRUE)
  expect_equal(prep$x_arm[, 1, ], old$xB, ignore_attr = TRUE)
  expect_equal(prep$n_arms, old$na)
})

test_that("prepared SMD data equal bcnma 0.1.0 times Hedges' J (R5)", {
  old  <- BACKWARD_COMPAT$jags_data_cont
  prep <- prepare_small_cont(BACKWARD_COMPAT$small_cont_data)
  # Two arms of 50 per study: df = 98, J = 1 - 3 / (4 * 98 - 1)
  hedges_j <- 1 - 3 / (4 * 98 - 1)
  expect_equal(prep$hedges_j, rep(hedges_j, 3))
  expect_equal(prep$outcome$y, old$y * hedges_j)
  expect_equal(prep$outcome$se^-2, old$prec / hedges_j^2)
  expect_equal(prep$x_arm, old$xA, ignore_attr = TRUE)
})

test_that("compare_combinations reproduces bcnma 0.1.0 arithmetic on fixed draws", {
  cases <- BACKWARD_COMPAT$compare_cases
  for (outcome in c("bin", "cont")) {
    fit <- BACKWARD_COMPAT[[paste0("compare_fit_", outcome)]]
    # R9 renamed the SSVS fields and model name (lasso -> ssvs)
    fit$samps_ssvs <- fit$samps_lasso
    old_results <- BACKWARD_COMPAT[[paste0("compare_results_", outcome)]]
    for (case_index in seq_len(nrow(cases))) {
      case <- cases[case_index, ]
      new_model <- if (case$model == "lasso") "ssvs" else case$model
      capture.output(new_result <- compare_combinations(
        fit, BACKWARD_COMPAT$compare_combos[[case$combo]],
        BACKWARD_COMPAT$compare_against[[case$against]], model = new_model))
      old_result <- old_results[[case_index]]
      expect_identical(new_result$effect_samples, old_result$effect_samples)
      expect_identical(new_result$estimate, old_result$estimate)
    }
  }
})

test_that("within-study contrasts are arm k minus arm 1", {
  prep <- prepare_small_bin()
  contrast <- .within_study_contrast(prep$x_arm, prep$n_arms)
  expect_equal(contrast[, 1, ], matrix(0, 4, 3))
  expect_equal(contrast[1, 2, ], c(-1, 1, 1))   # S1: A vs B+C
  expect_equal(contrast[2, 2, ], c(0, 0, 1))    # S2: A vs A+C
})

test_that("model code is short, TSD2-like and has optional blocks only when used", {
  settings <- list(likelihood = "binomial", effect_prior_sd = 10, baseline_prior_sd = 10,
                   tau_prior = .tau_prior_spec(c(-1.67, 0.46), "test"),
                   modifier_priors = NULL,
                   ssvs = list(inclusion_prob = 0.5, slab_ratio = 100, eta_prior_sd = 0.03))
  additive <- .build_jags_model_code("additive", settings)
  ssvs     <- .build_jags_model_code("ssvs", settings)
  expect_match(additive, "mean[i,k]  <- inprod(d[], x[i,k,])\n", fixed = TRUE)
  expect_false(grepl("gamma", additive, fixed = TRUE))
  expect_match(ssvs, "inprod(gamma[], xint[i,k,])", fixed = TRUE)
  expect_match(ssvs, "precd[i,k] <- prec * 2 * (k-1) / k", fixed = TRUE)
  expect_lt(length(strsplit(ssvs, "\n")[[1]]), MAX_MODEL_CODE_LINES)
  normal <- .build_jags_model_code("additive", modifyList(settings, list(likelihood = "normal")))
  expect_match(normal, "y[i,k] ~ dnorm(theta[i,k], pow(se[i,k], -2))", fixed = TRUE)
  expect_false(grepl("ORd", normal, fixed = TRUE))
})

test_that("model text adds beta terms only with modifiers", {
  settings <- list(likelihood = "normal", effect_prior_sd = 10, baseline_prior_sd = 10,
                   tau_prior = .tau_prior_spec(c(-1, 1), "test"),
                   modifier_priors = data.frame(precision = c(0.01, 0.04),
                                                truncation = c("T(0,)", ""),
                                                selection_group = NA))
  model_code <- .build_jags_model_code("additive", settings)
  expect_match(model_code, "inprod(beta[], z[i,k,])", fixed = TRUE)
  expect_match(model_code, "beta[1] ~ dnorm(0, 0.01) T(0,)", fixed = TRUE)
  expect_match(model_code, "beta[2] ~ dnorm(0, 0.04)\n", fixed = TRUE)
})

test_that("additive fit agrees with netmeta::discomb on a simulated network", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  sim_data <- simulate_identifiable_network()
  fit <- quietly_fit(bcnma_bin(
    sim_data, study, treat, r, n, components = SIM_COMPONENTS, interactions = "none",
    n.chains = 1, n.adapt = 500, n.burnin = 1000, n.iter = 3000, seed = SIM_SEED,
    quiet = TRUE))
  pairwise_data <- meta::pairwise(treat = treat, event = r, n = n, studlab = study,
                                  data = sim_data, sm = "OR")
  frequentist <- netmeta::discomb(TE, seTE, treat1, treat2, studlab,
                                  data = pairwise_data, sm = "OR")
  bayesian_median <- vapply(seq_along(SIM_COMPONENTS), function(component_index) {
    stats::median(fit$samps_additive[[paste0("d[", component_index, "]")]])
  }, numeric(1))
  frequentist_estimate <- frequentist$Comp.random[match(SIM_COMPONENTS, frequentist$comps)]
  expect_true(all(abs(bayesian_median - frequentist_estimate) < NETMETA_TOLERANCE))
  expect_output(bcnma_model_code(fit, "additive"), "inprod(d[], x[i,k,])", fixed = TRUE)
  capture.output(printed_code <- bcnma_model_code(fit))
  expect_identical(printed_code, fit$model_code$additive)
})
