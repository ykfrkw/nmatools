# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: continuous-outcome scale handling: MD means centred on arm 1 and
#          MD priors scaled by the median pooled SD (review item R4).
# Inputs:  simulated large-mean MD network (below)
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

MD_CONTROL_MEAN   <- 360
MD_OUTCOME_SD     <- 60
MD_TRUE_EFFECT    <- c(A = 20, B = 10)
MD_ARM_SIZE       <- 100
MD_TOLERANCE      <- 5    # minutes; MCMC error + sampling error of 24 studies

# 24 two-arm studies of an outcome measured in minutes (mean around 360).
simulate_large_mean_md <- function(n_studies = 24) {
  set.seed(SIM_SEED)
  designs <- list(c("A", "A+B"), c("B", "A+B"), c("A", "B"))
  arm_rows <- lapply(seq_len(n_studies), function(study_number) {
    design   <- designs[[(study_number - 1) %% length(designs) + 1]]
    baseline <- stats::rnorm(1, MD_CONTROL_MEAN, 10)
    true_mean <- vapply(strsplit(design, "+", fixed = TRUE), function(parts) {
      baseline + sum(MD_TRUE_EFFECT[parts])
    }, numeric(1))
    data.frame(study = sprintf("M%02d", study_number), treat = design,
               mean = stats::rnorm(2, true_mean, MD_OUTCOME_SD / sqrt(MD_ARM_SIZE)),
               sd = MD_OUTCOME_SD, n = MD_ARM_SIZE, stringsAsFactors = FALSE)
  })
  do.call(rbind, arm_rows)
}

test_that("MD means are centred on arm 1 and priors follow the pooled SD", {
  md_data <- simulate_large_mean_md()
  prep <- .bcnma_cont_prepare_data(md_data, "study", "treat", "mean", "sd", "n",
                                   c("A", "B"), "+", "MD", "within")
  expect_equal(prep$outcome$y[, 1], rep(0, prep$n_studies))
  expect_equal(prep$median_pooled_sd, MD_OUTCOME_SD)
  expect_equal(.location_prior_sd_cont(prep, "effect"), 10 * MD_OUTCOME_SD)
  tau_prior <- .default_tau_prior_cont(prep)
  expect_equal(tau_prior$family, "half_normal_tau")
  expect_equal(tau_prior$sd, MD_OUTCOME_SD)
})

test_that("SMD construction: Hedges' J, control SD of arm 1, error on bad SD (R5)", {
  prep_control <- .bcnma_cont_prepare_data(small_cont_data, "study", "treat", "mean", "sd",
                                           "n", SIM_COMPONENTS, "+", "SMD", "control")
  # S1 rows in data order: arm 1 = "A" (SD 3.5, n 50) -> df 49
  expect_equal(prep_control$sigma_pooled[1], 3.5)
  expect_equal(prep_control$hedges_j[1], 1 - 3 / (4 * 49 - 1))
  bad_data <- transform(small_cont_data, sd = c(4, NA, 4.2, 3.9, 0, 4.1, 4, 4.3))
  expect_error(.bcnma_cont_prepare_data(bad_data, "study", "treat", "mean", "sd", "n",
                                        SIM_COMPONENTS, "+", "SMD", "within"),
               "S1 \\(arm 1\\), S1 \\(arm 2\\)")
})

test_that("tau_prior accepts numeric, log-normal and half-normal specifications", {
  default_prior <- .default_tau_prior_bin()
  expect_identical(.tau_prior_spec(NULL, default_prior), default_prior)
  expect_equal(.tau_prior_spec(c(-2, 1), default_prior)$meanlog, -2)
  expect_equal(.tau_prior_spec(list(family = "half_normal_tau", sd = 0.5),
                               default_prior)$sd, 0.5)
  expect_error(.tau_prior_spec(list(family = "gamma"), default_prior), "half_normal_tau")
  expect_error(.tau_prior_spec(c(-2, 1, 3), default_prior), "one meanlog")
})

test_that("a large-mean MD outcome recovers the true effect", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  fit <- quietly_fit(bcnma_cont(
    simulate_large_mean_md(), study, treat, mean, sd, n, components = c("A", "B"),
    sm = "MD", interactions = "none", n.chains = 1, n.adapt = 500, n.burnin = 1000,
    n.iter = 3000, seed = SIM_SEED, quiet = TRUE))
  posterior_median <- c(stats::median(fit$samps_additive[["d[1]"]]),
                        stats::median(fit$samps_additive[["d[2]"]]))
  expect_true(all(abs(posterior_median - MD_TRUE_EFFECT) < MD_TOLERANCE))
  expect_match(fit$model_code$additive, "tau  ~ dnorm(0, 0.000277778) T(0,)", fixed = TRUE)
})
