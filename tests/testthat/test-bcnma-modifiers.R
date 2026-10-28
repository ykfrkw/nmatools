# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Design-level tests for component values (doses) and arm-level effect
# modifiers (no JAGS needed): row alignment, ordinal cumulative coding, NA
# handling, non-identifiability warning, JAGS model text, `at` codes.

ORDINAL_LEVELS <- c("low", "mid", "high")
ordinal_spec <- list(list(column = "level", components = "C", type = "ordinal",
                          levels = ORDINAL_LEVELS, label = "skill"))

test_that("component_values stay aligned with their rows after study re-ordering", {
  for (prepare in list(prepare_small_bin, prepare_small_cont)) {
    prep <- prepare(values = .validate_component_values(dose_values, 6, SIM_COMPONENTS))
    expect_equal(prep$studies, c("S1", "S2", "S3"))
    expect_equal(prep$xA[1, , 3], c(0, 4))     # S1: rows 2 (A) then 5 (B+C)
    expect_equal(prep$xA[2, , 3], c(0, 2.5))   # S2: rows 1 then 3
    expect_equal(prep$xA[3, , 3], c(0, 7))     # S3: rows 4 then 6
    expect_equal(prep$xB[, 2], c(0, 0, 1))
  }
  expect_error(.validate_component_values(dose_values[, 3:1], 6, SIM_COMPONENTS), "exactly")
  expect_error(.validate_component_values(dose_values[1:5, ], 6, SIM_COMPONENTS), "rows")
  expect_error(.validate_component_values(replace(dose_values, 1, NA), 6, SIM_COMPONENTS), "NA")
  expect_warning(
    expect_warning(prepare_small_bin(values = cbind(dose_values[, 1:2], C = 0)), "zero in every arm"),
    "disagree on presence")
  expect_warning(prepare_small_cont(values = cbind(dose_values[, 1:2], C = c(1, 0, 0, 0, 0, 0))),
                 "disagree on presence")
})

test_that("ordinal modifiers use cumulative (incremental) coding for both outcome types", {
  for (case in list(list(data = small_bin_data, prepare = prepare_small_bin),
                    list(data = small_cont_data, prepare = prepare_small_cont))) {
    specs  <- .normalise_modifier_specs(ordinal_spec, case$data, SIM_COMPONENTS)
    design <- suppressMessages(.build_modifier_design(specs, case$data, case$prepare(case$data)))
    expect_equal(design$Q, 2)
    expect_equal(design$terms$term, c("skill(low->mid):C", "skill(mid->high):C"))
    expect_equal(design$wA[, 2, 1], c(1, 1, 0))  # S1 mid, S2 high, S3 low
    expect_equal(design$wA[, 2, 2], c(0, 1, 0))
    expect_equal(design$wA[, 1, ], matrix(0, 3, 2))
  }
  bad_data <- transform(small_bin_data, level = c(NA, NA, "expert", NA, "mid", "low"))
  expect_error(.build_modifier_design(.normalise_modifier_specs(ordinal_spec, bad_data, SIM_COMPONENTS),
                                      bad_data, prepare_small_bin(bad_data)), "not in 'levels'")
})

test_that("modifier specs are validated", {
  expect_error(.normalise_modifier_specs(list(column = "nope", components = "C", type = "continuous"),
                                         small_bin_data, SIM_COMPONENTS), "not found")
  expect_error(.normalise_modifier_specs(list(column = "level", components = "Z", type = "ordinal",
                                              levels = ORDINAL_LEVELS),
                                         small_bin_data, SIM_COMPONENTS), "known components")
  expect_error(.normalise_modifier_specs(list(column = "level", components = "C", type = "ordinal"),
                                         small_bin_data, SIM_COMPONENTS), "levels")
  expect_error(.normalise_modifier_specs(list(column = "r", components = "C", type = "continuous",
                                              monotone = "increasing"),
                                         small_bin_data, SIM_COMPONENTS), "ordinal modifiers only")
})

test_that("NA covariate is an error only where the component is present", {
  bad_data <- transform(small_cont_data, level = c(NA, NA, NA, NA, "mid", "low"))
  spec <- list(column = "level", components = "C", type = "ordinal", levels = ORDINAL_LEVELS)
  expect_error(.build_modifier_design(.normalise_modifier_specs(spec, bad_data, SIM_COMPONENTS),
                                      bad_data, prepare_small_cont(bad_data)), "S2 \\(arm 2\\)")
})

test_that("a term with no within-study contrast triggers a non-identifiability warning", {
  both_arms_data <- data.frame(study = rep(c("S1", "S2"), each = 2), treat = c("A+C", "B+C"),
                               r = 10, n = 50, skill = rep(c(2, 4), each = 2))
  spec  <- list(column = "skill", components = "C", type = "continuous")
  specs <- .normalise_modifier_specs(spec, both_arms_data, SIM_COMPONENTS)
  prep  <- .bcnma_bin_prepare_data(both_arms_data, "study", "treat", "r", "n", SIM_COMPONENTS)
  expect_warning(suppressMessages(.build_modifier_design(specs, both_arms_data, prep)),
                 "not identifiable")
})

test_that("model text adds beta terms on the right scale for each outcome type", {
  priors <- data.frame(precision = c(0.01, 0.04), truncation = c("T(0,)", ""))
  bin_model  <- .jags_model_ssvs(3, 3, modifier_priors = priors)
  cont_model <- .jags_model_additive_cont(3, modifier_priors = priors)
  expect_match(bin_model, "beta[1] * wA[i,k,1]", fixed = TRUE)
  expect_match(bin_model, "beta[1] ~ dnorm(0, 0.010000) T(0,)", fixed = TRUE)
  expect_match(bin_model, "ORbeta[q] <- exp(beta[q])", fixed = TRUE)
  expect_match(cont_model, "- (beta[1] * wA[i,1,1]", fixed = TRUE)
  expect_match(cont_model, "beta[2] ~ dnorm(0, 0.040000)\n", fixed = TRUE)
  expect_false(grepl("ORbeta", cont_model, fixed = TRUE))
})

test_that("`at` codes follow the term definitions", {
  specs  <- .normalise_modifier_specs(ordinal_spec, small_bin_data, SIM_COMPONENTS)
  design <- suppressMessages(.build_modifier_design(specs, small_bin_data, prepare_small_bin()))
  fit <- list(modifier_terms = design$terms, modifier_specs = specs)
  expect_equal(.modifier_codes_at(fit, list(skill = "high")), c(1, 1))
  expect_equal(.modifier_codes_at(fit, list(skill = "mid")), c(1, 0))
  expect_message(codes <- .modifier_codes_at(fit, NULL), "reference level")
  expect_equal(codes, c(0, 0))
  expect_error(.modifier_codes_at(fit, list(skill = "expert")), "must be one of")
  expect_error(.modifier_codes_at(fit, list(other = 1)), "not among modifier labels")
})

test_that("named numeric combinations are converted to component values", {
  expect_equal(unname(.combo_to_values(c(C = 2.5, A = 1), SIM_COMPONENTS)), c(1, 0, 2.5))
  expect_equal(.combo_labels(c(C = 2.5, A = 1), c(A = 1, B = 0, C = 2.5), SIM_COMPONENTS),
               c("A", "C=2.5"))
  expect_error(.combo_to_values(c(Z = 1), SIM_COMPONENTS), "Unknown: Z")
})
