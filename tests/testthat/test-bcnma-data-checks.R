# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: input checks of the arm-level data: unknown treatment tokens
#          and the `inactive` argument (review item R8), single-arm studies
#          (R16) and the deprecated lasso names (R9).
# Inputs:  small_bin_data (helper-bcnma-data.R)
# Outputs: none (testthat expectations)
# Depends: nothing beyond nmatools internals (no JAGS)

with_placebo <- rbind(
  small_bin_data,
  data.frame(study = c("S5", "S5"), treat = c("placebo", "A+placebo"), r = c(8, 13), n = 50,
             level = NA, stringsAsFactors = FALSE)
)

test_that("unknown treatment tokens are an error listing them", {
  expect_error(.bcnma_bin_prepare_data(with_placebo, "study", "treat", "r", "n",
                                       SIM_COMPONENTS),
               "neither 'components' nor 'inactive': placebo")
})

test_that("inactive tokens are accepted and mean no active component", {
  prep <- .bcnma_bin_prepare_data(with_placebo, "study", "treat", "r", "n",
                                  SIM_COMPONENTS, inactive = "placebo")
  placebo_study <- match("S5", prep$studies)
  expect_equal(prep$x_arm[placebo_study, 1, ], c(0, 0, 0))
  expect_equal(prep$x_arm[placebo_study, 2, ], c(1, 0, 0))
  expect_error(.bcnma_bin_prepare_data(with_placebo, "study", "treat", "r", "n",
                                       SIM_COMPONENTS, inactive = c("placebo", "A")),
               "both 'components' and 'inactive': A")
})

test_that("single-arm studies are an error naming them (R16)", {
  single_arm <- rbind(small_bin_data, data.frame(study = "S9", treat = "A", r = 3, n = 20,
                                                 level = NA, stringsAsFactors = FALSE))
  expect_error(.bcnma_bin_prepare_data(single_arm, "study", "treat", "r", "n", SIM_COMPONENTS),
               "at least 2 arms.*: S9")
})

test_that("deprecated lasso names map to SSVS with a one-time message (R9)", {
  rm(list = ls(.bcnma_deprecations_shown), envir = .bcnma_deprecations_shown)
  expect_message(expect_equal(.normalise_interactions("lasso"), "ssvs"), "deprecated")
  expect_silent(.normalise_interactions("lasso"))
  expect_message(expect_equal(.normalise_interactions("lasso_informative"),
                              "ssvs_informative"), "deprecated")
  expect_equal(.normalise_interactions("none"), "none")
  expect_message(expect_equal(.resolve_inclusion_prob(0.5, lasso_prob = 0.8), 0.8),
                 "inclusion_prob")
  expect_error(.resolve_inclusion_prob(1.5, NULL), "strictly between 0 and 1")
  expect_message(expect_equal(.normalise_model_name("lasso"), "ssvs"), "deprecated")
})
