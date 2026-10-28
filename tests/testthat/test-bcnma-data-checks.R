# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: input checks of the arm-level data: unknown treatment tokens
#          and the `inactive` argument (review item R8).
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
