# Tests for the Sensitivity-based Domain 1 (Study limitations) judgment.
# judge_rob_direct_sens() lives in inst/app/modules/_d1_sens_judge.R; this
# file is sourced standalone (no shiny needed).

helper_path <- system.file("app", "modules", "_d1_sens_judge.R",
                           package = "nmatools")
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  helper_path <- testthat::test_path("..", "..", "inst", "app",
                                     "modules", "_d1_sens_judge.R")
}
source(helper_path, local = TRUE)

test_that("clean: no high-RoB studies returns 'no'", {
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("low", "low", "some concerns"),
      te_vec  = c(-0.30, -0.40, -0.35),
      se_vec  = c(0.10, 0.10, 0.12)
    ),
    "no"
  )
})

test_that("inflated: high-RoB inflates |TE| beyond threshold returns 'some_concerns'", {
  # Three high-RoB studies (~75% IV weight) push |TE| from 0.30 (low) to ~0.62 pooled.
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("high", "high", "high", "low"),
      te_vec  = c(-0.60, -0.70, -0.65, -0.30),
      se_vec  = c(0.10, 0.10, 0.10, 0.20)
    ),
    "some_concerns"
  )
})

test_that("sign-flip: excluding high-RoB reverses pooled TE sign returns 'serious'", {
  # All-in pooled TE is negative; the only low-RoB study is positive, flipping the sign.
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("high", "high", "high", "low"),
      te_vec  = c(-0.50, -0.60, -0.40, +0.30),
      se_vec  = c(0.10, 0.10, 0.10, 0.20)
    ),
    "serious"
  )
})

test_that("only high-RoB studies returns conservative 'some_concerns'", {
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("high", "high"),
      te_vec  = c(-0.50, -0.60),
      se_vec  = c(0.10, 0.10)
    ),
    "some_concerns"
  )
})

test_that("dominance threshold gates the rate-down (low share of high-RoB returns 'no')", {
  # Single high-RoB study with weight ~5% — well under the 60% dominance threshold.
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("high",  "low",  "low",  "low",  "low"),
      te_vec  = c(-0.80,  -0.30, -0.30, -0.30, -0.30),
      se_vec  = c(0.50,    0.10,  0.10,  0.10,  0.10)
    ),
    "no"
  )
})
