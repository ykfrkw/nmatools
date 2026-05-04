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

test_that("small inflation under threshold returns 'no'", {
  # One high-RoB study with tiny IV weight (~1%); pooled |TE| barely changes.
  expect_equal(
    judge_rob_direct_sens(
      rob_vec = c("high",  "low",  "low",  "low",  "low"),
      te_vec  = c(-0.80,  -0.30, -0.30, -0.30, -0.30),
      se_vec  = c(0.50,    0.10,  0.10,  0.10,  0.10)
    ),
    "no"
  )
})

test_that("judge_rob_direct_sens_v exposes te_all, te_low, inflation, sign_flip", {
  # Inflated: 3 high + 1 low, pooled |TE| inflates beyond 10%
  res_inf <- judge_rob_direct_sens_v(
    rob_vec = c("high", "high", "high", "low"),
    te_vec  = c(-0.60, -0.70, -0.65, -0.30),
    se_vec  = c(0.10, 0.10, 0.10, 0.20)
  )
  expect_equal(res_inf$judgement, "some_concerns")
  expect_true(res_inf$inflation > 0.10)
  expect_false(res_inf$sign_flip)
  expect_true(!is.na(res_inf$te_all) && !is.na(res_inf$te_low))

  # Sign-flip
  res_flip <- judge_rob_direct_sens_v(
    rob_vec = c("high", "high", "high", "low"),
    te_vec  = c(-0.50, -0.60, -0.40, +0.30),
    se_vec  = c(0.10, 0.10, 0.10, 0.20)
  )
  expect_equal(res_flip$judgement, "serious")
  expect_true(res_flip$sign_flip)

  # All high-RoB: te_low not computable -> NA
  res_all <- judge_rob_direct_sens_v(
    rob_vec = c("high", "high"),
    te_vec  = c(-0.50, -0.60),
    se_vec  = c(0.10, 0.10)
  )
  expect_equal(res_all$judgement, "some_concerns")
  expect_true(is.na(res_all$te_low))
  expect_true(is.na(res_all$inflation))
})
