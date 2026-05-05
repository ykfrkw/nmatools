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

# ---------------------------------------------------------------------------
# Sensitivity-based, split-evidence p-value variant
# ---------------------------------------------------------------------------

test_that("split-p: no high-RoB studies returns 'no'", {
  expect_equal(
    judge_rob_direct_split_p(
      rob_vec = c("low", "low", "some concerns"),
      te_vec  = c(-0.30, -0.40, -0.35),
      se_vec  = c(0.10, 0.10, 0.12)
    ),
    "no"
  )
})

test_that("split-p: only high-RoB studies returns 'serious' (no comparator)", {
  expect_equal(
    judge_rob_direct_split_p(
      rob_vec = c("high", "high", "high"),
      te_vec  = c(-0.50, -0.60, -0.55),
      se_vec  = c(0.10, 0.10, 0.10)
    ),
    "serious"
  )
})

test_that("split-p: low+some and high agree (small delta) returns 'no'", {
  # Both pools centred near -0.50 -> p will be near 1
  expect_equal(
    judge_rob_direct_split_p(
      rob_vec = c("low", "some concerns", "high", "high"),
      te_vec  = c(-0.50, -0.52, -0.48, -0.51),
      se_vec  = c(0.10,  0.10,  0.10,  0.10)
    ),
    "no"
  )
})

test_that("split-p: sign flip overrides p-value to 'serious'", {
  # te_ls > 0, te_h < 0 -> sign flip
  expect_equal(
    judge_rob_direct_split_p(
      rob_vec = c("low", "low", "high", "high"),
      te_vec  = c(0.30, 0.40, -0.30, -0.40),
      se_vec  = c(0.10, 0.10, 0.10, 0.10)
    ),
    "serious"
  )
})

test_that("split-p: large delta (p < 0.05) returns 'serious'", {
  # te_ls = 0.0 (SE 0.07), te_h = 0.6 (SE 0.07) -> delta = -0.6, SE_diff ~0.10,
  # z ~6 -> p < 1e-9
  res <- judge_rob_direct_split_p_v(
    rob_vec = c("low", "low", "high", "high"),
    te_vec  = c(0.00, 0.00, 0.60, 0.60),
    se_vec  = c(0.10, 0.10, 0.10, 0.10)
  )
  # Sign-of-zero edge: te_ls = 0 might or might not register sign_flip.
  # The judgement we care about is "serious" via either route.
  expect_equal(res$judgement, "serious")
  expect_true(res$p_value < 0.001 || isTRUE(res$sign_flip))
})

test_that("split-p: moderate delta (0.05 < p <= 0.10) returns 'some_concerns'", {
  # Tuned so that two-sided p is ~0.07.
  # te_ls = 0.0 (SE 0.10), te_h = 0.30 (SE 0.10) -> SE_diff ~0.1414, z ~2.12,
  # p ~ 0.034 -> too small. Widen SEs to push p up.
  # te_ls = 0.0 (SE 0.20), te_h = 0.30 (SE 0.20) -> SE_diff ~0.283, z ~1.06,
  # p ~ 0.29 -> too large.
  # te_ls = 0.0 (SE 0.10), te_h = 0.20 (SE 0.10) -> z ~ 1.41, p ~ 0.158 -> still
  # large. Tune to z ~ 1.81 (p ~ 0.07): delta=0.30, se_diff=0.166.
  # se_ls = se_h = sqrt(0.166^2/2) ~ 0.1175.
  res <- judge_rob_direct_split_p_v(
    rob_vec = c("low", "low", "low", "high", "high", "high"),
    te_vec  = c(0.00, 0.00, 0.00, 0.30, 0.30, 0.30),
    se_vec  = c(0.20, 0.20, 0.20, 0.20, 0.20, 0.20)
  )
  expect_equal(res$judgement, "some_concerns")
  expect_true(res$p_value > 0.05 && res$p_value <= 0.10)
})

test_that("split-p: returns shape-stable list with k_ls / k_h / p_value", {
  res <- judge_rob_direct_split_p_v(
    rob_vec = c("low", "some concerns", "high"),
    te_vec  = c(-0.30, -0.40, -0.35),
    se_vec  = c(0.10, 0.10, 0.10)
  )
  expect_named(res, c("judgement", "te_ls", "te_h", "se_ls", "se_h",
                      "k_ls", "k_h", "p_value", "sign_flip"))
  expect_equal(res$k_ls, 2L)
  expect_equal(res$k_h, 1L)
})
