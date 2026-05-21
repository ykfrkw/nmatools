# Tests for ROB-MEN pairwise-to-network judgement helpers.

utils_path <- testthat::test_path("..", "..", "inst", "app",
                                  "modules", "utils.R")
helper_path <- testthat::test_path("..", "..", "inst", "app",
                                   "modules", "module_C_robmen.R")
if (!file.exists(utils_path)) {
  utils_path <- system.file("app", "modules", "utils.R", package = "nmatools")
}
if (!file.exists(helper_path)) {
  helper_path <- system.file("app", "modules", "module_C_robmen.R",
                             package = "nmatools")
}
source(utils_path, local = TRUE)
source(helper_path, local = TRUE)

FAV_ONE <- "Substantial contribution from bias \u2013 favouring one treatment"
BALANCED <- "Substantial contribution from bias \u2013 balanced"
NO_CONTRIB <- "No substantial contribution from bias"
NO_SSE <- "No evidence of small-study effects"
SSE_IN <- "Evidence of small-study effects \u2013 reinforcing biased contribution"
SSE_NOT <- "Evidence of small-study effects \u2013 not reinforcing biased contribution"

test_that("derive_indirect_bias copies pairwise judgement and direction", {
  expect_equal(derive_indirect_bias("No bias detected", ""), "No suspected bias")
  expect_equal(derive_indirect_bias("Some concerns", ""), "Some concerns")
  expect_equal(
    derive_indirect_bias("Suspected bias", "t1"),
    "Suspected bias favouring t1"
  )
  expect_equal(
    derive_indirect_bias("Suspected bias", "t2"),
    "Suspected bias favouring t2"
  )
  expect_equal(derive_indirect_bias("Suspected bias", ""), "Some concerns")
})

test_that("compute_overall_robmen preserves mixed/direct Table 5 behaviour", {
  expect_equal(compute_overall_robmen(NO_CONTRIB, NO_SSE), "Low risk")
  expect_equal(compute_overall_robmen(NO_CONTRIB, SSE_NOT), "Some concerns")
  expect_equal(compute_overall_robmen(BALANCED, NO_SSE), "Low risk")
  expect_equal(compute_overall_robmen(BALANCED, SSE_IN), "Some concerns")
  expect_equal(compute_overall_robmen(FAV_ONE, NO_SSE), "Some concerns")
  expect_equal(compute_overall_robmen(FAV_ONE, SSE_NOT), "Some concerns")
  expect_equal(compute_overall_robmen(FAV_ONE, SSE_IN), "High risk")
})

test_that("indirect-evidence bias upgrades only-indirect estimates by direction", {
  expect_equal(
    compute_overall_robmen(
      NO_CONTRIB, NO_SSE,
      evidence_type = "indirect",
      indirect_bias = "Suspected bias favouring t1",
      pct_fav_t1 = 0, pct_fav_t2 = 0
    ),
    "Some concerns"
  )

  expect_equal(
    compute_overall_robmen(
      FAV_ONE, NO_SSE,
      evidence_type = "indirect",
      indirect_bias = "Suspected bias favouring t1",
      pct_fav_t1 = 35, pct_fav_t2 = 5
    ),
    "High risk"
  )

  expect_equal(
    compute_overall_robmen(
      FAV_ONE, NO_SSE,
      evidence_type = "indirect",
      indirect_bias = "Suspected bias favouring t1",
      pct_fav_t1 = 5, pct_fav_t2 = 35
    ),
    "Some concerns"
  )

  expect_equal(
    compute_overall_robmen(
      BALANCED, NO_SSE,
      evidence_type = "indirect",
      indirect_bias = "Suspected bias favouring t2",
      pct_fav_t1 = 20, pct_fav_t2 = 20
    ),
    "Some concerns"
  )
})
