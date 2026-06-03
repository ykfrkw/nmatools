# Tests for ROB-MEN pairwise-to-network judgement helpers.

try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

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

test_that("pairwise choices use two categories with treatment-specific direction", {
  choices <- pairwise_bias_choices("A", "B")
  expect_equal(unname(choices),
               c("", "No bias detected",
                 "Suspected bias favouring A",
                 "Suspected bias favouring B"))
  expect_false("Some concerns" %in% choices)
  expect_false("Insufficient data" %in% choices)
  expect_identical(WITHIN_CHOICES("A", "B"), choices)
  expect_identical(ACROSS_CHOICES("A", "B"), choices)
  expect_equal(unname(OVERALL_PW_CHOICES("A", "B")),
               c("", "No bias detected",
                 "Suspected bias favouring A",
                 "Suspected bias favouring B"))
})

test_that("pairwise overall carries direction and resolves conflicts", {
  expect_equal(
    compute_overall_pw("Suspected bias favouring A",
                       "No bias detected", "A", "B"),
    "Suspected bias favouring A"
  )
  expect_equal(
    compute_overall_pw("No bias detected",
                       "Suspected bias favouring B", "A", "B"),
    "Suspected bias favouring B"
  )
  expect_equal(
    compute_overall_pw("Suspected bias favouring A",
                       "Suspected bias favouring B", "A", "B"),
    "Suspected bias favouring A"
  )
  expect_equal(
    compute_overall_pw("No bias detected", "No bias detected", "A", "B"),
    "No bias detected"
  )
})

test_that("pairwise overall reverts when within is set back to No bias", {
  # Forward cascade depends on the overall being recomputed from the source
  # within/across inputs every time. When ① is reverted from suspected back
  # to "No bias detected" (with across unset/no-bias), the overall must
  # revert to "No bias detected" rather than remain stale.
  expect_equal(
    compute_overall_pw("Suspected bias favouring A", "", "A", "B"),
    "Suspected bias favouring A"
  )
  expect_equal(
    compute_overall_pw("No bias detected", "", "A", "B"),
    "No bias detected"
  )
  expect_equal(
    compute_overall_pw("No bias detected", "No bias detected", "A", "B"),
    "No bias detected"
  )
})

test_that("derive_indirect_bias copies pairwise judgement direction", {
  expect_equal(derive_indirect_bias("No bias detected", "A", "B"),
               "No suspected bias")
  expect_equal(
    derive_indirect_bias("Suspected bias favouring A", "A", "B"),
    "Suspected bias favouring t1"
  )
  expect_equal(
    derive_indirect_bias("Suspected bias favouring B", "A", "B"),
    "Suspected bias favouring t2"
  )
  expect_equal(derive_indirect_bias("Suspected bias", "A", "B"),
               "No suspected bias")
})

test_that("ROB-ME helper maps to two ROB-MEN pairwise categories", {
  expect_equal(robme_derive_rating("no", "", "A", "B"),
               "No bias detected")
  expect_equal(robme_derive_rating("unclear", "", "A", "B"),
               "No bias detected")
  expect_equal(robme_derive_rating("yes", "no", "A", "B"),
               "No bias detected")
  expect_equal(robme_derive_rating("yes", "t1", "A", "B"),
               "Suspected bias favouring A")
  expect_equal(robme_derive_rating("yes", "t2", "A", "B"),
               "Suspected bias favouring B")
})

test_that("make_pw_row omits funnel and separate direction inputs below k=10", {
  row_9 <- make_pw_row(shiny::NS("rob"), "A:B", "A", "B",
                       n_direct = 9, across_default = "")
  html_9 <- paste(as.character(row_9), collapse = "")
  expect_false(grepl("funnel_btn", html_9, fixed = TRUE))
  expect_false(grepl("bias_dir_", html_9, fixed = TRUE))

  row_10 <- make_pw_row(shiny::NS("rob"), "A:B", "A", "B",
                        n_direct = 10, across_default = "")
  html_10 <- paste(as.character(row_10), collapse = "")
  expect_true(grepl("funnel_btn", html_10, fixed = TRUE))
  expect_false(grepl("bias_dir_", html_10, fixed = TRUE))
})

test_that("compute_auto_contrib honours the user threshold", {
  expect_equal(
    compute_auto_contrib(0, pct_fav_t1 = 14, pct_fav_t2 = 0,
                         threshold_pp = 15),
    "No substantial contribution from bias"
  )
  expect_equal(
    compute_auto_contrib(0, pct_fav_t1 = 14, pct_fav_t2 = 0,
                         threshold_pp = 10),
    "Substantial contribution from bias \u2013 favouring one treatment"
  )
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
