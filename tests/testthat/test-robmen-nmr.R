# Tests for compute_network_nmr(), the pure helper backing ROB-MEN network
# meta-regression small-study effect estimates.

try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

helper_path <- system.file("app", "modules", "_robmen_nmr.R",
                           package = "nmatools")
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  helper_path <- testthat::test_path("..", "..", "inst", "app",
                                     "modules", "_robmen_nmr.R")
}
source(helper_path, local = TRUE)

skip_without_metafor <- function() {
  testthat::skip_if_not_installed("metafor")
}

demo_nmr_data <- function() {
  data.frame(
    studlab = paste0("s", 1:9),
    t1 = c("A", "A", "A", "A", "A", "A", "B", "B", "B"),
    t2 = c("B", "B", "B", "C", "C", "C", "C", "C", "C"),
    y  = c(0.25, 0.20, 0.12, 0.45, 0.39, 0.31, 0.20, 0.18, 0.14),
    se = c(0.30, 0.24, 0.16, 0.32, 0.25, 0.18, 0.28, 0.22, 0.17),
    stringsAsFactors = FALSE
  )
}

indirect_nmr_data <- function() {
  data.frame(
    studlab = paste0("s", 1:6),
    t1 = c("A", "A", "A", "A", "A", "A"),
    t2 = c("B", "B", "B", "C", "C", "C"),
    y  = c(0.20, 0.17, 0.12, 0.50, 0.42, 0.35),
    se = c(0.30, 0.22, 0.16, 0.32, 0.24, 0.18),
    stringsAsFactors = FALSE
  )
}

sparse_nmr_data <- function() {
  data.frame(
    studlab = paste0("s", 1:3),
    t1 = c("A", "A", "B"),
    t2 = c("B", "C", "C"),
    y  = c(0.20, 0.45, 0.16),
    se = c(0.30, 0.22, 0.16),
    stringsAsFactors = FALSE
  )
}

test_that("compute_network_nmr returns a stable schema for all treatment pairs", {
  skip_without_metafor()
  res <- compute_network_nmr(demo_nmr_data(), c("A", "B", "C"),
                             reference = "A")

  expect_named(res, c("comparison", "t1", "t2", "nmr_te", "nmr_lo",
                      "nmr_hi", "status"))
  expect_equal(res$comparison, c("A vs B", "A vs C", "B vs C"))
  expect_equal(res$t1, c("A", "A", "B"))
  expect_equal(res$t2, c("B", "C", "C"))
})

test_that("compute_network_nmr happy path locks fixed variance estimates", {
  skip_without_metafor()
  res <- compute_network_nmr(demo_nmr_data(), c("A", "B", "C"),
                             reference = "A")

  expect_true(all(is.finite(res$nmr_te)))
  expect_true(all(res$nmr_lo < res$nmr_te))
  expect_true(all(res$nmr_te < res$nmr_hi))
  expect_equal(res$status, rep("ok", 3))
  expect_equal(round(res$nmr_te, 6),
               c(0.143514, 0.288936, 0.145422),
               tolerance = 1e-6)
  expect_equal(round(res$nmr_lo, 6),
               c(-0.075905, 0.009222, -0.072879),
               tolerance = 1e-6)
  expect_equal(round(res$nmr_hi, 6),
               c(0.362934, 0.568650, 0.363723),
               tolerance = 1e-6)
})

test_that("compute_network_nmr estimates an indirect comparison", {
  skip_without_metafor()
  res <- compute_network_nmr(indirect_nmr_data(), c("A", "B", "C"),
                             reference = "A")
  bc <- res[res$comparison == "B vs C", , drop = FALSE]

  expect_equal(nrow(bc), 1)
  expect_true(is.finite(bc$nmr_te))
  expect_true(bc$nmr_lo < bc$nmr_te)
  expect_true(bc$nmr_te < bc$nmr_hi)
})

test_that("compute_network_nmr supports variance, se, and random models", {
  skip_without_metafor()
  dat <- demo_nmr_data()
  by_var <- compute_network_nmr(dat, c("A", "B", "C"), reference = "A",
                                covar = "variance")
  by_se <- compute_network_nmr(dat, c("A", "B", "C"), reference = "A",
                               covar = "se")
  by_re <- compute_network_nmr(dat, c("A", "B", "C"), reference = "A",
                               model_type = "random")

  expect_true(all(is.finite(by_var$nmr_te)))
  expect_true(all(is.finite(by_se$nmr_te)))
  expect_true(all(is.finite(by_re$nmr_te)))
  expect_false(isTRUE(all.equal(by_var$nmr_te, by_se$nmr_te,
                                tolerance = 1e-8)))
})

test_that("compute_network_nmr retreats safely for unfittable inputs", {
  skip_without_metafor()
  one_study <- compute_network_nmr(demo_nmr_data()[1, ], c("A", "B", "C"),
                                   reference = "A")
  expect_true(all(is.na(one_study$nmr_te)))
  expect_equal(one_study$status, rep("unfitted", 3))

  disconnected <- compute_network_nmr(demo_nmr_data(), c("A", "B", "C", "D"),
                                      reference = "A")
  has_d <- disconnected$t1 == "D" | disconnected$t2 == "D"
  expect_true(all(is.na(disconnected$nmr_te[has_d])))
  expect_equal(disconnected$status[has_d], rep("unfitted", sum(has_d)))
  expect_true(all(is.finite(disconnected$nmr_te[!has_d])))
  expect_equal(disconnected$status[!has_d], rep("ok", sum(!has_d)))
})

test_that("compute_network_nmr falls back from unrelated to common slopes", {
  skip_without_metafor()
  res <- compute_network_nmr(sparse_nmr_data(), c("A", "B", "C"),
                             reference = "A", coef_type = "unrelated")

  expect_true(all(is.finite(res$nmr_te)))
  expect_equal(res$status, rep("fallback_common", 3))
})
