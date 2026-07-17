# Tests for normalize_injected_data() — the pure helper that gives the
# R-side injection path (cinema(data = ...) / launch_nma_evaluator()) the
# same column-alias auto-detection and ROB/indirectness value auto-mapping
# as the GUI upload path. Lives in inst/app/modules/module_A_data_input.R.
# Mirrors the loading pattern used by test-robmen-bg-plots.R, but prefers
# the source tree so the tests exercise the current (not installed) module.

# Module files contain UTF-8 string literals; in a C locale, source(...,
# encoding = "UTF-8") can crash under testthat, so force a UTF-8 CTYPE
# locale first (same guard as inst/app/app.R).
for (.loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) break
}

module_dir <- testthat::test_path("..", "..", "inst", "app", "modules")
if (!dir.exists(module_dir)) {
  module_dir <- system.file("app", "modules", package = "nmatools")
}
source(file.path(module_dir, "utils.R"), local = TRUE, encoding = "UTF-8")
source(file.path(module_dir, "module_A_data_input.R"),
       local = TRUE, encoding = "UTF-8")

# load_w2i()-shaped arm-level binary data (id / t / r aliases, L-M-H rob,
# integer indirectness) — built inline so the unit tests need no package data
w2i_shaped <- function() {
  data.frame(
    id           = c("S1", "S1", "S2", "S2"),
    t            = c("CBT-I", "Pharmacotherapy", "CBT-I", "Combination"),
    n            = c(20L, 10L, 15L, 18L),
    r            = c(9L, 4L, 3L, 4L),
    rob          = c("L", "L", "H", "M"),
    indirectness = c(1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

# ---- 1. Alias renaming + value auto-mapping on w2i-shaped data -------------
test_that("normalize_injected_data renames aliases and maps rob/indirectness", {
  out <- normalize_injected_data(w2i_shaped(), "binary")

  expect_true(all(c("studlab", "treat", "n", "event",
                    "rob", "indirectness") %in% names(out)))
  expect_false(any(c("id", "t", "r") %in% names(out)))
  expect_equal(out$studlab, c("S1", "S1", "S2", "S2"))
  expect_equal(out$event,   c(9L, 4L, 3L, 4L))
  expect_equal(out$rob, c("low", "low", "high", "some concerns"))
  expect_equal(out$indirectness, rep("low", 4))
})

# ---- 2. Missing rob / indirectness columns default to "low" ----------------
test_that("normalize_injected_data defaults absent rob/indirectness to low", {
  df  <- w2i_shaped()[, c("id", "t", "n", "r")]
  out <- normalize_injected_data(df, "binary")

  expect_equal(out$rob,          rep("low", 4))
  expect_equal(out$indirectness, rep("low", 4))
})

# ---- 3. Already-canonical data passes through unchanged --------------------
test_that("normalize_injected_data leaves canonical data untouched", {
  df <- data.frame(
    studlab      = c("S1", "S1"),
    treat        = c("A", "B"),
    n            = c(10L, 12L),
    event        = c(2L, 3L),
    rob          = c("low", "some concerns"),
    indirectness = c("low", "high"),
    stringsAsFactors = FALSE
  )
  out <- normalize_injected_data(df, "binary")

  expect_identical(out, df)
})

# ---- 4. Full injected path: load_w2i() -> normalize -> convert_binary ------
test_that("load_w2i() data survives the full injected binary path", {
  testthat::skip_if_not_installed("nmatools")

  d      <- nmatools::load_w2i()
  result <- convert_binary(normalize_injected_data(d, "binary"), "OR")

  expect_null(result$error)
  expect_equal(nrow(result$data), 15L)
  expect_equal(length(unique(result$data$studlab)), 9L)
  expect_true(all(as.character(result$data$rob) %in%
                    c("low", "some concerns", "high")))
})
