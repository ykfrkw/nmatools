# Tests for build_robmen_plots() — the pure helper that backs spec-13's
# async ROB-MEN plot pipeline. Lives in inst/app/modules/_robmen_bg_plots.R
# and is sourced standalone (no shiny needed). Mirrors the loading pattern
# used by test-grade-rob-direct.R.

helper_path <- system.file("app", "modules", "_robmen_bg_plots.R",
                           package = "nmatools")
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  helper_path <- testthat::test_path("..", "..", "inst", "app",
                                     "modules", "_robmen_bg_plots.R")
}
source(helper_path, local = TRUE)

skip_without_meta <- function() {
  testthat::skip_if_not_installed("meta")
}

# Per-test scratch directory; a fresh dir keeps file collisions impossible
# even if run_id repeats.
scratch_dir <- function() {
  d <- file.path(tempdir(), paste0("robmen_test_", as.integer(Sys.time()),
                                   "_", sample.int(.Machine$integer.max, 1)))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# ---- 1. Happy path: small comparison (k=2) -- forest only, funnel omitted ---
test_that("build_robmen_plots: k=2 writes forest PNG and skips funnel", {
  skip_without_meta()
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "A vs B"),
    study_subset = data.frame(
      studlab = c("Smith 2020", "Jones 2021"),
      y       = c(-0.30, -0.40),
      se      = c(0.10,  0.12)),
    sm      = "MD",
    run_id  = 1L,
    out_dir = od)

  expect_equal(res$comp_key, "A vs B")
  expect_equal(res$k, 2L)
  expect_true(is.na(res$error))
  expect_true(file.exists(res$forest_path))
  expect_gt(file.info(res$forest_path)$size, 0)
  expect_true(is.na(res$funnel_path))   # k < 10
})

# ---- 2. Single study (k=1) — forest still writes, no funnel ---------------
test_that("build_robmen_plots: k=1 still produces a forest plot", {
  skip_without_meta()
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "C vs D"),
    study_subset = data.frame(
      studlab = "Solo 2019",
      y       = -0.50,
      se      =  0.15),
    sm      = "MD",
    run_id  = 2L,
    out_dir = od)

  expect_equal(res$k, 1L)
  expect_true(is.na(res$error))
  expect_true(file.exists(res$forest_path))
  expect_true(is.na(res$funnel_path))
})

# ---- 3. Empty subset — both paths NA, k = 0, no error ---------------------
test_that("build_robmen_plots: empty subset returns NA paths and k=0", {
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "E vs F"),
    study_subset = data.frame(
      studlab = character(0),
      y       = numeric(0),
      se      = numeric(0)),
    out_dir = od)

  expect_equal(res$k, 0L)
  expect_true(is.na(res$forest_path))
  expect_true(is.na(res$funnel_path))
  expect_true(is.na(res$error))
})

# ---- 4. NULL subset — same shape, no error ---------------------------------
test_that("build_robmen_plots: NULL subset returns NA paths gracefully", {
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "G vs H"),
    study_subset = NULL,
    out_dir = od)

  expect_equal(res$k, 0L)
  expect_true(is.na(res$forest_path))
  expect_true(is.na(res$funnel_path))
  expect_true(is.na(res$error))
})

# ---- 5. Missing required columns — error message returned -----------------
test_that("build_robmen_plots: missing y/se columns surface as $error", {
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "I vs J"),
    study_subset = data.frame(studlab = "X", y = -0.3),  # se missing
    out_dir = od)

  expect_true(is.na(res$forest_path))
  expect_true(!is.na(res$error))
  expect_match(res$error, "Missing required column", fixed = FALSE)
  expect_match(res$error, "se", fixed = TRUE)
})

# ---- 6. All-NA / non-positive SE — k=0 with informative error -------------
test_that("build_robmen_plots: all-NA y or zero SE filtered out", {
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "K vs L"),
    study_subset = data.frame(
      studlab = c("a", "b"),
      y       = c(NA_real_, -0.30),
      se      = c(0.10,     0)),       # one NA, one zero SE
    out_dir = od)

  expect_equal(res$k, 0L)
  expect_true(is.na(res$forest_path))
  expect_true(is.na(res$funnel_path))
  expect_match(res$error, "No valid studies", fixed = TRUE)
})

# ---- 7. Large comparison (k=12) — funnel also written --------------------
test_that("build_robmen_plots: k>=10 produces both forest and funnel", {
  skip_without_meta()
  od  <- scratch_dir()
  k   <- 12L
  set.seed(42)
  res <- build_robmen_plots(
    comp         = list(comp_key = "M vs N"),
    study_subset = data.frame(
      studlab = paste0("Trial", seq_len(k)),
      y       = rnorm(k, mean = -0.3, sd = 0.15),
      se      = runif(k, min = 0.08, max = 0.20)),
    sm      = "MD",
    run_id  = 7L,
    out_dir = od)

  expect_equal(res$k, k)
  expect_true(is.na(res$error))
  expect_true(file.exists(res$forest_path))
  expect_true(file.exists(res$funnel_path))
  expect_gt(file.info(res$funnel_path)$size, 0)
})

# ---- 8. File names contain run_id (cache key) -----------------------------
test_that("build_robmen_plots: filename embeds run_id and a sanitised label", {
  skip_without_meta()
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = list(comp_key = "Drug X / Drug Y (high dose)"),
    study_subset = data.frame(
      studlab = "S1",
      y       = -0.4,
      se      =  0.1),
    run_id  = 99L,
    out_dir = od)

  expect_match(basename(res$forest_path), "robmen_forest_99_", fixed = TRUE)
  # No raw spaces / slashes / parens in filename
  expect_false(grepl("[ /()]", basename(res$forest_path)))
})

# ---- 9. comp argument as bare string still works --------------------------
test_that("build_robmen_plots: bare-string comp_key is accepted", {
  skip_without_meta()
  od  <- scratch_dir()
  res <- build_robmen_plots(
    comp         = "O vs P",
    study_subset = data.frame(
      studlab = "S",
      y       = -0.20,
      se      =  0.08),
    out_dir = od)

  expect_equal(res$comp_key, "O vs P")
  expect_true(file.exists(res$forest_path))
})

# ---- 10. data.frame row (one-row df) for comp also works ------------------
test_that("build_robmen_plots: 1-row data.frame as comp works", {
  skip_without_meta()
  od  <- scratch_dir()
  comp_df <- data.frame(comp_key = "Q vs R", t1 = "Q", t2 = "R",
                        stringsAsFactors = FALSE)
  res <- build_robmen_plots(
    comp         = comp_df,
    study_subset = data.frame(
      studlab = "S1", y = -0.10, se = 0.05),
    out_dir = od)

  expect_equal(res$comp_key, "Q vs R")
  expect_true(file.exists(res$forest_path))
})
