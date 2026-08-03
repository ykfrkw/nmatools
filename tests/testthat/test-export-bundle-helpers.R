# Tests for the bundle-export helpers in inst/app/modules/_export_helpers.R.
# Focus: write_pairwise_appendix_docx() must survive builder_fn return values
# that omit fields (error / funnel_path / k). Those probes used to be
# `!is.na(res$field)`, which is `if (logical(0))` -> "argument is of length
# zero" whenever the field is absent. Loading pattern mirrors
# test-robmen-bg-plots.R.

helper_path <- system.file("app", "modules", "_export_helpers.R",
                           package = "nmatools")
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  helper_path <- testthat::test_path("..", "..", "inst", "app",
                                     "modules", "_export_helpers.R")
}
source(helper_path, local = TRUE, encoding = "UTF-8")

# _export_helpers.R uses %||% but doesn't define it (app.R sources the
# operator helpers first); provide it locally for standalone sourcing.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

skip_without_officer <- function() {
  testthat::skip_if_not_installed("officer")
}

# Minimal stand-ins: write_pairwise_appendix_docx() only reads net$sm and the
# t1/t2/studlab/y/se columns, so a bare list and a small data.frame suffice.
fake_net <- function() list(sm = "MD")

fake_pairwise_df <- function() {
  data.frame(
    t1      = c("A", "A", "B", "B"),
    t2      = c("B", "B", "C", "C"),
    studlab = c("S1", "S2", "S3", "S4"),
    y       = c(-0.30, -0.40, 0.20, 0.10),
    se      = c(0.10, 0.12, 0.09, 0.11),
    stringsAsFactors = FALSE)
}

out_docx <- function() {
  file.path(tempdir(), paste0("pairwise_appendix_test_",
                              sample.int(.Machine$integer.max, 1), ".docx"))
}

# A real PNG so body_add_img() has something valid to embed.
stub_png <- function() {
  p <- file.path(tempdir(), paste0("stub_", sample.int(1e6, 1), ".png"))
  grDevices::png(p, width = 200, height = 150)
  plot.new(); title(main = "stub")
  grDevices::dev.off()
  p
}

# ---- 1. builder returns a list with NO $error element ----------------------
test_that("write_pairwise_appendix_docx: builder without $error still writes", {
  skip_without_officer()
  png <- stub_png()
  f   <- out_docx()

  # Note the absent `error` field — this is what used to blow up.
  builder <- function(comp, study_subset, sm, run_id, out_dir, ...) {
    list(comp_key = comp$comp_key, forest_path = png,
         funnel_path = NA_character_, k = nrow(study_subset))
  }

  expect_no_error(
    write_pairwise_appendix_docx(
      net = fake_net(), pairwise_df = fake_pairwise_df(),
      sm = "MD", file = f, builder_fn = builder))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
})

# ---- 2. builder omits $funnel_path and $k as well --------------------------
test_that("write_pairwise_appendix_docx: absent funnel_path/k are tolerated", {
  skip_without_officer()
  png <- stub_png()
  f   <- out_docx()

  # Only forest_path is reported; k must fall back to the subset row count
  # and the funnel branch must not compare NULL < 10.
  builder <- function(comp, study_subset, sm, run_id, out_dir, ...) {
    list(forest_path = png)
  }

  expect_no_error(
    write_pairwise_appendix_docx(
      net = fake_net(), pairwise_df = fake_pairwise_df(),
      sm = "MD", file = f, builder_fn = builder))
  expect_true(file.exists(f))
})

# ---- 3. builder returns a tibble-like row without an `error` column --------
test_that("write_pairwise_appendix_docx: no uninitialised-column warning", {
  skip_without_officer()
  testthat::skip_if_not_installed("tibble")
  png <- stub_png()
  f   <- out_docx()

  # `$` on a missing tibble column warns "Unknown or uninitialised column";
  # the helper now goes through names() so nothing is emitted.
  builder <- function(comp, study_subset, sm, run_id, out_dir, ...) {
    tibble::tibble(forest_path = png, k = nrow(study_subset))
  }

  expect_no_warning(
    write_pairwise_appendix_docx(
      net = fake_net(), pairwise_df = fake_pairwise_df(),
      sm = "MD", file = f, builder_fn = builder))
  expect_true(file.exists(f))
})

# ---- 4. No renderable section — the !any_section paragraph must write ------
test_that("write_pairwise_appendix_docx: writes when no section renders", {
  skip_without_officer()
  f <- out_docx()

  # forest_path is always NA, so any_section stays FALSE and the fallback
  # paragraph runs. That call used to pass two positional strings to
  # body_add_par(), colliding with the named style argument.
  builder <- function(comp, study_subset, sm, run_id, out_dir, ...) {
    list(forest_path = NA_character_, funnel_path = NA_character_,
         k = nrow(study_subset), error = "stubbed failure")
  }

  expect_no_error(
    write_pairwise_appendix_docx(
      net = fake_net(), pairwise_df = fake_pairwise_df(),
      sm = "MD", file = f, builder_fn = builder))
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
})

# ---- 5. builder that throws — caught per comparison, document still made ---
test_that("write_pairwise_appendix_docx: a throwing builder is isolated", {
  skip_without_officer()
  f <- out_docx()

  builder <- function(comp, study_subset, sm, run_id, out_dir, ...) {
    stop("builder exploded")
  }

  expect_no_error(
    write_pairwise_appendix_docx(
      net = fake_net(), pairwise_df = fake_pairwise_df(),
      sm = "MD", file = f, builder_fn = builder))
  expect_true(file.exists(f))
})
