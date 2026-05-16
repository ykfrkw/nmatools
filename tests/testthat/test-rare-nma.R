# Tests for rare-event NMA workflow.

# Build a synthetic, sparse 3-treatment network where MH-NMA should fire.
make_rare_net <- function(seed = 42) {
  set.seed(seed)
  trts <- c("A", "B", "C")
  studies <- list(
    list(id = "S01", arms = c("A", "B")),
    list(id = "S02", arms = c("A", "B")),
    list(id = "S03", arms = c("A", "C")),
    list(id = "S04", arms = c("A", "C")),
    list(id = "S05", arms = c("B", "C")),
    list(id = "S06", arms = c("B", "C")),
    list(id = "S07", arms = c("A", "B")),
    list(id = "S08", arms = c("A", "C")),
    list(id = "S09", arms = c("A", "B", "C"))
  )
  rows <- do.call(rbind, lapply(studies, function(s) {
    n <- sample(80:160, length(s$arms), replace = TRUE)
    # ~0.3 % event rate baseline; treatments slightly differ
    p <- c(A = 0.003, B = 0.002, C = 0.005)[s$arms]
    e <- vapply(seq_along(s$arms), function(i)
                  stats::rbinom(1, n[i], p[i]), integer(1L))
    data.frame(id = s$id, t = s$arms, n = n, r = e,
               stringsAsFactors = FALSE)
  }))
  rows
}

make_pairwise_binary <- function(d) {
  meta::pairwise(
    data    = d,
    studlab = id,
    treat   = t,
    n       = n,
    event   = r,
    sm      = "OR"
  )
}

# ── Diagnostics ───────────────────────────────────────────────────────────────

test_that(".rare_nma_diagnostics flags rare data", {
  d <- make_rare_net()
  pw <- make_pairwise_binary(d)
  diag <- nmatools:::.rare_nma_diagnostics(pw)
  expect_s3_class(diag, "nma_rare_diagnostics")
  expect_true(diag$rare_flow)
  expect_true(diag$event_rate_overall < 0.01)
  expect_gt(diag$zero_cell_k, 0L)
  expect_match(diag$recommendation, "Rare-event NMA workflow", fixed = TRUE)
})

test_that(".rare_nma_diagnostics does NOT flag rich W2I data", {
  d <- nmatools::load_w2i()
  pw <- make_pairwise_binary(d)
  diag <- nmatools:::.rare_nma_diagnostics(pw)
  expect_false(diag$rare_flow)
  expect_gt(diag$event_rate_overall, 0.10)
})

test_that("format() works for nma_rare_diagnostics", {
  d <- make_rare_net()
  pw <- make_pairwise_binary(d)
  diag <- nmatools:::.rare_nma_diagnostics(pw)
  txt <- format(diag)
  expect_true(any(grepl("rare_flow", txt)))
  expect_true(any(grepl("Per-treatment event rates", txt)))
})

# ── Sensitivity panel ─────────────────────────────────────────────────────────

test_that(".run_rare_nma_sensitivity returns 4 methods x (T-1) rows", {
  d <- make_rare_net()
  pw <- make_pairwise_binary(d)
  tab <- nmatools:::.run_rare_nma_sensitivity(
    pw, sm = "OR", reference.group = "A", small.values = "undesirable"
  )
  expect_s3_class(tab, "data.frame")
  expect_setequal(unique(tab$method_id),
                  c("MH", "NCH", "IV_FE_CC", "IV_RE_CC"))
  # Each method should have 2 non-reference treatment rows (B, C).
  per_method <- table(tab$method_id)
  expect_true(all(per_method >= 2L))
})

# ── netmetawrap integration ──────────────────────────────────────────────────

test_that("netmetawrap(rare_events='always') produces sensitivity files", {
  d <- make_rare_net()
  out <- tempfile("nmarare_"); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  suppressMessages(
    netmetawrap(
      data            = d,
      studlab         = "id",
      treat           = "t",
      outcome         = "synth_rare",
      n               = "n",
      event           = "r",
      sm              = "OR",
      reference.group = "A",
      path            = out,
      rare_events     = "always",
      trim            = FALSE
    )
  )
  outdir <- file.path(out, "synth_rare")
  expect_true(dir.exists(outdir))
  expect_true(file.exists(file.path(
    outdir, "rare_diagnostics_synth_rare.txt")))
  expect_true(file.exists(file.path(
    outdir, "method_comparison_synth_rare.xlsx")))
  expect_true(file.exists(file.path(
    outdir, "method_table_synth_rare.rds")))
  expect_true(file.exists(file.path(
    outdir, "forest_rare_sensitivity_synth_rare.pdf")))
})

test_that("netmetawrap(rare_events='auto') keeps IV path on rich data", {
  d <- nmatools::load_w2i()
  out <- tempfile("nmarare_"); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  suppressMessages(
    fit <- netmetawrap(
      data            = d,
      studlab         = "id",
      treat           = "t",
      outcome         = "remission_lt",
      n               = "n",
      event           = "r",
      sm              = "OR",
      reference.group = "Pharmacotherapy",
      path            = out,
      rare_events     = "auto",
      trim            = FALSE
    )
  )
  outdir <- file.path(out, "remission_lt")
  # Diagnostics file is always written under "auto" for binary data.
  expect_true(file.exists(file.path(
    outdir, "rare_diagnostics_remission_lt.txt")))
  # But the sensitivity bundle should NOT be produced.
  expect_false(file.exists(file.path(
    outdir, "method_comparison_remission_lt.xlsx")))
})

test_that("netmetawrap(rare_events='never') skips diagnostics", {
  d <- nmatools::load_w2i()
  out <- tempfile("nmarare_"); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  suppressMessages(
    netmetawrap(
      data            = d,
      studlab         = "id",
      treat           = "t",
      outcome         = "remission_lt",
      n               = "n",
      event           = "r",
      sm              = "OR",
      reference.group = "Pharmacotherapy",
      path            = out,
      rare_events     = "never",
      trim            = FALSE
    )
  )
  outdir <- file.path(out, "remission_lt")
  expect_false(file.exists(file.path(
    outdir, "rare_diagnostics_remission_lt.txt")))
})
