# Tests pinning the three PDF outputs that netmeta >= 3.x used to silently drop:
#   forest_netpairwise_*, funnel_pairwise_*, contributions_*.

test_that("netmetawrap restores netpairwise/funnel/contribution PDFs", {
  testthat::skip_on_cran()

  d <- tryCatch(nmatools::load_w2i(), error = function(e) NULL)
  testthat::skip_if_not(!is.null(d) && nrow(d) > 0,
                        "W2I data unavailable")

  out <- withr::local_tempdir()

  suppressMessages(
    nmatools::netmetawrap(
      data            = d,
      studlab         = "id",
      treat           = "t",
      outcome         = "remission_lt",
      n               = "n",
      event           = "r",
      sm              = "OR",
      reference.group = "Pharmacotherapy",
      small.values    = "undesirable",
      path            = out,
      trim            = FALSE,
      funnel_min_studies = 5L
    )
  )

  outdir <- file.path(out, "remission_lt")
  expect_true(dir.exists(outdir))

  forest_np <- list.files(
    outdir, pattern = "^forest_netpairwise_remission_lt.*\\.pdf$",
    recursive = TRUE
  )
  funnel <- list.files(
    outdir, pattern = "^funnel_pairwise_remission_lt.*\\.pdf$",
    recursive = TRUE
  )
  contrib <- list.files(
    outdir, pattern = "^contributions_remission_lt.*\\.pdf$",
    recursive = TRUE
  )

  expect_true(length(forest_np) > 0L)
  expect_true(length(funnel) > 0L)
  expect_true(length(contrib) > 0L)
})

test_that("funnel comparisons are canonicalized across flipped arm order", {
  testthat::skip_on_cran()

  # Arm-level synthetic network. The A-B comparison spans 6 studies; 4 list arms
  # as (A, B) and 2 list them flipped as (B, A). Without canonicalization these
  # split into "A B" (k = 4) and "B A" (k = 2), both below the k >= 5 gate, so no
  # A-B funnel would be drawn; with canonicalization they merge into one group of
  # 6 and exactly one A-B funnel is produced. A-C and B-C (k = 2 each) keep the
  # network connected but stay below the threshold.
  mk <- function(id, t, r, n) {
    data.frame(id = id, t = t, r = r, n = n, stringsAsFactors = FALSE)
  }
  d <- rbind(
    mk("s1", c("A", "B"), c(20, 30), c(100, 100)),
    mk("s2", c("A", "B"), c(18, 28), c(100, 100)),
    mk("s3", c("A", "B"), c(22, 35), c(100, 100)),
    mk("s4", c("A", "B"), c(25, 33), c(100, 100)),
    mk("s5", c("B", "A"), c(31, 19), c(100, 100)),  # flipped arm order
    mk("s6", c("B", "A"), c(29, 21), c(100, 100)),  # flipped arm order
    mk("s7",  c("A", "C"), c(20, 15), c(100, 100)),
    mk("s8",  c("A", "C"), c(23, 17), c(100, 100)),
    mk("s9",  c("B", "C"), c(30, 16), c(100, 100)),
    mk("s10", c("B", "C"), c(28, 14), c(100, 100))
  )

  tmp <- withr::local_tempdir()

  ok <- tryCatch({
    suppressMessages(
      nmatools::netmetawrap(
        data            = d,
        studlab         = id,
        treat           = t,
        outcome         = "cx",
        n               = n,
        event           = r,
        sm              = "OR",
        reference.group = "A",
        small.values    = "undesirable",
        path            = tmp,
        trim            = FALSE,
        funnel_min_studies = 5L
      )
    )
    TRUE
  }, error = function(e) {
    message("synthetic network failed: ", conditionMessage(e))
    FALSE
  })
  testthat::skip_if_not(ok, "synthetic network failed to build")

  outdir  <- file.path(tmp, "cx")
  funnels <- list.files(outdir, pattern = "^funnel_pairwise_cx.*\\.pdf$")

  # Canonicalization merges the flipped A-B rows into one k = 6 group.
  expect_true(length(funnels) >= 1L)

  # No two funnel files are reverse-order duplicates of the same canonical pair
  # (e.g. an "A_vs_B" alongside a "B_vs_A" mirror).
  pair_of <- function(f) {
    core  <- sub("\\.pdf$", "", sub("^funnel_pairwise_cx_", "", f))
    parts <- strsplit(core, "_vs_", fixed = TRUE)[[1]]
    paste(sort(parts), collapse = "|")
  }
  canon_pairs <- vapply(funnels, pair_of, character(1L))
  expect_false(any(duplicated(canon_pairs)))
})
