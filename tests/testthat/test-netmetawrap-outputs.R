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
