# Regression tests for the dual/quad upper-triangle bug in color_league().
#
# These tests lean on the netleague() convention that nmatools relies on:
#   netleague(net)$random  ->  lower-left  = NMA estimate
#                              upper-right = direct (pairwise) estimate
# with both triangles pointing the same way.
#
# The upper-right triangle of a dual-outcome table must show outcome 2's
# NETWORK estimate.  Before the fix the code read league_mat2[i, j], which is
# netleague()'s upper triangle for outcome 2 -- the DIRECT estimate -- while
# .cell_entry() coloured the same cell from outcome 2's NMA matrix.
# The fix reads league_mat2[j, i] instead.  The same bug (and the same fix)
# applies to league_mat4, the lower sub-row of the upper-right triangle in
# quad mode.

make_two_outcome_nets <- function() {
  skip_if_not_installed("netmeta")
  data("Senn2013", package = "netmeta", envir = environment())
  d <- get("Senn2013", envir = environment())
  keep <- c("plac", "metf", "rosi", "acar")
  d <- d[d$treat1 %in% keep & d$treat2 %in% keep, ]

  # Scale (do not shift) so that multi-arm consistency is preserved while the
  # second outcome still differs from the first.
  d2 <- d
  d2$TE <- d2$TE * 0.6

  list(
    x  = netmeta::netmeta(TE, seTE, treat1, treat2, studlab, data = d,
                          sm = "MD"),
    x2 = netmeta::netmeta(TE, seTE, treat1, treat2, studlab, data = d2,
                          sm = "MD")
  )
}

# Read the written sheet back as a character matrix.
read_sheet <- function(path) {
  m <- openxlsx::read.xlsx(path, colNames = FALSE, sheet = 1)
  as.matrix(m)
}


test_that("color_league() dual mode shows outcome 2's NMA estimate on top", {
  skip_if_not_installed("openxlsx")
  nets <- make_two_outcome_nets()
  path <- file.path(tempdir(), "cl_dual.xlsx")
  on.exit(unlink(path), add = TRUE)

  suppressMessages(
    color_league(x = nets$x, x2 = nets$x2, sort_by = "alphabet",
                 palette_type = "solid", wrap_ci = FALSE, file = path)
  )
  sheet <- read_sheet(path)

  trts <- sort(nets$x$trts)
  lg2  <- as.matrix(netmeta::netleague(nets$x2, seq = trts, digits = 2,
                                       bracket = "(", separator = " to ",
                                       common = FALSE, random = TRUE)$random)
  dimnames(lg2) <- list(trts, trts)

  # color_league() rewrites the CI separator via .reformat_cell(), so
  # normalise both sides before comparing.
  norm <- function(v) gsub("[[:space:]]+", "",
                           gsub(" to ", ";", v, fixed = TRUE))

  checked <- 0L
  for (i in seq_along(trts)) {
    for (j in seq_along(trts)) {
      if (i >= j) next
      cell <- norm(sheet[i, j])
      # Fixed behaviour: the transposed (lower-triangle => NMA) cell
      expect_identical(cell, norm(lg2[j, i]))
      # Buggy behaviour: netleague()'s own upper triangle (direct estimate).
      # Only assert inequality where the two actually differ.
      if (!identical(norm(lg2[j, i]), norm(lg2[i, j])))
        expect_false(identical(cell, norm(lg2[i, j])))
      checked <- checked + 1L
    }
  }
  expect_gt(checked, 0L)
})


test_that("color_league() quad mode shows outcomes 2 and 4 as NMA estimates", {
  skip_if_not_installed("openxlsx")
  nets <- make_two_outcome_nets()
  path <- file.path(tempdir(), "cl_quad.xlsx")
  on.exit(unlink(path), add = TRUE)

  suppressMessages(
    color_league(x = nets$x, x2 = nets$x2, x3 = nets$x, x4 = nets$x2,
                 sort_by = "alphabet", palette_type = "solid",
                 wrap_ci = FALSE, file = path)
  )
  sheet <- read_sheet(path)

  trts <- sort(nets$x$trts)
  lg2  <- as.matrix(netmeta::netleague(nets$x2, seq = trts, digits = 2,
                                       bracket = "(", separator = " to ",
                                       common = FALSE, random = TRUE)$random)
  dimnames(lg2) <- list(trts, trts)

  # color_league() rewrites the CI separator via .reformat_cell(), so
  # normalise both sides before comparing.
  norm <- function(v) gsub("[[:space:]]+", "",
                           gsub(" to ", ";", v, fixed = TRUE))

  for (i in seq_along(trts)) {
    for (j in seq_along(trts)) {
      if (i >= j) next
      top <- norm(sheet[2L * i - 1L, j])   # outcome 2
      bot <- norm(sheet[2L * i, j])        # outcome 4
      expect_identical(top, norm(lg2[j, i]))
      expect_identical(bot, norm(lg2[j, i]))
      if (!identical(norm(lg2[j, i]), norm(lg2[i, j]))) {
        expect_false(identical(top, norm(lg2[i, j])))
        expect_false(identical(bot, norm(lg2[i, j])))
      }
    }
  }
})


test_that("color_league() single-outcome mode keeps direct estimates on top", {
  skip_if_not_installed("openxlsx")
  nets <- make_two_outcome_nets()
  path <- file.path(tempdir(), "cl_single.xlsx")
  on.exit(unlink(path), add = TRUE)

  suppressMessages(
    color_league(x = nets$x, sort_by = "alphabet", palette_type = "solid",
                 wrap_ci = FALSE, file = path)
  )
  sheet <- read_sheet(path)

  trts <- sort(nets$x$trts)
  lg   <- as.matrix(netmeta::netleague(nets$x, seq = trts, digits = 2,
                                       bracket = "(", separator = " to ",
                                       common = FALSE, random = TRUE)$random)
  dimnames(lg) <- list(trts, trts)

  # color_league() rewrites the CI separator via .reformat_cell(), so
  # normalise both sides before comparing.
  norm <- function(v) gsub("[[:space:]]+", "",
                           gsub(" to ", ";", v, fixed = TRUE))

  for (i in seq_along(trts))
    for (j in seq_along(trts))
      if (i < j)
        expect_identical(norm(sheet[i, j]), norm(lg[i, j]))
})
