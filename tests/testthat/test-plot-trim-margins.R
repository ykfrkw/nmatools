# Tests for figure sizing / trimming (R/utils_trim.R, R/utils_plot.R):
#   * trimmed PDFs keep a uniform white border of `trim_margin` inches,
#   * paged output never slices through a text line and every page has a border,
#   * netsplit forests are sized from the drawn layout, so all comparisons fit.

TEST_DENSITY <- 150L

# Ink edges (blank pixels between the image border and the first ink pixel).
ink_edges <- function(frame) {
  is_ink  <- nmatools:::.ink_matrix(frame, fuzz = 30L)  # width x height
  has_col <- rowSums(is_ink) > 0L
  has_row <- colSums(is_ink) > 0L
  first_true <- function(v) which(v)[1L] - 1L
  c(left = first_true(has_col), right = first_true(rev(has_col)),
    top = first_true(has_row), bottom = first_true(rev(has_row)))
}

# Number of separate text lines (runs of ink rows) in a frame.
count_ink_bands <- function(frame) {
  has_row <- colSums(nmatools:::.ink_matrix(frame, fuzz = 30L)) > 0L
  sum(rle(has_row)$values)
}

draw_filled_box <- function() {
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0.2, 0.2, 0.8, 0.8, col = "black")
}

test_that("trimmed PDF has a uniform white border of trim_margin", {
  testthat::skip_if_not_installed("magick")
  file <- withr::local_tempfile(fileext = ".pdf")

  nmatools:::.save_plot(file, width = 5, height = 4, trim = TRUE,
                        trim_margin = 0.2, expr = draw_filled_box())

  img   <- magick::image_read_pdf(file, density = TEST_DENSITY)
  edges <- ink_edges(img[1L])
  expected_px <- 0.2 * TEST_DENSITY
  expect_true(all(edges >= expected_px - 3))
  expect_true(all(edges <= expected_px + 6))
  expect_lt(max(edges) - min(edges), 5)
})

test_that("trim_margin = 0 gives a tight crop", {
  testthat::skip_if_not_installed("magick")
  file <- withr::local_tempfile(fileext = ".pdf")

  nmatools:::.save_plot(file, width = 5, height = 4, trim = TRUE,
                        trim_margin = 0, expr = draw_filled_box())

  img <- magick::image_read_pdf(file, density = TEST_DENSITY)
  expect_true(all(ink_edges(img[1L]) <= 4))
})

test_that("paged output keeps every text line and borders every page", {
  testthat::skip_if_not_installed("magick")
  n_lines   <- 60L
  file_base <- file.path(withr::local_tempdir(), "tall")

  draw_lines <- function() {
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, n_lines + 1))
    graphics::text(0.5, seq_len(n_lines), paste("Line", seq_len(n_lines)))
  }

  nmatools:::.save_plot_paged(file_base, plot_fn = draw_lines,
                              full_width = 4, full_height = 15,
                              a4_height_in = 4, trim = TRUE,
                              trim_margin = 0.2)

  pages <- sort(Sys.glob(paste0(file_base, "_p*.pdf")))
  expect_gt(length(pages), 1L)

  bands <- 0L
  for (page in pages) {
    img <- magick::image_read_pdf(page, density = TEST_DENSITY)
    expect_lte(magick::image_info(img)$height, 4 * TEST_DENSITY + 2)
    expect_true(all(ink_edges(img[1L]) >= 0.2 * TEST_DENSITY - 3))
    bands <- bands + count_ink_bands(img[1L])
  }
  # A line cut between two pages would be counted twice.
  expect_equal(bands, n_lines)
})

# Fully connected network: every pair of `trts` compared in one 2-arm study,
# so the netsplit has choose(length(trts), 2) comparisons.
make_full_network <- function(trts) {
  pairs <- utils::combn(trts, 2L)
  n_pairs <- ncol(pairs)
  event_seq <- 10 + (seq_len(2L * n_pairs) * 7) %% 23
  pw <- meta::pairwise(
    treat   = list(pairs[1L, ], pairs[2L, ]),
    event   = list(event_seq[seq_len(n_pairs)], event_seq[-seq_len(n_pairs)]),
    n       = list(rep(100, n_pairs), rep(100, n_pairs)),
    studlab = paste0("s", seq_len(n_pairs)),
    sm      = "OR"
  )
  suppressWarnings(netmeta::netmeta(pw, common = FALSE))
}

fit_netsplit <- function(net) {
  ns <- netmeta::netsplit(net, prediction = TRUE)
  plot_fn <- function() {
    meta::forest(ns, separate = TRUE, prediction = TRUE, show = "all")
  }
  hint <- nmatools:::.netsplit_height_hint(nmatools:::.netsplit_n_comparisons(ns))
  list(ns = ns, plot_fn = plot_fn,
       size = nmatools:::.fit_device_size(plot_fn, 8, hint))
}

test_that("netsplit forest is sized so all comparisons fit", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("magick")

  fit_big   <- fit_netsplit(make_full_network(LETTERS[1:7]))  # 21 comparisons
  fit_small <- fit_netsplit(make_full_network(LETTERS[1:3]))  #  3 comparisons

  # netmeta >= 3.x keeps comparisons in `comparison`, not `comparisons`.
  expect_equal(nmatools:::.netsplit_n_comparisons(fit_big$ns), 21L)
  expect_equal(nmatools:::.netsplit_n_comparisons(fit_small$ns), 3L)
  expect_gt(fit_big$size[["height"]], 4 * fit_small$size[["height"]])

  # Drawn at the fitted size, no ink touches the device edge (nothing clipped).
  for (fit in list(fit_big, fit_small)) {
    file <- withr::local_tempfile(fileext = ".pdf")
    nmatools:::.save_plot(file, width = fit$size[["width"]],
                          height = fit$size[["height"]], trim = FALSE,
                          expr = fit$plot_fn())
    img <- magick::image_read_pdf(file, density = 40L)
    expect_true(all(ink_edges(img[1L]) > 0L))
  }
})

test_that("netgraph margins widen with long treatment names", {
  short <- nmatools:::.netgraph_mar(c("A", "B"))
  long  <- nmatools:::.netgraph_mar(c("A", strrep("x", 30)))
  expect_length(short, 4L)
  expect_gt(long[1L], short[1L])
})
