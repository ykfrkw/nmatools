# utils_trim.R -- internal helpers that size, save, trim and page plot PDFs
#
# Purpose : Write plots to PDF so that nothing is clipped and every page keeps a
#           uniform white margin after whitespace trimming.
# Inputs  : A plot expression / function plus size hints (inches).
# Outputs : PDF files (rasterised by magick when trimming or paging).
# Depends : grDevices, magick.
#
# Why measuring: meta::forest() lays its grid out in absolute units (text
# widths, fixed row heights) and centres it on the device. If the device is
# smaller than that layout, the overflow is cut off on BOTH sides, so size
# formulas that miss a column or a row silently lose content. Instead we draw
# once on a generous scratch device, find the real ink extent, and size the
# final device from that.

TRIM_DENSITY_DPI   <- 150L  # raster resolution of trimmed / paged output
MEASURE_DENSITY_DPI <- 40L  # cheap resolution for the measuring pass
MEASURE_PAD_IN     <- 0.5   # slack added on every side of a measured plot
MEASURE_MAX_TRIES  <- 4L    # doublings of the scratch device before giving up
INK_SAFETY_PX      <- 2L    # keep anti-aliased glyph edges the fuzz misses
A4_HEIGHT_IN       <- 11.69
RULE_MAX_PX        <- 6L    # ink pixels per row still treated as blank (rules)

# -- Pixel analysis -------------------------------------------------------------

# Logical matrix (width x height) of "ink" pixels: darker than white by more
# than `fuzz` percent. Transparent areas are flattened onto white first.
.ink_matrix <- function(frame, fuzz) {
  flat <- magick::image_flatten(magick::image_background(frame, "white"))
  gray <- magick::image_data(flat, channels = "gray")
  intensity <- as.integer(gray[1L, , ])
  dim(intensity) <- dim(gray)[2:3]
  intensity < 255 * (1 - fuzz / 100)
}

# Bounding box of the ink in one frame (1-based pixel indices), or NULL when
# the frame is blank.
.content_bbox <- function(frame, fuzz) {
  is_ink   <- .ink_matrix(frame, fuzz)
  ink_cols <- which(rowSums(is_ink) > 0L)
  ink_rows <- which(colSums(is_ink) > 0L)
  if (length(ink_cols) == 0L) return(NULL)
  list(
    x0 = min(ink_cols), x1 = max(ink_cols),
    y0 = min(ink_rows), y1 = max(ink_rows),
    width = nrow(is_ink), height = ncol(is_ink)
  )
}

# Crop one frame to its ink (optionally rows only) and add a white border of
# `margin_px` on all four sides.
.trim_frame <- function(frame, fuzz, margin_px, is_vertical_only = FALSE) {
  frame <- magick::image_flatten(magick::image_background(frame, "white"))
  bbox  <- .content_bbox(frame, fuzz)
  if (!is.null(bbox)) {
    x0 <- if (is_vertical_only) 1L else max(1L, bbox$x0 - INK_SAFETY_PX)
    x1 <- if (is_vertical_only) bbox$width else
      min(bbox$width, bbox$x1 + INK_SAFETY_PX)
    y0 <- max(1L, bbox$y0 - INK_SAFETY_PX)
    y1 <- min(bbox$height, bbox$y1 + INK_SAFETY_PX)
    frame <- magick::image_crop(
      frame,
      geometry = magick::geometry_area(x1 - x0 + 1L, y1 - y0 + 1L,
                                       x0 - 1L, y0 - 1L)
    )
    frame <- magick::image_repage(frame)
  }
  if (margin_px <= 0L) return(frame)
  magick::image_border(frame, color = "white",
                       geometry = sprintf("%dx%d", margin_px, margin_px))
}

.margin_px <- function(margin_in, density) {
  max(0L, as.integer(round(margin_in * density)))
}

# -- Trimming -------------------------------------------------------------------

# Trim every page of a PDF to its content and add a uniform white margin.
.trim_pdf <- function(file, fuzz = 30L, margin = 0.2,
                      density = TRIM_DENSITY_DPI) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("magick package not available; skipping trim.")
    return(invisible(NULL))
  }
  img <- tryCatch(
    magick::image_read_pdf(file, density = density),
    error = function(e) { warning("magick could not read ", file); NULL }
  )
  if (is.null(img)) return(invisible(NULL))
  margin_px <- .margin_px(margin, density)
  pages <- lapply(seq_along(img), function(i) {
    .trim_frame(img[i], fuzz = fuzz, margin_px = margin_px)
  })
  magick::image_write(do.call(c, pages), path = file, format = "pdf",
                      density = density)
  invisible(file)
}

# -- Saving ---------------------------------------------------------------------

# Save a base-R / grid plot expression to PDF, optionally trimming whitespace.
.save_plot <- function(file, width, height, expr, trim = TRUE,
                       trim_fuzz = 30L, trim_margin = 0.2) {
  grDevices::pdf(file, width = width, height = height)
  tryCatch(force(expr), finally = grDevices::dev.off())
  if (trim) .trim_pdf(file, fuzz = trim_fuzz, margin = trim_margin)
  invisible(file)
}

# Draw `plot_fn` on a scratch device and return the ink extent in inches
# (c(width, height)). The device grows until the ink no longer touches an
# edge, i.e. until nothing was clipped. NULL if measuring is impossible.
.measure_plot <- function(plot_fn, width, height,
                          density = MEASURE_DENSITY_DPI) {
  if (!requireNamespace("magick", quietly = TRUE)) return(NULL)
  scratch_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(scratch_file), add = TRUE)
  for (attempt in seq_len(MEASURE_MAX_TRIES)) {
    grDevices::pdf(scratch_file, width = width, height = height)
    # Warnings/messages are emitted again by the final draw; do not double them.
    tryCatch(suppressWarnings(suppressMessages(plot_fn())),
             finally = grDevices::dev.off())
    img  <- tryCatch(magick::image_read_pdf(scratch_file, density = density),
                     error = function(e) NULL)
    if (is.null(img)) return(NULL)
    bbox <- .content_bbox(img[1L], fuzz = 5L)
    if (is.null(bbox)) return(NULL)
    is_clipped_x <- bbox$x0 <= 1L || bbox$x1 >= bbox$width
    is_clipped_y <- bbox$y0 <= 1L || bbox$y1 >= bbox$height
    if (!is_clipped_x && !is_clipped_y) {
      return(c(width  = (bbox$x1 - bbox$x0 + 1L) / density,
               height = (bbox$y1 - bbox$y0 + 1L) / density))
    }
    if (is_clipped_x) width  <- width * 2
    if (is_clipped_y) height <- height * 2
  }
  warning("Plot still touches the device edge after ", MEASURE_MAX_TRIES,
          " enlargements; output may be clipped.")
  NULL
}

# Device size for a plot whose layout is in absolute units (meta::forest):
# the measured ink extent plus slack, never smaller than needed. The hints only
# seed the scratch device (made generous so one pass is usually enough).
.fit_device_size <- function(plot_fn, width_hint, height_hint) {
  measured <- .measure_plot(plot_fn, width = width_hint * 2,
                            height = height_hint * 2)
  if (is.null(measured)) return(c(width = width_hint, height = height_hint))
  measured + 2 * MEASURE_PAD_IN
}

# Save a forest-type plot at its measured size (single PDF).
.save_fitted_plot <- function(file, plot_fn, width_hint, height_hint,
                              trim = TRUE, trim_fuzz = 30L,
                              trim_margin = 0.2) {
  size <- .fit_device_size(plot_fn, width_hint, height_hint)
  .save_plot(file = file, width = size[["width"]], height = size[["height"]],
             trim = trim, trim_fuzz = trim_fuzz, trim_margin = trim_margin,
             expr = plot_fn())
}

# -- Paging ---------------------------------------------------------------------

# Row offsets (0-based) where pages start. Each cut goes through the longest
# run of blank rows in the lower part of the page, so text lines and whole
# comparison blocks are not sliced; only a page without any blank row is cut
# hard at the page limit.
.find_page_starts <- function(has_ink_row, page_px) {
  total_px <- length(has_ink_row)
  starts   <- 0L
  y0       <- 0L
  while (total_px - y0 > page_px) {
    window_lo <- y0 + floor(page_px * 0.5) + 1L
    window_hi <- y0 + page_px
    window    <- window_lo:window_hi
    blank_run <- rle(!has_ink_row[window])
    run_end   <- cumsum(blank_run$lengths)
    run_len   <- ifelse(blank_run$values, blank_run$lengths, 0L)
    if (max(run_len) == 0L) {
      y0 <- window_hi
    } else {
      best <- max(which(run_len == max(run_len)))
      y0   <- window_lo - 1L + run_end[best] - run_len[best] %/% 2L
    }
    starts <- c(starts, as.integer(y0))
  }
  starts
}

# Render a potentially tall plot at its measured size, then split it into
# A4-height pages, each trimmed vertically and given the white margin.
# A plot that fits one page is saved without a _p suffix.
.save_plot_paged <- function(file_base, plot_fn, full_width, full_height,
                             a4_height_in = A4_HEIGHT_IN,
                             density = TRIM_DENSITY_DPI,
                             trim = TRUE, trim_fuzz = 30L,
                             trim_margin = 0.2) {
  size <- .fit_device_size(plot_fn, full_width, full_height)
  full_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(full_file), add = TRUE)
  grDevices::pdf(full_file, width = size[["width"]], height = size[["height"]])
  tryCatch(plot_fn(), finally = grDevices::dev.off())

  img <- if (requireNamespace("magick", quietly = TRUE)) {
    tryCatch(magick::image_read_pdf(full_file, density = density),
             error = function(e) { warning("magick failed on ", full_file); NULL })
  }
  if (is.null(img)) {
    file.copy(full_file, paste0(file_base, ".pdf"), overwrite = TRUE)
    return(invisible(NULL))
  }

  fuzz      <- if (trim) trim_fuzz else 0L
  margin_px <- if (trim) .margin_px(trim_margin, density) else 0L
  img       <- .trim_frame(img[1L], fuzz = fuzz, margin_px = 0L)
  page_px   <- round(a4_height_in * density) - 2L * margin_px
  # Rows crossed only by thin vertical rules (the forest's line of no effect)
  # still count as blank, otherwise no row would ever qualify as a cut.
  ink_per_row <- colSums(.ink_matrix(img, max(fuzz, 5L)))
  has_ink_row <- ink_per_row > max(RULE_MAX_PX, 0.005 * magick::image_info(img)$width)
  total_w   <- magick::image_info(img)$width
  total_h   <- length(has_ink_row)
  starts    <- .find_page_starts(has_ink_row, page_px)
  ends      <- c(starts[-1L], total_h)

  for (p in seq_along(starts)) {
    page <- magick::image_crop(
      img,
      geometry = magick::geometry_area(total_w, ends[p] - starts[p], 0L,
                                       starts[p])
    )
    page <- magick::image_repage(page)
    if (trim) {
      page <- .trim_frame(page, fuzz = fuzz, margin_px = margin_px,
                          is_vertical_only = TRUE)
    }
    suffix <- if (length(starts) > 1L) paste0("_p", p) else ""
    magick::image_write(page, path = paste0(file_base, suffix, ".pdf"),
                        format = "pdf", density = density)
  }
  invisible(NULL)
}
