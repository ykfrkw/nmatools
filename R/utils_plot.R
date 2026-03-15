# utils_plot.R — internal plot helpers

# ── xlim helpers ──────────────────────────────────────────────────────────────

# Round xlim outward to publication-friendly boundaries.
.round_xlim <- function(val, sm, side = c("lower", "upper")) {
  side <- match.arg(side)
  is_ratio <- sm %in% c("OR", "RR", "HR")

  if (is_ratio) {
    # Exponentiated scale
    breaks_lower <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
    breaks_upper <- c(1, 2, 5, 10, 20, 50, 100)
    if (side == "lower") {
      idx <- which(breaks_lower <= val)
      if (length(idx) == 0L) return(0.01)
      return(breaks_lower[max(idx)])
    } else {
      idx <- which(breaks_upper >= val)
      if (length(idx) == 0L) return(100)
      return(breaks_upper[min(idx)])
    }
  } else {
    # Difference scale — keep as-is with small buffer
    return(val)
  }
}

.calc_xlim <- function(net_meta, reference_group, sm, wide = FALSE) {
  if (wide) {
    lo <- as.vector(net_meta$lower.random)
    hi <- as.vector(net_meta$upper.random)
  } else {
    lo <- net_meta$lower.random[, reference_group]
    hi <- net_meta$upper.random[, reference_group]
  }

  is_ratio <- sm %in% c("OR", "RR", "HR")
  if (is_ratio) {
    lo_exp <- exp(min(lo, na.rm = TRUE))
    hi_exp <- exp(max(hi, na.rm = TRUE))
    xlim_lo <- .round_xlim(lo_exp, sm, "lower")
    xlim_hi <- .round_xlim(hi_exp, sm, "upper")
  } else {
    buffer  <- if (wide) 0 else 0.5
    xlim_lo <- min(lo, -buffer, na.rm = TRUE)
    xlim_hi <- max(hi,  buffer, na.rm = TRUE)
  }
  c(xlim_lo, xlim_hi)
}

# ── Figure dimension helpers ───────────────────────────────────────────────────

.calc_forest_width <- function(net_meta, base = 6.5, extra_per_char = 1 / 9,
                                rightpad = 4) {
  longest <- max(nchar(net_meta$trts))
  extra   <- max(longest - 9L, 0L) * extra_per_char
  base + extra + rightpad
}

.calc_forest_height <- function(n_trts, type = c("reference", "pairwise"),
                                 base = 2.5, per_trt = 0.25,
                                 per_study_pair = 0.20) {
  type <- match.arg(type)
  base + n_trts * per_trt
}

# Estimate A4 page height (inches) given number of comparison panels.
.est_panel_height <- function(n_comps, rows_per_comp = 6,
                               row_height_in = 0.22, base_in = 3) {
  max(base_in, n_comps * rows_per_comp * row_height_in + base_in)
}

# ── add_rows for heterogeneity ─────────────────────────────────────────────────

# Calculate extra blank rows needed before the heterogeneity stats block so it
# does not visually overlap with treatment rows.
#
# Overlap is caused by:
#   1. Number of leftcols: more columns → wider left panel → lines wrap more.
#   2. Max character length of studlab / treat names: longer strings wrap within
#      leftcol cells, increasing effective row height and consuming vertical space.
#
# effective_leftcols : character vector of leftcols that will actually be used
#                      (default leftcols merged with any user override).
# net_meta           : fitted netmeta / netmetabin object.
#
# Returns an integer >= 0 for `add.rows.before.reference`.
.calc_add_rows <- function(net_meta, effective_leftcols = c("studlab", "n.trts")) {
  n_cols <- length(effective_leftcols)

  # Longest study label visible in leftcols
  max_studlab <- if ("studlab" %in% effective_leftcols) {
    max(nchar(as.character(net_meta$studlab)), na.rm = TRUE)
  } else {
    0L
  }
  # Treatment names appear in the row labels regardless of leftcols
  max_treat <- max(nchar(net_meta$trts), na.rm = TRUE)
  max_len   <- max(max_studlab, max_treat)

  # Additive score: each factor contributes independently
  extra <- 0L

  # Contribution from number of leftcols
  if (n_cols >= 4L) extra <- extra + 2L
  if (n_cols >= 6L) extra <- extra + 2L   # stacks: >=6 gives +4 total

  # Contribution from label length
  if (max_len >= 25L) extra <- extra + 1L
  if (max_len >= 40L) extra <- extra + 1L  # stacks: >=40 gives +2 total
  if (max_len >= 60L) extra <- extra + 2L  # very long → wraps heavily

  extra
}

# ── Core save helpers ─────────────────────────────────────────────────────────

# Save a base-R plot expression to PDF, optionally trimming whitespace.
.save_plot <- function(file, width, height, expr, trim = TRUE,
                       trim_fuzz = 30L) {
  grDevices::pdf(file, width = width, height = height)
  tryCatch(force(expr), finally = grDevices::dev.off())
  if (trim) .trim_pdf(file, fuzz = trim_fuzz)
  invisible(file)
}

# Trim a PDF by rasterising with magick and rewriting.
.trim_pdf <- function(file, fuzz = 30L, density = 150L) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("magick package not available; skipping trim.")
    return(invisible(NULL))
  }
  img <- tryCatch(
    magick::image_read_pdf(file, density = density),
    error = function(e) { warning("magick could not read ", file); NULL }
  )
  if (is.null(img)) return(invisible(NULL))
  img <- magick::image_trim(img, fuzz = fuzz)
  magick::image_write(img, path = file, format = "pdf")
  invisible(file)
}

# ── Page-splitting helper (pixel-based) ───────────────────────────────────────

# Render a potentially tall plot, then split into A4-height PDF files.
# If total height fits in one page, saves a single file (no _p1 suffix).
.save_plot_paged <- function(file_base, plot_fn, full_width, full_height,
                              a4_height_in = 11.69, density = 150L,
                              trim = TRUE, trim_fuzz = 30L) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp, width = full_width, height = full_height)
  tryCatch(plot_fn(), finally = grDevices::dev.off())

  if (!requireNamespace("magick", quietly = TRUE)) {
    warning("magick not available; saving unsplit plot.")
    file.copy(tmp, paste0(file_base, ".pdf"), overwrite = TRUE)
    return(invisible(NULL))
  }

  img <- tryCatch(
    magick::image_read_pdf(tmp, density = density),
    error = function(e) { warning("magick failed on ", tmp); NULL }
  )
  if (is.null(img)) {
    file.copy(tmp, paste0(file_base, ".pdf"), overwrite = TRUE)
    return(invisible(NULL))
  }

  if (trim) img <- magick::image_trim(img, fuzz = trim_fuzz)

  info   <- magick::image_info(img)
  total_h <- info$height
  total_w <- info$width
  page_h  <- round(a4_height_in * density)

  n_pages <- ceiling(total_h / page_h)

  if (n_pages <= 1L) {
    magick::image_write(img, path = paste0(file_base, ".pdf"), format = "pdf")
  } else {
    for (p in seq_len(n_pages)) {
      y0     <- (p - 1L) * page_h
      crop_h <- min(page_h, total_h - y0)
      geo    <- magick::geometry_area(total_w, crop_h, 0L, y0)
      page_img <- magick::image_crop(img, geometry = geo)
      out_path <- paste0(file_base, "_p", p, ".pdf")
      magick::image_write(page_img, path = out_path, format = "pdf")
    }
  }
  invisible(NULL)
}

# ── Subset helpers for large plot objects ─────────────────────────────────────

# Subset a metalist (netpairwise result) to a subset of comparison indices.
.subset_metalist <- function(obj, idx) {
  sub <- obj[idx]
  class(sub) <- class(obj)
  sub
}

# Estimate total A4 rows for a metalist (one meta object per comparison).
# Safely handles non-list elements that may appear in netpairwise results.
.metalist_rows <- function(obj, header_rows = 4L) {
  vapply(obj, function(m) {
    if (!is.list(m) || is.null(m[["k"]])) return(as.integer(header_rows))
    as.integer(m[["k"]]) + as.integer(header_rows)
  }, integer(1L))
}

# Greedily assign elements to pages so each page <= max_rows.
.assign_pages <- function(row_vec, max_rows) {
  pages <- list()
  cur   <- integer(0)
  cur_r <- 0L
  for (i in seq_along(row_vec)) {
    r <- row_vec[[i]]
    if (cur_r + r > max_rows && length(cur) > 0L) {
      pages <- c(pages, list(cur))
      cur   <- i
      cur_r <- r
    } else {
      cur   <- c(cur, i)
      cur_r <- cur_r + r
    }
  }
  if (length(cur) > 0L) pages <- c(pages, list(cur))
  pages
}

# Save a metalist forest plot, splitting into pages if large.
.save_metalist_paged <- function(obj, forest_args, file_base,
                                  width, row_height_in = 0.22,
                                  base_height_in = 3, a4_rows = 45L,
                                  trim = TRUE, trim_fuzz = 30L) {
  # Guard: some netmeta versions include non-meta elements in netpairwise result.
  is_valid <- vapply(obj, function(m) is.list(m) && !is.null(m[["k"]]), logical(1L))
  if (!any(is_valid)) {
    warning("netpairwise result contains no valid comparison objects; skipping paged forest.")
    return(invisible(NULL))
  }
  if (!all(is_valid)) {
    obj_cls    <- class(obj)
    obj        <- obj[is_valid]
    class(obj) <- obj_cls
  }

  rows_vec <- .metalist_rows(obj)
  pages    <- .assign_pages(rows_vec, a4_rows)
  n_pages  <- length(pages)

  for (p in seq_len(n_pages)) {
    idx      <- pages[[p]]
    sub_obj  <- .subset_metalist(obj, idx)
    sub_rows <- sum(rows_vec[idx])
    height   <- max(base_height_in, sub_rows * row_height_in + base_height_in)
    suffix   <- if (n_pages > 1L) paste0("_p", p) else ""
    out_file <- paste0(file_base, suffix, ".pdf")

    .save_plot(
      file      = out_file,
      width     = width,
      height    = height,
      trim      = trim,
      trim_fuzz = trim_fuzz,
      expr      = do.call(meta::forest, c(list(sub_obj), forest_args))
    )
  }
  invisible(NULL)
}

# Save a netsplit forest plot (pixel-split approach due to complex internals).
.save_netsplit_paged <- function(ns_obj, forest_args, file_base,
                                  width, a4_rows = 45L,
                                  row_height_in = 0.22, base_height_in = 3,
                                  trim = TRUE, trim_fuzz = 30L) {
  n_comps   <- length(ns_obj$comparisons)
  total_rows <- n_comps * 7L + 10L   # ~7 rows per comparison + margin
  full_h    <- max(base_height_in, total_rows * row_height_in + base_height_in)

  plot_fn <- function() {
    do.call(
      meta::forest,
      c(list(ns_obj, separate = TRUE, prediction = TRUE, show = "all"),
        forest_args)
    )
  }

  .save_plot_paged(
    file_base   = file_base,
    plot_fn     = plot_fn,
    full_width  = width,
    full_height = full_h,
    a4_height_in = 11.69,
    density     = 150L,
    trim        = trim,
    trim_fuzz   = trim_fuzz
  )
  invisible(NULL)
}
