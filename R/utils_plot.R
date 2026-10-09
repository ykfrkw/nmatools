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
  # Use random matrices when present, else fall back to common-effect (MH/NCH).
  use_random <- isTRUE(net_meta$random) && !is.null(net_meta$lower.random)
  lo_mat <- if (use_random) net_meta$lower.random else net_meta$lower.common
  hi_mat <- if (use_random) net_meta$upper.random else net_meta$upper.common
  if (wide) {
    lo <- as.vector(lo_mat)
    hi <- as.vector(hi_mat)
  } else {
    lo <- lo_mat[, reference_group]
    hi <- hi_mat[, reference_group]
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

# Size HINTS only: forest plots are drawn at their measured size by
# .save_fitted_plot() / .save_plot_paged(), so these just seed the scratch
# device and are the fallback when magick is unavailable.

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

# Margins (lines) for netgraph(): labels sit outside the nodes and are clipped
# at the figure region (netgraph sets xpd = TRUE), so long treatment names
# need wider margins. Excess whitespace is removed by trimming afterwards.
.netgraph_mar <- function(trts, chars_per_line = 4, min_lines = 4,
                          max_lines = 12) {
  longest <- max(nchar(as.character(trts)), 0L)
  rep(min(max_lines, max(min_lines, ceiling(longest / chars_per_line) + 2)), 4L)
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
# The heights computed here are only hints for the measuring pass in
# .save_fitted_plot(); the final device size comes from the drawn layout.
.save_metalist_paged <- function(obj, forest_args, file_base,
                                  width, row_height_in = 0.22,
                                  base_height_in = 3, a4_rows = 45L,
                                  trim = TRUE, trim_fuzz = 30L,
                                  trim_margin = 0.2) {
  # netmeta >= 3.x: netpairwise() returns a SINGLE meta object (class
  # "netpairwise") with all comparisons stacked, instead of a per-comparison
  # list. meta::forest() has a forest.netpairwise method; draw it once at its
  # measured size and cut it into A4 pages at blank rows (_p1, _p2, ...).
  if (inherits(obj, "netpairwise") && !is.null(obj[["k"]])) {
    n_comp     <- length(obj$bylevs %||% obj$k.w)
    total_rows <- sum(obj$k.w %||% obj$k, na.rm = TRUE) + n_comp * 6L + 4L
    height     <- max(base_height_in, total_rows * row_height_in + base_height_in)
    .save_plot_paged(
      file_base   = file_base,
      plot_fn     = function() do.call(meta::forest, c(list(obj), forest_args)),
      full_width  = width,
      full_height = height,
      trim        = trim,
      trim_fuzz   = trim_fuzz,
      trim_margin = trim_margin
    )
    return(invisible(NULL))
  }

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

    .save_fitted_plot(
      file        = paste0(file_base, suffix, ".pdf"),
      plot_fn     = function() do.call(meta::forest, c(list(sub_obj), forest_args)),
      width_hint  = width,
      height_hint = height,
      trim        = trim,
      trim_fuzz   = trim_fuzz,
      trim_margin = trim_margin
    )
  }
  invisible(NULL)
}

# Number of comparisons in a netsplit object. netmeta >= 3.x stores them in
# `comparison` (singular); `comparisons` is NULL there, which used to size the
# netsplit forest for zero comparisons and cut off everything but the bottom.
.netsplit_n_comparisons <- function(ns_obj) {
  comps <- ns_obj[["comparison"]] %||% ns_obj[["comparisons"]] %||%
    ns_obj[["random"]][["comparison"]] %||% ns_obj[["common"]][["comparison"]]
  length(comps)
}

# Height hint (inches) for a netsplit forest with show = "all" and a
# prediction interval: per comparison a header line, direct, indirect, network
# and prediction rows plus a blank separator, then column headers and axis.
.netsplit_height_hint <- function(n_comps, rows_per_comp = 6L,
                                  row_height_in = 0.22, base_height_in = 3) {
  base_height_in + n_comps * rows_per_comp * row_height_in
}

# Save a netsplit forest plot. The plot is drawn at its measured size and then
# cut into A4 pages at blank rows (pixel-based, because forest.netsplit has no
# per-comparison subsetting).
.save_netsplit_paged <- function(ns_obj, forest_args, file_base,
                                  width, a4_rows = 45L,
                                  row_height_in = 0.22, base_height_in = 3,
                                  trim = TRUE, trim_fuzz = 30L,
                                  trim_margin = 0.2) {
  n_comps <- .netsplit_n_comparisons(ns_obj)
  full_h  <- .netsplit_height_hint(n_comps, row_height_in = row_height_in,
                                   base_height_in = base_height_in)

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
    trim        = trim,
    trim_fuzz   = trim_fuzz,
    trim_margin = trim_margin
  )
  invisible(NULL)
}

# ── Pairwise funnel construction ───────────────────────────────────────────────

# Build per-comparison meta objects from a pairwise data frame (df_pw), for use
# with meta::funnel. Works on all netmeta versions (independent of netpairwise).
# df_pw is the output of meta::pairwise: columns treat1, treat2, TE, seTE,
# studlab. meta::pairwise preserves within-study arm order and does NOT
# canonicalize direction, so the same comparison can appear as both "A vs B" and
# "B vs A". We canonicalize by sorting each treatment pair and flipping the TE
# sign for rows whose treat1 is the higher-sorted element, so every row in a
# group points the same way (lo vs hi). Rows with NA TE/seTE are dropped BEFORE
# the min_studies count so the k >= min_studies gate matches the usable studies.
# Returns a named list of "meta" objects whose names are "<lo> vs <hi>".
# Groups below min_studies are dropped; metagen failures are skipped.
.build_funnel_pairs <- function(df_pw, sm, min_studies) {
  df_pw <- as.data.frame(df_pw)
  df_pw <- df_pw[!(is.na(df_pw$TE) | is.na(df_pw$seTE)), , drop = FALSE]
  if (nrow(df_pw) == 0L) return(list())

  # Canonical (sorted) ordered pair for each row.
  canon <- t(apply(
    cbind(as.character(df_pw$treat1), as.character(df_pw$treat2)), 1L, sort
  ))
  lo <- canon[, 1L]
  hi <- canon[, 2L]
  key <- paste(lo, hi)

  # Harmonize direction: every row expressed as the lo vs hi contrast.
  df_pw$.TE_h <- ifelse(df_pw$treat1 == lo, df_pw$TE, -df_pw$TE)

  out <- list()
  for (k in unique(key)) {
    sel   <- key == k
    group <- df_pw[sel, , drop = FALSE]
    if (nrow(group) < min_studies) next
    lbl <- paste0(lo[sel][1L], " vs ", hi[sel][1L])
    m_pw <- tryCatch(
      meta::metagen(
        .TE_h, seTE, studlab = studlab, data = group,
        sm = sm, common = FALSE, random = TRUE, method.tau = "REML"
      ),
      error = function(e) NULL
    )
    if (is.null(m_pw)) next
    out[[lbl]] <- m_pw
  }
  out
}

# ── Contribution heatmap ───────────────────────────────────────────────────────

# Render a direct-evidence contribution matrix (comparison x comparison) as a
# labelled ggplot2 heatmap and save it to a self-contained PDF via ggsave.
# ggplot already crops the output, so no magick trim is applied; the white
# border comes from plot.margin instead (same width as the trim margin).
.save_netcontrib_heatmap <- function(cm, outcome, file, width, height,
                                     margin_in = 0.2) {
  cm_df <- as.data.frame(as.table(as.matrix(cm)))
  names(cm_df) <- c("network_comparison", "direct_comparison", "contribution")
  p <- ggplot2::ggplot(
    cm_df,
    ggplot2::aes(x = direct_comparison, y = network_comparison,
                 fill = contribution)
  ) +
    ggplot2::geom_tile(color = "grey70") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", contribution)),
      size = 3.4
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue",
                                 limits = c(0, 1)) +
    ggplot2::labs(
      title = paste0("Direct Evidence Contributions: ", outcome),
      x = "Direct comparison", y = "Network comparison",
      fill = "Contribution"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      plot.margin = ggplot2::margin(margin_in, margin_in, margin_in,
                                    margin_in, unit = "in")
    )
  ggplot2::ggsave(file = file, plot = p, width = width, height = height,
                  bg = "white")
  invisible(file)
}
