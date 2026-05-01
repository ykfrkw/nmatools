#' @title Vitruvian plot (per-treatment multi-outcome spider plot)
#'
#' @description Creates one circular (polar) chart per treatment, following
#'   the design of Nikolakopoulou et al. (2022) and Ostinelli et al.
#'   Each spoke represents one outcome; bar height is the absolute effect
#'   (event rate for binary outcomes, mean difference for continuous).
#'   Bar colour encodes the p-value vs the reference treatment using a
#'   continuous diverging gradient (deep green = beneficial, white =
#'   non-significant, deep red = harmful). The reference treatment is shown
#'   in a uniform grey-blue. A semi-transparent reference-rate area is
#'   overlaid on all non-reference charts to aid visual comparison.
#'
#' @references
#' Nikolakopoulou A et al. (2022). Elife. \doi{10.7554/eLife.73630}
#'
#' @param outcomes A list of outcome specifications. Each element is a list
#'   with the following fields:
#'   \itemize{
#'     \item \code{x}: A \code{netmeta} object or a character path to an
#'       \code{.rds} file containing one.
#'     \item \code{name}: Internal outcome name.
#'     \item \code{label}: Spoke label displayed on the plot.
#'     \item \code{small_values}: \code{"desirable"} or \code{"undesirable"}.
#'     \item \code{reference}: Reference treatment (optional; overrides the
#'       top-level \code{reference} argument for this outcome).
#'     \item \code{cer}: Control event rate (proportion, 0–1). For binary
#'       outcomes, estimated automatically from reference-arm data if
#'       \code{NULL}. For continuous outcomes (\code{sm = "SMD"} or
#'       \code{"MD"}), supplying a numeric \code{cer} triggers conversion to
#'       absolute risk via the SMD approximation
#'       lnOR = pi/sqrt(3) * SMD, so bars are on the same scale as binary
#'       outcomes.
#'     \item \code{pooled_sd}: Pooled standard deviation (numeric). Used when
#'       \code{sm = "MD"} and \code{cer} is supplied, to convert MD to SMD
#'       (SMD = MD / pooled_sd) before applying the approximation. If
#'       \code{NULL}, estimated automatically from \code{seTE}, \code{n1},
#'       and \code{n2} in the netmeta data slot via
#'       \code{.estimate_pooled_sd()}.
#'     \item \code{group}: Optional group name (character). Outcomes sharing the
#'       same group name are visually grouped: the outer label band is coloured
#'       by group, a thicker radial boundary line is drawn at group borders, and
#'       an arc label for the group name is displayed in the outer band.
#'     \item \code{trivial_range}: Per-outcome trivial range on the log scale
#'       for OR/RR/HR or raw scale for MD/SMD. A numeric vector \code{c(lo, hi)}.
#'       When the point estimate falls within this interval, the bar is shown in
#'       steel blue. Overrides the top-level \code{trivial_range}.
#'   }
#' @param trivial_range Default trivial range applied to every outcome that does
#'   not specify its own \code{trivial_range}. Numeric vector \code{c(lo, hi)}.
#'   \code{NULL} (default) disables trivial-effect highlighting.
#' @param reference Reference treatment name.
#' @param treatments Character vector of treatments to include. \code{NULL}
#'   uses all treatments from the first outcome's \code{netmeta} object.
#' @param treat_labels Named character vector for renaming treatments.
#' @param common Logical. Use common-effects model? Default \code{FALSE}.
#' @param fixed Deprecated. Use \code{common} instead.
#' @param digits Decimal places for displayed values (default 1).
#' @param as_percent Logical. Multiply binary absolute effects by 100 for
#'   percentage display? Default \code{TRUE}.
#' @param ncol Number of columns in the facet grid (default 3).
#' @param ref_color Colour used for the reference treatment's bars and the
#'   semi-transparent reference overlay (default \code{"#aec6cf"}).
#' @param ref_alpha Transparency of the reference-rate background area on
#'   non-reference treatment charts (default \code{0.35}).
#' @param group_colors Named character vector of colours for groups. Names
#'   must match group names used in \code{outcomes}. Groups not listed here
#'   fall back to the built-in palette.
#' @param strip_color Fill colour for the treatment-label strip above each
#'   panel (default \code{"#2c3e50"}).
#' @param show_legend Logical. Attach a p-value colour legend to the right of
#'   the plot? Requires the \pkg{patchwork} package. Default \code{TRUE}.
#' @param file Output file path. \code{NULL} returns the ggplot object.
#'   Supported extensions: \code{".pdf"}, \code{".png"}, \code{".svg"}.
#' @param width Plot width in inches. \code{NULL} (default) auto-computes as
#'   \code{ncol * 4} inches (plus 1.8 inches for the legend when
#'   \code{show_legend = TRUE}), so each panel is always 4 inches wide
#'   regardless of \code{ncol}.
#' @param height Plot height in inches. \code{NULL} (default) auto-computes as
#'   \code{ceiling(n_treatments / ncol) * 4} inches, so each panel is always
#'   4 inches tall regardless of \code{ncol}.
#' @return A \code{ggplot} object (invisibly if \code{file} is specified).
#' @export
#'
#' @importFrom geomtextpath geom_textpath
#' @importFrom ggplot2 ggplot aes geom_col geom_ribbon geom_point geom_text geom_label geom_hline
#'   geom_segment coord_polar position_nudge scale_fill_identity scale_x_continuous
#'   scale_y_continuous expansion facet_wrap labs theme_minimal theme
#'   element_text element_blank element_line element_rect unit ggsave
#' @importFrom meta metaprop
#' @importFrom stats setNames plogis
vitruvian <- function(outcomes,
                      reference,
                      trivial_range = NULL,
                      treatments   = NULL,
                      treat_labels = NULL,
                      common       = FALSE,
                      fixed        = NULL,
                      digits       = 1,
                      as_percent   = TRUE,
                      ncol         = 3,
                      ref_color    = "#aec6cf",
                      ref_alpha    = 0.35,
                      group_colors = NULL,
                      strip_color  = "#2c3e50",
                      palette      = "GrYlRd",
                      show_legend  = TRUE,
                      file         = NULL,
                      width        = NULL,
                      height       = NULL) {

  # ---- 1. Setup ----
  if (!is.null(fixed)) {
    message("Argument 'fixed' is deprecated in vitruvian(); use 'common' instead.")
    common <- fixed
  }

  outcomes <- lapply(outcomes, function(oc) {
    if (is.character(oc$x)) oc$x <- readRDS(oc$x)
    oc
  })

  # ---- 2. Treatment list ----
  if (is.null(treatments)) {
    treatments <- unique(unlist(lapply(outcomes, function(oc) oc$x$trts)))
  }

  trt_display <- treatments
  names(trt_display) <- treatments
  if (!is.null(treat_labels)) {
    idx <- match(treatments, names(treat_labels))
    trt_display[!is.na(idx)] <- treat_labels[idx[!is.na(idx)]]
  }

  # outcome_names: unique internal IDs (always oc$name) used as factor levels.
  # outcome_labels: display text (may contain duplicates) used only for annotation.
  outcome_names  <- vapply(outcomes, function(oc) oc$name, character(1))
  outcome_labels <- vapply(outcomes,
    function(oc) if (!is.null(oc$label)) oc$label else oc$name,
    character(1))

  # ---- 3. Compute absolute effects, signed p-values, reference rates ----
  rows <- list()

  for (k in seq_along(outcomes)) {
    oc  <- outcomes[[k]]
    nx  <- oc$x
    ref <- if (!is.null(oc$reference)) oc$reference else reference
    sv  <- if (!is.null(oc$small_values)) oc$small_values else "undesirable"
    tr  <- if (!is.null(oc$trivial_range)) oc$trivial_range else trivial_range

    te_mat   <- .nm_mat(nx, "TE",   common)
    pval_mat <- .nm_mat(nx, "pval", common)

    is_binary <- nx$sm %in% c("OR", "RR", "RD")
    is_or     <- nx$sm == "OR"
    is_rr     <- nx$sm == "RR"
    is_rd     <- nx$sm == "RD"
    is_smd    <- nx$sm == "SMD"
    is_md     <- nx$sm == "MD"

    cer       <- oc$cer
    pooled_sd <- oc$pooled_sd   # user-supplied; auto-estimated below if NULL

    if (is_binary) {
      if (is.null(cer) || identical(cer, "metaprop")) {
        cer <- .estimate_cer(nx, ref, method = "metaprop")
      } else if (identical(cer, "simple")) {
        cer <- .estimate_cer(nx, ref, method = "simple")
      }
      # else: numeric value used as-is
    } else if (is_md && is.null(pooled_sd) && is.numeric(cer)) {
      # Auto-estimate pooled SD from seTE + n1/n2 in netmeta data slot
      pooled_sd <- .estimate_pooled_sd(nx)
    }

    # Continuous → OR conversion via SMD approximation:
    #   lnOR = π/√3 × SMD    (Cox & Snell 1989)
    # Requires: cer = numeric (0–1); for MD also pooled_sd = numeric.
    smd_to_or <- !is_binary && is.numeric(cer) &&
                 (is_smd || (is_md && is.numeric(pooled_sd)))

    ref_abs <- if (is_binary || smd_to_or) {
      if (as_percent) cer * 100 else cer
    } else 0

    for (trt in treatments) {
      is_ref  <- (trt == reference)
      trivial <- FALSE

      if (trt == ref) {
        abs_val   <- ref_abs
        signed_pv <- NA_real_

      } else if (!trt %in% rownames(te_mat) || !ref %in% rownames(te_mat)) {
        abs_val   <- NA_real_
        signed_pv <- NA_real_

      } else {
        te <- te_mat[trt, ref]   # log scale for OR/RR, raw for others
        pv <- pval_mat[trt, ref]

        if (is_or) {
          ar <- .or_to_ar(exp(te), cer)
        } else if (is_rr) {
          ar <- cer * exp(te)
        } else if (is_rd) {
          ar <- cer + te
        } else if (smd_to_or) {
          # Continuous → absolute risk via SMD approximation:
          # MD → SMD = MD / pooled_sd; SMD → lnOR = π/√3 × SMD
          smd_val <- if (is_md) te / pooled_sd else te
          ar      <- .or_to_ar(exp(pi / sqrt(3) * smd_val), cer)
        } else {
          ar <- te   # raw MD/SMD: no CER specified
        }
        abs_val <- if ((is_binary || smd_to_or) && as_percent) ar * 100 else ar

        signed_pv <- pv
        if (sv == "desirable"   && te > 0) signed_pv <- -pv
        if (sv == "undesirable" && te < 0) signed_pv <- -pv

        trivial <- !is.null(tr) && length(tr) == 2L &&
                   te >= tr[1] && te <= tr[2]
      }

      rows[[length(rows) + 1]] <- data.frame(
        treatment = trt,
        trt_label = trt_display[trt],
        outcome   = outcome_names[k],   # internal ID (unique)
        abs_val   = abs_val,
        ref_abs   = ref_abs,
        signed_pv = signed_pv,
        trivial   = trivial,
        is_ref    = is_ref,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_df <- do.call(rbind, rows)

  # ---- 4. Assign fill colours ----
  plot_df$fill_color <- ifelse(
    plot_df$is_ref,
    ref_color,
    pval_to_color_gradient(plot_df$signed_pv, trivial = plot_df$trivial,
                           palette = palette)
  )

  # ---- 5. Factor ordering ----
  plot_df$outcome     <- factor(plot_df$outcome, levels = outcome_names)
  plot_df$outcome_idx <- as.integer(plot_df$outcome)   # numeric x for continuous polar scale

  trt_order          <- c(reference, setdiff(treatments, reference))
  trt_labels_ordered <- trt_display[trt_order]
  plot_df$trt_label  <- factor(plot_df$trt_label, levels = trt_labels_ordered)

  # ---- Auto-compute width / height to keep each panel a fixed 4×4-inch square ----
  n_panels    <- length(trt_order)
  nrow_panels <- ceiling(n_panels / ncol)
  panel_size  <- 4   # inches per panel (width and height)
  legend_w    <- if (isTRUE(show_legend)) 1.8 else 0
  if (is.null(width))  width  <- ncol * panel_size + legend_w
  if (is.null(height)) height <- nrow_panels * panel_size

  # Value labels (shown as badges): always displayed as integers
  plot_df$val_label <- ifelse(
    !is.na(plot_df$abs_val),
    paste0(sprintf("%.0f", plot_df$abs_val), if (as_percent) "%" else ""),
    NA_character_
  )
  # Reference value labels (blue badge; NA for continuous outcomes where ref_abs=0)
  plot_df$ref_val_label <- ifelse(
    !is.na(plot_df$ref_abs) & plot_df$ref_abs > 0,
    paste0(sprintf("%.0f", plot_df$ref_abs), if (as_percent) "%" else ""),
    NA_character_
  )

  # ---- 6. Axis limits & grid ----
  y_max <- max(c(plot_df$abs_val, plot_df$ref_abs), na.rm = TRUE)
  y_max <- ceiling(y_max / 10) * 10
  if (!is.finite(y_max) || y_max == 0) y_max <- 50

  # Major grid at every 10%, minor at every 5%
  major_at <- seq(10, y_max, by = 10)
  minor_at <- seq(5,  y_max, by = 5)

  # n_oc needed for badge position calculation below
  n_oc <- length(outcome_names)

  # Badge positions: angle-aware rotation so that EER appears visually upper-left
  # and CER appears visually lower-right REGARDLESS of which sector they are in.
  badge_center_y <- y_max * 0.55
  vd    <- y_max * 0.09
  sq2   <- sqrt(2)
  alpha <- badge_center_y * 2 * pi / n_oc

  k_idx  <- plot_df$outcome_idx
  phi_k  <- 2 * pi * (k_idx - 0.5) / n_oc

  eer_dx <- (cos(phi_k) * (-vd/sq2) - sin(phi_k) * ( vd/sq2)) / alpha
  eer_dy <-  sin(phi_k) * (-vd/sq2) + cos(phi_k) * ( vd/sq2)
  cer_dx <- (cos(phi_k) * ( vd/sq2) - sin(phi_k) * (-vd/sq2)) / alpha
  cer_dy <-  sin(phi_k) * ( vd/sq2) + cos(phi_k) * (-vd/sq2)

  plot_df$eer_x <- k_idx + eer_dx
  plot_df$eer_y <- ifelse(!plot_df$is_ref & !is.na(plot_df$val_label),
                          badge_center_y + eer_dy, NA_real_)
  plot_df$cer_x <- k_idx + cer_dx
  plot_df$cer_y <- ifelse(!is.na(plot_df$ref_val_label),
                          badge_center_y + cer_dy, NA_real_)

  # ---- Outcome label arcs: use geom_textpath for true arc-following text ----
  # Each outcome gets its own arc path so the text follows the circle curvature.
  label_arc_df <- do.call(rbind, lapply(seq_len(n_oc), function(k) {
    n_pts <- 60
    data.frame(
      x        = seq(k - 0.49, k + 0.49, length.out = n_pts),
      y        = rep(y_max * 1.07, n_pts),
      arc_lab  = rep(outcome_labels[k], n_pts),
      arc_grp  = rep(as.character(k), n_pts),
      stringsAsFactors = FALSE
    )
  }))

  # ---- Group structure (optional) ----
  outcome_groups <- vapply(outcomes,
    function(oc) if (!is.null(oc$group)) as.character(oc$group) else NA_character_,
    character(1))
  has_groups <- !all(is.na(outcome_groups))

  # x-positions of group boundaries (between adjacent sectors with different groups)
  group_bounds_x <- numeric(0)
  group_arc_df   <- NULL

  # Colour palette for groups (up to 10 groups) — pastel tones
  group_palette_colors <- c(
    "#A8C4E0", "#F5CBA7", "#C5E0B4", "#FFE5A0",
    "#D6C0E8", "#A0DCF0", "#F5B7B1", "#C8DDD0",
    "#FFC8C8", "#C0B4D6"
  )
  grp_colors <- NULL

  if (has_groups) {
    unique_grps <- unique(outcome_groups[!is.na(outcome_groups)])

    # Default palette, then override with user-supplied group_colors
    grp_colors <- stats::setNames(
      group_palette_colors[seq_along(unique_grps)],
      unique_grps
    )
    if (!is.null(group_colors)) {
      common_grps <- intersect(names(group_colors), unique_grps)
      grp_colors[common_grps] <- group_colors[common_grps]
    }

    next_idx       <- c(seq(2L, n_oc), 1L)
    gi             <- outcome_groups
    gn             <- outcome_groups[next_idx]
    group_bounds_x <- seq_len(n_oc)[!is.na(gi) & !is.na(gn) & gi != gn] + 0.5

    # Group arc labels: path data for geom_textpath (true arc following)
    group_arc_df <- do.call(rbind, lapply(unique_grps, function(grp) {
      idx <- which(outcome_groups == grp)
      n_pts <- max(60, (max(idx) - min(idx) + 1) * 20)
      data.frame(
        x        = seq(min(idx) - 0.49, max(idx) + 0.49, length.out = n_pts),
        y        = rep(y_max * 1.14, n_pts),
        arc_lab  = rep(grp, n_pts),
        arc_grp  = rep(paste0("grp_", grp), n_pts),
        stringsAsFactors = FALSE
      )
    }))
  }

  # ---- Outer label band (group-coloured or uniform grey) ----
  # The band covers y_max to band_top; outcome labels sit within it.
  band_top <- y_max * 1.25

  band_color_vec <- if (has_groups) {
    vapply(outcome_groups, function(g) {
      if (is.na(g)) "#DCDCDC" else grp_colors[[g]]
    }, character(1))
  } else {
    rep("#DCDCDC", n_oc)
  }

  # Interpolated ring data for geom_ribbon: draws only the outer annular
  # ring (y_max to band_top) without covering the inner chart area.
  ring_df <- do.call(rbind, lapply(seq_len(n_oc), function(i) {
    data.frame(
      x        = seq(i - 0.5, i + 0.5, length.out = 30),
      ymin_val = y_max,
      ymax_val = band_top,
      fill_col = band_color_vec[i],
      sector   = i,
      stringsAsFactors = FALSE
    )
  }))

  # (connector lines removed)

  # ---- 7. Build ggplot ----
  # Y-scale upper limit: just enough room outside the band
  y_scale_top <- y_max * if (has_groups) 1.27 else 1.22

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = outcome_idx, y = abs_val)
  ) +
    # ---- Outer label band: coloured annular ring (y_max to band_top) ----
    ggplot2::geom_ribbon(
      data = ring_df,
      ggplot2::aes(x = x, ymin = ymin_val, ymax = ymax_val,
                   fill = fill_col, group = sector),
      colour = NA, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    # ---- Sector boundary radial lines ----
    ggplot2::annotate(
      "segment",
      x      = c(0.5 + 1e-3, seq(1.5, n_oc - 0.5, by = 1), n_oc + 0.5 - 1e-3),
      xend   = c(0.5 + 1e-3, seq(1.5, n_oc - 0.5, by = 1), n_oc + 0.5 - 1e-3),
      y      = 0,
      yend   = band_top,
      colour = "grey60", linewidth = 0.3
    ) +
    # ---- Reference event-rate area (semi-transparent) ----
    ggplot2::geom_col(
      ggplot2::aes(y = ref_abs),
      fill = ref_color, alpha = ref_alpha,
      width = 1.0, colour = NA
    ) +
    # ---- Main bars: coloured by p-value ----
    ggplot2::geom_col(
      ggplot2::aes(fill = I(fill_color)),
      width = 1.0, colour = "white", linewidth = 0.25
    ) +
    # ---- Major concentric circles at every 10% ----
    ggplot2::geom_hline(yintercept = major_at,
                        colour = "grey65", linewidth = 0.35, linetype = "solid") +
    # ---- Minor concentric circles at 5% ----
    ggplot2::geom_hline(yintercept = minor_at[minor_at %% 10 != 0],
                        colour = "grey82", linewidth = 0.2, linetype = "dashed") +
    # ---- Outer circle (solid ring at y_max) ----
    ggplot2::annotate("path",
                      x = seq(0.5, n_oc + 0.5, length.out = 360),
                      y = y_max,
                      colour = "grey30", linewidth = 0.5) +
    # ---- Outer band boundary (solid ring at band_top) ----
    ggplot2::annotate("path",
                      x = seq(0.5, n_oc + 0.5, length.out = 360),
                      y = band_top,
                      colour = "grey30", linewidth = 0.3) +
    # ---- Outcome labels: arc-following text via geom_textpath ----
    geomtextpath::geom_textpath(
      data = label_arc_df,
      ggplot2::aes(x = x, y = y, label = arc_lab, group = arc_grp),
      colour = "grey10", fontface = "bold", size = 2.5,
      hjust = 0.5, linecolour = NA, inherit.aes = FALSE
    ) +
    # ---- Group boundary lines: extend through the outer band ----
    (if (has_groups && length(group_bounds_x) > 0)
      ggplot2::geom_segment(
        data = data.frame(x = group_bounds_x, xend = group_bounds_x,
                          y = 0, yend = band_top),
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        colour = "grey30", linewidth = 0.7, inherit.aes = FALSE
      )
    else NULL) +
    # ---- Group arc labels: arc-following text via geom_textpath ----
    (if (has_groups && !is.null(group_arc_df))
      geomtextpath::geom_textpath(
        data = group_arc_df,
        ggplot2::aes(x = x, y = y, label = arc_lab, group = arc_grp),
        colour = "grey10", fontface = "bold", size = 3.0,
        hjust = 0.5, linecolour = NA, inherit.aes = FALSE
      )
    else NULL) +
    # ---- EER badge (dark grey, white text) ----
    ggplot2::geom_label(
      ggplot2::aes(x = eer_x, y = eer_y, label = val_label),
      inherit.aes = FALSE,
      fill = "#787276", colour = "white",
      size = 3.0, fontface = "bold",
      label.padding = ggplot2::unit(c(0.10, 0.28, 0.10, 0.28), "lines"),
      label.r = ggplot2::unit(0, "lines"),
      label.size = 0, na.rm = TRUE
    ) +
    # ---- CER badge (light blue rounded) ----
    ggplot2::geom_label(
      ggplot2::aes(x = cer_x, y = cer_y, label = ref_val_label),
      inherit.aes = FALSE,
      fill = "#DDF1FB", colour = "grey30",
      size = 3.0, fontface = "bold",
      label.padding = ggplot2::unit(c(0.10, 0.28, 0.10, 0.28), "lines"),
      label.r = ggplot2::unit(0.25, "lines"),
      label.size = 0, na.rm = TRUE
    ) +
    ggplot2::coord_polar(theta = "x", start = 0, clip = "off") +
    ggplot2::scale_y_continuous(
      limits  = c(0, y_scale_top),
      breaks  = major_at,
      expand  = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_oc),
      labels = outcome_labels,
      limits = c(0.5, n_oc + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::facet_wrap(~ trt_label, ncol = ncol) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_blank(),
      axis.ticks         = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text         = ggplot2::element_text(
        size = 10, face = "bold", colour = "white",
        margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 4, unit = "pt")
      ),
      strip.background   = ggplot2::element_rect(
        fill = strip_color, colour = strip_color, linewidth = 0,
        inherit.blank = FALSE
      ),
      panel.spacing      = ggplot2::unit(0.1, "cm"),
      plot.margin        = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )

  # ---- 8. Attach p-value colour legend ----
  if (show_legend && requireNamespace("patchwork", quietly = TRUE)) {
    leg <- pval_legend_ggplot(base_size = 7, palette = palette)
    p <- patchwork::wrap_plots(p, leg,
                               widths = c(width - legend_w, legend_w),
                               nrow = 1)
  }

  # ---- 9. Save or return ----
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("pdf", "png", "svg")) {
      stop("Unsupported file extension: ", ext, ". Use .pdf, .png, or .svg")
    }
    ggplot2::ggsave(file, plot = p, width = width, height = height,
                    units = "in", device = ext)
    message("Saved Vitruvian plot: ", file)
    return(invisible(p))
  }

  # Fixed-size preview: render at specified dimensions via temp PNG so
  # that the layout does not change when the viewer window is resized.
  if (interactive()) {
    tmp <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp, plot = p, width = width, height = height,
                    units = "in", dpi = 150)
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(tmp)
    } else {
      utils::browseURL(tmp)
    }
    return(invisible(p))
  }

  p
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Convert OR to absolute risk
#' @keywords internal
.or_to_ar <- function(or, cer) {
  (cer * or) / (1 - cer + cer * or)
}

#' Estimate CER from reference arm data in a netmeta object
#'
#' @param x A \code{netmeta} object.
#' @param reference Reference treatment name.
#' @param method \code{"metaprop"} (default) pools via GLMM / logit back-transform
#'   using \code{meta::metaprop()}; \code{"simple"} uses the naive pooled
#'   proportion (sum events / sum n).
#' @keywords internal
.estimate_cer <- function(x, reference, method = "metaprop") {
  d <- tryCatch(x$data, error = function(e) NULL)
  if (is.null(d)) {
    warning("Cannot estimate CER from netmeta object; using 0.5 as fallback.")
    return(0.5)
  }

  treat_col <- grep("^treat[12]$", colnames(d), value = TRUE)
  n_col     <- grep("^n[12]$",     colnames(d), value = TRUE)
  event_col <- grep("^event[12]$", colnames(d), value = TRUE)

  if (length(treat_col) < 2 || length(n_col) < 2 || length(event_col) < 2) {
    warning("Cannot locate event/n columns; using 0.5 as CER fallback.")
    return(0.5)
  }

  ref_events <- c()
  ref_n      <- c()

  for (i in 1:2) {
    mask <- d[[treat_col[i]]] == reference & !is.na(d[[event_col[i]]]) &
      !is.na(d[[n_col[i]]])
    ref_events <- c(ref_events, d[[event_col[i]]][mask])
    ref_n      <- c(ref_n,      d[[n_col[i]]][mask])
  }

  if (length(ref_events) == 0 || sum(ref_n) == 0) {
    warning("No reference arm data found; using 0.5 as CER fallback.")
    return(0.5)
  }

  if (method == "simple") {
    return(sum(ref_events) / sum(ref_n))
  }

  # method == "metaprop": pool via GLMM (logit scale)
  mp <- tryCatch(
    meta::metaprop(event = ref_events, n = ref_n,
                   method = "GLMM", sm = "PLOGIT",
                   method.tau = "ML"),
    error = function(e) NULL
  )

  if (!is.null(mp) && !is.na(mp$TE.random)) {
    return(stats::plogis(mp$TE.random))
  }

  # Fallback: simple pooled proportion
  warning("meta::metaprop() failed; falling back to simple pooled proportion.")
  sum(ref_events) / sum(ref_n)
}
