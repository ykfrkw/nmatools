# utils_viz.R - Internal helper functions for the visualization layer
# Not exported.

# ---------------------------------------------------------------------------
# parse_cinema()
# ---------------------------------------------------------------------------

#' Parse CINeMA CSV or data frame
#'
#' Reads a CINeMA report from a file path or accepts a data frame directly.
#' Column names are normalised by replacing spaces with dots.
#'
#' @param cinema File path (character) or data frame.
#' @return A data frame with normalised column names.
#' @keywords internal
parse_cinema <- function(cinema) {
  if (is.character(cinema)) {
    if (!file.exists(cinema)) {
      stop("CINeMA file not found: ", cinema)
    }
    df <- utils::read.csv(cinema, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (is.data.frame(cinema)) {
    df <- cinema
  } else {
    stop("`cinema` must be a file path or a data frame.")
  }
  # Normalise column names: replace spaces with dots
  colnames(df) <- gsub(" ", ".", colnames(df))
  df
}

# ---------------------------------------------------------------------------
# default_cinema_palette() / classic_cinema_palette() / colorblind_cinema_palette()
# ---------------------------------------------------------------------------

#' Pastel CINeMA confidence rating colour palette (default)
#'
#' Light pastel backgrounds with dark text. Each entry is a list with
#' \code{$bg} (background hex) and \code{$color} (text hex).
#'
#' @return A named list mapping confidence rating strings to colour pairs.
#' @keywords internal
default_cinema_palette <- function() {
  list(
    "very low" = list(bg = "#e8d0d0", color = "#8b0000"),
    "low"      = list(bg = "#f8edd7", color = "#daa521"),
    "moderate" = list(bg = "#cccce9", color = "#01008b"),
    "high"     = list(bg = "#d7e8d3", color = "#238b21")
  )
}

#' Classic CINeMA confidence rating colour palette
#'
#' Saturated backgrounds with white text, as commonly seen in published NMA
#' papers. Each entry is a list with \code{$bg} and \code{$color}.
#'
#' @return A named list mapping confidence rating strings to colour pairs.
#' @keywords internal
classic_cinema_palette <- function() {
  list(
    "very low" = list(bg = "#c0392b", color = "#ffffff"),
    "low"      = list(bg = "#e67e22", color = "#ffffff"),
    "moderate" = list(bg = "#2471a3", color = "#ffffff"),
    "high"     = list(bg = "#1e8449", color = "#ffffff")
  )
}

#' Colorblind-safe CINeMA confidence rating colour palette
#'
#' Based on the Okabe-Ito palette, distinguishable by viewers with the most
#' common forms of colour vision deficiency (deuteranopia / protanopia).
#' Each entry is a list with \code{$bg} and \code{$color}.
#'
#' @return A named list mapping confidence rating strings to colour pairs.
#' @keywords internal
colorblind_cinema_palette <- function() {
  list(
    "very low" = list(bg = "#CC79A7", color = "#000000"),  # mauve
    "low"      = list(bg = "#E69F00", color = "#000000"),  # amber
    "moderate" = list(bg = "#56B4E9", color = "#000000"),  # sky blue
    "high"     = list(bg = "#009E73", color = "#ffffff")   # teal
  )
}

#' Select a CINeMA confidence rating colour palette by name
#'
#' Convenience dispatcher. Returns one of the built-in palettes.
#'
#' @param type One of \code{"pastel"} (default), \code{"classic"}, or
#'   \code{"colorblind"}.
#' @return A named list as returned by \code{default_cinema_palette()} etc.
#' @export
cinema_palette <- function(type = c("pastel", "classic", "colorblind")) {
  type <- match.arg(type)
  switch(type,
    pastel     = default_cinema_palette(),
    classic    = classic_cinema_palette(),
    colorblind = colorblind_cinema_palette()
  )
}

# ---------------------------------------------------------------------------
# cinema_to_color()
# ---------------------------------------------------------------------------

#' Map CINeMA confidence rating to background and text colours
#'
#' @param rating Character string for the confidence rating (case-insensitive).
#' @param palette Named list from \code{default_cinema_palette()}.
#' @return A list with \code{$bg} and \code{$color}, or \code{NULL} if not found.
#' @keywords internal
cinema_to_color <- function(rating, palette) {
  if (is.na(rating) || is.null(rating)) return(NULL)
  key <- tolower(trimws(rating))
  pal_names <- tolower(names(palette))
  idx <- match(key, pal_names)
  if (is.na(idx)) return(NULL)
  palette[[idx]]
}

# ---------------------------------------------------------------------------
# get_cinema_rating()
# ---------------------------------------------------------------------------

#' Look up CINeMA confidence rating for a treatment pair
#'
#' Searches for "treat1:treat2" and "treat2:treat1" in the Comparison column.
#'
#' @param treat1 Name of treatment 1.
#' @param treat2 Name of treatment 2.
#' @param cinema_df A data frame returned by \code{parse_cinema()}.
#' @return Confidence rating string, or \code{NA} if not found.
#' @keywords internal
get_cinema_rating <- function(treat1, treat2, cinema_df) {
  if (is.null(cinema_df) || nrow(cinema_df) == 0) return(NA_character_)

  comp_col <- "Comparison"
  rating_col <- "Confidence.rating"

  # Accept both space-normalised and original column names
  if (!comp_col %in% colnames(cinema_df)) {
    alt <- grep("comparison", colnames(cinema_df), ignore.case = TRUE, value = TRUE)
    if (length(alt)) comp_col <- alt[1] else return(NA_character_)
  }
  if (!rating_col %in% colnames(cinema_df)) {
    alt <- grep("confidence", colnames(cinema_df), ignore.case = TRUE, value = TRUE)
    if (length(alt)) rating_col <- alt[1] else return(NA_character_)
  }

  cmp1 <- paste0(treat1, ":", treat2)
  cmp2 <- paste0(treat2, ":", treat1)

  idx <- which(cinema_df[[comp_col]] == cmp1 | cinema_df[[comp_col]] == cmp2)
  if (length(idx) == 0) return(NA_character_)
  as.character(cinema_df[[rating_col]][idx[1]])
}

# ---------------------------------------------------------------------------
# .nm_mat()
# ---------------------------------------------------------------------------

#' Retrieve a netmeta result matrix, handling both naming conventions
#'
#' netmeta >= 2.0.0 stores common-effects results as \code{TE.common},
#' \code{lower.common} etc. Older versions used \code{TE.fixed}.
#' This helper returns the correct matrix regardless of version.
#'
#' @param x A \code{netmeta} object.
#' @param type Matrix type: \code{"TE"}, \code{"lower"}, \code{"upper"},
#'   \code{"pval"}, or \code{"seTE"}.
#' @param common Logical. \code{TRUE} = common-effects, \code{FALSE} = random.
#' @return A matrix.
#' @keywords internal
.nm_mat <- function(x, type, common) {
  if (!common) {
    return(x[[paste0(type, ".random")]])
  }
  # Try new name first, fall back to old name
  new_slot <- paste0(type, ".common")
  old_slot <- paste0(type, ".fixed")
  if (!is.null(x[[new_slot]])) x[[new_slot]] else x[[old_slot]]
}

# ---------------------------------------------------------------------------
# sort_treatments()
# ---------------------------------------------------------------------------

#' Sort a treatment vector
#'
#' @param treatments Character vector of treatment names.
#' @param sort_by Sort method: "alphabet", "pscore", "es", "es_rev",
#'   "pvalue", "zscore", or "custom".
#' @param x A \code{netmeta} object (required for pscore/es/pvalue/zscore).
#' @param reference Reference treatment name (used for es/pvalue/zscore).
#' @param sort_order Custom order vector (used when sort_by = "custom").
#' @param common Logical. Use common-effects model? Default \code{FALSE} = random-effects model.
#' @param fixed Deprecated. Use \code{common} instead.
#' @return Sorted character vector.
#' @keywords internal
sort_treatments <- function(treatments,
                            sort_by    = "alphabet",
                            x          = NULL,
                            reference  = NULL,
                            sort_order = NULL,
                            common     = FALSE,
                            fixed      = NULL) {
  if (!is.null(fixed)) {
    message("Argument 'fixed' is deprecated in sort_treatments(); use 'common' instead.")
    common <- fixed
  }

  sort_by <- match.arg(sort_by,
                       c("alphabet", "pscore", "es", "es_rev",
                         "pvalue", "zscore", "custom"))

  switch(sort_by,

    alphabet = sort(treatments),

    pscore = {
      if (is.null(x)) stop("netmeta object required for sort_by = 'pscore'")
      pscore_obj <- netmeta::netrank(x, small.values = x$small.values, method = "P-score")
      if (common) {
        scores <- if (!is.null(pscore_obj$Pscore.common)) pscore_obj$Pscore.common else pscore_obj$Pscore.fixed
      } else {
        scores <- pscore_obj$Pscore.random
      }
      # Keep only treatments present in the supplied vector
      scores <- scores[names(scores) %in% treatments]
      # Any missing treatments get lowest priority
      missing <- setdiff(treatments, names(scores))
      ordered_names <- c(names(sort(scores, decreasing = TRUE)),
                         sort(missing))
      ordered_names[ordered_names %in% treatments]
    },

    es = {
      if (is.null(x) || is.null(reference))
        stop("netmeta object and reference required for sort_by = 'es'")
      te_mat <- .nm_mat(x, "TE", common)
      te_vec <- te_mat[treatments, reference]
      treatments[order(te_vec, decreasing = FALSE)]
    },

    es_rev = {
      if (is.null(x) || is.null(reference))
        stop("netmeta object and reference required for sort_by = 'es_rev'")
      te_mat <- .nm_mat(x, "TE", common)
      te_vec <- te_mat[treatments, reference]
      treatments[order(te_vec, decreasing = TRUE)]
    },

    pvalue = {
      if (is.null(x) || is.null(reference))
        stop("netmeta object and reference required for sort_by = 'pvalue'")
      pval_mat <- .nm_mat(x, "pval", common)
      pval_vec <- pval_mat[treatments, reference]
      treatments[order(pval_vec, decreasing = FALSE)]
    },

    zscore = {
      if (is.null(x) || is.null(reference))
        stop("netmeta object and reference required for sort_by = 'zscore'")
      te_mat   <- .nm_mat(x, "TE",   common)
      seTE_mat <- .nm_mat(x, "seTE", common)
      te_vec   <- te_mat[treatments, reference]
      se_vec   <- seTE_mat[treatments, reference]
      z_vec    <- te_vec / se_vec
      treatments[order(z_vec, decreasing = FALSE)]
    },

    custom = {
      if (is.null(sort_order)) {
        warning("sort_by = 'custom' but sort_order is NULL; falling back to alphabet")
        return(sort(treatments))
      }
      in_order  <- sort_order[sort_order %in% treatments]
      remaining <- sort(setdiff(treatments, sort_order))
      c(in_order, remaining)
    }
  )
}

# ---------------------------------------------------------------------------
# pval_palette()
# ---------------------------------------------------------------------------

#' Built-in p-value colour palettes
#'
#' Returns the anchor RGB values for the requested diverging gradient palette.
#' Two palettes are available:
#' \describe{
#'   \item{\code{"GrYlRd"}}{Green-Yellow-Red. Non-significant (p = 1) →
#'     golden yellow (\code{#FFD700}); significant beneficial → deep green
#'     (\code{#006837}); significant harmful → deep red (\code{#ca0020}).
#'     Recommended for Vitruvian plots.}
#'   \item{\code{"GrRd"}}{Green-White-Red. Non-significant (p = 1) →
#'     white (\code{#FFFFFF}); significant beneficial → deep green
#'     (\code{#006837}); significant harmful → deep red (\code{#ca0020}).
#'     Recommended for Kilim tables (Excel output).}
#' }
#'
#' @param name Palette name: \code{"GrYlRd"} (default) or \code{"GrRd"}.
#' @return A list with elements \code{$neutral}, \code{$green}, and \code{$red},
#'   each an integer vector of length 3 (R, G, B, 0–255).
#' @export
pval_palette <- function(name = c("GrYlRd", "GrRd")) {
  name <- match.arg(name)
  switch(name,
    GrYlRd = list(
      neutral = c(255L, 215L,   0L),   # #FFD700  golden yellow
      green   = c(  0L, 104L,  55L),   # #006837  deep green
      red     = c(202L,   0L,  32L)    # #ca0020  deep red
    ),
    GrRd = list(
      neutral = c(255L, 255L, 255L),   # #FFFFFF  white
      green   = c(  0L, 104L,  55L),   # #006837  deep green
      red     = c(202L,   0L,  32L)    # #ca0020  deep red
    )
  )
}

# ---------------------------------------------------------------------------
# pval_to_color_gradient()
# ---------------------------------------------------------------------------

#' Map a signed p-value to a continuous background colour
#'
#' Produces a smooth diverging gradient using the selected palette:
#' \itemize{
#'   \item Positive (beneficial) and small p → deep green (\code{#006837})
#'   \item Positive (beneficial) and large p → neutral colour (palette-dependent)
#'   \item Negative (harmful)    and small |p| → deep red (\code{#ca0020})
#'   \item Negative (harmful)    and large |p| → neutral colour
#'   \item Trivial effect (point estimate within \code{trivial}) → steel blue
#'     (\code{#4E88B4}), regardless of p-value.
#' }
#' The gradient is \code{t = min(1, -log10(|p|) / 2)} (t=0 at p=1, t=1 at p=0.01).
#'
#' @param signed_pv Numeric signed p-value or vector.
#' @param trivial Logical vector of the same length as \code{signed_pv}.
#'   \code{TRUE} = effect is within the trivial range and should be shown in
#'   steel blue regardless of direction or p-value. Default \code{NULL} = no
#'   trivial override.
#' @param palette Palette name passed to \code{\link{pval_palette}}.
#'   \code{"GrYlRd"} (default) or \code{"GrRd"}.
#' @return Character hex colour string (vectorised).
#' @keywords internal
pval_to_color_gradient <- function(signed_pv, trivial = NULL, palette = "GrYlRd") {
  pal         <- pval_palette(palette)
  neutral_rgb <- pal$neutral
  green_rgb   <- pal$green
  red_rgb     <- pal$red
  trivial_hex <- "#4E88B4"
  neutral_hex <- sprintf("#%02X%02X%02X", neutral_rgb[1], neutral_rgb[2], neutral_rgb[3])

  if (is.null(trivial)) trivial <- rep(FALSE, length(signed_pv))

  out          <- rep(neutral_hex, length(signed_pv))
  out[trivial] <- trivial_hex

  grad_idx <- which(!is.na(signed_pv) & !trivial)
  if (length(grad_idx) == 0) return(out)

  pv    <- signed_pv[grad_idx]
  p_abs <- pmax(1e-10, pmin(1, abs(pv)))
  t_val <- pmin(1, -log10(p_abs) / 2)       # p=1→0 (neutral), p=0.01→1 (full)

  is_ben <- pv >= 0
  end_r  <- ifelse(is_ben, green_rgb[1], red_rgb[1])
  end_g  <- ifelse(is_ben, green_rgb[2], red_rgb[2])
  end_b  <- ifelse(is_ben, green_rgb[3], red_rgb[3])

  r_out <- pmax(0L, pmin(255L, as.integer(round(neutral_rgb[1] + t_val * (end_r - neutral_rgb[1])))))
  g_out <- pmax(0L, pmin(255L, as.integer(round(neutral_rgb[2] + t_val * (end_g - neutral_rgb[2])))))
  b_out <- pmax(0L, pmin(255L, as.integer(round(neutral_rgb[3] + t_val * (end_b - neutral_rgb[3])))))

  out[grad_idx] <- sprintf("#%02X%02X%02X", r_out, g_out, b_out)
  out
}

# ---------------------------------------------------------------------------
# schneider_thoma_colors()
# ---------------------------------------------------------------------------

#' Assign colours using the SchneiderThoma2026 scheme
#'
#' Categorical colour assignment based on the relationship between effect
#' size, 95% CI, and a user-defined trivial (very small) effects range:
#' \itemize{
#'   \item \strong{Blue} (\code{#4E88B4}): point estimate \emph{and} entire
#'     95% CI fall within the very small effects range.
#'   \item \strong{Yellow} (\code{#FFD700}): point estimate is outside the
#'     very small effects range, the 95% CI does not include the value of no
#'     difference (statistically significant), but the 95% CI still overlaps
#'     with the very small effects range.
#'   \item \strong{Orange} (\code{#F08000}): point estimate \emph{and} the
#'     95% CI entirely exclude the very small effects range (i.e., the CI
#'     is fully beyond the trivial threshold).
#'   \item \strong{White} (\code{#FFFFFF}): all other cases (e.g.
#'     non-significant results, or trivial range not supplied).
#' }
#'
#' All values (\code{te}, \code{lo}, \code{hi}, \code{trivial_range}) must be
#' on the same scale: log scale for OR/RR/HR, raw scale for MD/SMD.
#' The null value (no difference) is always 0 on this scale.
#'
#' @param te Numeric vector of point estimates.
#' @param lo Numeric vector of lower 95% CI bounds.
#' @param hi Numeric vector of upper 95% CI bounds.
#' @param trivial_range Numeric vector \code{c(lo_thresh, hi_thresh)} defining
#'   the very small effects range. \code{NULL} returns white for all cells.
#' @return A list with:
#'   \item{\code{bg}}{Character vector of background hex colours.}
#'   \item{\code{text}}{Character vector of text hex colours (\code{"#FFFFFF"}
#'     or \code{"#000000"}).}
#' @keywords internal
schneider_thoma_colors <- function(te, lo, hi, trivial_range = NULL) {
  n        <- length(te)
  bg_out   <- rep("#FFFFFF", n)
  text_out <- rep("#000000", n)

  if (is.null(trivial_range) || length(trivial_range) != 2L) {
    return(list(bg = bg_out, text = text_out))
  }

  T_lo <- trivial_range[1]
  T_hi <- trivial_range[2]
  # null value on this scale is always 0 (log-OR = 0 → OR = 1, MD = 0, etc.)

  blue   <- "#4E88B4"
  yellow <- "#FFD700"
  orange <- "#F08000"

  valid <- !is.na(te) & !is.na(lo) & !is.na(hi)

  # Blue: entire CI within trivial range
  is_blue   <- valid & lo >= T_lo & hi <= T_hi
  # Orange: te and CI both entirely outside trivial on same side
  is_orange <- valid & !is_blue &
               ((te < T_lo & hi < T_lo) | (te > T_hi & lo > T_hi))
  # Yellow: te beyond trivial, CI excludes null but overlaps trivial
  is_yellow <- valid & !is_blue & !is_orange &
               ((te < T_lo & hi < 0 & hi >= T_lo) | (te > T_hi & lo > 0 & lo <= T_hi))

  bg_out[is_blue]   <- blue;   text_out[is_blue]   <- "#FFFFFF"
  bg_out[is_orange] <- orange; text_out[is_orange] <- "#FFFFFF"
  bg_out[is_yellow] <- yellow  # text stays "#000000"

  list(bg = bg_out, text = text_out)
}

# ---------------------------------------------------------------------------
# pval_legend_ggplot()
# ---------------------------------------------------------------------------

#' Build a p-value colour legend as a ggplot
#'
#' Returns a small ggplot showing the diverging gradient with
#' labelled breakpoints (p < 0.01, p = 0.05, p = 0.1, p = 1.00).
#'
#' @param base_size Base font size (default 7).
#' @param palette Palette name passed to \code{\link{pval_palette}}.
#'   \code{"GrYlRd"} (default) or \code{"GrRd"}.
#' @return A \code{ggplot} object.
#' @keywords internal
pval_legend_ggplot <- function(base_size = 7, palette = "GrYlRd") {
  n <- 500
  # y ranges from 1 (top = highly significant beneficial) to
  # -1 (bottom = highly significant harmful) via 0 (non-significant).
  # Mapping: y ≥ 0 → signed_pv = 10^(-4*y)  (y=0→p=1.0, y=1→p=0.0001)
  #          y < 0 → signed_pv = -(10^(4*y)) (y=0→p=-1.0≡white, y=-1→p=-0.0001)
  y_vals <- seq(1, -1, length.out = n)
  signed_pv_vals <- ifelse(
    y_vals >= 0,
    10^(-4 * y_vals),         # y=0 → 1.0 (white); y=1 → 0.0001 (yellow)
    -(10^(4 * y_vals))        # y=0 → -1.0 ≡ white; y=-1 → -0.0001 (yellow)
  )

  df_leg <- data.frame(y = y_vals, pv = signed_pv_vals, stringsAsFactors = FALSE)
  df_leg$color <- pval_to_color_gradient(df_leg$pv, palette = palette)

  # Breakpoints: y = -log10(p) / 4 (matching the 10^(-4*y) mapping)
  # y=1 → p=0.0001 (full green); y=0 → p=1.0 (yellow); y=-1 → p=0.0001 (full red)
  brk_y <- c(
     1,                          # p → 0  (deep green)
     -log10(0.01) / 4,           # p = 0.01
     -log10(0.05) / 4,           # p = 0.05
     -log10(0.1)  / 4,           # p = 0.1
     0,                          # p = 1.00 (yellow, non-significant)
     log10(0.1)   / 4,           # p = 0.1  harmful
     log10(0.05)  / 4,           # p = 0.05 harmful
    -log10(0.01) / 4,            # p = 0.01 harmful
    -1                           # p → 0  (deep red)
  )
  brk_lab <- c("p < 0.01", "p = 0.01", "p = 0.05", "p = 0.1",
               "p = 1.00 (yellow)",
               "p = 0.1", "p = 0.05", "p = 0.01", "p < 0.01")

  ggplot2::ggplot(df_leg, ggplot2::aes(x = 1, y = y, fill = color)) +
    ggplot2::geom_tile(width = 1, height = 2 / n) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      breaks = brk_y, labels = brk_lab,
      limits = c(-1, 1), expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x       = ggplot2::element_blank(),
      axis.ticks.x      = ggplot2::element_blank(),
      axis.text.y       = ggplot2::element_text(size = base_size, colour = "grey20",
                                                  hjust = 0),
      axis.ticks.y      = ggplot2::element_line(colour = "grey60", linewidth = 0.3),
      panel.grid        = ggplot2::element_blank(),
      plot.background   = ggplot2::element_blank(),
      panel.background  = ggplot2::element_blank(),
      plot.margin       = ggplot2::unit(c(2, 2, 2, 2), "mm")
    )
}

# ---------------------------------------------------------------------------
# format_ci_cell()
# ---------------------------------------------------------------------------

#' Format a single estimate + CI as a two-line string
#'
#' Returns a string with the point estimate on the first line and the CI
#' on the second line, e.g. \code{1.16} on one line followed by
#' \code{(0.68; 1.97)} on the next.
#'
#' @param est Point estimate (numeric).
#' @param lower Lower CI bound (numeric).
#' @param upper Upper CI bound (numeric).
#' @param fmt sprintf format string (default `"\%.2f"`).
#' @return Character string.
#' @keywords internal
format_ci_cell <- function(est, lower, upper, fmt = "%.2f") {
  paste0(
    sprintf(fmt, est),
    "\n(",
    sprintf(fmt, lower),
    "; ",
    sprintf(fmt, upper),
    ")"
  )
}

# ---------------------------------------------------------------------------
# .estimate_pooled_sd()
# ---------------------------------------------------------------------------

#' Estimate pooled SD from a netmeta object (MD outcomes)
#'
#' Recovers the pooled within-study standard deviation from the study-level
#' standard errors and sample sizes stored in \code{x$data}.
#'
#' The relationship between \code{seTE} and \code{pooled_sd} for a two-arm
#' continuous outcome is:
#' \deqn{\text{seTE}_i = \text{pooled\_sd}_i \cdot \sqrt{1/n_{1i} + 1/n_{2i}}}
#' Rearranging: \eqn{\text{pooled\_sd}_i = \text{seTE}_i \cdot
#' \sqrt{n_{1i} \cdot n_{2i} / (n_{1i} + n_{2i})}}.
#'
#' Study-level estimates are combined as a harmonic-mean-of-n weighted average.
#'
#' @param x A \code{netmeta} object with \code{sm = "MD"}.
#' @return Numeric scalar (estimated pooled SD), or \code{NULL} with a warning
#'   if \code{n1}/\code{n2}/\code{seTE} are not available.
#' @keywords internal
.estimate_pooled_sd <- function(x) {
  d <- tryCatch(x$data, error = function(e) NULL)
  if (is.null(d)) {
    warning("Cannot estimate pooled SD: netmeta data slot is empty.")
    return(NULL)
  }

  n_cols   <- grep("^n[12]$",    colnames(d), value = TRUE)
  seTE_col <- "seTE"

  if (length(n_cols) < 2 || !seTE_col %in% colnames(d)) {
    warning("Cannot estimate pooled SD: need n1, n2, and seTE columns in data.")
    return(NULL)
  }

  n1   <- d[[n_cols[1]]]
  n2   <- d[[n_cols[2]]]
  seTE <- d[[seTE_col]]

  ok <- !is.na(n1) & !is.na(n2) & !is.na(seTE) & n1 > 0 & n2 > 0 & seTE > 0
  if (!any(ok)) {
    warning("Cannot estimate pooled SD: no valid n1/n2/seTE rows found.")
    return(NULL)
  }

  n1v <- n1[ok]; n2v <- n2[ok]; seTEv <- seTE[ok]

  # pooled_sd per study = seTE × sqrt(n1 × n2 / (n1 + n2))
  sd_i  <- seTEv * sqrt(n1v * n2v / (n1v + n2v))
  # weight = harmonic mean of n (= effective sample size per arm)
  w_i   <- 2 * n1v * n2v / (n1v + n2v)

  stats::weighted.mean(sd_i, w = w_i)
}
