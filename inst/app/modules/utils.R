# =============================================================================
# utils.R \u2014 NMA Evaluator shared utilities
# =============================================================================
# Shared constants and helper functions used across all modules.
# Loaded first in app.R via source("modules/utils.R").
# =============================================================================

# ----------------------------------------------------------------------------
# Null-coalescing operator
# ----------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ----------------------------------------------------------------------------
# CINeMA colour palette (matches Excel default theme)
# ----------------------------------------------------------------------------
CINEMA_COLOURS <- c(
  "No concerns"    = "#70AD47",  # Excel green
  "Some concerns"  = "#FFC000",  # Excel gold/amber
  "Major concerns" = "#C00000",  # Excel dark red
  "Not assessed"   = "#BFBFBF"   # Grey
)
CINEMA_TEXT <- c(
  "No concerns"    = "white",
  "Some concerns"  = "black",
  "Major concerns" = "white",
  "Not assessed"   = "white"
)
CONFIDENCE_COLOURS <- c(
  "High"     = "#4472C4",  # Excel blue
  "Moderate" = "#5B9BD5",  # Excel light blue
  "Low"      = "#ED7D31",  # Excel orange
  "Very low" = "#C00000"   # Dark red
)
ROBMEN_COLOURS <- c(
  "Low risk"      = "#4CAF50",
  "Some concerns" = "#FF9800",
  "High risk"     = "#F44336",
  "Not assessed"  = "#9E9E9E"
)

# Dot-prefixed aliases used by module_D (backward compatibility)
.CINEMA_COL  <- CINEMA_COLOURS
.CINEMA_TXT  <- CINEMA_TEXT
.CONF_COL    <- CONFIDENCE_COLOURS
.ROBMEN_COL  <- ROBMEN_COLOURS

# ----------------------------------------------------------------------------
# netmetaviz-inspired colour palettes (classic & pastel)
# Reference: Seo et al. netmetaviz R package (PiYG diverging scale)
# Used by Module D for switchable palette display.
# ----------------------------------------------------------------------------
# Colour mapping rule for CINeMA domain ratings:
#   No concerns    → High confidence colour
#   Some concerns  → Low confidence colour
#   Major concerns → Very low confidence colour
#   Not assessed   → neutral grey
.NMV_PALETTES <- list(
  classic = list(
    # Confidence levels: netmetaviz classic_palette()
    conf = c(
      "High"     = "#1e8449",
      "Moderate" = "#2471a3",
      "Low"      = "#e67e22",
      "Very low" = "#c0392b",
      "Not set"  = "#bfbfbf"
    ),
    conf_txt = c(
      "High"     = "#ffffff",
      "Moderate" = "#ffffff",
      "Low"      = "#ffffff",
      "Very low" = "#ffffff",
      "Not set"  = "#ffffff"
    ),
    # CINeMA domain ratings: No concerns→High, Some concerns→Low, Major concerns→Very low
    cinema = c(
      "No concerns"    = "#1e8449",
      "Some concerns"  = "#e67e22",
      "Major concerns" = "#c0392b",
      "Not assessed"   = "#bfbfbf"
    ),
    cinema_txt = c(
      "No concerns"    = "#ffffff",
      "Some concerns"  = "#ffffff",
      "Major concerns" = "#ffffff",
      "Not assessed"   = "#ffffff"
    )
  ),
  pastel = list(
    # Confidence levels: netmetaviz pastel_palette()
    conf = c(
      "High"     = "#d7e8d3",
      "Moderate" = "#cccce9",
      "Low"      = "#f8edd7",
      "Very low" = "#e8d0d0",
      "Not set"  = "#e8e8e8"
    ),
    conf_txt = c(
      "High"     = "#238b21",
      "Moderate" = "#01008b",
      "Low"      = "#daa521",
      "Very low" = "#8b0000",
      "Not set"  = "#888888"
    ),
    # CINeMA domain ratings: No concerns→High, Some concerns→Low, Major concerns→Very low
    cinema = c(
      "No concerns"    = "#d7e8d3",
      "Some concerns"  = "#f8edd7",
      "Major concerns" = "#e8d0d0",
      "Not assessed"   = "#e8e8e8"
    ),
    cinema_txt = c(
      "No concerns"    = "#238b21",
      "Some concerns"  = "#daa521",
      "Major concerns" = "#8b0000",
      "Not assessed"   = "#888888"
    )
  )
)

# ----------------------------------------------------------------------------
# format_te_ci: render "TE [lo, hi]" with sm-aware back-transformation.
# OR/RR/HR are stored on log scale internally by netmeta; exp() to display.
# ----------------------------------------------------------------------------
format_te_ci <- function(te, lo, hi, sm, digits = 2) {
  if (length(sm) != 1L || is.null(sm) || is.na(sm)) sm <- ""
  if (toupper(sm) %in% c("OR", "RR", "HR")) {
    te <- exp(te); lo <- exp(lo); hi <- exp(hi)
  }
  if (is.na(te)) return("\u2014")
  ci_part <- if (!is.na(lo) && !is.na(hi))
    paste0(" [", format(round(lo, digits), nsmall = digits), ", ",
           format(round(hi, digits), nsmall = digits), "]")
  else ""
  paste0(format(round(te, digits), nsmall = digits), ci_part)
}

# Header label for a TE column given the summary measure.
te_col_label <- function(sm) {
  s <- toupper(sm %||% "")
  if (s %in% c("OR", "RR", "HR")) paste0(s, " [95% CI]") else "TE [95% CI]"
}

# ----------------------------------------------------------------------------
# delta_default_for_measure: clinical-threshold default per effect measure.
#   SMD : 0.2          (Cohen's small effect)
#   OR  : 1.25         (small clinically meaningful odds shift)
#   RR  : 1.2
#   MD  : pooled within-group SD * 0.2 (so "MD 0.2*SD" maps to SMD 0.2);
#         requires se / n1 / n2 columns. Falls back to 0.2 if data is
#         unavailable or the pooled SD cannot be reconstructed.
# ----------------------------------------------------------------------------
delta_default_for_measure <- function(em, data = NULL) {
  if (is.null(em) || !nzchar(em)) em <- "SMD"
  if (em == "SMD") return(0.2)
  if (em == "OR")  return(1.25)
  if (em == "RR")  return(1.2)
  if (em == "MD") {
    if (is.null(data) || !is.data.frame(data)) return(0.2)
    if (!all(c("se", "n1", "n2") %in% names(data))) return(0.2)
    ok <- !is.na(data$se) & data$se > 0 &
          !is.na(data$n1) & !is.na(data$n2) &
          data$n1 > 0 & data$n2 > 0
    if (!any(ok)) return(0.2)
    # se(MD) = s_p * sqrt(1/n1 + 1/n2)  =>  s_p = se / sqrt(1/n1 + 1/n2)
    s_p  <- data$se[ok] / sqrt(1 / data$n1[ok] + 1 / data$n2[ok])
    wts  <- data$n1[ok] + data$n2[ok]
    pooled_sd <- sqrt(sum(wts * s_p^2) / sum(wts))
    if (is.na(pooled_sd) || pooled_sd <= 0) return(0.2)
    return(round(pooled_sd * 0.2, 3))
  }
  0.2
}

# ----------------------------------------------------------------------------
# build_netmeta_forest: render the canonical netmeta::forest() output to the
# currently-open graphics device, parameterised by a Display Options list.
# Used by both the inline imageOutput render and the dl_forest download
# handler so the on-screen plot and the downloaded PNG always match.
#
# `opts` is a flat list. All fields are optional; missing fields fall back to
# forest.netmeta()'s own defaults.
#   $reference        : reference treatment (string)
#   $sortvar          : "pscore" / "estimate" / "alpha" / NULL = netmeta default
#   $small_values     : "desirable" / "undesirable" — feeds netrank() for
#                       pscore sort. Defaults to "desirable".
#   $xlim             : numeric length-2 (NA-tolerant; both NA = auto)
#   $log_scale        : logical; for OR/RR/HR maps to backtransf=TRUE +
#                       drawing on log scale via meta defaults
#   $show_k           : logical; adds "k" to leftcols if TRUE
#   $show_weight      : logical; adds "w.random" / "w.common" to rightcols
#   $prediction       : logical; show prediction interval row
#   $print_hetstat    : logical; show tau^2 + I^2 row
#   $estimate_shape   : "diamond" (default) / "square"
#   $smlab            : header text (string)
#   $label_left       : left-side label (string)
#   $label_right      : right-side label (string)
#   $smaller_text     : logical; fontsize 0.85 vs 1.0
# ----------------------------------------------------------------------------
build_netmeta_forest <- function(net, opts = list()) {
  if (is.null(net)) stop("build_netmeta_forest: 'net' is required")

  pooled_kind <- if (isTRUE(net$random)) "random" else "common"

  # Sort variable
  sortvar <- NULL
  sort_key <- opts$sortvar %||% "pscore"
  small_v  <- opts$small_values %||% "desirable"
  if (identical(sort_key, "pscore")) {
    pr <- tryCatch(netmeta::netrank(net, small.values = small_v),
                   error = function(e) NULL)
    if (!is.null(pr)) {
      ps <- pr$Pscore.random %||% pr$Pscore.common
      if (!is.null(ps)) {
        # forest.netmeta uses sortvar evaluated in net$trts order;
        # higher P-score → top, so negate when desirable.
        sortvar <- if (identical(small_v, "undesirable")) ps[net$trts]
                   else                                    -ps[net$trts]
      }
    }
  } else if (identical(sort_key, "estimate")) {
    # Use the random/common TE column from netmeta's leag table.
    te_mat <- net$TE.random %||% net$TE.common
    ref    <- opts$reference %||% net$reference.group %||% net$trts[1]
    if (!is.null(te_mat) && ref %in% rownames(te_mat))
      sortvar <- -te_mat[net$trts, ref]   # largest at top
  } else if (identical(sort_key, "alpha")) {
    sortvar <- net$trts
  } # else NULL → forest.netmeta uses its own default order (net$seq)

  # Columns
  leftcols  <- "studlab"
  if (isTRUE(opts$show_k)) leftcols <- c(leftcols, "k")
  rightcols <- c("effect", "ci")
  if (isTRUE(opts$show_weight))
    rightcols <- c(rightcols,
                   if (identical(pooled_kind, "random")) "w.random"
                   else                                  "w.common")

  # xlim
  xlim <- NULL
  if (!is.null(opts$xlim) && length(opts$xlim) == 2 &&
      all(!is.na(opts$xlim)))
    xlim <- as.numeric(opts$xlim)

  # backtransf only matters for OR/RR/HR; netmeta picks backtransf by sm.
  # We keep its default unless user asked for the raw log scale.
  backtransf <- isTRUE(opts$log_scale %||% TRUE)

  # Diamond vs square — forest.meta has no per-row override here, but the
  # `plotwidth` family + `col.diamond` controls colour. Shape is fixed in
  # this iteration; the toggle is informational. Documented in spec-09.

  fontsize <- if (isTRUE(opts$smaller_text)) 0.85 else 1.0

  # Build the call. Pass-through args via list so we can drop NULLs cleanly.
  call_args <- list(
    x                = net,
    pooled           = pooled_kind,
    reference.group  = opts$reference %||% net$reference.group,
    leftcols         = leftcols,
    rightcols        = rightcols,
    backtransf       = backtransf,
    print.tau2       = isTRUE(opts$print_hetstat),
    print.I2         = isTRUE(opts$print_hetstat),
    overall.hetstat  = isTRUE(opts$print_hetstat),
    fontsize         = fontsize,
    smlab            = opts$smlab %||% NULL,
    label.left       = opts$label_left  %||% NULL,
    label.right      = opts$label_right %||% NULL
  )
  if (!is.null(sortvar)) call_args$sortvar <- sortvar
  if (!is.null(xlim))    call_args$xlim    <- xlim
  # NB: `prediction` is a forest.netsplit / forest.meta arg, NOT a
  # forest.netmeta arg — passing it errors out. Spec-09's "Show prediction
  # interval row" toggle is therefore a no-op in this iteration; reopen if
  # netmeta upstream adds prediction support to forest.netmeta.

  # Drop NULL entries so forest.netmeta uses its own defaults.
  call_args <- call_args[!vapply(call_args, is.null, logical(1))]

  # netmeta::forest is not exported under that name; the S3 method is
  # netmeta:::forest.netmeta and the generic `forest` lives in the meta
  # package. Call the method directly to avoid relying on namespace search.
  do.call(getFromNamespace("forest.netmeta", "netmeta"), call_args)
  invisible(NULL)
}

# ----------------------------------------------------------------------------
# get_contrib_matrix: extract the contribution matrix from a netcontrib object.
# Tries multiple field names across different netmeta versions.
# ----------------------------------------------------------------------------
get_contrib_matrix <- function(contrib) {
  if (is.null(contrib)) return(NULL)
  contrib[["random"]]                       %||%
    contrib[["common"]]                     %||%
    contrib[["fixed"]]                      %||%
    contrib[["contribution.matrix.random"]] %||%
    contrib[["contribution.matrix"]]        %||%
    contrib[["contribution.matrix.common"]] %||%
    contrib[["H.random"]]                   %||%
    contrib[["H"]]
}
