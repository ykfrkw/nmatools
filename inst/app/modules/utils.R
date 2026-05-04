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
