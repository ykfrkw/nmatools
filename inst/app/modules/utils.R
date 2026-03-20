# =============================================================================
# utils.R — NMA Evaluator shared utilities
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
