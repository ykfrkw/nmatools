#' @title Kilim plot (multi-outcome coloured summary table)
#'
#' @description Creates a Kilim plot: a summary table showing NMA results across
#'   multiple outcomes, with cells coloured by the signed p-value (green =
#'   beneficial, red = harmful, blue = trivial effect). Reference cells are
#'   uncoloured. Output is an Excel (\code{.xlsx}) file.
#'
#' @param outcomes A list of outcome specifications. Each element is a list
#'   with the following fields:
#'   \itemize{
#'     \item \code{x}: A \code{netmeta} object, or a character path to an
#'       \code{.rds} file containing one.
#'     \item \code{name}: Outcome name (used to construct column names
#'       internally).
#'     \item \code{reference}: Reference treatment name.
#'     \item \code{small_values}: \code{"desirable"} or \code{"undesirable"}.
#'       Whether small values of the effect estimate are good (e.g., OR < 1
#'       means better when \code{small_values = "desirable"}).
#'     \item \code{digits}: Decimal places (default 2; use 0 for
#'       minutes/percentages).
#'     \item \code{label}: Column header label (optional; defaults to
#'       \code{name}).
#'     \item \code{trivial_range}: Per-outcome trivial range on the \emph{effect}
#'       scale (before exponentiation for OR/RR/HR). A numeric vector of length 2
#'       \code{c(lo, hi)} on the \strong{log} scale for OR/RR/HR, or the raw
#'       scale for MD/SMD. When the point estimate falls within this interval,
#'       the cell is shown in steel blue regardless of p-value. Overrides the
#'       top-level \code{trivial_range} for this outcome.
#'   }
#' @param trivial_range Default trivial range applied to every outcome that does
#'   not specify its own \code{trivial_range}. Numeric vector \code{c(lo, hi)}
#'   on the log scale for OR/RR/HR, raw scale for MD/SMD. \code{NULL} (default)
#'   disables trivial-effect highlighting.
#' @param sort_by How to sort treatment rows: \code{"alphabet"} (default),
#'   \code{"pscore"}, \code{"es"}, \code{"es_rev"}, \code{"pvalue"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param sort_order Custom order vector (used when \code{sort_by = "custom"}).
#' @param treat_labels Named character vector for renaming treatments.
#' @param show_ci Show 95\% CI? (default \code{TRUE}).
#' @param common Logical. Use common-effects model? Default \code{FALSE} = random-effects model.
#' @param fixed Deprecated. Use \code{common} instead.
#' @param reference Reference treatment for sorting (taken from first
#'   outcome if not specified).
#' @param file Output file path (must end in \code{.xlsx}).
#' @return Invisibly returns the \code{openxlsx} workbook object.
#' @export
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle setColWidths saveWorkbook
kilim <- function(outcomes,
                  trivial_range = NULL,
                  sort_by    = "alphabet",
                  sort_order = NULL,
                  treat_labels = NULL,
                  show_ci    = TRUE,
                  common     = FALSE,
                  fixed      = NULL,
                  reference  = NULL,
                  palette    = "GrRd",
                  file       = "kilim.xlsx") {

  if (!is.list(outcomes) || length(outcomes) == 0)
    stop("`outcomes` must be a non-empty list.")

  valid_palettes <- c("GrYlRd", "GrRd", "SchneiderThoma2026")
  if (!palette %in% valid_palettes)
    stop("`palette` must be one of: ", paste(valid_palettes, collapse = ", "))

  if (!is.null(fixed)) {
    message("Argument 'fixed' is deprecated in kilim(); use 'common' instead.")
    common <- fixed
  }

  # ---- 1. Load netmeta objects ----
  outcomes <- lapply(outcomes, function(oc) {
    if (is.character(oc$x)) {
      oc$x <- readRDS(oc$x)
    }
    oc
  })

  # ---- 2. Determine reference ----
  first_x <- outcomes[[1]]$x
  if (is.null(reference)) {
    reference <- outcomes[[1]]$reference
    if (is.null(reference)) reference <- first_x$reference.group
    if (is.null(reference)) reference <- first_x$trts[1]
  }

  # ---- 3. Get all treatments from all outcomes (union) ----
  # Sort treatments from the first outcome normally; append extra treatments
  # (from other outcomes not in first_x) alphabetically at the end.
  first_trts <- first_x$trts
  extra_trts <- sort(setdiff(
    unique(unlist(lapply(outcomes[-1], function(oc) oc$x$trts))),
    first_trts
  ))

  # ---- 4. Sort treatments ----
  sorted_first <- sort_treatments(
    treatments = first_trts,
    sort_by    = sort_by,
    x          = first_x,
    reference  = reference,
    sort_order = sort_order,
    common     = common
  )
  treatments <- c(sorted_first, extra_trts)

  # ---- 5. Apply treat_labels ----
  display_labels <- treatments
  names(display_labels) <- treatments
  if (!is.null(treat_labels)) {
    idx <- match(treatments, names(treat_labels))
    display_labels[!is.na(idx)] <- treat_labels[idx[!is.na(idx)]]
  }

  # ---- 6. Build data frame: one row per treatment ----
  # Columns: "treatment_label", then for each outcome: effect_col and p_col
  df <- data.frame(treatment_label = display_labels[treatments],
                   stringsAsFactors = FALSE)

  effect_col_names  <- character(length(outcomes))
  p_col_names       <- character(length(outcomes))
  outcome_labels    <- character(length(outcomes))
  trivial_col_names <- character(length(outcomes))
  # Extra columns for SchneiderThoma2026 (raw TE/lo/hi on log/raw scale)
  te_raw_col_names  <- character(length(outcomes))
  lo_raw_col_names  <- character(length(outcomes))
  hi_raw_col_names  <- character(length(outcomes))
  trivial_ranges    <- vector("list", length(outcomes))

  for (k in seq_along(outcomes)) {
    oc  <- outcomes[[k]]
    nm  <- oc$name
    ref <- if (!is.null(oc$reference)) oc$reference else reference
    sv  <- if (!is.null(oc$small_values)) oc$small_values else "undesirable"
    dg  <- if (!is.null(oc$digits)) oc$digits else 2
    lbl <- if (!is.null(oc$label)) oc$label else nm
    nx  <- oc$x
    # Per-outcome trivial range overrides top-level argument
    tr  <- if (!is.null(oc$trivial_range)) oc$trivial_range else trivial_range

    fmt <- paste0("%.", dg, "f")

    outcome_labels[k]    <- lbl
    effect_col_names[k]  <- paste0("effect_", nm)
    p_col_names[k]       <- paste0("pval_", nm)
    trivial_col_names[k] <- paste0("trivial_", nm)
    te_raw_col_names[k]  <- paste0("te_raw_", nm)
    lo_raw_col_names[k]  <- paste0("lo_raw_", nm)
    hi_raw_col_names[k]  <- paste0("hi_raw_", nm)
    trivial_ranges[[k]]  <- tr

    # Effect and CI matrices (may need exp() for OR/RR)
    te_mat    <- .nm_mat(nx, "TE",    common)
    lo_mat    <- .nm_mat(nx, "lower", common)
    hi_mat    <- .nm_mat(nx, "upper", common)
    pval_mat  <- .nm_mat(nx, "pval",  common)

    is_log_scale <- nx$sm %in% c("OR", "RR", "HR")

    # Build per-treatment cell strings, signed p-values, trivial flags
    effect_cells   <- character(length(treatments))
    pvals_signed   <- numeric(length(treatments))
    trivial_flags  <- logical(length(treatments))
    te_raw_cells   <- numeric(length(treatments))
    lo_raw_cells   <- numeric(length(treatments))
    hi_raw_cells   <- numeric(length(treatments))

    for (i in seq_along(treatments)) {
      trt <- treatments[i]

      if (trt == ref || !ref %in% rownames(te_mat) || !trt %in% rownames(te_mat)) {
        effect_cells[i]  <- if (trt == ref) "Reference" else NA_character_
        pvals_signed[i]  <- NA_real_
        trivial_flags[i] <- FALSE
        te_raw_cells[i]  <- NA_real_
        lo_raw_cells[i]  <- NA_real_
        hi_raw_cells[i]  <- NA_real_
        next
      }

      raw_te <- te_mat[trt, ref]   # on log scale for OR/RR/HR
      raw_lo <- lo_mat[trt, ref]
      raw_hi <- hi_mat[trt, ref]
      te_val <- raw_te
      lo_val <- raw_lo
      hi_val <- raw_hi
      pv     <- pval_mat[trt, ref]

      if (is_log_scale) {
        te_val <- exp(te_val)
        lo_val <- exp(lo_val)
        hi_val <- exp(hi_val)
      }

      if (show_ci) {
        effect_cells[i] <- format_ci_cell(te_val, lo_val, hi_val, fmt = fmt)
      } else {
        effect_cells[i] <- sprintf(fmt, te_val)
      }

      # Signed p-value: negative means unfavourable direction
      # raw_te is on the log scale for OR/RR/HR, raw scale for others
      signed_pv <- pv
      if (sv == "desirable" && raw_te > 0)   signed_pv <- -pv
      if (sv == "undesirable" && raw_te < 0) signed_pv <- -pv
      pvals_signed[i] <- signed_pv

      # Trivial flag: point estimate within trivial_range (on log scale)
      trivial_flags[i] <- !is.null(tr) &&
        length(tr) == 2L &&
        raw_te >= tr[1] && raw_te <= tr[2]

      # Raw (log/raw scale) values for SchneiderThoma2026
      te_raw_cells[i] <- raw_te
      lo_raw_cells[i] <- raw_lo
      hi_raw_cells[i] <- raw_hi
    }

    df[[effect_col_names[k]]]  <- effect_cells
    df[[p_col_names[k]]]       <- pvals_signed
    df[[trivial_col_names[k]]] <- trivial_flags
    df[[te_raw_col_names[k]]]  <- te_raw_cells
    df[[lo_raw_col_names[k]]]  <- lo_raw_cells
    df[[hi_raw_col_names[k]]]  <- hi_raw_cells
  }

  # ---- 7. Build xlsx workbook ----
  # Header row: "Treatment" + outcome labels
  header_row <- c("Treatment", outcome_labels)

  # Data rows: treatment_label + effect columns
  display_df <- df[, c("treatment_label", effect_col_names), drop = FALSE]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Kilim")

  # Write header
  openxlsx::writeData(wb, sheet = 1,
                      x = as.data.frame(t(header_row), stringsAsFactors = FALSE),
                      startRow = 1, colNames = FALSE)

  # Write data
  openxlsx::writeData(wb, sheet = 1, x = display_df,
                      startRow = 2, colNames = FALSE)

  n_rows <- nrow(display_df)
  n_cols <- length(effect_col_names) + 1L  # +1 for treatment_label

  # Base style: centre + wrap
  base_sty <- openxlsx::createStyle(halign = "center", valign = "center",
                                    wrapText = TRUE)
  openxlsx::addStyle(wb, sheet = 1, style = base_sty,
                     rows = seq_len(n_rows + 1L),
                     cols = seq_len(n_cols),
                     gridExpand = TRUE)

  # Header style: bold + bottom border
  hdr_sty <- openxlsx::createStyle(halign = "center", valign = "center",
                                   textDecoration = "bold",
                                   border = "bottom", borderStyle = "thin")
  openxlsx::addStyle(wb, sheet = 1, style = hdr_sty,
                     rows = 1L, cols = seq_len(n_cols),
                     gridExpand = TRUE)

  # Apply kilim colours to each outcome column
  is_st2026 <- identical(palette, "SchneiderThoma2026")

  for (k in seq_along(outcomes)) {
    if (is_st2026) {
      st_cols <- schneider_thoma_colors(
        te  = df[[te_raw_col_names[k]]],
        lo  = df[[lo_raw_col_names[k]]],
        hi  = df[[hi_raw_col_names[k]]],
        trivial_range = trivial_ranges[[k]]
      )
      bg_col  <- st_cols$bg
      tx_col  <- st_cols$text
      non_na  <- which(!is.na(df[[te_raw_col_names[k]]]))
    } else {
      p       <- df[[p_col_names[k]]]
      trivial <- df[[trivial_col_names[k]]]
      bg_col  <- pval_to_color_gradient(p, trivial = trivial, palette = palette)
      # White text when significant (p < 0.05) or trivial (steel blue bg)
      tx_col  <- ifelse((!is.na(p) & abs(p) < 0.05) | (!is.na(trivial) & trivial),
                        "#FFFFFF", "#000000")
      non_na  <- which(!is.na(p))
    }

    col_idx <- k + 1L  # offset by treatment_label column
    if (length(non_na) > 0) {
      pairs <- unique(data.frame(bg = bg_col[non_na], tx = tx_col[non_na],
                                 stringsAsFactors = FALSE))
      for (j in seq_len(nrow(pairs))) {
        sty <- openxlsx::createStyle(
          fgFill     = pairs$bg[j],
          fontColour = pairs$tx[j],
          halign     = "center",
          valign     = "center",
          wrapText   = TRUE
        )
        idx_j <- non_na[bg_col[non_na] == pairs$bg[j] & tx_col[non_na] == pairs$tx[j]]
        openxlsx::addStyle(wb, sheet = 1, style = sty,
                           rows = idx_j + 1L, cols = col_idx,
                           gridExpand = TRUE)
      }
    }
  }

  openxlsx::setColWidths(wb, sheet = 1, cols = seq_len(n_cols), widths = "auto")

  # ---- 7b. Legend sheet ----
  openxlsx::addWorksheet(wb, "Legend")

  if (is_st2026) {
    legend_rows <- data.frame(
      label = c(
        "Blue   (#4E88B4): te and 95% CI entirely within very small effects range",
        "Yellow (#FFD700): te beyond threshold, significant (CI excludes null), CI overlaps trivial",
        "Orange (#F08000): te and 95% CI entirely beyond the very small effects threshold",
        "White  (#FFFFFF): non-significant or other (CI crosses null)"
      ),
      bg  = c("#4E88B4", "#FFD700", "#F08000", "#FFFFFF"),
      txt = c("#FFFFFF", "#000000", "#FFFFFF", "#000000"),
      stringsAsFactors = FALSE
    )
    hdr_label <- "SchneiderThoma2026 colour scheme"
  } else {
    legend_rows <- data.frame(
      label    = c("p < 0.01 (beneficial, deep green)",
                   "p = 0.05 (beneficial)",
                   "p = 0.1  (beneficial)",
                   "p = 1.00 (non-significant, neutral)",
                   "p = 0.1  (harmful)",
                   "p = 0.05 (harmful)",
                   "p < 0.01 (harmful, deep red)"),
      signed_pv = c(0.009, 0.05, 0.1, 1.0, -0.1, -0.05, -0.009),
      stringsAsFactors = FALSE
    )
    legend_rows$bg  <- pval_to_color_gradient(legend_rows$signed_pv, palette = palette)
    legend_rows$txt <- ifelse(abs(legend_rows$signed_pv) < 0.05, "#FFFFFF", "#000000")
    hdr_label <- "Colour scale (p-value vs reference)"
  }

  openxlsx::writeData(wb, sheet = "Legend",
                      x = data.frame(V1 = hdr_label),
                      startRow = 1, colNames = FALSE)

  openxlsx::writeData(wb, sheet = "Legend",
                      x = data.frame(V1 = legend_rows$label),
                      startRow = 2, colNames = FALSE)

  hdr_leg_sty <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(wb, sheet = "Legend", style = hdr_leg_sty,
                     rows = 1L, cols = 1L)

  for (i in seq_len(nrow(legend_rows))) {
    sty_leg <- openxlsx::createStyle(
      fgFill     = legend_rows$bg[i],
      fontColour = legend_rows$txt[i],
      halign     = "left",
      valign     = "center"
    )
    openxlsx::addStyle(wb, sheet = "Legend", style = sty_leg,
                       rows = i + 1L, cols = 1L)
  }
  openxlsx::setColWidths(wb, sheet = "Legend", cols = 1L, widths = 30)

  # ---- 8. Save ----
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  message("Saved Kilim plot: ", file)

  invisible(wb)
}
