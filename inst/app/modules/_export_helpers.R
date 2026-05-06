# =============================================================================
# Export helpers for the Bundle ZIP (Phase B onwards)
# =============================================================================
# Pure helpers — no shiny / DT / plotly. Sourced by inst/app/app.R after the
# other helper files. Keeping them in a standalone file means the Word /
# Excel renderers can be smoke-tested in isolation and don't bloat the
# main module.
#
# Public functions
# ----------------
# write_landscape_table_docx(df, file, ...)
#   Write `df` to a single-table Word document in landscape A4 orientation.
#   Optional `cell_colors` / `cell_text_colors` matrices (same dim as df)
#   colour the body cells; the header row is always grey/bold.
#
# write_table_xlsx(df, file, sheet, ...)
#   Write `df` to an .xlsx workbook with bold header, auto column widths,
#   and (optionally) coloured cells.
#
# build_league_table_df(net, merged, sm = NULL)
#   Convert the NMA league matrix into a printable data.frame:
#     - Lower triangle: "TE [lo, hi]" for col vs row
#     - Diagonal: treatment name
#     - Upper triangle: empty
#   Returns list(df, cell_colors, cell_text_colors) for the writer helpers.
#
# build_robmen_export_df(robmen_results)
#   Pretty-print the list returned by moduleC_server's robmen_results
#   reactive into a data.frame ready for export. Returns NULL when
#   ROB-MEN has not been run.
# =============================================================================

# ----------------------------------------------------------------------------
# write_landscape_table_docx
# ----------------------------------------------------------------------------
write_landscape_table_docx <- function(df, file,
                                       title            = NULL,
                                       subtitle         = NULL,
                                       cell_colors      = NULL,
                                       cell_text_colors = NULL,
                                       footer           = NULL) {
  stopifnot(is.data.frame(df), nrow(df) >= 0)
  if (!requireNamespace("officer", quietly = TRUE))
    stop("officer is required for Word export")
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("flextable is required for Word export")

  ft <- flextable::flextable(df)
  ft <- flextable::theme_box(ft)
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#f0f0f0", part = "header")
  ft <- flextable::align(ft, align = "left", part = "header")

  # Cell colouring: iterate explicitly because flextable's `bg(j = ...)`
  # vectorises over rows but not over (i, j) tuples in one call.
  if (!is.null(cell_colors) && nrow(df) > 0) {
    for (j in seq_len(ncol(df))) {
      for (i in seq_len(nrow(df))) {
        bg <- cell_colors[i, j]
        if (!is.null(bg) && !is.na(bg) && nzchar(bg)) {
          ft <- flextable::bg(ft, i = i, j = j, bg = bg, part = "body")
        }
        if (!is.null(cell_text_colors)) {
          tx <- cell_text_colors[i, j]
          if (!is.null(tx) && !is.na(tx) && nzchar(tx)) {
            ft <- flextable::color(ft, i = i, j = j, color = tx,
                                   part = "body")
          }
        }
      }
    }
  }
  ft <- flextable::autofit(ft)

  doc <- officer::read_docx()
  if (!is.null(title) && nzchar(title))
    doc <- officer::body_add_par(doc, title, style = "heading 1")
  if (!is.null(subtitle) && nzchar(subtitle))
    doc <- officer::body_add_par(doc, subtitle, style = "Normal")
  doc <- flextable::body_add_flextable(doc, ft)
  if (!is.null(footer) && nzchar(footer))
    doc <- officer::body_add_par(doc, footer, style = "Normal")

  # Force landscape A4 for the whole document.
  ps <- officer::prop_section(
    page_size    = officer::page_size(orient = "landscape",
                                      width  = 11.69,
                                      height = 8.27),
    page_margins = officer::page_mar(top    = 0.5, bottom = 0.5,
                                     left   = 0.5, right  = 0.5,
                                     header = 0.25, footer = 0.25,
                                     gutter = 0)
  )
  doc <- officer::body_set_default_section(doc, ps)
  print(doc, target = file)
  invisible(file)
}

# ----------------------------------------------------------------------------
# write_table_xlsx
# ----------------------------------------------------------------------------
write_table_xlsx <- function(df, file,
                             sheet            = "Table",
                             cell_colors      = NULL,
                             cell_text_colors = NULL) {
  stopifnot(is.data.frame(df))
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("openxlsx is required for Excel export")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(
    wb, sheet, df,
    headerStyle = openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#f0f0f0",
      halign = "left", border = "TopBottom"))

  if (!is.null(cell_colors) && nrow(df) > 0) {
    for (j in seq_len(ncol(df))) {
      for (i in seq_len(nrow(df))) {
        bg <- cell_colors[i, j]
        if (is.null(bg) || is.na(bg) || !nzchar(bg)) next
        tx <- if (!is.null(cell_text_colors)) cell_text_colors[i, j] else NA
        if (is.na(tx) || !nzchar(tx)) tx <- "#000000"
        st <- openxlsx::createStyle(
          fgFill     = bg, fontColour = tx,
          halign     = "center")
        openxlsx::addStyle(wb, sheet, st, rows = i + 1L, cols = j,
                           gridExpand = TRUE, stack = TRUE)
      }
    }
  }
  openxlsx::setColWidths(wb, sheet, cols = seq_along(df), widths = "auto")
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  invisible(file)
}

# ----------------------------------------------------------------------------
# build_league_table_df
# ----------------------------------------------------------------------------
# Produces a data.frame that mirrors the on-screen lower-triangle league
# table: each cell shows the NMA estimate of `column vs row`. Returns
# parallel cell_colors / cell_text_colors matrices keyed by the comparison's
# CINeMA confidence level so the Word / Excel writers can colour cells the
# same way the inline view does.
# ----------------------------------------------------------------------------
build_league_table_df <- function(net,
                                  merged,
                                  sm   = NULL,
                                  conf_bg  = NULL,
                                  conf_txt = NULL) {
  stopifnot(!is.null(net), !is.null(merged))
  if (is.null(conf_bg))
    conf_bg <- c("High"     = "#1e8449",
                 "Moderate" = "#2471a3",
                 "Low"      = "#e67e22",
                 "Very low" = "#c0392b",
                 "Not set"  = "#bfbfbf")
  if (is.null(conf_txt))
    conf_txt <- setNames(rep("#ffffff", length(conf_bg)), names(conf_bg))

  trts <- sort(net$trts)
  k    <- length(trts)
  if (k == 0)
    return(list(df = data.frame(), cell_colors = NULL,
                cell_text_colors = NULL))

  is_ratio <- !is.null(sm) && toupper(sm) %in% c("OR", "RR", "HR")

  conf_final <- ifelse(nzchar(merged$confidence),
                       merged$confidence, merged$suggested_confidence)
  conf_final[!nzchar(conf_final)] <- "Not set"
  conf_lookup <- setNames(conf_final, merged$comparison)

  te_mat <- net$TE.random %||% net$TE.common
  lo_mat <- net$lower.random %||% net$lower.common
  hi_mat <- net$upper.random %||% net$upper.common

  body  <- matrix("", nrow = k, ncol = k)
  bg_m  <- matrix(NA_character_, nrow = k, ncol = k)
  tx_m  <- matrix(NA_character_, nrow = k, ncol = k)

  for (ri in seq_len(k)) {
    for (ci in seq_len(k)) {
      row_trt <- trts[ri]; col_trt <- trts[ci]
      if (ri == ci) {
        body[ri, ci] <- row_trt
        bg_m[ri, ci] <- "#e8e8e8"
        tx_m[ri, ci] <- "#000000"
        next
      }
      if (ci > ri) next  # upper triangle stays empty

      # Lower triangle: estimate of col_trt vs row_trt.
      sorted <- sort(c(row_trt, col_trt))
      key    <- paste(sorted[1], sorted[2], sep = " vs ")

      te_raw <- if (!is.null(te_mat)) te_mat[sorted[1], sorted[2]] else NA_real_
      lo_raw <- if (!is.null(lo_mat)) lo_mat[sorted[1], sorted[2]] else NA_real_
      hi_raw <- if (!is.null(hi_mat)) hi_mat[sorted[1], sorted[2]] else NA_real_

      if (is.na(te_raw)) {
        body[ri, ci] <- "—"
        bg_m[ri, ci] <- "#f0f0f0"
        next
      }

      # Reorient if needed so cell (row, col) shows col_trt vs row_trt.
      if (row_trt == sorted[1]) {
        te_show <- -te_raw; lo_show <- -hi_raw; hi_show <- -lo_raw
      } else {
        te_show <-  te_raw; lo_show <-  lo_raw; hi_show <-  hi_raw
      }
      if (is_ratio) {
        te_show <- exp(te_show); lo_show <- exp(lo_show); hi_show <- exp(hi_show)
      }

      body[ri, ci] <- sprintf("%.2f [%.2f, %.2f]",
                              te_show, lo_show, hi_show)

      cv <- conf_lookup[key]
      if (!is.na(cv) && cv %in% names(conf_bg))  bg_m[ri, ci] <- unname(conf_bg[cv])
      if (!is.na(cv) && cv %in% names(conf_txt)) tx_m[ri, ci] <- unname(conf_txt[cv])
    }
  }

  df <- data.frame(Treatment = trts, body, stringsAsFactors = FALSE,
                   check.names = FALSE)
  names(df)[-1] <- trts

  # Prepend a column of NA for the new "Treatment" column in the colour mats
  bg_full <- cbind(NA_character_, bg_m)
  tx_full <- cbind(NA_character_, tx_m)
  # Treatment column header style: bold black on white
  bg_full[, 1] <- "#fafafa"
  tx_full[, 1] <- "#000000"

  list(df = df, cell_colors = bg_full, cell_text_colors = tx_full)
}

# ----------------------------------------------------------------------------
# build_robmen_export_df
# ----------------------------------------------------------------------------
# `robmen_results` is the list returned by moduleC_server's robmen_results
# reactive. Schema varies between releases; we accept either:
#   - a data.frame with columns comparison + various ratings
#   - a list with $table_pairwise, $table_final, etc.
# Returns NULL when nothing usable is available so the caller can skip.
# ----------------------------------------------------------------------------
build_robmen_export_df <- function(robmen_results) {
  if (is.null(robmen_results)) return(NULL)
  if (is.data.frame(robmen_results) && nrow(robmen_results) > 0)
    return(robmen_results)
  # If it's a list with named slots, prefer a "final" table; otherwise the
  # first data.frame we find.
  if (is.list(robmen_results)) {
    if (is.data.frame(robmen_results$table_final))
      return(robmen_results$table_final)
    if (is.data.frame(robmen_results$final))
      return(robmen_results$final)
    for (el in robmen_results) {
      if (is.data.frame(el) && nrow(el) > 0) return(el)
    }
  }
  NULL
}
