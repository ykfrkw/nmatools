#' @title League table coloured by CINeMA confidence ratings
#'
#' @description Creates a colour-coded NMA league table (n x n square matrix)
#'   where each cell is coloured according to the CINeMA confidence rating for
#'   that comparison. The diagonal cells show treatment names on a grey
#'   background.
#'
#'   \strong{Dual-outcome mode} (\code{x2} supplied): lower-left triangle
#'   shows outcome 1 (\code{x}); upper-right triangle shows outcome 2
#'   (\code{x2}).
#'
#'   \strong{Quad-outcome mode} (\code{x3} and/or \code{x4} supplied): each
#'   off-diagonal cell is split into a top and bottom sub-row.
#'   \itemize{
#'     \item Lower-left top sub-row: outcome 1 (\code{x})
#'     \item Lower-left bottom sub-row: outcome 3 (\code{x3})
#'     \item Upper-right top sub-row: outcome 2 (\code{x2})
#'     \item Upper-right bottom sub-row: outcome 4 (\code{x4})
#'   }
#'   Diagonal cells are merged vertically and show the treatment label.
#'
#' @param x A \code{netmeta} object (outcome 1, lower-left triangle top row).
#' @param cinema Path to a CINeMA report CSV, or a data frame with columns
#'   \code{"Comparison"} and \code{"Confidence rating"}.
#'   Used for outcome 1.
#' @param x2 Optional \code{netmeta} object (outcome 2, upper-right triangle
#'   top row). Must contain the same treatments as \code{x}.
#' @param cinema2 CINeMA data for \code{x2}.
#' @param x3 Optional \code{netmeta} object (outcome 3, lower-left triangle
#'   bottom row). Activates quad-outcome mode.
#'   Must contain the same treatments as \code{x}.
#' @param cinema3 CINeMA data for \code{x3}.
#' @param x4 Optional \code{netmeta} object (outcome 4, upper-right triangle
#'   bottom row). Activates quad-outcome mode.
#'   Must contain the same treatments as \code{x}.
#' @param cinema4 CINeMA data for \code{x4}.
#' @param label1 Label for outcome 1; written as a note below the table.
#' @param label2 Label for outcome 2; written as a note below the table.
#' @param label3 Label for outcome 3; written as a note below the table.
#' @param label4 Label for outcome 4; written as a note below the table.
#' @param sort_by How to sort treatments: \code{"alphabet"} (default),
#'   \code{"pscore"}, \code{"es"}, \code{"es_rev"}, \code{"pvalue"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param sort_order Custom order vector (used when \code{sort_by = "custom"}).
#' @param treat_labels Named character vector for renaming treatments.
#' @param digits Number of decimal places (default 2).
#' @param bracket Opening bracket character for CI (default \code{"("}).
#' @param separator Separator between CI bounds (default \code{" to "}).
#' @param wrap_ci Logical. Put CI on a new line below the estimate?
#'   Default \code{TRUE}.
#' @param common Use common-effects model? Default \code{FALSE}.
#' @param fixed Deprecated. Use \code{common} instead.
#' @param palette Named list of colour pairs for CINeMA ratings.
#'   If \code{NULL}, \code{palette_type} is used.
#' @param palette_type Coloring mode. One of:
#'   \itemize{
#'     \item \code{"pastel"} (default) — CINeMA confidence ratings, pastel colours.
#'     \item \code{"classic"}          — CINeMA confidence ratings, vivid colours.
#'     \item \code{"colorblind"}       — CINeMA confidence ratings, Okabe–Ito palette.
#'     \item \code{"SchneiderThoma2026"} — CI vs trivial range (Blue/Yellow/Orange/White).
#'       Requires \code{trivial_range}.
#'     \item \code{"solid"}            — All off-diagonal cells filled with a single
#'       colour. Use \code{fill_color} / \code{fill_color2} etc. to specify the colour
#'       per outcome.
#'   }
#' @param trivial_range Numeric vector \code{c(lo, hi)} on the log scale (for OR/RR/HR)
#'   or raw scale (for MD/SMD). Required when \code{palette_type = "SchneiderThoma2026"}.
#' @param fill_color Hex colour for all off-diagonal cells of outcome 1 when
#'   \code{palette_type = "solid"} (default \code{"#E2EFDA"}, light green).
#' @param fill_color2 Hex colour for outcome 2 cells in dual/quad mode
#'   (default \code{"#D0E4F7"}, light blue).
#' @param fill_color3 Hex colour for outcome 3 cells in quad mode
#'   (default \code{"#FCE4D6"}, light orange).
#' @param fill_color4 Hex colour for outcome 4 cells in quad mode
#'   (default \code{"#EAD1DC"}, light pink).
#' @param header_bg Background colour for diagonal treatment-name cells
#'   (default \code{"#BFBFBF"}).
#' @param file Output \code{.xlsx} path.
#' @return Invisibly returns the \code{openxlsx} workbook object.
#' @export
#'
#' @importFrom netmeta netleague
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle
#'   addStyle saveWorkbook mergeCells
color_league <- function(x,
                         cinema        = NULL,
                         x2            = NULL,
                         cinema2       = NULL,
                         x3            = NULL,
                         cinema3       = NULL,
                         x4            = NULL,
                         cinema4       = NULL,
                         label1        = NULL,
                         label2        = NULL,
                         label3        = NULL,
                         label4        = NULL,
                         sort_by       = "alphabet",
                         sort_order    = NULL,
                         treat_labels  = NULL,
                         digits        = 2,
                         bracket       = "(",
                         separator     = " to ",
                         wrap_ci       = TRUE,
                         common        = FALSE,
                         fixed         = NULL,
                         palette       = NULL,
                         palette_type  = c("pastel", "classic", "colorblind",
                                           "SchneiderThoma2026", "solid"),
                         trivial_range = NULL,
                         fill_color    = "#E2EFDA",
                         fill_color2   = "#D0E4F7",
                         fill_color3   = "#FCE4D6",
                         fill_color4   = "#EAD1DC",
                         header_bg     = "#BFBFBF",
                         file          = "color_league.xlsx") {
  palette_type <- match.arg(palette_type)
  is_st2026    <- identical(palette_type, "SchneiderThoma2026")
  is_solid     <- identical(palette_type, "solid")

  if (is_st2026 && is.null(trivial_range))
    stop('`trivial_range` is required when palette_type = "SchneiderThoma2026".')

  if (is.null(palette) && !is_st2026 && !is_solid)
    palette <- cinema_palette(palette_type)

  if (!is.null(fixed)) {
    message("Argument 'fixed' is deprecated in color_league(); use 'common' instead.")
    common <- fixed
  }

  # Raw TE/CI matrices — needed for SchneiderThoma2026 mode
  mats  <- if (is_st2026) .nm_mats(x,  common) else NULL
  mats2 <- if (is_st2026 && !is.null(x2)) .nm_mats(x2, common) else NULL
  mats3 <- if (is_st2026 && !is.null(x3)) .nm_mats(x3, common) else NULL
  mats4 <- if (is_st2026 && !is.null(x4)) .nm_mats(x4, common) else NULL

  # ---- 1. Parse CINeMA ----
  .empty_cinema <- function()
    data.frame(Comparison = character(0), `Confidence rating` = character(0),
               check.names = FALSE)

  cinema_df  <- if (!is.null(cinema))  parse_cinema(cinema)  else .empty_cinema()
  cinema_df2 <- if (!is.null(cinema2)) parse_cinema(cinema2) else .empty_cinema()
  cinema_df3 <- if (!is.null(cinema3)) parse_cinema(cinema3) else .empty_cinema()
  cinema_df4 <- if (!is.null(cinema4)) parse_cinema(cinema4) else .empty_cinema()

  # ---- 2. Normalise file extension ----
  if (tolower(tools::file_ext(file)) == "docx") {
    file <- sub("\\.docx$", ".xlsx", file, ignore.case = TRUE)
    message("color_league() outputs xlsx only; saving as: ", file)
  }

  # ---- 3. Sort treatments ----
  treatments <- x$trts
  treatments <- sort_treatments(
    treatments = treatments,
    sort_by    = sort_by,
    x          = x,
    reference  = if (!is.null(x$reference.group)) x$reference.group else treatments[1],
    sort_order = sort_order,
    common     = common
  )

  # ---- 4. Apply treat_labels ----
  display_labels <- treatments
  if (!is.null(treat_labels)) {
    idx <- match(treatments, names(treat_labels))
    display_labels[!is.na(idx)] <- treat_labels[idx[!is.na(idx)]]
  }

  # ---- 5. Build league matrices ----
  .check_trts <- function(net, name) {
    missing_trts <- setdiff(treatments, net$trts)
    if (length(missing_trts) > 0)
      stop(name, " is missing treatments present in x: ",
           paste(missing_trts, collapse = ", "))
  }

  league_mat  <- .build_league_mat(x, treatments, digits, bracket, separator, common)
  league_mat2 <- if (!is.null(x2)) {
    .check_trts(x2, "x2")
    .build_league_mat(x2, treatments, digits, bracket, separator, common)
  } else NULL
  league_mat3 <- if (!is.null(x3)) {
    .check_trts(x3, "x3")
    .build_league_mat(x3, treatments, digits, bracket, separator, common)
  } else NULL
  league_mat4 <- if (!is.null(x4)) {
    .check_trts(x4, "x4")
    .build_league_mat(x4, treatments, digits, bracket, separator, common)
  } else NULL

  # ---- 6. Build and save xlsx ----
  wb <- openxlsx::createWorkbook()
  .color_league_write_sheet(
    wb             = wb,
    sheet_name     = "League Table",
    treatments     = treatments,
    display_labels = display_labels,
    league_mat     = league_mat,
    league_mat2    = league_mat2,
    league_mat3    = league_mat3,
    league_mat4    = league_mat4,
    cinema_df      = cinema_df,
    cinema_df2     = cinema_df2,
    cinema_df3     = cinema_df3,
    cinema_df4     = cinema_df4,
    palette        = palette,
    palette_type   = palette_type,
    mats           = mats,
    mats2          = mats2,
    mats3          = mats3,
    mats4          = mats4,
    trivial_range  = trivial_range,
    fill_color     = fill_color,
    fill_color2    = fill_color2,
    fill_color3    = fill_color3,
    fill_color4    = fill_color4,
    header_bg      = header_bg,
    bracket        = bracket,
    separator      = separator,
    wrap_ci        = wrap_ci,
    label1         = label1,
    label2         = label2,
    label3         = label3,
    label4         = label4
  )
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  message("Saved league table: ", file)
  invisible(wb)
}


#' @title League tables for multiple outcomes in one xlsx file
#'
#' @description Creates one Excel workbook with one sheet per outcome.
#'   Each sheet is a colour-coded league table identical to what
#'   \code{\link{color_league}()} produces in single-outcome mode.
#'
#' @param outcomes A named list of \code{netmeta} objects.
#'   Names are used as sheet names (truncated to 31 characters for Excel).
#' @param cinema \code{NULL} (no CINeMA for any outcome), a single path/data
#'   frame applied to all outcomes, or a named list with the same names as
#'   \code{outcomes} (use \code{NULL} for outcomes without CINeMA).
#' @param sort_by,sort_order,treat_labels,digits,bracket,separator,wrap_ci,common
#'   Passed to each sheet. See \code{\link{color_league}()} for details.
#' @param palette,palette_type,header_bg Colour settings passed to each sheet.
#' @param file Output \code{.xlsx} path (default \code{"color_league_multi.xlsx"}).
#' @return Invisibly returns the \code{openxlsx} workbook object.
#' @export
#'
#' @importFrom openxlsx createWorkbook saveWorkbook
color_league_multi <- function(outcomes,
                               cinema        = NULL,
                               sort_by       = "alphabet",
                               sort_order    = NULL,
                               treat_labels  = NULL,
                               digits        = 2,
                               bracket       = "(",
                               separator     = " to ",
                               wrap_ci       = TRUE,
                               common        = FALSE,
                               palette       = NULL,
                               palette_type  = c("pastel", "classic", "colorblind",
                                                  "SchneiderThoma2026", "solid"),
                               trivial_range = NULL,
                               fill_color    = "#E2EFDA",
                               header_bg     = "#BFBFBF",
                               file          = "color_league_multi.xlsx") {
  if (!is.list(outcomes) || length(outcomes) == 0)
    stop("`outcomes` must be a non-empty named list of netmeta objects.")
  if (is.null(names(outcomes)) || any(nchar(names(outcomes)) == 0))
    stop("`outcomes` must have non-empty names for each element.")

  palette_type <- match.arg(palette_type)
  is_st2026    <- identical(palette_type, "SchneiderThoma2026")
  is_solid     <- identical(palette_type, "solid")

  if (is_st2026 && is.null(trivial_range))
    stop('`trivial_range` is required when palette_type = "SchneiderThoma2026".')

  if (is.null(palette) && !is_st2026 && !is_solid)
    palette <- cinema_palette(palette_type)

  .empty_cinema <- function()
    data.frame(Comparison = character(0), `Confidence rating` = character(0),
               check.names = FALSE)

  wb <- openxlsx::createWorkbook()

  for (outcome_name in names(outcomes)) {
    x <- outcomes[[outcome_name]]

    cinema_i <- if (is.list(cinema) && !is.data.frame(cinema)) {
      cinema[[outcome_name]]
    } else {
      cinema
    }
    cinema_df <- if (is.null(cinema_i)) .empty_cinema() else parse_cinema(cinema_i)

    treatments <- x$trts
    treatments <- sort_treatments(
      treatments = treatments,
      sort_by    = sort_by,
      x          = x,
      reference  = if (!is.null(x$reference.group)) x$reference.group else treatments[1],
      sort_order = sort_order,
      common     = common
    )

    display_labels <- treatments
    if (!is.null(treat_labels)) {
      idx <- match(treatments, names(treat_labels))
      display_labels[!is.na(idx)] <- treat_labels[idx[!is.na(idx)]]
    }

    league_mat <- .build_league_mat(x, treatments, digits, bracket, separator, common)

    mats_i <- if (is_st2026) .nm_mats(x, common) else NULL

    sheet_name <- substr(outcome_name, 1, 31)
    .color_league_write_sheet(
      wb             = wb,
      sheet_name     = sheet_name,
      treatments     = treatments,
      display_labels = display_labels,
      league_mat     = league_mat,
      league_mat2    = NULL,
      league_mat3    = NULL,
      league_mat4    = NULL,
      cinema_df      = cinema_df,
      cinema_df2     = .empty_cinema(),
      cinema_df3     = .empty_cinema(),
      cinema_df4     = .empty_cinema(),
      palette        = palette,
      palette_type   = palette_type,
      mats           = mats_i,
      mats2          = NULL,
      mats3          = NULL,
      mats4          = NULL,
      trivial_range  = trivial_range,
      fill_color     = fill_color,
      fill_color2    = fill_color,
      fill_color3    = fill_color,
      fill_color4    = fill_color,
      header_bg      = header_bg,
      bracket        = bracket,
      separator      = separator,
      wrap_ci        = wrap_ci,
      label1         = NULL,
      label2         = NULL,
      label3         = NULL,
      label4         = NULL
    )
  }

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  message("Saved multi-outcome league table: ", file)
  invisible(wb)
}


# ---------------------------------------------------------------------------
# Internal: build league matrix from a netmeta object
# ---------------------------------------------------------------------------
.build_league_mat <- function(x, treatments, digits, bracket, separator, common) {
  league <- tryCatch(
    netmeta::netleague(x, seq = treatments, digits = digits,
                       bracket = bracket, separator = separator,
                       common = common, random = !common),
    error = function(e)
      netmeta::netleague(x, seq = treatments, digits = digits,
                         bracket = bracket, separator = separator,
                         fixed = common, random = !common)
  )
  if (common) {
    if (!is.null(league$common)) league$common else league$fixed
  } else {
    league$random
  }
}


# ---------------------------------------------------------------------------
# Internal: reformat a single league-table cell
# ---------------------------------------------------------------------------
.reformat_cell <- function(cell, bracket, separator, wrap_ci) {
  if (is.na(cell) || cell == "") return(cell)
  sep_before_bracket <- if (wrap_ci) paste0("\n", bracket) else paste0(" ", bracket)
  cell <- gsub(
    paste0("\\s*", gsub("([().])", "\\\\\\1", bracket)),
    sep_before_bracket,
    cell
  )
  cell <- gsub(separator, "; ", cell, fixed = TRUE)
  cell
}


# ---------------------------------------------------------------------------
# Internal: write one league-table sheet into an existing workbook
#
#   Supports three modes:
#     single  — one outcome (league_mat only)
#     dual    — two outcomes (league_mat + league_mat2)
#     quad    — four outcomes: each off-diagonal cell split into top/bottom
#               sub-rows.  Activated when league_mat3 OR league_mat4 is
#               non-NULL.  Diagonal cells are merged vertically.
# ---------------------------------------------------------------------------
.color_league_write_sheet <- function(wb, sheet_name,
                                      treatments, display_labels,
                                      league_mat, league_mat2,
                                      league_mat3, league_mat4,
                                      cinema_df,  cinema_df2,
                                      cinema_df3, cinema_df4,
                                      palette, palette_type,
                                      mats,   mats2,
                                      mats3,  mats4,
                                      trivial_range,
                                      fill_color,  fill_color2,
                                      fill_color3, fill_color4,
                                      header_bg,
                                      bracket, separator, wrap_ci,
                                      label1, label2, label3, label4) {
  n       <- length(treatments)
  is_quad <- !is.null(league_mat3) || !is.null(league_mat4)

  openxlsx::addWorksheet(wb, sheet_name)

  if (!is_quad) {
    # ------------------------------------------------------------------
    # Single / dual mode: one Excel row per treatment
    # ------------------------------------------------------------------
    df_display <- as.data.frame(matrix("", nrow = n, ncol = n),
                                stringsAsFactors = FALSE)
    colnames(df_display) <- display_labels

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) {
          df_display[i, j] <- display_labels[i]
        } else if (i > j) {
          df_display[i, j] <- .reformat_cell(league_mat[i, j],
                                             bracket, separator, wrap_ci)
        } else {
          mat <- if (!is.null(league_mat2)) league_mat2 else league_mat
          df_display[i, j] <- .reformat_cell(mat[i, j],
                                             bracket, separator, wrap_ci)
        }
      }
    }

    openxlsx::writeData(wb, sheet = sheet_name, df_display, colNames = FALSE)

    base_sty <- openxlsx::createStyle(halign = "center", valign = "center",
                                      wrapText = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = base_sty,
                       rows = seq_len(n), cols = seq_len(n), gridExpand = TRUE)

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) {
          sty <- openxlsx::createStyle(fgFill = header_bg, halign = "center",
                                       valign = "center", wrapText = TRUE)
          openxlsx::addStyle(wb, sheet = sheet_name, style = sty,
                             rows = i, cols = j)
        } else {
          cdf   <- if (i > j) cinema_df else cinema_df2
          mats_ <- if (i > j) mats      else mats2
          fc    <- if (i > j) fill_color else fill_color2
          entry <- .cell_entry(treatments[i], treatments[j], mats_, cdf,
                               palette, palette_type, trivial_range, fc)
          if (!is.null(entry)) {
            sty <- openxlsx::createStyle(fgFill = entry$bg, fontColour = entry$color,
                                         halign = "center", valign = "center",
                                         wrapText = TRUE)
            openxlsx::addStyle(wb, sheet = sheet_name, style = sty,
                               rows = i, cols = j)
          }
        }
      }
    }

    # Note rows
    note_row <- n + 2L
    .write_notes(wb, sheet_name, note_row,
                 label1, label2, NULL, NULL, is_quad = FALSE)

  } else {
    # ------------------------------------------------------------------
    # Quad mode: two Excel rows per treatment
    #   top_row = 2i-1  (outcomes 1 and 2)
    #   bot_row = 2i    (outcomes 3 and 4)
    # ------------------------------------------------------------------
    n_rows <- 2L * n

    base_sty <- openxlsx::createStyle(halign = "center", valign = "center",
                                      wrapText = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = base_sty,
                       rows = seq_len(n_rows), cols = seq_len(n),
                       gridExpand = TRUE)

    for (i in seq_len(n)) {
      top_row <- 2L * i - 1L
      bot_row <- 2L * i

      for (j in seq_len(n)) {
        if (i == j) {
          # Diagonal: merge and write treatment label
          openxlsx::mergeCells(wb, sheet = sheet_name,
                               cols = j, rows = c(top_row, bot_row))
          openxlsx::writeData(wb, sheet = sheet_name,
                              x = display_labels[i],
                              startRow = top_row, startCol = j,
                              colNames = FALSE)
          diag_sty <- openxlsx::createStyle(fgFill = header_bg,
                                            halign = "center", valign = "center",
                                            wrapText = TRUE)
          openxlsx::addStyle(wb, sheet = sheet_name, style = diag_sty,
                             rows = c(top_row, bot_row), cols = j,
                             gridExpand = TRUE)

        } else if (i > j) {
          # Lower-left:  top = outcome 1,  bottom = outcome 3
          .write_quad_cell(wb, sheet_name, top_row, bot_row, j,
                           league_mat,  cinema_df,  mats,  fill_color,
                           league_mat3, cinema_df3, mats3, fill_color3,
                           treatments, bracket, separator, wrap_ci,
                           palette, palette_type, trivial_range)

        } else {
          # Upper-right: top = outcome 2,  bottom = outcome 4
          mat_top <- if (!is.null(league_mat2)) league_mat2 else league_mat
          .write_quad_cell(wb, sheet_name, top_row, bot_row, j,
                           mat_top,     cinema_df2, mats2, fill_color2,
                           league_mat4, cinema_df4, mats4, fill_color4,
                           treatments, bracket, separator, wrap_ci,
                           palette, palette_type, trivial_range)
        }
      }
    }

    note_row <- n_rows + 2L
    .write_notes(wb, sheet_name, note_row,
                 label1, label2, label3, label4, is_quad = TRUE)
  }

  invisible(wb)
}


# ---------------------------------------------------------------------------
# Internal: write one sub-row pair (top/bottom) for quad mode
# ---------------------------------------------------------------------------
.write_quad_cell <- function(wb, sheet_name, top_row, bot_row, col,
                             mat_top, cinema_top, mats_top, fc_top,
                             mat_bot, cinema_bot, mats_bot, fc_bot,
                             treatments, bracket, separator, wrap_ci,
                             palette, palette_type, trivial_range) {
  i <- (top_row + 1L) %/% 2L   # logical row index
  j <- col                       # logical col index

  # Top sub-row
  val_top <- .reformat_cell(mat_top[i, j], bracket, separator, wrap_ci)
  openxlsx::writeData(wb, sheet = sheet_name, x = val_top,
                      startRow = top_row, startCol = col, colNames = FALSE)
  entry_top <- .cell_entry(treatments[i], treatments[j], mats_top, cinema_top,
                            palette, palette_type, trivial_range, fc_top)
  if (!is.null(entry_top)) {
    sty <- openxlsx::createStyle(fgFill = entry_top$bg, fontColour = entry_top$color,
                                 halign = "center", valign = "center", wrapText = TRUE)
    openxlsx::addStyle(wb, sheet = sheet_name, style = sty,
                       rows = top_row, cols = col)
  }

  # Bottom sub-row
  if (!is.null(mat_bot)) {
    val_bot <- .reformat_cell(mat_bot[i, j], bracket, separator, wrap_ci)
    openxlsx::writeData(wb, sheet = sheet_name, x = val_bot,
                        startRow = bot_row, startCol = col, colNames = FALSE)
    entry_bot <- .cell_entry(treatments[i], treatments[j], mats_bot, cinema_bot,
                              palette, palette_type, trivial_range, fc_bot)
    if (!is.null(entry_bot)) {
      sty <- openxlsx::createStyle(fgFill = entry_bot$bg, fontColour = entry_bot$color,
                                   halign = "center", valign = "center", wrapText = TRUE)
      openxlsx::addStyle(wb, sheet = sheet_name, style = sty,
                         rows = bot_row, cols = col)
    }
  }
}


# ---------------------------------------------------------------------------
# Internal: get raw TE/lower/upper matrices from a netmeta object
# ---------------------------------------------------------------------------
.nm_mats <- function(x, common) {
  sfx <- if (common) "common" else "random"
  list(
    te = x[[paste0("TE.",    sfx)]],
    lo = x[[paste0("lower.", sfx)]],
    hi = x[[paste0("upper.", sfx)]]
  )
}


# ---------------------------------------------------------------------------
# Internal: determine cell background/text colour for one cell
# ---------------------------------------------------------------------------
#   t_row, t_col : treatment names (row and column of the league table)
#   mats         : list(te, lo, hi) from .nm_mats(), or NULL
#   cinema_df    : CINeMA data frame
#   palette      : cinema_palette list (used for CINeMA modes)
#   palette_type : one of "pastel"/"classic"/"colorblind"/"SchneiderThoma2026"/"solid"
#   trivial_range: c(lo, hi) for ST2026 mode
#   fill_color   : hex string for solid mode
# Returns list(bg, color) or NULL (→ no cell colour).
.cell_entry <- function(t_row, t_col, mats, cinema_df,
                        palette, palette_type, trivial_range, fill_color) {
  if (identical(palette_type, "solid")) {
    return(list(bg = fill_color, color = "#000000"))
  }

  if (identical(palette_type, "SchneiderThoma2026")) {
    if (is.null(mats) ||
        !t_row %in% rownames(mats$te) || !t_col %in% colnames(mats$te))
      return(NULL)
    te <- mats$te[t_row, t_col]
    lo <- mats$lo[t_row, t_col]
    hi <- mats$hi[t_row, t_col]
    cols <- schneider_thoma_colors(te, lo, hi, trivial_range)
    if (is.na(cols$bg)) return(NULL)
    return(list(bg = cols$bg, color = cols$text))
  }

  # CINeMA-based modes (pastel / classic / colorblind)
  cinema_to_color(get_cinema_rating(t_col, t_row, cinema_df), palette)
}


# ---------------------------------------------------------------------------
# Internal: write outcome-label notes below the table
# ---------------------------------------------------------------------------
.write_notes <- function(wb, sheet_name, note_row,
                         label1, label2, label3, label4,
                         is_quad) {
  if (is_quad) {
    notes <- list(
      list(label = label1, sym = "\u2199 Lower-left (top): "),
      list(label = label2, sym = "\u2197 Upper-right (top): "),
      list(label = label3, sym = "\u2199 Lower-left (bottom): "),
      list(label = label4, sym = "\u2197 Upper-right (bottom): ")
    )
  } else {
    notes <- list(
      list(label = label1, sym = "\u2199 Lower-left: "),
      list(label = label2, sym = "\u2197 Upper-right: ")
    )
  }
  for (nt in notes) {
    if (!is.null(nt$label)) {
      openxlsx::writeData(
        wb, sheet = sheet_name,
        x         = data.frame(v = paste0(nt$sym, nt$label), stringsAsFactors = FALSE),
        startRow  = note_row, startCol = 1, colNames = FALSE
      )
      note_row <- note_row + 1L
    }
  }
}
