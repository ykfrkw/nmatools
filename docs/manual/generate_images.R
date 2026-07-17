# =============================================================================
# generate_images.R -- Generate all static example images for the user manual
#
# Usage (from the package root):
#   Rscript docs/manual/generate_images.R
#
# The script is idempotent: it rebuilds every PNG under docs/manual/images/
# and every text snippet under docs/manual/snippets/ from scratch on each run.
# All intermediate analysis outputs (netmetawrap() runs, xlsx tables, PDFs)
# are written to a temporary working directory and discarded; only the final
# PNGs / snippets land in the repository.
#
# Requirements:
#   - installed nmatools (>= 0.2.0) and its dependencies
#   - pdftoppm (poppler) on PATH or at /opt/homebrew/bin/pdftoppm
#   - chromote + a local Chrome/Chromium (for xlsx -> PNG rendering; see
#     the note inside xlsx2png() for why QuickLook is not used)
# =============================================================================

library(nmatools)

set.seed(42)

# ---- 0. Paths ---------------------------------------------------------------

if (!file.exists("DESCRIPTION"))
  stop("Run this script from the package root: Rscript docs/manual/generate_images.R")

img_dir  <- file.path("docs", "manual", "images")
snip_dir <- file.path("docs", "manual", "snippets")
work_dir <- file.path(tempdir(), "manual_outputs")   # analysis outputs (temporary)
viz_dir  <- file.path(work_dir, "viz")               # gallery xlsx intermediates

unlink(work_dir, recursive = TRUE)
for (p in c(img_dir, snip_dir, work_dir, viz_dir))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)

PDFTOPPM <- Sys.which("pdftoppm")
if (!nzchar(PDFTOPPM)) PDFTOPPM <- "/opt/homebrew/bin/pdftoppm"
if (!file.exists(PDFTOPPM)) stop("pdftoppm not found; install poppler.")

# ---- 1. Helpers -------------------------------------------------------------

#' Convert one page of a PDF to PNG via pdftoppm.
#' Set `trim = TRUE` to crop surrounding whitespace (for PDFs not already
#' trimmed by netmetawrap, e.g. the rare-event sensitivity panel).
pdf2png <- function(pdf, out_png, dpi = 150, page = 1, trim = FALSE) {
  if (!file.exists(pdf)) stop("PDF not found: ", pdf)
  prefix <- tempfile("p2p_")
  status <- system2(PDFTOPPM,
                    c("-png", "-r", dpi, "-f", page, "-l", page,
                      shQuote(pdf), shQuote(prefix)))
  if (status != 0) stop("pdftoppm failed for: ", pdf)
  produced <- Sys.glob(paste0(prefix, "*.png"))   # pdftoppm appends -N page suffix
  if (length(produced) == 0) stop("pdftoppm produced no output for: ", pdf)
  file.copy(produced[1], out_png, overwrite = TRUE)
  unlink(produced)
  if (trim) {
    magick::image_write(
      magick::image_trim(magick::image_read(out_png), fuzz = 5),
      out_png)
  }
  message("  [pdf2png] ", out_png)
  invisible(out_png)
}

#' First file in `dir` matching `pattern` (handles _p1/_p2 pagination suffixes).
find_first <- function(dir, pattern) {
  f <- sort(list.files(dir, pattern = pattern, full.names = TRUE))
  if (length(f) == 0) stop("No file matching '", pattern, "' in ", dir)
  f[1]
}

# A single headless-Chrome session is created lazily and reused for all
# xlsx -> PNG conversions, then closed at the end of the script.
.chromote_session <- NULL
get_chromote <- function() {
  if (is.null(.chromote_session)) {
    options(chromote.headless = "new")
    .chromote_session <<- chromote::ChromoteSession$new()
    # Large viewport so even the widest table fits without clipping.
    .chromote_session$Emulation$setDeviceMetricsOverride(
      width = 2800, height = 2400, deviceScaleFactor = 1, mobile = FALSE)
  }
  .chromote_session
}

.argb2hex <- function(x) {
  # openxlsx stores colors as an ARGB string "FFRRGGBB". The value is a named
  # character vector whose name can be " rgb" (note the leading space), so we
  # index by position, never by name.
  if (is.null(x) || length(x) == 0) return(NA_character_)
  v <- unname(as.character(x))[1]
  if (is.na(v) || nchar(v) < 8) return(NA_character_)
  paste0("#", substr(v, 3, 8))
}

.col2num <- function(s) {
  Reduce(function(a, ch) a * 26L + match(ch, LETTERS), strsplit(s, "")[[1]], 0L)
}

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Render one sheet of an xlsx workbook to PNG.
#'
#' STRATEGY NOTE: the first approach tried was macOS QuickLook
#' (`qlmanage -t -s 2000 -o <dir> <xlsx>`), but in headless / sandboxed
#' environments qlmanage exits silently without writing a thumbnail, so no
#' PNG (colored or otherwise) is ever produced. flextable::save_as_image()
#' was ruled out because webshot2 is not installed. Instead we re-render the
#' workbook faithfully: cell values, fills, font colors, and merged regions
#' are read via openxlsx, emitted as an HTML table, and screenshotted with
#' {chromote} (headless Chrome). This reproduces the colored fills exactly
#' as stored in the workbook.
xlsx2png <- function(xlsx, out_png, sheet = 1, scale = 2, font_px = 15) {
  if (!file.exists(xlsx)) stop("xlsx not found: ", xlsx)
  wb         <- openxlsx::loadWorkbook(xlsx)
  sheet_name <- if (is.numeric(sheet)) names(wb)[sheet] else sheet
  sheet_idx  <- match(sheet_name, names(wb))

  vals <- openxlsx::read.xlsx(wb, sheet = sheet_name, colNames = FALSE,
                              skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  vals <- as.matrix(vals)
  vals[is.na(vals)] <- ""
  nr <- nrow(vals); nc <- ncol(vals)

  # Per-cell style matrices, filled by replaying styleObjects in order.
  bg   <- matrix(NA_character_, nr, nc)
  fg   <- matrix(NA_character_, nr, nc)
  bold <- matrix(FALSE, nr, nc)
  for (so in wb$styleObjects) {
    if (!identical(so$sheet, sheet_name)) next
    rows <- so$rows; cols <- so$cols
    if (length(rows) != length(cols)) {
      g <- expand.grid(r = unique(rows), c = unique(cols))
      rows <- g$r; cols <- g$c
    }
    st      <- so$style
    fill    <- .argb2hex(st$fill$fillFg)
    fontcol <- .argb2hex(st$fontColour)
    is_bold <- any(toupper(st$fontDecoration) == "BOLD")
    for (k in seq_along(rows)) {
      r <- rows[k]; c <- cols[k]
      if (r > nr || c > nc) next
      if (!is.na(fill))    bg[r, c] <- fill
      if (!is.na(fontcol)) fg[r, c] <- fontcol
      if (is_bold)         bold[r, c] <- TRUE
    }
  }

  # Merged regions -> rowspan/colspan; covered cells are skipped.
  span_r <- matrix(1L, nr, nc); span_c <- matrix(1L, nr, nc)
  skip   <- matrix(FALSE, nr, nc)
  merges <- unlist(wb$worksheets[[sheet_idx]]$mergeCells)
  refs   <- regmatches(merges, regexpr("[A-Z]+[0-9]+:[A-Z]+[0-9]+", merges))
  for (ref in refs) {
    corners <- strsplit(ref, ":")[[1]]
    # .col2num() takes ONE column ref, so map over both corners (passing the
    # length-2 vector would silently use only the first corner and collapse
    # every column span to 1).
    cc <- vapply(gsub("[0-9]", "", corners), .col2num, integer(1), USE.NAMES = FALSE)
    rr <- as.integer(gsub("[A-Z]", "", corners))
    r1 <- min(rr); r2 <- min(max(rr), nr); c1 <- min(cc); c2 <- min(max(cc), nc)
    if (r1 > nr || c1 > nc) next
    span_r[r1, c1] <- r2 - r1 + 1L
    span_c[r1, c1] <- c2 - c1 + 1L
    for (r in r1:r2) for (c in c1:c2)
      if (!(r == r1 && c == c1)) skip[r, c] <- TRUE
  }

  # Trailing caption rows (e.g. color_league() outcome-label notes): a row
  # whose first cell carries a single-row merge across the FULL sheet width
  # and no fill. Only the trailing block of the sheet is considered — scan
  # from the bottom; empty rows and caption rows form the trailing block and
  # the scan stops at the first normal row. Caption rows are rendered as
  # plain text lines below the table (not as table rows), so their long text
  # cannot stretch column 1; empty spacer rows in the trailing block are
  # dropped. Sheets without such rows (kilim, context tables, single-outcome
  # league) render exactly as before.
  is_empty_row <- vapply(seq_len(nr), function(r)
    all(vals[r, ] == ""), logical(1))
  is_caption_row <- vapply(seq_len(nr), function(r)
    nc > 1L && span_c[r, 1] == nc && span_r[r, 1] == 1L && is.na(bg[r, 1]),
    logical(1))
  caption_rows <- integer(0)
  last_tbl_row <- nr
  while (last_tbl_row >= 1L &&
         (is_caption_row[last_tbl_row] || is_empty_row[last_tbl_row])) {
    if (is_caption_row[last_tbl_row] && !is_empty_row[last_tbl_row])
      caption_rows <- c(last_tbl_row, caption_rows)
    last_tbl_row <- last_tbl_row - 1L
  }
  if (length(caption_rows) == 0) last_tbl_row <- nr   # no captions: keep all rows

  # Emit the HTML table.
  cells <- character(0)
  for (r in seq_len(last_tbl_row)) {
    row_html <- "<tr>"
    for (c in seq_len(nc)) {
      if (skip[r, c]) next
      sty <- paste0(
        "border:1px solid #808080;padding:5px 12px;text-align:center;",
        "vertical-align:middle;",
        if (!is.na(bg[r, c])) paste0("background:", bg[r, c], ";") else "",
        if (!is.na(fg[r, c])) paste0("color:", fg[r, c], ";") else "",
        if (bold[r, c]) "font-weight:bold;" else ""
      )
      span <- paste0(
        if (span_r[r, c] > 1L) paste0(" rowspan=\"", span_r[r, c], "\"") else "",
        if (span_c[r, c] > 1L) paste0(" colspan=\"", span_c[r, c], "\"") else ""
      )
      txt <- gsub("\n", "<br>", .html_escape(vals[r, c]), fixed = TRUE)
      row_html <- paste0(row_html, "<td", span, " style=\"", sty, "\">", txt, "</td>")
    }
    cells <- c(cells, paste0(row_html, "</tr>"))
  }

  # Caption rows: borderless left-aligned text lines below the table.
  captions <- vapply(caption_rows, function(r) {
    paste0(
      "<div style=\"margin-top:6px;text-align:left;",
      "font-family:Helvetica,Arial,sans-serif;font-size:", font_px, "px\">",
      gsub("\n", "<br>", .html_escape(vals[r, 1]), fixed = TRUE),
      "</div>"
    )
  }, character(1))

  html <- paste0(
    "<html><head><meta charset=\"utf-8\"></head>",
    "<body style=\"margin:0;padding:8px;background:#ffffff;display:inline-block\">",
    "<div id=\"wrap\" style=\"display:inline-block\">",
    "<table id=\"tbl\" style=\"border-collapse:collapse;",
    "font-family:Helvetica,Arial,sans-serif;font-size:", font_px, "px\">",
    paste(cells, collapse = ""), "</table>",
    paste(captions, collapse = ""),
    "</div></body></html>"
  )
  tf <- tempfile(fileext = ".html")
  writeLines(html, tf, useBytes = TRUE)

  b <- get_chromote()
  p <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file://", tf), wait_ = FALSE)
  b$wait_for(p)
  sel <- if (length(caption_rows) > 0) "#wrap" else "#tbl"
  b$screenshot(out_png, selector = sel, scale = scale, expand = 8)
  unlink(tf)
  message("  [xlsx2png] ", out_png)
  invisible(out_png)
}

#' Save the printed representation of an object as a text snippet.
save_snippet <- function(obj, out_txt) {
  writeLines(utils::capture.output(print(obj)), file.path(snip_dir, out_txt))
  message("  [snippet]  ", out_txt)
}

# =============================================================================
# 2. Pipeline images: one netmetawrap() run on remission_lt
# =============================================================================
message("== Pipeline run: remission_lt ==")

d <- load_w2i()

pipe_path <- file.path(work_dir, "pipeline")
net1 <- netmetawrap(
  data            = d,
  studlab         = id,
  treat           = t,
  outcome         = "remission_lt",
  n               = n,
  event           = r,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",
  path            = pipe_path
)
out1 <- file.path(pipe_path, "remission_lt")

pdf2png(file.path(out1, "netgraph_remission_lt.pdf"),
        file.path(img_dir, "pipeline_netgraph.png"))
pdf2png(file.path(out1, "forest_remission_lt.pdf"),
        file.path(img_dir, "pipeline_forest.png"))
pdf2png(find_first(out1, "^forest_netsplit_remission_lt.*\\.pdf$"),
        file.path(img_dir, "pipeline_netsplit.png"))
xlsx2png(file.path(out1, "leaguetable_remission_lt.xlsx"),
         file.path(img_dir, "pipeline_league_xlsx.png"))

# COMPATIBILITY SHIM (netmeta >= 3.x): netpairwise() now returns a single
# meta object instead of a list of per-comparison objects, so netmetawrap()
# 0.2.0 silently skips its forest_netpairwise_*, funnel_pairwise_* and
# contributions_* outputs on this netmeta version. The three plots below
# reproduce the intended content directly, using the same plotting calls
# and styling that netmetawrap() uses internally.

## Shim 1: all-pairwise-comparisons forest (forest_netpairwise_*)
np_obj <- netmeta::netpairwise(net1, common = FALSE, prediction = TRUE)
np_pdf <- file.path(out1, "shim_forest_netpairwise.pdf")
grDevices::pdf(np_pdf, width = 10, height = 9)
meta::forest(np_obj,
             smlab      = "remission_lt\n(Random Effects Model)",
             leftcols   = "studlab",
             prediction = TRUE)
grDevices::dev.off()
pdf2png(np_pdf, file.path(img_dir, "pipeline_netpairwise.png"), page = 1)

## Shim 2: contour-enhanced funnel plot (funnel_pairwise_*)
# netmetawrap() only draws funnels for pairs with k >= funnel_min_studies
# (default 10); the largest W2I pair (CBT-I vs Pharmacotherapy) has k = 7,
# so this example lowers the threshold implicitly by plotting that pair.
pw_all <- meta::pairwise(treat = t, event = r, n = n, data = d,
                         studlab = id, sm = "OR")
pair_sub <- subset(pw_all,
                   (treat1 == "CBT-I" & treat2 == "Pharmacotherapy") |
                   (treat1 == "Pharmacotherapy" & treat2 == "CBT-I"))
m_pair <- meta::metagen(TE, seTE, studlab = studlab, data = pair_sub,
                        sm = "OR", common = FALSE, random = TRUE)
fun_pdf <- file.path(out1, "shim_funnel_pairwise.pdf")
grDevices::pdf(fun_pdf, width = 7, height = 6)
meta::funnel(m_pair,
             contour.levels = c(0.90, 0.95, 0.99),
             col.contour    = c("darkgray", "gray", "lightgray"),
             main           = "CBT-I vs Pharmacotherapy",
             studlab        = TRUE)
graphics::legend("topright",
                 fill   = c("darkgray", "gray", "lightgray"),
                 legend = c("p < 0.10", "p < 0.05", "p < 0.01"),
                 bg     = "white")
grDevices::dev.off()
pdf2png(fun_pdf, file.path(img_dir, "pipeline_funnel.png"))

## Shim 3: direct evidence contribution heatmap (contributions_*)
# netmeta >= 3.x dropped the plot() method for netcontrib objects; render
# the random-effects contribution matrix as a labeled heatmap instead.
nc_obj <- netmeta::netcontrib(net1, method = "random")
cm     <- nc_obj$random
cm_df  <- as.data.frame(as.table(as.matrix(cm)))
names(cm_df) <- c("network_comparison", "direct_comparison", "contribution")
p_cm <- ggplot2::ggplot(cm_df,
          ggplot2::aes(x = direct_comparison, y = network_comparison,
                       fill = contribution)) +
  ggplot2::geom_tile(color = "grey70") +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", contribution)),
                     size = 3.4) +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue",
                               limits = c(0, 1)) +
  ggplot2::labs(title = "Direct Evidence Contributions: remission_lt",
                x = "Direct comparison", y = "Network comparison",
                fill = "Contribution") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
ggplot2::ggsave(file.path(img_dir, "pipeline_contributions.png"),
                p_cm, width = 8, height = 4.5, dpi = 220, bg = "white")
message("  [ggsave]   pipeline_contributions.png")

# Text snippets for chapter authors (real output excerpts).
for (f in c("netmeta_remission_lt.txt", "global_test_remission_lt.txt",
            "local_test_remission_lt.txt"))
  file.copy(file.path(out1, f), file.path(snip_dir, f), overwrite = TRUE)
writeLines(list.files(out1, recursive = TRUE),
           file.path(snip_dir, "pipeline_output_tree.txt"))

# ---- 2b. Customized forest plot (forest_args overrides) ---------------------
message("== Custom forest run ==")
netmetawrap(
  data            = d,
  studlab         = id,
  treat           = t,
  outcome         = "remission_lt_custom",
  n               = n,
  event           = r,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",
  path            = file.path(work_dir, "pipeline_custom"),
  forest_args     = list(
    xlim        = c(0.2, 5),
    label.left  = "Favours pharmacotherapy",
    label.right = "Favours treatment",
    col.square  = "steelblue"
  )
)
pdf2png(file.path(work_dir, "pipeline_custom", "remission_lt_custom",
                  "forest_remission_lt_custom.pdf"),
        file.path(img_dir, "pipeline_forest_custom.png"))

# =============================================================================
# 3. Rare-event workflow (rare_events = "always")
# =============================================================================
message("== Rare-event run: dropout_pt ==")
netmetawrap(
  data            = d,
  studlab         = id,
  treat           = t,
  outcome         = "dropout_pt_rare",
  n               = n,
  event           = n_dropout_pt,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "desirable",
  rare_events     = "always",
  path            = file.path(work_dir, "rare")
)
rare_out <- file.path(work_dir, "rare", "dropout_pt_rare")
pdf2png(file.path(rare_out, "forest_rare_sensitivity_dropout_pt_rare.pdf"),
        file.path(img_dir, "rare_sensitivity.png"), trim = TRUE)
file.copy(file.path(rare_out, "rare_diagnostics_dropout_pt_rare.txt"),
          file.path(snip_dir, "rare_diagnostics_dropout_pt_rare.txt"),
          overwrite = TRUE)

# =============================================================================
# 4. Transitivity plots
# =============================================================================
message("== Transitivity ==")
# Non-numeric covariates must be converted to numeric first (README example).
d$rob_num <- c(L = 0, M = 1, H = 2)[d$rob]
plot_transitivity(
  data           = d,
  studlab        = id,
  treat          = t,
  covariate_cols = c("rob_num", "indirectness"),
  outcome        = "transitivity",
  path           = work_dir
)
trans_out <- file.path(work_dir, "transitivity")
pdf2png(file.path(trans_out, "transitivity_transitivity_rob_num.pdf"),
        file.path(img_dir, "transitivity_rob.png"))
pdf2png(file.path(trans_out, "transitivity_transitivity_indirectness.pdf"),
        file.path(img_dir, "transitivity_indirectness.png"))

# =============================================================================
# 5. Visualization gallery
# =============================================================================
message("== Visualization gallery ==")

net_lt  <- build_w2i_netmeta("remission_lt")   # long-term remission
net_dlt <- build_w2i_netmeta("dropout_lt")     # long-term dropout
net_pt  <- build_w2i_netmeta("remission_pt")   # post-treatment remission
net_dpt <- build_w2i_netmeta("dropout_pt")     # post-treatment dropout
ci_fp   <- w2i_cinema_path()                   # CINeMA CSV (remission_lt only)

# trivial_range is on the LOG scale for OR: treat OR 0.91-1.10 as trivial.
trivial <- log(c(1 / 1.1, 1.1))

# ---- 5a. color_league() variants -------------------------------------------
league_xlsx <- function(file) file.path(viz_dir, file)

color_league(x = net_lt, cinema = ci_fp,
             file = league_xlsx("league_pastel.xlsx"))
xlsx2png(league_xlsx("league_pastel.xlsx"),
         file.path(img_dir, "league_pastel.png"))

color_league(x = net_lt, cinema = ci_fp, palette_type = "classic",
             file = league_xlsx("league_classic.xlsx"))
xlsx2png(league_xlsx("league_classic.xlsx"),
         file.path(img_dir, "league_classic.png"))

color_league(x = net_lt, cinema = ci_fp, palette_type = "colorblind",
             file = league_xlsx("league_colorblind.xlsx"))
xlsx2png(league_xlsx("league_colorblind.xlsx"),
         file.path(img_dir, "league_colorblind.png"))

color_league(x = net_lt, palette_type = "solid", fill_color = "#E2EFDA",
             sort_by = "pscore",
             file = league_xlsx("league_solid.xlsx"))
xlsx2png(league_xlsx("league_solid.xlsx"),
         file.path(img_dir, "league_solid.png"))

color_league(x = net_lt, cinema = ci_fp, sort_by = "pscore",
             file = league_xlsx("league_pscore.xlsx"))
xlsx2png(league_xlsx("league_pscore.xlsx"),
         file.path(img_dir, "league_pscore.png"))

# Dual outcome: lower-left = LT remission, upper-right = LT dropout.
color_league(x = net_lt, cinema = ci_fp, x2 = net_dlt,
             label1 = "Remission (long-term)",
             label2 = "Dropout (long-term)",
             sort_by = "pscore",
             file = league_xlsx("league_dual.xlsx"))
xlsx2png(league_xlsx("league_dual.xlsx"),
         file.path(img_dir, "league_dual.png"))

# Quad outcome: all four outcomes packed in one sheet.
color_league(x = net_lt, cinema = ci_fp,
             x2 = net_pt, x3 = net_dlt, x4 = net_dpt,
             label1 = "Remission (long-term)",
             label2 = "Remission (post-tx)",
             label3 = "Dropout (long-term)",
             label4 = "Dropout (post-tx)",
             sort_by = "pscore",
             file = league_xlsx("league_quad.xlsx"))
xlsx2png(league_xlsx("league_quad.xlsx"),
         file.path(img_dir, "league_quad.png"))

# Schneider-Thoma 2026 categorical palette (trivial_range required, log scale).
color_league(x = net_lt, sort_by = "pscore",
             palette_type = "SchneiderThoma2026",
             trivial_range = trivial,
             file = league_xlsx("league_st2026.xlsx"))
xlsx2png(league_xlsx("league_st2026.xlsx"),
         file.path(img_dir, "league_st2026.png"))

# ---- 5b. color_league_multi() ------------------------------------------------
# Produces a multi-sheet workbook (one sheet per outcome); the PNG shows
# sheet 1 ("Remission (LT)") only.
color_league_multi(
  outcomes = list(
    "Remission (LT)" = net_lt,
    "Dropout (LT)"   = net_dlt,
    "Remission (PT)" = net_pt,
    "Dropout (PT)"   = net_dpt
  ),
  cinema = list(
    "Remission (LT)" = ci_fp,
    "Dropout (LT)"   = NULL,
    "Remission (PT)" = NULL,
    "Dropout (PT)"   = NULL
  ),
  sort_by = "pscore",
  file    = league_xlsx("league_multi.xlsx")
)
xlsx2png(league_xlsx("league_multi.xlsx"),
         file.path(img_dir, "league_multi.png"), sheet = 1)

# ---- 5c. color_forest() -------------------------------------------------------
png(file.path(img_dir, "cforest_default.png"),
    width = 2100, height = 900, res = 220)
color_forest(x = net_lt, cinema = ci_fp)
dev.off()
message("  [png]      cforest_default.png")

# ---- 5d. color_netgraph() -----------------------------------------------------
# netgraph() sets its own xlim/ylim internally, so passing xlim/ylim through
# color_netgraph()'s ... collides ("matched by multiple actual arguments").
# Use netgraph's own `scale` (range expansion factor) plus a generous outer
# margin so node labels are not clipped.
ng_scale <- 1.35

png(file.path(img_dir, "cnetgraph_pastel.png"),
    width = 1800, height = 1500, res = 220)
par(mar = c(1, 3, 1, 3))
color_netgraph(x = net_lt, cinema = ci_fp, scale = ng_scale)
dev.off()

png(file.path(img_dir, "cnetgraph_classic.png"),
    width = 1800, height = 1500, res = 220)
par(mar = c(1, 3, 1, 3))
color_netgraph(x = net_lt, cinema = ci_fp, palette_type = "classic",
               scale = ng_scale)
dev.off()

# No cinema argument: every edge falls back to col_no_cinema.
png(file.path(img_dir, "cnetgraph_nocinema.png"),
    width = 1800, height = 1500, res = 220)
par(mar = c(1, 3, 1, 3))
color_netgraph(x = net_lt, col_no_cinema = "steelblue", scale = ng_scale)
dev.off()
message("  [png]      cnetgraph_*.png")

# ---- 5e. kilim() --------------------------------------------------------------
kilim_outcomes <- list(
  list(x = net_lt,  name = "remission_lt", reference = "Pharmacotherapy",
       small_values = "undesirable", label = "Remission\n(long-term)", digits = 2),
  list(x = net_dlt, name = "dropout_lt",   reference = "Pharmacotherapy",
       small_values = "desirable",   label = "Dropout\n(long-term)",   digits = 2),
  list(x = net_pt,  name = "remission_pt", reference = "Pharmacotherapy",
       small_values = "undesirable", label = "Remission\n(post-tx)",   digits = 2),
  list(x = net_dpt, name = "dropout_pt",   reference = "Pharmacotherapy",
       small_values = "desirable",   label = "Dropout\n(post-tx)",     digits = 2)
)

kilim(outcomes = kilim_outcomes, sort_by = "pscore",
      file = league_xlsx("kilim_grrd.xlsx"))               # default GrRd
xlsx2png(league_xlsx("kilim_grrd.xlsx"),
         file.path(img_dir, "kilim_grrd.png"))

kilim(outcomes = kilim_outcomes, palette = "GrYlRd", sort_by = "pscore",
      file = league_xlsx("kilim_gryrd.xlsx"))
xlsx2png(league_xlsx("kilim_gryrd.xlsx"),
         file.path(img_dir, "kilim_gryrd.png"))

kilim(outcomes = kilim_outcomes, palette = "SchneiderThoma2026",
      trivial_range = trivial, sort_by = "pscore",
      file = league_xlsx("kilim_st2026.xlsx"))
xlsx2png(league_xlsx("kilim_st2026.xlsx"),
         file.path(img_dir, "kilim_st2026.png"))

# ---- 5f. vitruvian() ----------------------------------------------------------
# cer values = published Pharmacotherapy reference event rates (see sample_viz.R).
vit_outcomes_plain <- list(
  list(x = net_lt,  name = "remission_lt", label = "Remission\n(long-term)",
       small_values = "undesirable", cer = 0.28),
  list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(long-term)",
       small_values = "desirable",   cer = 0.39),
  list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
       small_values = "undesirable", cer = 0.28),
  list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
       small_values = "desirable",   cer = 0.16)
)
vit_outcomes_grouped <- list(
  list(x = net_lt,  name = "remission_lt", label = "Remission",
       small_values = "undesirable", cer = 0.28, group = "Long-term"),
  list(x = net_dlt, name = "dropout_lt",   label = "Dropout",
       small_values = "desirable",   cer = 0.39, group = "Long-term"),
  list(x = net_pt,  name = "remission_pt", label = "Remission",
       small_values = "undesirable", cer = 0.28, group = "Post-treatment"),
  list(x = net_dpt, name = "dropout_pt",   label = "Dropout",
       small_values = "desirable",   cer = 0.16, group = "Post-treatment")
)

vitruvian(outcomes = vit_outcomes_plain, reference = "Pharmacotherapy",
          digits = 1, ncol = 3,
          file = file.path(img_dir, "vitruvian_basic.png"))

vitruvian(outcomes = vit_outcomes_plain, reference = "Pharmacotherapy",
          trivial_range = trivial, digits = 1, ncol = 3,
          file = file.path(img_dir, "vitruvian_trivial.png"))

vitruvian(outcomes = vit_outcomes_grouped, reference = "Pharmacotherapy",
          digits = 1, ncol = 3,
          file = file.path(img_dir, "vitruvian_grouped.png"))
message("  [png]      vitruvian_*.png")

# ---- 5g. Evidence frameworks: min_context() / part_context() ------------------
mc_lt <- min_context(x = net_lt, cinema = ci_fp,
                     reference = "Pharmacotherapy", small_values = "undesirable")
mc_dlt <- min_context(x = net_dlt,
                      reference = "Pharmacotherapy", small_values = "desirable")
mc_pt <- min_context(x = net_pt,
                     reference = "Pharmacotherapy", small_values = "undesirable")
mc_dpt <- min_context(x = net_dpt,
                      reference = "Pharmacotherapy", small_values = "desirable")

save_snippet(mc_lt, "min_context_df.txt")

table_min_context(mc_lt, file = league_xlsx("context_min_table.xlsx"))
xlsx2png(league_xlsx("context_min_table.xlsx"),
         file.path(img_dir, "context_min_table.png"))

table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)" = mc_lt,
    "Dropout (long-term)"   = mc_dlt,
    "Remission (post-tx)"   = mc_pt,
    "Dropout (post-tx)"     = mc_dpt
  ),
  sep  = ", ",
  file = league_xlsx("context_min_multi.xlsx")
)
xlsx2png(league_xlsx("context_min_multi.xlsx"),
         file.path(img_dir, "context_min_multi.png"))

pc_lt <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.12),          # SWD: absolute risk difference of 12 pp
  cer          = 0.28,             # Pharmacotherapy long-term remission rate
  small_values = "undesirable",
  cinema       = ci_fp
)
save_snippet(pc_lt, "part_context_df.txt")

# part_context() has no dedicated table writer; write the returned data
# frame to xlsx via openxlsx and render it with the same converter used
# for every other table image.
pc_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(pc_wb, "part_context")
openxlsx::writeData(pc_wb, sheet = 1, pc_lt)
openxlsx::saveWorkbook(pc_wb, league_xlsx("context_part_table.xlsx"),
                       overwrite = TRUE)
xlsx2png(league_xlsx("context_part_table.xlsx"),
         file.path(img_dir, "context_part_table.png"))

# ---- Close the shared Chrome session -----------------------------------------
if (!is.null(.chromote_session)) .chromote_session$close()

# =============================================================================
# 6. Manifest check
# =============================================================================
manifest <- c(
  # pipeline
  "pipeline_netgraph.png", "pipeline_forest.png", "pipeline_netpairwise.png",
  "pipeline_netsplit.png", "pipeline_contributions.png", "pipeline_funnel.png",
  "pipeline_league_xlsx.png", "pipeline_forest_custom.png",
  # rare events
  "rare_sensitivity.png",
  # transitivity
  "transitivity_rob.png", "transitivity_indirectness.png",
  # league tables
  "league_pastel.png", "league_classic.png", "league_colorblind.png",
  "league_solid.png", "league_pscore.png", "league_dual.png",
  "league_quad.png", "league_st2026.png", "league_multi.png",
  # forest / netgraph
  "cforest_default.png",
  "cnetgraph_pastel.png", "cnetgraph_classic.png", "cnetgraph_nocinema.png",
  # kilim
  "kilim_grrd.png", "kilim_gryrd.png", "kilim_st2026.png",
  # vitruvian
  "vitruvian_basic.png", "vitruvian_trivial.png", "vitruvian_grouped.png",
  # evidence frameworks
  "context_min_table.png", "context_min_multi.png", "context_part_table.png"
)
missing <- manifest[!file.exists(file.path(img_dir, manifest))]
if (length(missing) > 0) {
  message("MISSING images:\n  ", paste(missing, collapse = "\n  "))
}
stopifnot(all(file.exists(file.path(img_dir, manifest))))

snippets <- c(
  "netmeta_remission_lt.txt", "global_test_remission_lt.txt",
  "local_test_remission_lt.txt", "pipeline_output_tree.txt",
  "rare_diagnostics_dropout_pt_rare.txt",
  "min_context_df.txt", "part_context_df.txt"
)
missing_snip <- snippets[!file.exists(file.path(snip_dir, snippets))]
if (length(missing_snip) > 0) {
  message("MISSING snippets:\n  ", paste(missing_snip, collapse = "\n  "))
}
stopifnot(all(file.exists(file.path(snip_dir, snippets))))

message("All ", length(manifest), " images and ", length(snippets),
        " snippets generated successfully.")
