# =============================================================================
# nmatools - Visualization sample script using bundled W2I data
#
# Open nmatools.Rproj in RStudio, then run this file line-by-line. Output
# files are written to the project root.
#
# Available outcomes:
#   remission_lt  : long-term remission        (OR; higher = better)
#   dropout_lt    : long-term dropout          (OR; lower  = better)
#   remission_pt  : post-treatment remission   (OR; higher = better)
#   dropout_pt    : post-treatment dropout     (OR; lower  = better)
#
# Bundled CINeMA confidence ratings cover remission_lt only.
# =============================================================================

# 0. Setup --------------------------------------------------------------------
devtools::load_all(reset = TRUE)


# 1. Load sample data and build netmeta objects -------------------------------

## 1a. Raw trial data
d <- load_w2i()
head(d)
#>            id               t   n  r n_dropout r_pt n_dropout_pt rob indirectness
#> 1   Gross2011           CBT-I  20  9         3   12            2   L            1
#> 2   Gross2011 Pharmacotherapy  10  4         2    5            1   L            1
#> ...

## 1b. Build netmeta objects for all four outcomes
net_lt  <- build_w2i_netmeta("remission_lt")   # long-term remission
net_dlt <- build_w2i_netmeta("dropout_lt")     # long-term dropout
net_pt  <- build_w2i_netmeta("remission_pt")   # post-treatment remission
net_dpt <- build_w2i_netmeta("dropout_pt")     # post-treatment dropout

# Path to the bundled CINeMA CSV (remission_lt only)
cinema_path <- w2i_cinema_path()
# equivalent to: system.file("extdata", "w2i_cinema.csv", package = "nmatools")


# 2. color_league() — League table with CINeMA coloring ----------------------
#
# CINeMA ratings are available for remission_lt only.
# Cell background color = CINeMA confidence rating.
#
# Dual-outcome mode (x2 argument):
#   With x2 supplied, one table holds two outcomes:
#     lower-left triangle  = x   (annotated by label1)
#     upper-right triangle = x2  (annotated by label2)
#   Use cinema / cinema2 to attach a separate CINeMA file per outcome.

## Alphabetical (default; pastel palette)
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  sort_by = "alphabet",
  file    = "color_league_alphabet.xlsx"
)

## P-score order
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  sort_by = "pscore",
  file    = "color_league_pscore.xlsx"
)

## Custom order (sort_by = "custom", sort_order = c(...))
color_league(
  x          = net_lt,
  cinema     = cinema_path,
  sort_by    = "custom",
  sort_order = c("CBT-I", "Combination", "Pharmacotherapy"),
  file       = "color_league_custom.xlsx"
)

## Classic palette (saturated background + white text — common in published NMAs)
color_league(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "classic",
  file         = "color_league_classic.xlsx"
)

## Colorblind-safe palette (Okabe-Ito)
color_league(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "colorblind",
  file         = "color_league_colorblind.xlsx"
)

## Pass a palette list directly (legacy interface)
color_league(
  x       = net_lt,
  cinema  = cinema_path,
  palette = cinema_palette("classic"),
  file    = "color_league_compat.xlsx"
)


## 2b. Dual outcome (lower-left = remission_lt, upper-right = dropout_lt)
#
# lower-left:  net_lt  (long-term remission, CINeMA available -> pastel colors)
# upper-right: net_dlt (long-term dropout, no CINeMA -> uncolored)
# Footnote rows show "Lower-left: ..." / "Upper-right: ...".

color_league(
  x       = net_lt,
  cinema  = cinema_path,
  x2      = net_dlt,
  label1  = "Remission (long-term)",
  label2  = "Dropout (long-term)",
  sort_by = "pscore",
  file    = "color_league_dual_lt.xlsx"
)

## Post-treatment version (lower-left = remission_pt, upper-right = dropout_pt)
color_league(
  x       = net_pt,
  x2      = net_dpt,
  label1  = "Remission (post-tx)",
  label2  = "Dropout (post-tx)",
  sort_by = "pscore",
  file    = "color_league_dual_pt.xlsx"
)


## 2c. Quad outcome — four outcomes in one sheet, split top/bottom in each cell
#
# Each off-diagonal cell is split into two sub-rows (Excel rows = 2n).
#   lower-left, top    : x  = net_lt  (LT remission, CINeMA available -> pastel)
#   lower-left, bottom : x3 = net_dlt (LT dropout, no CINeMA -> uncolored)
#   upper-right, top   : x2 = net_pt  (PT remission, no CINeMA -> uncolored)
#   upper-right, bottom: x4 = net_dpt (PT dropout, no CINeMA -> uncolored)
# Diagonal cells are merged vertically and show the treatment name.
# Four footnote rows are written below the table.

color_league(
  x       = net_lt,
  cinema  = cinema_path,
  x2      = net_pt,
  x3      = net_dlt,
  x4      = net_dpt,
  label1  = "Remission (long-term)",
  label2  = "Remission (post-tx)",
  label3  = "Dropout (long-term)",
  label4  = "Dropout (post-tx)",
  sort_by = "pscore",
  file    = "color_league_quad.xlsx"
)


## 2d. color_league_multi() — four outcomes in one workbook (one sheet each)
#
# outcomes: named list (names become Excel sheet names)
# cinema:   named list (specify only outcomes with a CINeMA file; NULL otherwise)
#           Pass a single NULL to drop CINeMA across all sheets.
#
# Note: only remission_lt has CINeMA data; the other three pass cinema = NULL.

## Default (pastel palette, P-score order)
color_league_multi(
  outcomes = list(
    "Remission (LT)" = net_lt,
    "Dropout (LT)"   = net_dlt,
    "Remission (PT)" = net_pt,
    "Dropout (PT)"   = net_dpt
  ),
  cinema = list(
    "Remission (LT)" = cinema_path,
    "Dropout (LT)"   = NULL,
    "Remission (PT)" = NULL,
    "Dropout (PT)"   = NULL
  ),
  sort_by = "pscore",
  file    = "color_league_4outcomes.xlsx"
)

## Classic palette
color_league_multi(
  outcomes = list(
    "Remission (LT)" = net_lt,
    "Dropout (LT)"   = net_dlt,
    "Remission (PT)" = net_pt,
    "Dropout (PT)"   = net_dpt
  ),
  cinema = list(
    "Remission (LT)" = cinema_path,
    "Dropout (LT)"   = NULL,
    "Remission (PT)" = NULL,
    "Dropout (PT)"   = NULL
  ),
  sort_by      = "pscore",
  palette_type = "classic",
  file         = "color_league_4outcomes_classic.xlsx"
)


## 2e. SchneiderThoma2026 palette (no CINeMA needed; trivial_range required)
#
# Cells are categorically colored from the relationship between the 95% CI
# and the user-defined trivial range:
#   Blue   : entire 95% CI within the trivial range (clinically trivial)
#   Yellow : point estimate beyond trivial, but CI overlaps the trivial range
#   Orange : point estimate AND 95% CI both fully beyond the trivial range
#            (statistically significant beneficial or harmful)
#   White  : near null / non-significant (none of the above)
# cinema = is unused here (silently ignored if supplied).
# trivial_range is on the log scale for OR/RR/HR.

color_league(
  x             = net_lt,
  sort_by       = "pscore",
  palette_type  = "SchneiderThoma2026",
  trivial_range = log(c(1/1.1, 1.1)),
  file          = "color_league_st2026.xlsx"
)

## Dual outcome with SchneiderThoma2026
color_league(
  x             = net_lt,
  x2            = net_dlt,
  label1        = "Remission (long-term)",
  label2        = "Dropout (long-term)",
  sort_by       = "pscore",
  palette_type  = "SchneiderThoma2026",
  trivial_range = log(c(1/1.1, 1.1)),
  file          = "color_league_dual_st2026.xlsx"
)


## 2f. Solid fill — every off-diagonal cell uses one color (no CINeMA used)
#
# Useful when you want a uniform colored "frame" across multiple subgroup
# tables, or you simply want the cells filled without any evidence-based
# coloring. fill_color  = lower-left (outcome 1) cell color;
#          fill_color2 = upper-right (outcome 2) cell color (dual mode).

color_league(
  x            = net_lt,
  sort_by      = "pscore",
  palette_type = "solid",
  fill_color   = "#E2EFDA",     # light green
  file         = "color_league_solid.xlsx"
)

## Dual outcome + solid fill (different color per triangle)
color_league(
  x            = net_lt,
  x2           = net_dlt,
  label1       = "Remission (long-term)",
  label2       = "Dropout (long-term)",
  sort_by      = "pscore",
  palette_type = "solid",
  fill_color   = "#E2EFDA",     # lower-left: light green (remission)
  fill_color2  = "#FCE4D6",     # upper-right: light orange (dropout)
  file         = "color_league_dual_solid.xlsx"
)


# 3. color_netgraph() — Network graph with CINeMA edge coloring ---------------
#
# Each edge (direct comparison) is drawn in the color of its CINeMA confidence
# rating. By default, node size = total participants in trials including each
# treatment (computed from x$data n1/n2). With remission_lt, only this one
# outcome has CINeMA data; comparisons without a rating use col_no_cinema
# (default grey60).
#
# Other defaults applied (overridable via ...):
#   plastic = FALSE, pch = 21, col.points = "black", bg.points = "gray"
#   thickness = "number.of.studies"
#   number.of.studies = TRUE, pos.number.of.studies = 0.45

## Default (pastel palette, node size = N)
color_netgraph(
  x      = net_lt,
  cinema = cinema_path
)

## Classic palette
color_netgraph(
  x            = net_lt,
  cinema       = cinema_path,
  palette_type = "classic"
)

## Without CINeMA (single edge color, node size still proportional to N)
color_netgraph(
  x             = net_lt,
  col_no_cinema = "steelblue"
)


# 4. kilim() — multi-outcome summary table -----------------------------------
#
# Rows = treatments, columns = outcomes.
# palette = "GrRd" (default) / "GrYlRd"  : continuous gradient over signed
#   p-value (green = beneficial, red = harmful, blue = trivial).
# palette = "SchneiderThoma2026" : categorical 4-color scheme based on the
#   relationship between the CI and trivial_range. Requires trivial_range.
# For dropout outcomes, set small_values = "desirable" (lower is better).

## 4a. Basic (no trivial_range)
kilim(
  outcomes = list(
    list(
      x            = net_lt,
      name         = "remission_lt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
    ),
    list(
      x            = net_pt,
      name         = "remission_pt",
      reference    = "Pharmacotherapy",
      small_values = "undesirable",
      label        = "Remission\n(post-tx)",
      digits       = 2
    ),
    list(
      x            = net_dpt,
      name         = "dropout_pt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(post-tx)",
      digits       = 2
    )
  ),
  sort_by = "pscore",
  file    = "kilim_4outcomes.xlsx"
)

## 4b. With trivial_range (OR 0.91-1.10 -> blue)
#
# trivial_range is on the log scale for OR/RR/HR.
# log(1/1.1) ~ -0.095, log(1.1) ~ 0.095.
kilim(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", reference = "Pharmacotherapy",
         small_values = "undesirable", label = "Remission\n(long-term)", digits = 2),
    list(x = net_dlt, name = "dropout_lt",   reference = "Pharmacotherapy",
         small_values = "desirable",  label = "Dropout\n(long-term)",   digits = 2),
    list(x = net_pt,  name = "remission_pt", reference = "Pharmacotherapy",
         small_values = "undesirable", label = "Remission\n(post-tx)",   digits = 2),
    list(x = net_dpt, name = "dropout_pt",   reference = "Pharmacotherapy",
         small_values = "desirable",  label = "Dropout\n(post-tx)",     digits = 2)
  ),
  trivial_range = log(c(1/1.1, 1.1)),   # treat OR 0.91-1.10 as trivial
  sort_by = "pscore",
  file    = "kilim_4outcomes_trivial.xlsx"
)

## 4c. SchneiderThoma2026 palette (trivial_range required)
#
# Cells are colored by the relationship between the 95% CI and trivial_range.
# trivial_range is on the log scale for OR/RR/HR.
kilim(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", reference = "Pharmacotherapy",
         small_values = "undesirable", label = "Remission\n(long-term)", digits = 2),
    list(x = net_dlt, name = "dropout_lt",   reference = "Pharmacotherapy",
         small_values = "desirable",  label = "Dropout\n(long-term)",   digits = 2),
    list(x = net_pt,  name = "remission_pt", reference = "Pharmacotherapy",
         small_values = "undesirable", label = "Remission\n(post-tx)",   digits = 2),
    list(x = net_dpt, name = "dropout_pt",   reference = "Pharmacotherapy",
         small_values = "desirable",  label = "Dropout\n(post-tx)",     digits = 2)
  ),
  trivial_range = log(c(1/1.1, 1.1)),
  palette = "SchneiderThoma2026",
  sort_by = "pscore",
  file    = "kilim_4outcomes_st2026.xlsx"
)

## 4d. Per-outcome trivial_range overrides
kilim(
  outcomes = list(
    list(
      x             = net_lt,
      name          = "remission_lt",
      reference     = "Pharmacotherapy",
      small_values  = "undesirable",
      label         = "Remission\n(long-term)",
      digits        = 2,
      trivial_range = log(c(1/2, 2))   # this outcome only: OR 0.5-2.0
    ),
    list(
      x            = net_dlt,
      name         = "dropout_lt",
      reference    = "Pharmacotherapy",
      small_values = "desirable",
      label        = "Dropout\n(long-term)",
      digits       = 2
      # no per-outcome trivial_range -> uses the top-level value below
    )
  ),
  trivial_range = log(c(1/1.1, 1.1)),
  sort_by = "alphabet",
  file    = "kilim_trivial_peroutcome.xlsx"
)


# 5. vitruvian() — per-treatment polar (spider) plot --------------------------
#
# One panel per treatment; each spoke = one outcome.
# Bar height = absolute risk (%) (or raw effect for continuous outcomes when
# cer is not supplied). Bar color = continuous p-value gradient.
# The reference treatment is shown in steel-blue; a blue dot marks the
# reference value on every other treatment's panel.
#
# Argument structure mirrors kilim() (a list of outcome specs).
#
# Continuous outcomes (SMD / MD):
#   When the netmeta object has sm = "SMD" or "MD" and cer is supplied, the
#   estimate is converted to OR via the SMD approximation
#   lnOR = pi/sqrt(3) * SMD (Cox & Snell 1989), so bars share the absolute-risk
#   scale used for binary outcomes.
#     SM = "SMD" : te is treated as SMD directly
#     SM = "MD"  : SMD = MD / pooled_sd, then converted
#                  pooled_sd is auto-estimated from seTE / n1 / n2 if omitted
#   Without cer, raw MD/SMD values are plotted (no conversion).
#
# Display sizing:
#   When file = NULL and interactive(), a PNG of the requested width/height is
#   written to tempfile() and shown in the RStudio Viewer (or system viewer),
#   so resizing the Zoom window does not reflow the layout. Set file = "x.png"
#   to write to disk instead.
#
# Group coloring:
#   Each spoke's outer ring is colored by `group`. Outcomes with the same
#   group share a color, and the group name is rendered as an arc label in
#   the outer band. The inner panel area is transparent.

## 5a. Basic (4 outcomes, 3 treatments) — file = NULL -> Viewer at fixed size
#
# cer values used below are the published Pharmacotherapy reference rates:
#   remission_lt  28%  long-term remission
#   dropout_lt    39%  long-term dropout
#   remission_pt  28%  post-treatment remission
#   dropout_pt    16%  post-treatment dropout
#
# Without cer (or with cer = "metaprop"), the reference event rate is
# auto-estimated via meta::metaprop() (GLMM); cer = "simple" uses a plain
# average. Specifying numeric cer keeps numbers identical to the publication.
#
# When width / height are omitted, they are computed from ncol and the number
# of treatments (one panel = 4x4 inches).
# Example: 3 treatments x ncol = 3 -> 1 row -> width = 3*4 + 1.8 = 13.8, height = 4

vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission",
         small_values = "undesirable", cer = 0.28, group = "Long-term"),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout",
         small_values = "desirable",  cer = 0.39, group = "Long-term"),
    list(x = net_pt,  name = "remission_pt", label = "Remission",
         small_values = "undesirable", cer = 0.28, group = "Post-treatment"),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout",
         small_values = "desirable",  cer = 0.16, group = "Post-treatment")
  ),
  reference = "Pharmacotherapy",
  digits    = 1,
  ncol      = 3
  # width / height omitted -> auto-computed
)

## 5b. With trivial_range (OR 0.91-1.10 -> blue) — saved as PNG
vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission\n(long-term)",
         small_values = "undesirable", cer = 0.28, group = "Long-term"),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(long-term)",
         small_values = "desirable",  cer = 0.39, group = "Long-term"),
    list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
         small_values = "undesirable", cer = 0.28, group = "Post-treatment"),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
         small_values = "desirable",  cer = 0.16, group = "Post-treatment")
  ),
  reference     = "Pharmacotherapy",
  trivial_range = log(c(1/1.1, 1.1)),
  digits        = 1,
  ncol          = 3,
  file          = "vitruvian_4outcomes_trivial.png"
)

## 5c. PNG output (no group, no trivial)
vitruvian(
  outcomes = list(
    list(x = net_lt,  name = "remission_lt", label = "Remission\n(long-term)",
         small_values = "undesirable", cer = 0.28),
    list(x = net_dlt, name = "dropout_lt",   label = "Dropout\n(long-term)",
         small_values = "desirable",  cer = 0.39),
    list(x = net_pt,  name = "remission_pt", label = "Remission\n(post-tx)",
         small_values = "undesirable", cer = 0.28),
    list(x = net_dpt, name = "dropout_pt",   label = "Dropout\n(post-tx)",
         small_values = "desirable",  cer = 0.16)
  ),
  reference = "Pharmacotherapy",
  digits    = 1,
  ncol      = 3,
  file      = "vitruvian_4outcomes.png"
)


## 5d. Mixed binary + SMD / MD outcomes (interface example — not runnable
##     against the W2I dataset)
#
# Binary and continuous outcomes can be combined in a single chart.
#   SMD : supply numeric cer -> converted via lnOR = pi/sqrt(3) * SMD
#   MD  : supply cer + pooled_sd (or omit pooled_sd for auto-estimation)
#         pooled_sd is auto-estimated from netmeta$data (seTE / n1 / n2).
#
# W2I has no continuous outcome, so the snippets below are commented out.
# Replace net_smd / net_md with your real sm = "SMD" or "MD" netmeta objects.

## SMD outcome — pooled_sd not needed (te is already SMD-scaled)
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_lt,     # binary outcome (OR)
#       name         = "remission_lt",
#       label        = "Remission",
#       small_values = "undesirable",
#       cer          = 0.28,
#       group        = "Binary"
#     ),
#     list(
#       x            = net_smd,    # replace with a real SMD netmeta object
#       name         = "sleep_quality_smd",
#       label        = "Sleep quality\n(SMD)",
#       small_values = "desirable",
#       cer          = 0.30,       # baseline disturbance rate in the reference arm
#       group        = "Continuous"
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 2,
#   file      = "vitruvian_mixed_smd.png",
#   width     = 10,
#   height    = 5
# )

## MD outcome — pooled_sd supplied manually
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_md,     # replace with a real MD netmeta object
#       name         = "isi_md",
#       label        = "ISI (MD)",
#       small_values = "desirable",
#       cer          = 0.30,       # reference baseline rate
#       pooled_sd    = 5.2         # pooled SD (omit -> auto-estimated)
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 1,
#   file      = "vitruvian_md.png",
#   width     = 5,
#   height    = 5
# )

## MD outcome — pooled_sd auto-estimated (needs seTE/n1/n2 in netmeta$data)
# vitruvian(
#   outcomes = list(
#     list(
#       x            = net_md,
#       name         = "isi_md",
#       label        = "ISI (MD)",
#       small_values = "desirable",
#       cer          = 0.30        # pooled_sd omitted -> auto-estimated
#     )
#   ),
#   reference = "Pharmacotherapy",
#   digits    = 1,
#   ncol      = 1,
#   file      = "vitruvian_md_auto.png",
#   width     = 5,
#   height    = 5
# )


# 6. min_context() — minimally contextualized evidence framework --------------
#
# Treatments are grouped by statistical comparisons against the reference:
#   Group  0 : not significantly different from reference
#   Group +1 : significantly better than reference
#   Group +2 : significantly better than every Group +1 treatment
#   Group -1 : significantly worse than reference
#   ...
#
# n_thresholds = c(100, 400) adds an n_quality column (default on).
# CINeMA ratings are available for remission_lt only.

## 6a. Long-term remission with CINeMA + N quality
mc_lt <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt)
#>         treatment group cinema n_total n_quality
#> 1           CBT-I     1   High     ...      High
#> 2     Combination     0   ...      ...      ...
#> 3 Pharmacotherapy     0   <NA>     ...      ...

## 6b. Long-term dropout: only n_quality (no CINeMA)
mc_dlt <- min_context(
  x            = net_dlt,
  reference    = "Pharmacotherapy",
  small_values = "desirable"
)
print(mc_dlt)

## 6c. Custom n_thresholds (e.g. 50 / 200)
mc_lt_custom <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  n_thresholds = c(50, 200),
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt_custom)

## 6d. Drop n_quality column (n_thresholds = NULL)
mc_lt_noN <- min_context(
  x            = net_lt,
  cinema       = cinema_path,
  n_thresholds = NULL,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
print(mc_lt_noN)

## 6e. Post-treatment outcomes
mc_pt <- min_context(
  x            = net_pt,
  reference    = "Pharmacotherapy",
  small_values = "undesirable"
)
mc_dpt <- min_context(
  x            = net_dpt,
  reference    = "Pharmacotherapy",
  small_values = "desirable"
)

## 6f. table_min_context(): Group x CINeMA cross-tab (default)
tbl_cinema <- table_min_context(mc_lt)
print(tbl_cinema)

table_min_context(mc_lt, file = "min_context_remission_lt.docx")
table_min_context(mc_lt, file = "min_context_remission_lt.xlsx")

## 6g. table_min_context(): Group x N quality
tbl_n <- table_min_context(mc_lt, quality_col = "n_quality")
print(tbl_n)

table_min_context(mc_lt, quality_col = "n_quality",
                  file = "min_context_remission_lt_nquality.xlsx")


## 6h. table_min_context_multi(): summary across multiple outcomes
#
# Rows = outcome, columns = Group +2, +1, 0, -1, -2, ...
# CINeMA / n_quality are not split out separately.
# Treatment names within a cell are joined by `sep` (default: "\n").
# mc_lt, mc_dlt, mc_pt, mc_dpt are all created above.

# Inspect as a data.frame
tbl_multi <- table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)" = mc_lt,
    "Dropout (long-term)"   = mc_dlt,
    "Remission (post-tx)"   = mc_pt,
    "Dropout (post-tx)"     = mc_dpt
  )
)
print(tbl_multi)
#>                  Outcome Group +1                         Group 0
#> 1  Remission (long-term)    CBT-I  Combination\nPharmacotherapy
#> ...

# Comma-separated cells (better for Excel)
table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)" = mc_lt,
    "Dropout (long-term)"   = mc_dlt,
    "Remission (post-tx)"   = mc_pt,
    "Dropout (post-tx)"     = mc_dpt
  ),
  sep  = ", ",
  file = "min_context_multi.xlsx"
)

# docx output (landscape layout)
table_min_context_multi(
  outcome_list = list(
    "Remission (long-term)" = mc_lt,
    "Dropout (long-term)"   = mc_dlt,
    "Remission (post-tx)"   = mc_pt,
    "Dropout (post-tx)"     = mc_dpt
  ),
  sep  = "\n",
  file = "min_context_multi.docx"
)


# 7. part_context() — partially contextualized evidence framework -------------
#
# Effects are converted to an absolute scale and grouped by user-supplied
# thresholds. Unlike min_context(), the group boundaries are clinical
# (smallest worthwhile difference), not statistical significance.
#
# Threshold mechanics (e.g. thresholds = c(lo, hi)):
#   Group -1 : abs_effect < lo
#   Group  0 : lo <= abs_effect < hi   (the bin containing 0 is Group 0)
#   Group +1 : abs_effect >= hi
#
# Units of abs_effect:
#   binary (OR/RR)      : ARD (absolute risk difference vs the reference arm)
#   continuous (MD/SMD) : raw MD or SMD
#
# When small_values = "desirable", direction = -1; abs_effect being negative
# (i.e. lower than reference) maps to higher group numbers.

## 7a. Long-term remission: SWD = ARD 10% (two groups)
#   thresholds = c(0.10)
#     Group  0 : ARD < 0.10  (SWD not met)
#     Group +1 : ARD >= 0.10 (SWD met)
pc_lt <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.12),         # ARD 10% cutoff
  cer          = 0.28,            # Pharmacotherapy remission rate 28%
  small_values = "undesirable"
)
print(pc_lt)
#>         treatment abs_effect group cinema n_total n_quality
#> 1 Pharmacotherapy 0.00000000     0   <NA>     ...      ...
#> 2           CBT-I 0.12...        1   High     ...      High
#> 3     Combination 0.07...        0   ...      ...      ...

# Inspect bin labels
attr(pc_lt, "threshold_labels")

## 7b. Three-tier classification (harmful / equivalent / beneficial)
#   thresholds = c(-0.05, 0.10)
#     Group -1 : ARD < -0.05         (>5% absolute harm)
#     Group  0 : -0.05 <= ARD < 0.10 (equivalent or trivial)
#     Group +1 : ARD >= 0.10         (SWD met)
pc_lt_3cat <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(-0.05, 0.10),
  cer          = 0.28,
  small_values = "undesirable"
)
print(pc_lt_3cat)

## 7c. Near-SWD band (a middle group just below the SWD cutoff)
#   thresholds = c(0.07, 0.10)
#     Group  0 : ARD < 0.07           (clearly below SWD)
#     Group +1 : 0.07 <= ARD < 0.10   (near SWD)
#     Group +2 : ARD >= 0.10          (SWD met)
pc_lt_near <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.07, 0.10),
  cer          = 0.28,
  small_values = "undesirable"
)
print(pc_lt_near)

## 7d. Long-term dropout: small_values = "desirable" (lower dropout is better)
#   thresholds = c(-0.10, 0.10)
#   abs_effect = ARD vs reference (positive = more dropout, negative = less)
#   direction = -1 -> negative abs_effect favors the treatment.
#     Group +1 : ARD < -0.10           (>=10% reduction in dropout = beneficial)
#     Group  0 : -0.10 <= ARD < 0.10   (no clinically meaningful difference)
#     Group -1 : ARD >= 0.10           (>=10% increase in dropout = harmful)
pc_dlt <- part_context(
  x            = net_dlt,
  reference    = "Pharmacotherapy",
  thresholds   = c(-0.10, 0.10),
  cer          = 0.39,
  small_values = "desirable"
)
print(pc_dlt)

## 7e. CINeMA + N quality together
pc_lt_full <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.10),
  cer          = 0.28,
  small_values = "undesirable",
  cinema       = cinema_path,
  n_thresholds = c(100, 400)
)
print(pc_lt_full)

## 7f. Drop the n_quality column (n_thresholds = NULL)
pc_lt_noN <- part_context(
  x            = net_lt,
  reference    = "Pharmacotherapy",
  thresholds   = c(0.10),
  cer          = 0.28,
  small_values = "undesirable",
  n_thresholds = NULL
)
print(pc_lt_noN)

# =============================================================================
# End of sample_viz.R
# =============================================================================
