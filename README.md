# nmatools

R package providing one-stop wrappers for network meta-analysis (NMA) using the `netmeta` package.

## Install

```r
# Install from GitHub (recommended)
# install.packages("remotes")   # if not yet installed
remotes::install_github("ykfrkw/nmatools")
```

Dependencies (`netmeta`, `meta`, `dplyr`, `writexl`, `magick`, etc.) are installed automatically.

## Quick start

```r
library(nmatools)

# Built-in W2I sample data (Furukawa et al. 2024)
d <- load_w2i()
head(d)

# Single outcome — unquoted column names (like meta::pairwise)
netmetawrap(
  data            = d,
  studlab         = id,
  treat           = t,
  outcome         = "remission_lt",
  n               = n,
  event           = r,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable"
)
# → results are written to outputs/remission_lt/
```

All outputs are written to `./outputs/remission_lt/`:

| File | Contents |
|------|----------|
| `data_remission_lt.csv` | Arm-level data used |
| `netmeta_remission_lt.rds` | Fitted netmeta object |
| `netmeta_remission_lt.txt` | Print output |
| `global_test_remission_lt.txt` | `decomp.design()` |
| `local_test_remission_lt.txt` | `netsplit()` |
| `leaguetable_remission_lt.xlsx` | League table |
| `netgraph_remission_lt.pdf` | Network graph |
| `forest_remission_lt.pdf` | Forest plot vs reference |
| `forest_netpairwise_remission_lt.pdf` | All pairwise forest plots |
| `forest_netsplit_remission_lt.pdf` | Netsplit forest |
| `funnel_pairwise_remission_lt_*.pdf` | Contour-enhanced funnel plot (pairs with k ≥ 10) |
| `contributions_remission_lt.pdf` | Direct evidence contribution heatmap |

Large plots are automatically split into `_p1.pdf`, `_p2.pdf`, … to fit A4.

## Interactive GUI (CINeMA + ROB-MEN)

`cinema()` launches a Shiny application for interactive assessment of confidence
in NMA results, implementing:

- **CINeMA** (Nikolakopoulou et al. 2020) — six-domain confidence framework
- **ROB-MEN** (Chiocchia et al. 2021) — risk of bias due to missing evidence

```r
library(nmatools)

# Launch with no data — upload via the GUI
cinema()

# Pre-load data directly from R
d <- load_w2i()
cinema(d, format = "binary", effect_measure = "OR")
```

### Supported input formats

| `format` | Required columns | Typical use |
|----------|-----------------|-------------|
| `"binary"` | studlab / treat / n / event | Counts data (OR, RR) |
| `"continuous"` | studlab / treat / n / mean / sd | Continuous outcomes (SMD, MD) |
| `"pairwise"` | studlab / t1 / t2 / y / se | Pre-computed effects |

Column names are case-insensitive and trimmed automatically.

### Modules

| Module | Function |
|--------|----------|
| A — Data Input | Upload CSV/Excel or inject `data` directly |
| B — CINeMA | Domains D1–D6 with auto-algorithm; manual override |
| C — ROB-MEN | Pairwise + final rating tables; syncs to CINeMA D2 |
| D — Dashboard | Forest plot, league table, CINeMA summary, CSV/PNG export |

## Multiple outcomes

```r
library(nmatools)

d <- load_w2i()

params_list <- list(
  list(outcome = "remission_lt", n = "n", event = "r",            sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_lt",   n = "n", event = "n_dropout",    sm = "OR", small.values = "desirable"),
  list(outcome = "remission_pt", n = "n", event = "r_pt",         sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_pt",   n = "n", event = "n_dropout_pt", sm = "OR", small.values = "desirable")
)

run_nma_batch(
  params_list   = params_list,
  .default_args = list(
    data            = d,
    studlab         = "id",
    treat           = "t",
    reference.group = "Pharmacotherapy",
    path            = "./outputs"
  )
)
# → outputs/ automatically creates 4 sub directories
```

## Overriding defaults

All key arguments of `netmeta()`, `forest()`, `netpairwise()`, and `netsplit()` can be overridden:

```r
netmetawrap(
  ...,
  netmeta_args     = list(incr = 0.5),
  forest_args      = list(leftcols = c("studlab", "n.trts", "rob"),
                           xlim = c(0.1, 10)),
  netpairwise_args = list(prediction = FALSE),
  netsplit_args    = list(prediction = FALSE)
)
```

## Subnetworks

Disconnected networks are automatically detected. Each subnetwork is analysed separately and saved in a subdirectory:

```
outputs/
└── remission_lt/
    ├── data_remission_lt.csv        ← main network (returns early)
    ├── remission_lt_subnet_1/
    │   └── ...
    └── remission_lt_subnet_2/
        └── ...
```

## Project setup

```r
library(nmatools)

# Scaffold the recommended project layout
create_nma_project("~/my_nma_project")
# Creates: data/, outputs/, utils/ + copies template script
```

## Transitivity check

`plot_transitivity()` produces strip + box plots of study-level covariates grouped by
treatment comparison, to assess the transitivity assumption visually.

```r
library(nmatools)

d <- load_w2i()

# Non-numeric covariates must be converted first (see below)
d$rob_num <- ifelse(d$rob == "L", 0L, 1L)

plot_transitivity(
  data           = d,
  studlab        = id,
  treat          = t,
  covariate_cols = c("rob_num", "indirectness"),
  outcome        = "remission_lt",
  path           = "./outputs"
)
# → outputs/remission_lt/transitivity_remission_lt_rob_num.pdf
```

### Data preparation before calling `plot_transitivity()`

**Non-numeric covariates must be converted to numeric:**

```r
# Risk of bias: "L" / "H" → 0 / 1
d$rob_num <- ifelse(d$rob == "L", 0L, 1L)

# Categorical with 3 levels
d$design_num <- c("RCT" = 0, "quasi-RCT" = 1, "observational" = 2)[d$design]
```

**Proportions (%) must be expressed as a proportion (0–1), not a percentage:**

```r
# Wrong: d$female_pct = 65  (means 65%)
# Right:
d$female_prop <- d$female_pct / 100
```

Passing raw percentages will produce visually correct relative differences but
the y-axis will be misleading (values like 45–75 instead of 0.45–0.75).

### Multi-arm covariate handling

For studies with ≥ 2 arms, the covariate value per study is computed as:

- **Multiple non-NA values across arms** → simple mean
- **Exactly one non-NA value** → that value is used as-is
- **All NA** → excluded from that covariate's plot

This handles datasets where study-level characteristics are stored redundantly
in every arm row, or where only one arm row has a value filled in.

## Visualizations

A suite of publication-quality visualizations for `netmeta` results. Run
`?<function>` for full argument lists, or see [sample_viz.R](sample_viz.R)
for a walkthrough that exercises every option.

| Function | Output | Coloring basis |
|---|---|---|
| `color_league()` | `.xlsx` | CINeMA confidence / Schneider-Thoma 2026 / solid |
| `color_league_multi()` | `.xlsx` (multi-sheet) | Same as above, across outcomes |
| `color_forest()` | plot (side effect) | CINeMA confidence vs reference |
| `color_netgraph()` | plot (side effect) | CINeMA confidence per edge |
| `kilim()` | `.xlsx` / `.docx` | Signed p-value gradient / Schneider-Thoma 2026 |
| `vitruvian()` | `ggplot` / `.png` / `.pdf` / `.svg` | Signed p-value gradient |
| `min_context()` | data frame | — (statistical grouping) |
| `part_context()` | data frame | — (clinical-threshold grouping) |

```r
library(nmatools)

# Build netmeta objects from the bundled W2I sample data
net   <- build_w2i_netmeta("remission_lt")
ci_fp <- w2i_cinema_path()

# League table colored by CINeMA confidence
color_league(x = net, cinema = ci_fp, file = "league.xlsx")

# Forest plot with CI squares colored by CINeMA confidence
color_forest(x = net, cinema = ci_fp)
```

### `color_league()` palettes

| `palette_type` | Behavior |
|---|---|
| `"pastel"` (default) | CINeMA confidence ratings, pastel backgrounds |
| `"classic"` | CINeMA confidence ratings, vivid backgrounds (white text) |
| `"colorblind"` | CINeMA confidence ratings, Okabe-Ito palette |
| `"SchneiderThoma2026"` | CI vs trivial range (Blue / Yellow / Orange / White). Requires `trivial_range`. |
| `"solid"` | Single fill across all off-diagonal cells (no CINeMA used) |

`color_league()` also supports dual-outcome (`x2`) and quad-outcome (`x3`,
`x4`) layouts that pack multiple outcomes into one sheet, plus per-outcome
CINeMA files via `cinema2`, `cinema3`, `cinema4`. CINeMA CSV exports from the
[CINeMA web tool](https://cinema.ispm.unibe.ch/) are accepted directly; both
`"A:B"` and `"B:A"` comparison labels are matched automatically.

### `kilim()` and `vitruvian()` palettes

For multi-outcome plots, both functions accept:

- `palette = "GrYlRd"` — green / yellow / red continuous gradient over the
  signed p-value (recommended for Vitruvian plots).
- `palette = "GrRd"` — green / white / red continuous gradient (recommended
  for Kilim Excel output).
- `palette = "SchneiderThoma2026"` — categorical 4-color scheme (Blue,
  Yellow, Orange, White) based on the relationship between the 95% CI and
  `trivial_range`.

`vitruvian()` plots absolute risks (binary outcomes via `cer`, continuous
outcomes via `lnOR = pi/sqrt(3) * SMD`) on a per-treatment polar chart.
Outcomes can be visually grouped via `group = ` so spokes from the same
construct share an outer-ring color and an arc label.

### Schneider-Thoma 2026 color scheme

Categorical coloring based on the relationship between the 95% CI and a
user-defined trivial-effects zone:

| Color | Condition |
|---|---|
| Blue (`#4E88B4`) | Entire 95% CI within the trivial zone |
| Yellow (`#FFD700`) | Point estimate beyond trivial; CI significant but overlaps trivial |
| Orange (`#F08000`) | Point estimate AND entire CI beyond trivial |
| White | All other cases (non-significant, mixed) |

`trivial_range` must be on the same scale as the effect estimates: log scale
for OR/RR/HR (e.g. `log(c(1/1.1, 1.1))`), raw scale for MD/SMD.

### Evidence frameworks

`min_context()` classifies treatments by significance vs reference and
versus each other (Tikkinen et al. 2021). `part_context()` classifies by
absolute effect against user-defined clinical thresholds
(Brignardello-Petersen et al. 2020). Both attach optional CINeMA and
N-based quality columns. Helpers `table_min_context()` and
`table_min_context_multi()` produce ready-to-paste cross-tabulations to
xlsx or docx.

## Functions

| Function | Description |
|----------|-------------|
| `netmetawrap()` | Single-outcome NMA pipeline |
| `run_nma_batch()` | Multi-outcome batch pipeline |
| `plot_transitivity()` | Transitivity assessment plots |
| `load_w2i()` | Load bundled W2I sample data |
| `build_w2i_netmeta()` | Build a `netmeta` object from W2I sample data |
| `w2i_cinema_path()` | Path to the bundled W2I CINeMA CSV |
| `create_nma_project()` | Scaffold project directory structure |
| `cinema()` | Interactive GUI for CINeMA + ROB-MEN assessment |
| `color_league()` / `color_league_multi()` | Colored league tables |
| `color_forest()` | Forest plot with CINeMA-colored CI squares |
| `color_netgraph()` | Network graph with CINeMA-colored edges |
| `kilim()` | Multi-outcome Kilim plot |
| `vitruvian()` | Per-treatment polar plot of absolute effects |
| `min_context()` / `table_min_context()` / `table_min_context_multi()` | Minimally contextualized evidence framework |
| `part_context()` | Partially contextualized evidence framework |
| `cinema_palette()` / `pval_palette()` | Built-in color palettes |

## Sample data

`w2i_trials` — Arm-level data from Furukawa et al. (2024) W2I NMA.
Three treatments (CBT-I, Combination, Pharmacotherapy) for chronic insomnia;
4 binary outcomes (remission and dropout at long-term and post-treatment).

```r
d  <- load_w2i()                # arm-level trial data
ci <- w2i_cinema_path()         # path to bundled CINeMA CSV (remission_lt only)
```

> Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment choices for long-term remission of chronic insomnia disorder in adults: a systematic review and network meta-analysis. *Psychiatry Clin Neurosci*. 2024;78(11):646-653. https://doi.org/10.1111/pcn.13730

## References

**netmeta** (core NMA engine):

> Schwarzer G, Rücker G, Krahn U, König J (2024). *netmeta: Network Meta-Analysis using Frequentist Methods*. R package. https://cran.r-project.org/package=netmeta

> Rücker G, Schwarzer G (2015). Ranking treatments in frequentist network meta-analysis works without resampling methods. *BMC Medical Research Methodology*, 15, 58. https://doi.org/10.1186/s12874-015-0060-8

> Rücker G (2012). Network meta-analysis, electrical networks and graph theory. *Research Synthesis Methods*, 3(4), 312–324. https://doi.org/10.1002/jrsm.1058

**meta** (pairwise meta-analysis and forest plots):

> Schwarzer G, Carpenter JR, Rücker G (2015). *Meta-Analysis with R*. Springer. https://doi.org/10.1007/978-3-319-21416-0

**CINeMA** (confidence in NMA):

> Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: An approach for assessing confidence in the results of a network meta-analysis. *PLoS Med*. 2020;17(4):e1003082. https://doi.org/10.1371/journal.pmed.1003082

**ROB-MEN** (risk of bias due to missing evidence):

> Chiocchia V, Nikolakopoulou A, Higgins JPT, et al. ROB-MEN: a tool to assess the risk of bias due to missing evidence in network meta-analysis. *BMC Med*. 2021;19:304. https://doi.org/10.1186/s12916-021-02166-3

**Visualization methods:**

- **Kilim plot**: Seo M, Furukawa TA, Veroniki AA, et al. The Kilim plot: A tool for visualizing network meta-analysis results for multiple outcomes. *Res Synth Methods*. 2021;12(1):86–95. https://doi.org/10.1002/jrsm.1428
- **Vitruvian plot**: Ostinelli EG, Efthimiou O, Naci H, et al. Vitruvian plot: a visualisation tool for multiple outcomes in network meta-analysis. *Evid Based Ment Health*. 2022;25(e1):e65–e70. https://doi.org/10.1136/ebmental-2022-300457
- **Schneider-Thoma 2026 color scheme**: Schneider-Thoma J, Zhu Y, Qin M, et al. Comparative efficacy and tolerability of antidopaminergic and muscarinic antipsychotics for acute schizophrenia: a network meta-analysis. *Lancet*. 2026;407(10531):876–891. https://doi.org/10.1016/S0140-6736(25)02365-7
- **Minimally contextualized framework**: Tikkinen KAO, Guyatt GH, Dening SM, et al. Drug effects and natural history of disease in minimally and partially contextualised evidence frameworks. *BMJ*. 2021;372:m3900. https://doi.org/10.1136/bmj.m3900
- **Partially contextualized framework**: Brignardello-Petersen R, Izcovich A, Rochwerg B, et al. GRADE approach to drawing conclusions from a network meta-analysis using a partially contextualised framework. *BMJ*. 2020;371:m3907. https://doi.org/10.1136/bmj.m3907

If you use nmatools in published research, please also cite the above packages directly:

```r
citation("netmeta")
citation("meta")
```
