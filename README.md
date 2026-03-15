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
# → outputs/remission_lt/ に結果が出力される
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
# → outputs/ 以下に4つのサブディレクトリが作成される
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

## Functions

| Function | Description |
|----------|-------------|
| `netmetawrap()` | Single-outcome NMA pipeline |
| `run_nma_batch()` | Multi-outcome batch pipeline |
| `plot_transitivity()` | Transitivity assessment plots |
| `load_w2i()` | Load bundled W2I sample data |
| `create_nma_project()` | Scaffold project directory structure |

## Sample data

`w2i_trials` — Arm-level data from Furukawa et al. (2024) W2I NMA.
Three treatments (CBT-I, Combination, Pharmacotherapy) for chronic insomnia;
4 binary outcomes (remission and dropout at long-term and post-treatment).

> Furukawa TA, Sato H, Hirata N, et al. Combined mindfulness-based cognitive therapy and worry postponement training for generalized anxiety disorder: A randomized controlled trial. *(replace with actual W2I citation)*

## References

**netmeta** (core NMA engine):

> Schwarzer G, Rücker G, Krahn U, König J (2024). *netmeta: Network Meta-Analysis using Frequentist Methods*. R package. https://cran.r-project.org/package=netmeta

> Rücker G, Schwarzer G (2015). Ranking treatments in frequentist network meta-analysis works without resampling methods. *BMC Medical Research Methodology*, 15, 58. https://doi.org/10.1186/s12874-015-0060-8

> Rücker G (2012). Network meta-analysis, electrical networks and graph theory. *Research Synthesis Methods*, 3(4), 312–324. https://doi.org/10.1002/jrsm.1058

**meta** (pairwise meta-analysis and forest plots):

> Schwarzer G, Carpenter JR, Rücker G (2015). *Meta-Analysis with R*. Springer. https://doi.org/10.1007/978-3-319-21416-0

If you use nmatools in published research, please also cite the above packages directly:

```r
citation("netmeta")
citation("meta")
```
