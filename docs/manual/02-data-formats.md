[Manual home](README.md) › Data formats

# Data Formats

This chapter describes the input formats that nmatools accepts, documents the
bundled `w2i_trials` sample dataset column by column, introduces the data-access
helpers, explains the CINeMA CSV export format used by the coloring functions,
and shows how to scaffold a project directory.

## Arm-based long format

The primary input format for `netmetawrap()` and `run_nma_batch()` is
**arm-based long format**: one row per treatment arm of each study. A three-arm
study occupies three rows; a two-arm study occupies two.

For **binary** outcomes, the required columns are:

| Role | Meaning |
|------|---------|
| `studlab` | Study label (identifier) |
| `treat` | Treatment label |
| `n` | Number of participants in the arm |
| `event` | Number of events in the arm |

For **continuous** outcomes, the required columns are:

| Role | Meaning |
|------|---------|
| `studlab` | Study label (identifier) |
| `treat` | Treatment label |
| `n` | Number of participants in the arm |
| `mean` | Arm mean |
| `sd` | Arm standard deviation |

These "roles" are not fixed column names. In `netmetawrap()` you map each role
to an actual column in your data by passing the column name to the matching
argument (`studlab =`, `treat =`, `n =`, `event =`, or `mean_col =` / `sd_col =`
for continuous outcomes) as a **quoted string**, for example `studlab = "id"`.

> **Column-name rule.** In the scripting interface (`netmetawrap()`,
> `run_nma_batch()`) the exact column names you pass must match the data
> frame's columns exactly, including case. The interactive GUI (`cinema()`)
> is more forgiving: it matches the required column names case-insensitively
> and trims surrounding whitespace.

> **Quote every column name.** Quoted strings are the documented style
> throughout this manual, and they work in every scripting interface:
> `studlab = "id"`, `treat = "t"`, `n = "n"`, `event = "r"`,
> `mean_col = "..."`, `sd_col = "..."`. A direct `netmetawrap()` or
> `plot_transitivity()` call additionally tolerates unquoted names
> (`studlab = id`), for familiarity with `meta::pairwise()`. `run_nma_batch()`
> cannot: it forwards its arguments through `do.call()`, which evaluates them
> as ordinary values, so an unquoted `id` is looked up as a variable and is not
> found. Quoting everywhere therefore keeps a script portable between the
> single-outcome and batch interfaces.

## Pairwise contrast format

For results that are already reduced to study-level treatment contrasts, the
interactive GUI also accepts **pairwise contrast format** via
`cinema(..., format = "pairwise")`:

| Role | Meaning |
|------|---------|
| `studlab` | Study label |
| `t1` | First treatment of the contrast |
| `t2` | Second treatment of the contrast |
| `y` | Effect estimate (contrast) |
| `se` | Standard error of the estimate |

The three supported GUI formats and their required columns are:

| `format` | Required columns | Typical use |
|----------|-----------------|-------------|
| `"binary"` | studlab / treat / n / event | Count data (OR, RR) |
| `"continuous"` | studlab / treat / n / mean / sd | Continuous outcomes (SMD, MD) |
| `"pairwise"` | studlab / t1 / t2 / y / se | Pre-computed effects |

## The bundled `w2i_trials` dataset

The package bundles the **W2I** (Waking to Insomnia) dataset from Furukawa et
al. 2024: arm-level data from nine trials comparing CBT-I, Combination (CBT-I
plus pharmacotherapy), and Pharmacotherapy for chronic insomnia, with four
binary outcomes. Load it with `load_w2i()`:

```r
library(nmatools)

d <- load_w2i()
head(d)
colnames(d)
```

The complete column dictionary is:

| Column | Meaning |
|--------|---------|
| `id` | Study identifier (author plus year) |
| `t` | Treatment: `"CBT-I"`, `"Combination"`, or `"Pharmacotherapy"` |
| `n` | Number of randomized participants in the arm |
| `r` | Remission events at long-term follow-up |
| `n_dropout` | Dropout events at long-term follow-up |
| `r_pt` | Remission events at post-treatment |
| `n_dropout_pt` | Dropout events at post-treatment |
| `rob` | Risk of bias: `"L"` (low), `"M"` (some concerns), `"H"` (high) |
| `indirectness` | Indirectness score (`1` = no concerns) |

The four binary outcomes are therefore assembled by pairing `n` with one of the
four event columns: `r` (remission, long-term), `n_dropout` (dropout,
long-term), `r_pt` (remission, post-treatment), and `n_dropout_pt` (dropout,
post-treatment). The `rob` and `indirectness` columns are study-level
covariates used for the transitivity plots in [Chapter 5](05-transitivity.md).

> Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment
> choices for long-term remission of chronic insomnia disorder in adults: a
> systematic review and network meta-analysis. *Psychiatry Clin Neurosci*.
> 2024;78(11):646-653. https://doi.org/10.1111/pcn.13730

## Data-access helpers

Three convenience functions expose the bundled data and its companion CINeMA
ratings:

- **`load_w2i()`** reads the bundled `w2i_trials` CSV fresh from the package's
  `inst/extdata/` directory and returns it as a plain data frame.

- **`build_w2i_netmeta(outcome =, reference =)`** loads the data, runs
  `meta::pairwise()`, and fits a `netmeta` object for a chosen outcome — a
  one-liner for demonstrating the visualization functions on a known network.
  `outcome` is one of `"remission_lt"` (default), `"dropout_lt"`,
  `"remission_pt"`, or `"dropout_pt"`; `reference` defaults to
  `"Pharmacotherapy"`. Each outcome carries the appropriate `small.values`
  direction internally (remission outcomes are `"undesirable"`, dropout
  outcomes are `"desirable"`).

  ```r
  library(nmatools)

  net <- build_w2i_netmeta("remission_lt")
  net <- build_w2i_netmeta("dropout_lt", reference = "CBT-I")
  ```

- **`w2i_cinema_path()`** returns the file path to the bundled W2I CINeMA
  confidence-rating CSV. The bundled ratings cover the long-term remission
  outcome only. Pass this path to any function that accepts a `cinema =`
  argument, such as `color_league()`, `color_forest()`, or `color_netgraph()`.

  ```r
  library(nmatools)

  ci_fp <- w2i_cinema_path()
  ```

## CINeMA CSV export format

The coloring functions read confidence ratings exported from the
[CINeMA web tool](https://cinema.ispm.unibe.ch/). The bundled W2I file has one
row per treatment comparison and the following columns:

```
Comparison, Number of studies, Within-study bias, Reporting bias,
Indirectness, Imprecision, Heterogeneity, Incoherence,
Confidence rating, Reason(s) for downgrading
```

The bundled example looks like this:

```
"Comparison","Number of studies","Within-study bias",...,"Confidence rating","Reason(s) for downgrading"
"CBT-I:Combination",5,"Some concerns",...,"High","[]"
"CBT-I:Pharmacotherapy",7,"No concerns",...,"Moderate","[]"
"Combination:Pharmacotherapy",3,"Some concerns",...,"Moderate","[]"
```

The key field the coloring functions consume is **Confidence rating**
(`"High"`, `"Moderate"`, `"Low"`, or `"Very low"`), which is mapped to a cell
or edge color.

> **Comparison-label matching.** The `Comparison` column uses a colon-separated
> `"A:B"` label. When nmatools joins the CINeMA ratings to a network, it matches
> comparisons in either orientation automatically: both `"A:B"` and `"B:A"`
> resolve to the same comparison. You therefore do not need to reorder the
> exported comparisons to match the internal treatment ordering.

## Scaffolding a project

`create_nma_project(path =, open_template =)` builds the recommended project
layout and copies an analysis template script:

```r
library(nmatools)

create_nma_project("~/my_nma_project")
```

The resulting directory tree is:

```
my_nma_project/
├── data/       ← put arm-level input data files here
├── outputs/    ← netmetawrap() writes all results here
└── utils/
    └── run_analysis_template.R   ← copied template to adapt
```

`create_nma_project()` never overwrites an existing directory or template; it
reports which paths already exist and which it created, and returns the vector
of created paths invisibly. Set `open_template = TRUE` to open the copied
template in RStudio automatically after creation (this requires a running
RStudio session and the `rstudioapi` package).

---
Prev: [Installation and quick start](01-installation.md) · Next: [The NMA pipeline](03-nma-pipeline.md)
