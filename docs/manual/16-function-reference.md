[Manual home](README.md) › Function reference

# 16. Function reference

Every function exported by nmatools, grouped by role. The **Chapter** column
links to the manual chapter that documents the function in full; the **Help**
column gives the R help topic — type it at the console (for example
`?netmetawrap`) for the complete argument list and runnable examples. The
package index is available via `help(package = "nmatools")`.

## Pipelines

The scripted analysis entry points that fit networks and write results to disk.

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `netmetawrap()` | Single-outcome NMA pipeline: fit, test, tabulate, and plot one outcome to disk. | [Ch. 3](03-nma-pipeline.md) | `?netmetawrap` |
| `run_nma_batch()` | Run the `netmetawrap()` pipeline across many outcomes from one specification. | [Ch. 4](04-batch-and-rare-events.md) | `?run_nma_batch` |
| `plot_transitivity()` | Strip-and-box plots of study-level covariates by comparison, for transitivity assessment. | [Ch. 5](05-transitivity.md) | `?plot_transitivity` |

## Data and setup

Loading the bundled sample data and scaffolding a project.

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `load_w2i()` | Load the bundled arm-level W2I insomnia sample dataset. | [Ch. 2](02-data-formats.md) | `?load_w2i` |
| `build_w2i_netmeta()` | Fit a `netmeta` object for one W2I outcome (a one-liner for the visualization demos). | [Ch. 2](02-data-formats.md) | `?build_w2i_netmeta` |
| `w2i_cinema_path()` | Return the path to the bundled W2I CINeMA confidence-rating CSV. | [Ch. 2](02-data-formats.md) | `?w2i_cinema_path` |
| `create_nma_project()` | Scaffold the recommended project layout (`data/`, `outputs/`, `utils/`) and copy a template script. | [Ch. 2](02-data-formats.md) | `?create_nma_project` |

## GUI

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `cinema()` | Launch the interactive Shiny app for CINeMA + ROB-MEN confidence assessment. | [Ch. 6–10](06-gui-overview.md) | `?cinema` |

## Visualization

Publication-quality tables and plots for `netmeta` results, plus the palette
helpers and the rare-event sensitivity plot.

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `color_league()` | Colored league table (`.xlsx`); up to four outcomes packed into one sheet. | [Ch. 11](11-league-tables.md) | `?color_league` |
| `color_league_multi()` | Multi-sheet league workbook, one league table per outcome. | [Ch. 11](11-league-tables.md) | `?color_league_multi` |
| `color_forest()` | Forest plot with CI squares colored by CINeMA confidence. | [Ch. 12](12-colored-forest-netgraph.md) | `?color_forest` |
| `color_netgraph()` | Network graph with edges colored by CINeMA confidence. | [Ch. 12](12-colored-forest-netgraph.md) | `?color_netgraph` |
| `kilim()` | Multi-outcome Kilim table shaded by signed p-value (`.xlsx` / `.docx`). | [Ch. 13](13-kilim-vitruvian.md) | `?kilim` |
| `vitruvian()` | Per-treatment polar chart of absolute effects across outcomes. | [Ch. 13](13-kilim-vitruvian.md) | `?vitruvian` |
| `cinema_palette()` | Return a built-in CINeMA confidence palette (pastel / classic / colorblind). | [Ch. 12](12-colored-forest-netgraph.md) | `?cinema_palette` |
| `pval_palette()` | Return the signed-p-value diverging gradient used by Kilim/Vitruvian. | [Ch. 12](12-colored-forest-netgraph.md) | `?pval_palette` |
| `plot_rare_nma_sensitivity()` | Faceted four-method rare-event sensitivity forest plot. | [Ch. 4](04-batch-and-rare-events.md) | `?plot_rare_nma_sensitivity` |

## Evidence frameworks

Minimally and partially contextualized treatment classification and their
cross-tabulation helpers.

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `min_context()` | Minimally contextualized framework: classify treatments by statistical comparison (Tikkinen 2021). | [Ch. 14](14-evidence-frameworks.md) | `?min_context` |
| `part_context()` | Partially contextualized framework: classify by absolute effect against clinical thresholds (Brignardello-Petersen 2020). | [Ch. 14](14-evidence-frameworks.md) | `?part_context` |
| `table_min_context()` | Cross-tabulate `min_context()` groups against a quality column (`.xlsx` / `.docx`). | [Ch. 14](14-evidence-frameworks.md) | `?table_min_context` |
| `table_min_context_multi()` | Summarize several `min_context()` results in one table, one row per outcome. | [Ch. 14](14-evidence-frameworks.md) | `?table_min_context_multi` |

All 21 exported functions above are listed in `NAMESPACE`. For a live index in
your session, run `help(package = "nmatools")`.

---

# Literature references

## netmeta (core NMA engine)

> Schwarzer G, Rücker G, Krahn U, König J (2024). *netmeta: Network Meta-Analysis using Frequentist Methods*. R package. https://cran.r-project.org/package=netmeta

> Rücker G, Schwarzer G (2015). Ranking treatments in frequentist network meta-analysis works without resampling methods. *BMC Medical Research Methodology*, 15, 58. https://doi.org/10.1186/s12874-015-0060-8

> Rücker G (2012). Network meta-analysis, electrical networks and graph theory. *Research Synthesis Methods*, 3(4), 312–324. https://doi.org/10.1002/jrsm.1058

## meta (pairwise meta-analysis and forest plots)

> Schwarzer G, Carpenter JR, Rücker G (2015). *Meta-Analysis with R*. Springer. https://doi.org/10.1007/978-3-319-21416-0

## CINeMA (confidence in NMA)

> Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: An approach for assessing confidence in the results of a network meta-analysis. *PLoS Med*. 2020;17(4):e1003082. https://doi.org/10.1371/journal.pmed.1003082

> Papakonstantinou T, Nikolakopoulou A, Higgins JPT, Egger M, Salanti G. CINeMA: Software for semiautomated assessment of the confidence in the results of network meta-analysis. *Campbell Syst Rev*. 2020;16(1):e1080. https://doi.org/10.1002/cl2.1080

## ROB-MEN (risk of bias due to missing evidence)

> Chiocchia V, Nikolakopoulou A, Higgins JPT, et al. ROB-MEN: a tool to assess the risk of bias due to missing evidence in network meta-analysis. *BMC Med*. 2021;19:304. https://doi.org/10.1186/s12916-021-02166-3

## Rare events (Mantel-Haenszel NMA)

> Efthimiou O, Rücker G, Schwarzer G, Higgins JPT, Egger M, Salanti G. Network meta-analysis of rare events using the Mantel-Haenszel method. *Stat Med*. 2019;38(16):2992–3012. https://doi.org/10.1002/sim.8158

## Visualization methods

- **Kilim plot:** Seo M, Furukawa TA, Veroniki AA, et al. The Kilim plot: A tool for visualizing network meta-analysis results for multiple outcomes. *Res Synth Methods*. 2021;12(1):86–95. https://doi.org/10.1002/jrsm.1428
- **Vitruvian plot:** Ostinelli EG, Efthimiou O, Naci H, et al. Vitruvian plot: a visualisation tool for multiple outcomes in network meta-analysis. *Evid Based Ment Health*. 2022;25(e1):e65–e70. https://doi.org/10.1136/ebmental-2022-300457
- **Schneider-Thoma 2026 color scheme:** Schneider-Thoma J, Zhu Y, Qin M, et al. Comparative efficacy and tolerability of antidopaminergic and muscarinic antipsychotics for acute schizophrenia: a network meta-analysis. *Lancet*. 2026;407(10531):876–891. https://doi.org/10.1016/S0140-6736(25)02365-7

## Evidence frameworks

- **Minimally contextualized framework:** Tikkinen KAO, Guyatt GH, Dening SM, et al. Drug effects and natural history of disease in minimally and partially contextualised evidence frameworks. *BMJ*. 2021;372:m3900. https://doi.org/10.1136/bmj.m3900
- **Partially contextualized framework:** Brignardello-Petersen R, Izcovich A, Rochwerg B, et al. GRADE approach to drawing conclusions from a network meta-analysis using a partially contextualised framework. *BMJ*. 2020;371:m3907. https://doi.org/10.1136/bmj.m3907

## Sample data (W2I)

> Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment choices for long-term remission of chronic insomnia disorder in adults: a systematic review and network meta-analysis. *Psychiatry Clin Neurosci*. 2024;78(11):646–653. https://doi.org/10.1111/pcn.13730

If you use nmatools in published research, please also cite the underlying
packages directly:

```r
citation("netmeta")
citation("meta")
```

---

Prev: [15. Troubleshooting](15-troubleshooting.md) · Next: [Manual home](README.md)
