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
| `run_nma_batch()` | Run the `netmetawrap()` pipeline across many outcomes, with the shared arguments written once. | [Ch. 4](04-batch-and-rare-events.md) | `?run_nma_batch` |
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

## Bayesian component NMA

Bayesian random-effects component NMA in JAGS (formerly the `bcnma` package).
Fitting needs JAGS and the suggested packages rjags and MCMCvis; see
[Ch. 17](17-bayesian-cnma.md).

| Function | Purpose | Chapter | Help |
|---|---|---|---|
| `bcnma_bin()` | Bayesian cNMA for binary outcomes (OR): additive, SSVS-LASSO, or informative SSVS-LASSO interactions, optional component values and effect modifiers. | [Ch. 17](17-bayesian-cnma.md) | `?bcnma_bin` |
| `bcnma_cont()` | Bayesian cNMA for continuous outcomes (SMD or MD), same models and options. | [Ch. 17](17-bayesian-cnma.md) | `?bcnma_cont` |
| `compare_combinations()` | Posterior effect of one component combination vs. another (main effects, interactions, modifiers at `at`). | [Ch. 17](17-bayesian-cnma.md) | `?compare_combinations` |
| `forest()` | Re-exported `meta::forest()`; the `bcnma_bin` / `bcnma_cont` methods draw a component forest plot (ggplot2). | [Ch. 17](17-bayesian-cnma.md) | `?forest.bcnma_bin` |
| `plot_interactions()` | Median interaction coefficient vs. posterior inclusion probability for all component pairs. | [Ch. 17](17-bayesian-cnma.md) | `?plot_interactions` |
| `summarise_interactions()` | Data frame of all pairwise interactions sorted by \|gamma\|. | [Ch. 17](17-bayesian-cnma.md) | `?summarise_interactions` |
| `place()` | Interaction index of a component pair. | [Ch. 17](17-bayesian-cnma.md) | `?place` |
| `which.place()` | Component pair of an interaction index. | [Ch. 17](17-bayesian-cnma.md) | `?which.place` |
| `all.interactions()` | All interaction indices within a component combination. | [Ch. 17](17-bayesian-cnma.md) | `?all.interactions` |

The fitted objects also have exported `print()` and `summary()` methods
(`?print.bcnma_bin`, `?print.bcnma_cont`); `?bcnma` gives an overview.

All 30 exported functions above are listed in `NAMESPACE`. For a live index in
your session, run `help(package = "nmatools")`.

---

# Literature references

## netmeta (core NMA engine)

> Balduzzi S, Rücker G, Nikolakopoulou A, Papakonstantinou T, Salanti G, Efthimiou O, Schwarzer G (2023). netmeta: An R Package for Network Meta-Analysis Using Frequentist Methods. Journal of Statistical Software, 106(2), 1–40. doi:10.18637/jss.v106.i02.

## meta (pairwise meta-analysis and forest plots)

> Balduzzi S, Rücker G, Schwarzer G (2019). How to perform a meta-analysis with R: a practical tutorial. Evidence-Based Mental Health, 22, 153–160. DOI: 10.1136/ebmental-2019-300117

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

## Bayesian component NMA

> Furukawa Y, Sakata M, Yamamoto R, et al. Components and delivery formats of cognitive behavioral therapy for chronic insomnia in adults: a systematic review and component network meta-analysis. *JAMA Psychiatry*. 2024;81(4):357–365. https://doi.org/10.1001/jamapsychiatry.2023.5060

- **Heterogeneity prior:** Turner RM, Jackson D, Wei Y, Thompson SG, Higgins JPT. Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis. *Stat Med*. 2015;34(6):984–998.
- **Spike-and-slab variable selection (SSVS):** George EI, McCulloch RE. Variable selection via Gibbs sampling. *J Am Stat Assoc*. 1993;88(423):881–889.

## Sample data (W2I)

> Furukawa Y, Sakata M, Furukawa TA, Efthimiou O, Perlis M. Initial treatment choices for long-term remission of chronic insomnia disorder in adults: a systematic review and network meta-analysis. *Psychiatry Clin Neurosci*. 2024;78(11):646–653. https://doi.org/10.1111/pcn.13730

If you use nmatools in published research, please also cite the underlying
packages directly:

```r
citation("netmeta")
citation("meta")
```

---

Prev: [15. Troubleshooting](15-troubleshooting.md) · Next: [17. Bayesian component NMA](17-bayesian-cnma.md)
