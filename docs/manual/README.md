# nmatools user manual

**nmatools** is an R package that wraps the [`netmeta`](https://cran.r-project.org/package=netmeta)
frequentist network meta-analysis engine into a one-stop workflow. It gives you
a scripted NMA pipeline that fits a network and writes a complete set of
results — model, consistency tests, league table, and publication-ready plots —
to disk with a single call; a suite of publication visualizations (colored
league tables, forest and network graphs, Kilim and Vitruvian plots, and
minimally / partially contextualized evidence tables); and an interactive
CINeMA + ROB-MEN Shiny GUI for rating confidence in the results.

## How this manual is organized

The manual is 16 chapters in six groups. Read straight through, or jump to the
chapter you need.

### Getting started

| Chapter | What it covers |
|---|---|
| [1. Installation and quick start](01-installation.md) | Requirements, installing from GitHub, and a five-minute NMA on the bundled sample data. |
| [2. Data formats](02-data-formats.md) | Arm-based and pairwise input formats, the `w2i_trials` column dictionary, data-access helpers, the CINeMA CSV format, and project scaffolding. |

### Scripted pipeline

| Chapter | What it covers |
|---|---|
| [3. The NMA pipeline](03-nma-pipeline.md) | Every `netmetawrap()` argument, the complete output-file table, override hooks, and subnetwork handling. |
| [4. Batch runs and rare events](04-batch-and-rare-events.md) | Running many outcomes with `run_nma_batch()` and the automatic Mantel-Haenszel rare-event workflow. |
| [5. Transitivity](05-transitivity.md) | Visual assessment of the transitivity assumption with `plot_transitivity()`. |

### GUI guide

| Chapter | What it covers |
|---|---|
| [6. GUI overview](06-gui-overview.md) | Launch modes, the navigation bar, and the intended CINeMA + ROB-MEN workflow. |
| [7. Configuration tab](07-gui-configuration.md) | Loading data, format detection, NMA settings, ROB-MEN MCMC settings, and running the analysis. |
| [8. CINeMA domains](08-gui-cinema-domains.md) | The auto-computed and manually overridable domain tabs ①③④⑤⑥. |
| [9. ROB-MEN](09-gui-robmen.md) | Domain ② and the embedded ROB-MEN assessment of risk of bias due to missing evidence. |
| [10. Report and export](10-gui-report-export.md) | The summary grid, figures, and the ZIP bundle export that bridges back to the scripted functions. |

### Visualizations

| Chapter | What it covers |
|---|---|
| [11. Colored league tables](11-league-tables.md) | `color_league()` and `color_league_multi()`: palettes, sorting, and multi-outcome layouts. |
| [12. Colored forest and network graphs](12-colored-forest-netgraph.md) | `color_forest()`, `color_netgraph()`, and the palette helpers. |
| [13. Kilim and Vitruvian plots](13-kilim-vitruvian.md) | `kilim()` and `vitruvian()`: multi-outcome signed-p-value and absolute-effect visualizations. |

### Evidence frameworks

| Chapter | What it covers |
|---|---|
| [14. Evidence frameworks](14-evidence-frameworks.md) | `min_context()` and `part_context()`: minimally and partially contextualized treatment classification. |

### Reference

| Chapter | What it covers |
|---|---|
| [15. Troubleshooting and FAQ](15-troubleshooting.md) | Common problems by symptom, with cause and fix. |
| [16. Function reference](16-function-reference.md) | Every exported function in one grouped table, plus the full literature reference list. |

## Suggested reading paths

- **Run a full NMA from a CSV.** Install and try the quick start, learn the
  input format, then drive the pipeline:
  [1](01-installation.md) → [2](02-data-formats.md) → [3](03-nma-pipeline.md)
  (add [4](04-batch-and-rare-events.md) for many outcomes at once).
- **Rate confidence with CINeMA / ROB-MEN.** Work through the GUI end to end:
  [6](06-gui-overview.md) → [7](07-gui-configuration.md) →
  [8](08-gui-cinema-domains.md) → [9](09-gui-robmen.md) →
  [10](10-gui-report-export.md).
- **Build publication figures.** Turn fitted networks into colored tables and
  plots, then summarize across outcomes:
  [11](11-league-tables.md) → [12](12-colored-forest-netgraph.md) →
  [13](13-kilim-vitruvian.md) → [14](14-evidence-frameworks.md).

---

Next: [1. Installation and quick start](01-installation.md)
