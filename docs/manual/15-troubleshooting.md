[Manual home](README.md) › Troubleshooting

# 15. Troubleshooting and FAQ

This chapter collects the problems users hit most often, organized by symptom.
Each entry states what you see, why it happens, and how to fix it, with a
pointer to the chapter that covers the topic in full. If your problem is not
here, see [Getting help](#158-getting-help) at the end.

---

## 15.1 Arguments and deprecations

### `fixed =` is deprecated — use `common =`

> **Symptom.** A call warns that `fixed` is deprecated, or your common-effect
> override is silently ignored.
>
> **Cause.** Recent `netmeta` renamed the common-effect (formerly
> "fixed-effect") switch from `fixed =` to `common =`. nmatools follows the new
> name throughout.
>
> **Fix.** Use `common =` everywhere:
>
> - In pipeline overrides, pass it through `netmeta_args`, e.g.
>   `netmeta_args = list(common = TRUE)` (see [Chapter 3](03-nma-pipeline.md)).
> - In the evidence-framework functions that expose it directly —
>   `min_context()`, `part_context()` — set the `common =` argument (default
>   `FALSE`, i.e. random-effects; see [Chapter 14](14-evidence-frameworks.md)).
>
> A recent `netmeta` (≥ 3.x) is required precisely because the pipeline relies
> on the modern `common =` / `random =` names; see [Chapter 1](01-installation.md).

### `run_nma_batch()` rejects unquoted column names

> **Symptom.** A batch run fails with an error such as `object 'id' not found`,
> while the identical mapping works in a direct `netmetawrap()` call.
>
> **Cause.** `netmetawrap()` accepts column-role arguments either **unquoted**
> (non-standard evaluation, `studlab = id`) or as strings. `run_nma_batch()`
> forwards each `params_list` element through `do.call()`, which evaluates the
> list entries as ordinary values — an unquoted `id` is looked up as a variable
> and is not found.
>
> **Fix.** In `run_nma_batch()` (and therefore inside `.default_args` and every
> `params_list` element) quote every column-role name: `studlab = "id"`,
> `treat = "t"`, `n = "n"`, `event = "r"`, `mean_col = "..."`, `sd_col = "..."`.
> See [Chapter 3](03-nma-pipeline.md) for the NSE rules and
> [Chapter 4](04-batch-and-rare-events.md) for the batch interface.

### `trivial_range` colors look wrong

> **Symptom.** With the Schneider-Thoma 2026 palette, every cell (or spoke) is
> mis-colored — nothing lands in the trivial zone, or everything does.
>
> **Cause.** `trivial_range` must be on the **same scale as the effect
> estimates**, and that scale differs by summary measure. For ratio measures
> (OR, RR, HR) `netmeta` works on the **log** scale; for mean differences
> (MD, SMD) the estimates are already on the **raw** scale. Passing a raw ratio
> range such as `c(0.91, 1.10)` to an OR outcome mis-colors every cell.
>
> **Fix.**
>
> - **OR / RR / HR:** pass the log of the ratio bounds, e.g.
>   `trivial_range = log(c(1/1.1, 1.1))` for odds ratios of roughly 0.91–1.10.
> - **MD / SMD:** pass the raw range directly, e.g. `trivial_range = c(-0.2, 0.2)`.
>
> This applies to `color_league()` / `color_league_multi()`
> ([Chapter 11](11-league-tables.md)) and `kilim()` / `vitruvian()`
> ([Chapter 13](13-kilim-vitruvian.md)). A `trivial_range` may be set once at
> the top level or overridden per outcome. The scheme follows Schneider-Thoma
> et al. 2026.

---

## 15.2 Network graphs and plotting

### `color_netgraph()` / `netgraph()` errors on `xlim`

> **Symptom.** Passing `xlim` (or `ylim`) to `color_netgraph()` raises
> `formal argument "xlim" matched by multiple actual arguments`, or clips nodes
> and labels off the edge of the device.
>
> **Cause.** `netgraph()` sets the plot limits internally to fit the node layout
> it computes. Because `color_netgraph()` forwards `...` to `netmeta::netgraph()`,
> an extra `xlim` collides with the value the wrapper already supplies.
>
> **Fix.** Do not pass `xlim` / `ylim`. To give long treatment labels more room,
> zoom out with `netgraph()`'s own `scale =` argument instead:
>
> ```r
> color_netgraph(x = net_lt, cinema = cinema_path, scale = 1.3)
> ```
>
> See [Chapter 12](12-colored-forest-netgraph.md).

---

## 15.3 Rare events and auto-switching

### The analysis switched methods unexpectedly

> **Symptom.** A binary outcome quietly produced a Mantel-Haenszel
> common-effect model and a `rare_diagnostics_*.txt` file plus a four-method
> sensitivity panel you did not ask for — or, conversely, you expected the
> rare-event workflow and got the standard inverse-variance analysis.
>
> **Cause.** Under the default `rare_events = "auto"`, nmatools runs rare-event
> diagnostics and, if the network is flagged sparse (`rare_flow = TRUE`),
> switches the primary analysis to a common-effect Mantel-Haenszel model with
> no continuity correction (Efthimiou et al. 2019). The flag is raised by a low
> overall or per-treatment event rate (below 1%), a treatment with zero total
> events, a high fraction of zero-arm studies combined with a low event rate,
> or very few studies with events in all arms.
>
> **Fix.** Control the switch explicitly with the `rare_events` argument of
> `netmetawrap()` (also valid inside any `params_list` element):
>
> - `rare_events = "always"` — force the rare-event workflow regardless of the flag.
> - `rare_events = "never"` — disable the diagnostics and sensitivity panel and
>   keep the standard inverse-variance random-effects analysis.
> - `rare_events = "auto"` (default) — decide from the diagnostics.
>
> Read `rare_diagnostics_{outcome}.txt` to see which flags fired. For continuous
> outcomes the argument is ignored (with a warning if you set `"always"`). See
> [Chapter 4](04-batch-and-rare-events.md).

---

## 15.4 Networks and output

### Disconnected network — where did my results go?

> **Symptom.** The top-level call returned `NULL` and the usual result files are
> not in `outputs/{outcome}/`; instead there are nested `{outcome}_subnet_1/`,
> `{outcome}_subnet_2/` folders.
>
> **Cause.** Before fitting, `netmetawrap()` calls `netmeta::netconnection()`.
> When it detects more than one subnetwork it analyzes each separately, writes a
> full result set into a per-subnetwork sub-directory, and returns `NULL`
> invisibly — there is no single network-wide model to return.
>
> **Fix.** This is expected behavior, not an error. Look inside each
> `{outcome}_subnet_N/` folder for that subnetwork's outputs. Within a
> subnetwork the requested `reference.group` is honored if present, otherwise the
> largest-N treatment in that subnetwork is used. See
> [Chapter 3](03-nma-pipeline.md).

### Where do output files go?

> **Symptom.** You cannot find the files a run wrote.
>
> **Cause.** Output paths are relative to the R working directory. The `path`
> argument defaults to `"./outputs"`, and each outcome is written to
> `{path}/{outcome}/`.
>
> **Fix.** Check your working directory with `getwd()`, or pass an explicit
> `path =`. Scaffolding a project with `create_nma_project()` gives you a
> ready-made `outputs/` folder (see [Chapter 2](02-data-formats.md)). The full
> per-outcome file list is in [Chapter 3](03-nma-pipeline.md).

---

## 15.5 Transitivity plots

### A covariate is missing from the transitivity plot

> **Symptom.** `plot_transitivity()` produced no plot for one of your
> `covariate_cols`, or reported that a column was skipped.
>
> **Cause.** The function plots **numeric columns only**; non-numeric columns
> are skipped with a message. Letter-coded or categorical variables (for
> example a risk-of-bias column stored as `"L"` / `"M"` / `"H"`) are non-numeric.
>
> **Fix.** Convert the covariate to numeric first, then pass the numeric column:
>
> ```r
> d$rob_num    <- c("L" = 0, "M" = 1, "H" = 2)[d$rob]                    # ordered severity
> d$design_num <- c("RCT" = 0, "quasi-RCT" = 1, "observational" = 2)[d$design]
> ```
>
> See [Chapter 5](05-transitivity.md).

### The transitivity y-axis is on a strange scale

> **Symptom.** A proportion covariate plots on a 45–75 axis instead of 0.45–0.75.
>
> **Cause.** nmatools expects proportions on the **0–1 scale**, not as
> percentages. Raw percentages still plot with correct relative differences, but
> the y-axis is misleading.
>
> **Fix.** Divide percentage columns by 100 before plotting:
> `d$female_prop <- d$female_pct / 100`. See [Chapter 5](05-transitivity.md).

---

## 15.6 The `cinema()` GUI

### Garbled labels / mojibake in the GUI

> **Symptom.** CINeMA or ROB-MEN judgement labels render as garbled characters
> (mojibake), or differ between machines.
>
> **Cause.** A non-UTF-8 locale. On startup `cinema()` forces the
> `en_US.UTF-8` locale so judgement labels render identically on every platform,
> but a locale that cannot be set will surface as mangled text.
>
> **Fix.** Ensure a UTF-8 locale is available before launching. In R:
>
> ```r
> Sys.setlocale("LC_ALL", "en_US.UTF-8")
> ```
>
> Or set the environment before starting R (macOS/Linux):
>
> ```sh
> export LANG=en_US.UTF-8
> ```
>
> See [Chapter 6](06-gui-overview.md).

### The Run button never appears / a red error banner

> **Symptom.** Section 5 (Run Analysis) stays a placeholder, or you see
> "Please map all required columns (\*)…" or "Missing columns: …".
>
> **Cause.** The data have not passed validation. The Run section is hidden
> until every required field is mapped and the outcome type matches the columns
> present. Uploaded files (unlike the bundled demo data) may need the Column
> Mapping and ROB / Indirectness Value Mapping panels.
>
> **Fix.** Resolve the error/warning banner first: open the Column Mapping panel
> and set every field marked `*`, and correct the **Outcome type** and
> **Structural format** radios in section 2 if auto-detection was wrong. The demo
> data ship standardized, so those panels do not appear for them; your own file
> may. The Configuration-tab troubleshooting table in
> [Chapter 7](07-gui-configuration.md) lists each banner and its fix.

### Demo data works but my own data does not

> **Symptom.** **Load SLEEPI demo data** runs fine, but your uploaded file
> errors on validation.
>
> **Cause.** The demo dataset ships with standardized `rob` / `indirectness`
> columns and the exact columns each format needs. Your own file may use
> non-standard value labels or be missing a required column for the chosen
> format (binary needs `studlab` / `treat` / `n` / `event`; continuous needs
> `studlab` / `treat` / `n` / `mean` / `sd`; pairwise needs
> `studlab` / `t1` / `t2` / `y` / `se`).
>
> **Fix.** Use the Column Mapping panel to bind your columns to the required
> fields, and the ROB / Indirectness Value Mapping panel to map raw labels to
> *Low risk* / *Some concerns* / *High risk* / *(Exclude row)*. If a ROB or
> indirectness column is absent entirely, every row defaults to `low`. See
> [Chapters 6–7](06-gui-overview.md) for launch modes and required columns, and
> [Chapter 8](08-gui-cinema-domains.md) onward for the domain tabs.

### The analysis is slow

> **Symptom.** Clicking **▶ Run CINeMA + ROB-MEN Analysis** takes a long time.
>
> **Cause.** ROB-MEN runs a Bayesian Egger test via JAGS MCMC. A typical run
> completes in about 20–40 seconds, but large networks or high iteration counts
> take longer.
>
> **Fix.** In the **ROB-MEN Bayesian Settings** block, lower the **Iterations**
> (default 10000) and/or **Burn-in** (default 1000) for a faster exploratory
> pass, then raise them again for the final assessment. The regression-slope
> priors are fixed and not user-editable. See
> [Chapter 7](07-gui-configuration.md); the export and report options are covered
> in [Chapter 10](10-gui-report-export.md).

---

## 15.7 Installation

> **Symptom.** The package will not load, or PDF trimming fails.
>
> **Cause and fix.**
>
> - **R too old.** nmatools uses the native pipe (`|>`) and requires **R ≥ 4.1**.
> - **`netmeta` too old.** A recent `netmeta` (≥ 3.x) is required for the
>   `common =` / `random =` argument names.
> - **`magick` / ImageMagick.** Output PDFs are trimmed of surrounding
>   whitespace via `magick`, which depends on the ImageMagick system library. On
>   some Linux distributions install the system headers first (for example
>   `libmagick++-dev` on Debian/Ubuntu). If `magick` is unavailable, disable
>   trimming per call with `trim = FALSE`.
>
> See [Chapter 1](01-installation.md) for full installation requirements.

---

## 15.8 Getting help

- **Function help.** Every exported function has a help page. Type `?netmetawrap`
  (or any function name) at the R console for its full argument list and
  examples. [Chapter 16](16-function-reference.md) lists every exported function
  with its help topic and manual chapter.
- **Package index.** `help(package = "nmatools")` opens the index of all help
  pages and package metadata.
- **Report a bug.** Open an issue at the project's GitHub repository
  (<https://github.com/ykfrkw/nmatools/issues>). Include your R version
  (`R.version.string`), the nmatools version
  (`packageVersion("nmatools")`), and a minimal reproducible example.

---

Prev: [14. Evidence frameworks](14-evidence-frameworks.md) · Next: [16. Function reference](16-function-reference.md)
