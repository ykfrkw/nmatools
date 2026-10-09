# nmatools 0.2.2 (development)

## Bug fixes: figure sizing and trimming

* `forest_netsplit_*` lost every comparison except the last few. With
  netmeta >= 3.x the comparisons live in `netsplit$comparison` (singular);
  the old code read `$comparisons` (NULL), sized the page for zero
  comparisons and meta's centred layout was cut off at the top. Forest plots
  (reference, netpairwise, netsplit) are now drawn once on a scratch device,
  their real extent is measured, and the final device is sized from that, so
  no row or column is clipped whatever the label lengths or column set.
* Tall forest plots (netsplit and netpairwise) are split into A4 pages at
  blank rows, so a page break never slices through a text line or row.
  `forest_netpairwise_*` is therefore paged (`_p1`, `_p2`, ...) when it does
  not fit on one A4 page.
* Trimming no longer leaves zero margin or shaves anti-aliased glyph edges
  (the clipped "95%-CI" header). Every trimmed figure, and every page of
  paged figures, gets a uniform white border set by the new argument
  `trim_margin` (inches, default `0.2`) in `netmetawrap()`,
  `run_nma_batch()` and `plot_transitivity()`. The contributions heatmap uses
  it as its ggplot2 plot margin.
* `netgraph_*` reserves margin space for long treatment labels (netgraph
  clips labels at the figure region).

## Changes

* The between-study variance (tau^2) estimator now defaults to REML
  throughout the package, matching the CINeMA GUI. netmeta's own default is
  DerSimonian-Laird (DL).
  - `netmetawrap()` / `run_nma_batch()`: continuous outcomes and binary
    outcomes fitted with `netmetabin(method = "Inverse")` use
    `method.tau = "REML"`. Because `netmetabin()` has no `method.tau`
    argument, the estimator is applied through a temporary
    `meta::settings.meta(method.tau.netmeta = )` that is restored afterwards
    (also on error). Mantel-Haenszel and non-central hypergeometric models
    estimate no tau and are unaffected.
  - Override with `netmeta_args = list(method.tau = "DL")` (or any other
    estimator supported by netmeta).
  - `build_w2i_netmeta()` and the pairwise random-effects meta-analyses used
    for comparison-adjusted funnel plots now request REML explicitly, so
    results no longer depend on a user's global `settings.meta()`.

* Deliberate exceptions:
  - The rare-event sensitivity panel's `IV_RE_CC` comparator stays DL. It is
    a pre-specified comparator mirroring pmatools' `REIV_CC`; DL is now pinned
    explicitly instead of inherited from the global setting, and its label
    says so.
  - The baseline-risk GLMM in the Vitruvian plot (`meta::metaprop(method =
    "GLMM")`) keeps ML, because `metafor::rma.glmm()` supports only ML.
  - The rare-event sensitivity forest plot builds display-only `metagen()`
    containers (no pooling), so tau is never estimated or shown there.
