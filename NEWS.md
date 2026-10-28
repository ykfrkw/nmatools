# nmatools 0.3.0 (development)

## Bayesian component NMA (from bcnma)

* The standalone package bcnma (0.2.0, github.com/ykfrkw/bcnma) is merged
  into nmatools and superseded by it. `bcnma_bin()`, `bcnma_cont()`,
  `compare_combinations()`, `plot_interactions()`,
  `summarise_interactions()`, `place()`, `which.place()`,
  `all.interactions()` and the `print()` / `summary()` / `forest()` methods
  for `bcnma_bin` and `bcnma_cont` objects keep their names, arguments and
  results, so existing code works after replacing `library(bcnma)` with
  `library(nmatools)`.
* Bayesian random-effects component NMA in JAGS for binary (OR) and
  continuous (SMD / MD) outcomes, with additive, SSVS-LASSO and informative
  SSVS-LASSO interaction models; optional `component_values` (doses) and
  arm-level effect `modifiers` (continuous or ordinal, optionally monotone).
  `d[k]` of a modified component is its effect at the center / reference
  level; modifier effects are between-study (ecological) associations.
* `forest()` is now `meta::forest()`, re-exported by nmatools; the
  `bcnma_bin` / `bcnma_cont` methods are registered on it (bcnma defined its
  own `forest()` generic).
* rjags and MCMCvis (and the optional ggrepel) are Suggests, not Imports:
  nmatools installs and loads without JAGS. `bcnma_bin()` and `bcnma_cont()`
  stop with an informative error when rjags or MCMCvis is missing.
* The CBT-I component NMA example data and worked script are installed under
  `system.file("extdata", "bcnma", package = "nmatools")`.
* bcnma's testthat suite is included (`tests/testthat/test-bcnma-*.R`),
  including the backward-compatibility fixture built from bcnma 0.1.0; JAGS
  tests are skipped when rjags is not installed.
* New manual chapter: [17. Bayesian component NMA](docs/manual/17-bayesian-cnma.md).
* Orestis Efthimiou is credited (role `ths`) for conceptual guidance on
  Bayesian cNMA and SSVS modelling.

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
