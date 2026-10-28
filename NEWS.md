# nmatools 0.3.0 (development)

## Bayesian component NMA (from bcnma)

* The standalone package bcnma (0.2.0, github.com/ykfrkw/bcnma) is merged
  into nmatools and superseded by it. `bcnma_bin()`, `bcnma_cont()`,
  `compare_combinations()`, `plot_interactions()`,
  `summarise_interactions()`, `place()`, `which.place()`,
  `all.interactions()` and the `print()` / `summary()` / `forest()` methods
  for `bcnma_bin` and `bcnma_cont` objects keep their names; the
  methodological review below changed some arguments, fields and results
  (see "Breaking changes / results change").
* Bayesian random-effects component NMA in JAGS for binary (OR) and
  continuous (SMD / MD) outcomes, with additive, SSVS and informative SSVS
  interaction models; optional `component_values` (doses) and
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
* bcnma's testthat suite is included (`tests/testthat/test-bcnma-*.R`); the
  bcnma 0.1.0 fixture now backs numerical equivalence tests (prepared data,
  `compare_combinations()` arithmetic) instead of byte-identical model
  strings. JAGS tests are skipped when rjags is not installed.
* New manual chapter: [17. Bayesian component NMA](docs/manual/17-bayesian-cnma.md).
* Orestis Efthimiou is credited (role `ths`) for conceptual guidance on
  Bayesian cNMA and SSVS modelling.

### Methodological review: new features

* JAGS models are written in NICE DSU TSD2 notation with `inprod()` over
  within-study contrasts; `bcnma_model_code(fit, model)` prints the exact
  additive, SSVS or standard model code (stored in `fit$model_code`).
* Identifiability check before fitting (`fit$identifiability`).
* `inactive` argument for treatment tokens that are not components.
* `eta_prior_sd` exposes the SSVS spike scale; the posterior slab SD is
  printed.
* Convergence summaries (max Rhat / min n.eff per parameter group, with
  warnings) and Monte Carlo SE of inclusion probabilities.
* Residual deviance (`resdev`) in every model; pD and DIC
  (`fit$model_fit`, `fit$arm_deviance`).
* `check_additivity(fit)`: comparison with the standard full-interaction
  NMA.
* `compare_combinations()` reports a predictive interval for a new study and
  flags combinations / component pairs that no arm contains.
* `component_coverage(fit)` and `fit$priors` for reporting; manual sections
  on assumptions and a PRISMA-NMA / CINeMA reporting checklist.
* Spike-and-slab selection of effect modifiers (modifier `selection =
  "ssvs"`, `inclusion_prob`, `slab_sd`, `spike_sd`): one indicator per
  modifier x component, with P(modification present), MCSE and Bayes factor
  in `fit$modifier_selection`.
* `modifier_equivalence(fit, label, margin)`: region-of-practical-equivalence
  summary of an effect modifier (difference between the highest and lowest
  modifier value vs a pre-specified margin, with a verdict).
* Review guide for methodologists: `docs/review/bcnma-review-guide.md`.

### Breaking changes / results change

Results of fits made with bcnma or earlier development versions of
nmatools 0.3.0 will not be reproduced exactly:

* **tau -> tau2.** The log-normal heterogeneity prior is now (correctly)
  placed on the variance: the JAGS node is `tau2` and `tau = sqrt(tau2)`.
  Before, the same prior was put on the SD, so tau was larger a priori.
  Both are monitored and printed. `tau_prior` defaults to `NULL` (outcome
  default) and also accepts list specifications.
* **seed is now reproducible.** `seed` sets the JAGS RNG of every chain
  (`.RNG.seed = seed + chain`); `set.seed()` had no effect on JAGS, so old
  seeded runs will not match (and were not reproducible).
* **MD fits before this version were biased for large-mean outcomes**: the
  study baselines had a fixed N(0, 1000) prior, which shrinks baselines of
  outcomes measured on a large scale (e.g. minutes of sleep). MD means are
  now centred on arm 1 and the d / u / tau priors scale with the median
  pooled SD (tau: half-normal).
* **SMD is now Hedges' g** (small-sample correction J), and a missing or
  non-positive SD (or n) is an error instead of a warning with SD = 1.
* **SSVS renames:** `interactions = "lasso"` / `"lasso_informative"` ->
  `"ssvs"` / `"ssvs_informative"`, `lasso_prob` -> `inclusion_prob` (old
  names are deprecated aliases with a one-time message); fields
  `summary_lasso`, `samps_lasso`, `modifier_effects$lasso` ->
  `summary_ssvs`, `samps_ssvs`, `modifier_effects$ssvs`; `model = "lasso"`
  in `print()` / `forest()` / `compare_combinations()` -> `"ssvs"`.
* **Non-estimable interactions are fixed to 0.** Interactions never
  co-present, with zero within-study contrast, or aliased are left out of the
  SSVS model (previously they were sampled from their prior) and never
  reported as selected; non-identifiable main effects are an error.
* **New modifier defaults:** continuous modifiers are standardised
  internally and `beta ~ N(0, 1)` per covariate SD (ordinal increments
  N(0, 1)); the old default was `prior_sd = 10` on the raw code scale.
  Summary columns changed (`OR*` -> `ratio_of_OR*`, plus per-SD and
  diagnostic columns).
* **`inactive` is required** for treatment tokens that are not components
  (e.g. `"dt"` in the CBT-I example); unknown tokens were silently ignored
  before and are now an error. Single-arm studies are an error.
* Residual deviance and DIC add `resdev` to the samples and one more pass of
  `n.iter` iterations (with >= 2 chains).

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
