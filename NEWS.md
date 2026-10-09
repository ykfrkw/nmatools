# nmatools 0.2.2 (development)

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
