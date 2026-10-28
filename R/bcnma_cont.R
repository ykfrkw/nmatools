# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11 (rewritten 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: bcnma_cont(), the user-facing fit of a Bayesian random-effects
#          component NMA for continuous outcomes (normal arm-level
#          likelihood with known standard errors, SMD or MD scale) with
#          additive or SSVS interaction models.
# Inputs:  arm-level data.frame (one row per arm) and fitting options
# Outputs: object of class "bcnma_cont"
# Depends: R/bcnma_args.R, R/bcnma_prepare_cont.R, R/bcnma_priors.R,
#          R/bcnma_identifiability.R, R/bcnma_modifiers.R, R/bcnma_fit.R;
#          rjags + MCMCvis (Suggests)

#' Bayesian component network meta-analysis for continuous outcomes
#'
#' Fits a Bayesian random-effects component NMA (cNMA) for continuous outcomes
#' with a normal arm-level likelihood (arm standard errors treated as known)
#' in JAGS. Component effects are on the standardised mean difference (SMD,
#' Hedges' g) or raw mean difference (MD) scale. Interaction models,
#' identifiability checks and outputs are as in [bcnma_bin()].
#' Requires the JAGS system library and the suggested packages rjags and
#' MCMCvis; an informative error is raised when they are missing.
#'
#' @inheritParams bcnma_bin
#' @param mean                unquoted column name for arm mean
#' @param sd                  unquoted column name for arm standard deviation
#' @param sm                  character: `"SMD"` (default) or `"MD"`. With
#'   `"MD"`, each study's means are centred on its arm 1 (an exact location
#'   shift absorbed by the study baseline) and the default priors scale with
#'   the median within-study pooled SD s: `d[k] ~ N(0, (10 s)^2)` and a
#'   half-normal prior on tau with SD s.
#' @param pooled_sd           character: how to compute the within-study SD for
#'   standardisation when `sm = "SMD"`. `"within"` (default) pools all arms
#'   (df-weighted); `"control"` uses the SD of arm 1, i.e. the first row of the
#'   study in `data` (the data order defines the comparator). Arm means and
#'   SEs are divided by this SD and multiplied by Hedges' correction
#'   \eqn{J = 1 - 3/(4 df - 1)}, so contrasts are Hedges' g; the pooled SD is
#'   treated as known. Every arm needs a positive SD and n (otherwise an
#'   error). Mixing change scores and final values on the SMD scale is not
#'   handled and is left to the analyst.
#' @param eta_prior_sd        numeric: SD of the half-normal prior on the
#'   spike SD `eta` (default `1/sqrt(1000)`); see [bcnma_bin()]. The implied
#'   slab SD (median about 0.21, 97.5th percentile about 0.71 at the
#'   defaults) is on the SMD scale, or on the MD scale for `sm = "MD"`, where
#'   it should be rescaled to the outcome.
#' @param tau_prior           heterogeneity prior. `NULL` (default) uses the
#'   outcome default: for `sm = "SMD"` a log-normal prior `c(meanlog = -1.0,
#'   precision = 1/0.8^2)` on the variance tau^2 (a package default, not from
#'   a published category; prefer an outcome-specific published prior, e.g.
#'   Rhodes et al. 2015); for `sm = "MD"` a half-normal prior on the SD tau
#'   whose SD equals the median within-study pooled SD. Otherwise numeric
#'   `c(meanlog, precision)` (log-normal on tau^2), `list(family =
#'   "lognormal_tau2", meanlog, precision)` or `list(family =
#'   "half_normal_tau", sd)`. The prior used is stored in
#'   `fit$model_settings$tau_prior`.
#' @param component_values    optional numeric matrix or data.frame of
#'   component values (e.g. doses); `d[k]` becomes the SMD (or MD) per unit of
#'   component k. See [bcnma_bin()] for the full specification.
#' @param modifiers           optional list of arm-level effect-modifier
#'   specifications, exactly as in [bcnma_bin()]; `beta` is then the change in
#'   the component SMD (or MD) per code unit.
#'
#' @return object of class `bcnma_cont` with the same elements as
#'   [bcnma_bin()] except that `sm` is `"SMD"` or `"MD"` (no `ORd` columns),
#'   plus `pooled_sd` and `sigma_pooled` (per-study pooled SDs).
#'
#' @examples
#' \dontrun{
#' library(nmatools)
#'
#' # Example with continuous outcome (ISI score change from baseline)
#' fit <- bcnma_cont(
#'   data         = dat,
#'   studlab      = study,
#'   treat        = treatment_component,
#'   mean         = mean_change,
#'   sd           = sd_change,
#'   n            = n,
#'   components   = c("se", "sd", "cr", "th", "sr", "sc"),
#'   sm           = "SMD",
#'   interactions = "ssvs",
#'   seed         = 42
#' )
#' print(fit)
#' forest(fit)
#' compare_combinations(fit,
#'   combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
#'   combo2 = c("se"))
#' }
#'
#' @seealso [bcnma_bin()] for binary outcomes
#' @export
bcnma_cont <- function(data,
                       studlab,
                       treat,
                       mean,
                       sd,
                       n,
                       components,
                       component_separator = "+",
                       inactive            = NULL,
                       reference.group     = NULL,
                       sm                  = c("SMD", "MD"),
                       pooled_sd           = c("within", "control"),
                       interactions        = c("ssvs", "none", "ssvs_informative",
                                               "lasso", "lasso_informative"),
                       active_components   = NULL,
                       inclusion_prob      = BCNMA_DEFAULT_INCLUSION_PROB,
                       g                   = BCNMA_DEFAULT_SLAB_RATIO,
                       eta_prior_sd        = BCNMA_DEFAULT_ETA_PRIOR_SD,
                       tau_prior           = NULL,
                       n.chains            = 3,
                       n.adapt             = 2000,
                       n.burnin            = 5000,
                       n.iter              = 20000,
                       n.thin              = 1,
                       seed                = NULL,
                       quiet               = FALSE,
                       component_values    = NULL,
                       modifiers           = NULL,
                       lasso_prob          = NULL) {
  .bcnma_require_mcmc_packages("bcnma_cont")
  call         <- match.call()
  sm           <- match.arg(sm)
  pooled_sd    <- match.arg(pooled_sd)
  interactions <- .normalise_interactions(match.arg(interactions))
  studlab_col  <- .resolve_column(deparse(substitute(studlab)), data)
  treat_col    <- .resolve_column(deparse(substitute(treat)), data)
  mean_col     <- .resolve_column(deparse(substitute(mean)), data)
  sd_col       <- .resolve_column(deparse(substitute(sd)), data)
  n_col        <- .resolve_column(deparse(substitute(n)), data)
  .validate_components(components)
  ssvs_pairs <- .candidate_interaction_pairs(interactions, active_components, components)
  component_values <- .validate_component_values(component_values, nrow(data), components)
  modifier_specs   <- .normalise_modifier_specs(modifiers, data, components)

  if (!quiet) message("Preparing data...")
  prep <- .bcnma_cont_prepare_data(data, studlab_col, treat_col, mean_col, sd_col, n_col,
                                   components, component_separator, sm, pooled_sd,
                                   as.character(inactive))
  prep <- .apply_component_values(prep, component_values)
  identifiability <- .check_identifiability(prep, ssvs_pairs, interactions)

  options <- list(
    interactions      = interactions,
    active_components = if (interactions == "ssvs_informative") active_components,
    ssvs_pairs        = identifiability$ssvs_pairs,
    identifiability   = identifiability,
    inclusion_prob    = .resolve_inclusion_prob(inclusion_prob, lasso_prob),
    slab_ratio        = g,
    eta_prior_sd      = eta_prior_sd,
    tau_prior         = .tau_prior_spec(tau_prior, .default_tau_prior_cont(prep)),
    effect_prior_sd   = .location_prior_sd_cont(prep, "effect"),
    baseline_prior_sd = .location_prior_sd_cont(prep, "baseline"),
    component_values  = component_values,
    modifier_specs    = modifier_specs,
    modifier_design   = .build_modifier_design(modifier_specs, data, prep),
    reference_group   = reference.group,
    mcmc              = .mcmc_settings(n.chains, n.adapt, n.burnin, n.iter, n.thin, seed),
    quiet             = quiet
  )
  fitted <- .fit_bcnma_models(prep, options)
  if (!quiet) message("\nDone.")
  fit <- .assemble_bcnma_fit(call, prep, options, fitted, sm = sm)
  fit$pooled_sd    <- pooled_sd
  fit$sigma_pooled <- prep$sigma_pooled
  structure(fit, class = "bcnma_cont")
}
