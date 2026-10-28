# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11
#
# File description:
# Main bcnma_cont() function — Bayesian component NMA for continuous outcomes
# (SMD or MD) with LASSO/SSVS interactions.

#' Bayesian Component Network Meta-Analysis for Continuous Outcomes
#'
#' Fits a Bayesian random-effects component NMA (cNMA) for continuous outcomes
#' using a normal arm-level likelihood in JAGS. Component effects are estimated
#' on the standardised mean difference (SMD) or raw mean difference (MD) scale.
#' Supports the same three interaction models as \code{\link{bcnma_bin}}.
#' Requires the JAGS system library and the suggested packages rjags and
#' MCMCvis; an informative error is raised when they are missing.
#'
#' @param data                data.frame with one row per arm per study
#' @param studlab             unquoted column name for study identifier
#' @param treat               unquoted column name for treatment/component combination string
#' @param mean                unquoted column name for arm mean
#' @param sd                  unquoted column name for arm standard deviation
#' @param n                   unquoted column name for arm sample size (integer)
#' @param components          character vector of component abbreviations
#' @param component_separator character separating components in \code{treat} (default \code{"+"})
#' @param reference.group     character: reference component for display (optional)
#' @param sm                  character: \code{"SMD"} (default) or \code{"MD"}
#' @param pooled_sd           character: how to compute the within-study SD for
#'   standardisation when \code{sm = "SMD"}. \code{"within"} (default) pools all
#'   arms; \code{"control"} uses the reference arm SD only.
#' @param interactions        character: \code{"none"}, \code{"lasso"} (default),
#'   or \code{"lasso_informative"}
#' @param active_components   character vector of components whose pairwise
#'   interactions are modelled (only used when \code{interactions = "lasso_informative"})
#' @param lasso_prob          numeric: prior inclusion probability (default 0.5)
#' @param g                   numeric: spike-slab variance ratio (default 100)
#' @param tau_prior           numeric \code{c(meanlog, precision)}: log-normal
#'   prior on the between-study variance tau^2 (JAGS node \code{tau2};
#'   \code{tau = sqrt(tau2)} is the SD). Default \code{c(-1.0, 1/0.8^2)}.
#' @param n.chains            integer: MCMC chains (default 3)
#' @param n.adapt             integer: JAGS adaptation iterations (default 2000)
#' @param n.burnin            integer: burn-in iterations (default 5000)
#' @param n.iter              integer: sampling iterations per chain (default 20000)
#' @param n.thin              integer: thinning interval (default 1)
#' @param seed                integer or NULL: seed of the JAGS random number
#'   generators (see \code{\link{bcnma_bin}}); stored in \code{fit$seed}.
#' @param quiet               logical: suppress progress messages (default FALSE)
#' @param component_values    optional numeric matrix or data.frame of component
#'   values (e.g. doses); \code{d[k]} becomes the SMD (or MD) per unit of
#'   component k. See \code{\link{bcnma_bin}} for the full specification.
#'   Default \code{NULL} keeps the 0/1 indicators parsed from \code{treat}.
#' @param modifiers           optional list of arm-level effect-modifier
#'   specifications, exactly as in \code{\link{bcnma_bin}}; \code{beta} is then
#'   the change in the component SMD (or MD) per code unit and \code{d[k]} of a
#'   modified component is its effect at the center / reference level.
#'   Modifier effects are identified mainly by between-study variation and are
#'   observational (ecological) associations. Default \code{NULL}: no modifiers.
#'
#' @return object of class \code{bcnma_cont} (a list) with the same elements as
#'   \code{\link{bcnma_bin}} except that \code{sm} is \code{"SMD"} or \code{"MD"}
#'   (no \code{ORd} column in summaries) and an additional element
#'   \code{sigma_pooled} (numeric vector of per-study pooled SDs).
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
#'   interactions = "lasso",
#'   seed         = 42
#' )
#' print(fit)
#' forest(fit)
#' compare_combinations(fit,
#'   combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
#'   combo2 = c("se"))
#' }
#'
#' @seealso \code{\link{bcnma_bin}} for binary outcomes
#' @export
bcnma_cont <- function(data,
                       studlab,
                       treat,
                       mean,
                       sd,
                       n,
                       components,
                       component_separator = "+",
                       reference.group     = NULL,
                       sm                  = c("SMD", "MD"),
                       pooled_sd           = c("within", "control"),
                       interactions        = c("lasso", "none", "lasso_informative"),
                       active_components   = NULL,
                       lasso_prob          = BCNMA_DEFAULT_INCLUSION_PROB,
                       g                   = BCNMA_DEFAULT_SLAB_RATIO,
                       tau_prior           = BCNMA_TAU2_PRIOR_SMD,
                       n.chains            = 3,
                       n.adapt             = 2000,
                       n.burnin            = 5000,
                       n.iter              = 20000,
                       n.thin              = 1,
                       seed                = NULL,
                       quiet               = FALSE,
                       component_values    = NULL,
                       modifiers           = NULL) {
  .bcnma_require_mcmc_packages("bcnma_cont")
  call         <- match.call()
  sm           <- match.arg(sm)
  pooled_sd    <- match.arg(pooled_sd)
  interactions <- match.arg(interactions)
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
                                   components, component_separator, sm, pooled_sd)
  prep <- .apply_component_values(prep, component_values)

  options <- list(
    interactions      = interactions,
    active_components = if (interactions == "lasso_informative") active_components,
    ssvs_pairs        = ssvs_pairs,
    inclusion_prob    = lasso_prob,
    slab_ratio        = g,
    eta_prior_sd      = BCNMA_DEFAULT_ETA_PRIOR_SD,
    tau_prior         = .tau_prior_spec(tau_prior, "continuous default"),
    effect_prior_sd   = BCNMA_EFFECT_PRIOR_SD_SMD,
    baseline_prior_sd = BCNMA_BASELINE_PRIOR_SD_SMD,
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
