# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Main bcnma_bin() function — Bayesian component NMA with LASSO/SSVS interactions.
# Interface is modelled after netmeta() / discomb().

#' Bayesian Component Network Meta-Analysis with LASSO Interactions
#'
#' Fits a Bayesian random-effects component NMA (cNMA) using JAGS. Supports
#' three interaction models:
#' - `"none"`: fully additive (no interactions)
#' - `"lasso"`: all pairwise interactions with equiprobable SSVS priors
#' - `"lasso_informative"`: SSVS priors for `active_components` interactions only;
#'   all other interactions forced to zero
#'
#' Requires the JAGS system library and the suggested packages rjags and
#' MCMCvis; an informative error is raised when they are missing.
#'
#' @param data               data.frame with one row per arm per study
#' @param studlab             unquoted column name for study identifier
#' @param treat               unquoted column name for treatment/component combination string
#' @param event               unquoted column name for event count (integer)
#' @param n                   unquoted column name for arm sample size (integer)
#' @param components          character vector of component abbreviations (must match substrings in `treat`)
#' @param component_separator character separating components in the `treat` column (default `"+"`)
#' @param reference.group     character: reference component/combination for printing (optional)
#' @param interactions        character: `"none"`, `"lasso"` (default), or `"lasso_informative"`
#' @param active_components   character vector of components whose pairwise interactions
#'                            are modelled (only used when `interactions = "lasso_informative"`)
#' @param lasso_prob          numeric: prior inclusion probability for active interactions (default 0.5)
#' @param g                   numeric: spike-slab variance ratio; slab variance = g * spike variance (default 100)
#' @param tau_prior           numeric c(meanlog, precision): log-normal prior on the
#'   between-study variance tau^2 (JAGS node `tau2`; `tau = sqrt(tau2)` is the
#'   SD). Default c(-1.67, 1/1.472^2), a log-normal predictive distribution
#'   of the Turner et al. (2015) type (see the review guide for the open
#'   question on its exact category).
#' @param n.chains            integer: number of MCMC chains (default 3)
#' @param n.adapt             integer: JAGS adaptation iterations (default 2000)
#' @param n.burnin            integer: additional burn-in iterations after adaptation (default 5000)
#' @param n.iter              integer: sampling iterations per chain (default 20000)
#' @param n.thin              integer: thinning interval (default 1)
#' @param seed                integer or NULL: seed of the JAGS random number
#'   generators. Chain c uses \code{.RNG.name = "base::Mersenne-Twister"} and
#'   \code{.RNG.seed = seed + c}, so identical seeds give identical samples.
#'   \code{NULL} draws one seed from R's generator; it is stored in
#'   \code{fit$seed} (with the per-chain \code{fit$inits}) for re-running.
#' @param quiet               logical: suppress progress messages (default FALSE)
#' @param component_values    optional numeric matrix or data.frame with `nrow(data)`
#'   rows in the same order as `data` and column names exactly equal to
#'   `components`. Replaces the 0/1 indicators parsed from `treat` (e.g. number of
#'   sessions), so `d[k]` becomes the log-OR per unit of component k (linear
#'   dose-response). SSVS interactions still use presence (value != 0). Default
#'   `NULL` keeps the 0/1 indicators.
#' @param modifiers           optional list of arm-level effect-modifier
#'   specifications (a single specification may be passed without the outer
#'   list). Each is a list with `column` (arm-level column of `data`),
#'   `components` (components whose effect is modified; one term per component
#'   and level step), `type` (`"continuous"` or `"ordinal"`) and optionally
#'   `label` (default `column`) and `prior_sd` (default 10; `beta ~ N(0,
#'   prior_sd^2)`). Continuous: `center` (default: mean over arms where the
#'   component is present); the code is `value - center`. Ordinal: `levels`
#'   (required, lowest first) and `monotone` (`"none"`, `"increasing"`,
#'   `"decreasing"`); cumulative coding, so `beta_l` is the increment from level
#'   l to l+1. `NA` is allowed only in arms where the modified component is
#'   absent. The term `beta_q * (z[i,k,q] - z[i,1,q])` is added to the
#'   consistency equation, so `d[k]` of a modified component is its effect at
#'   the center (continuous) or reference level (ordinal). Modifier effects are
#'   identified mainly by between-study variation and are observational
#'   (ecological) associations. Default `NULL`: no modifiers.
#'
#' @return object of class `bcnma_bin` (a list) with elements:
#' \describe{
#'   \item{components}{character vector of component names}
#'   \item{n_components}{integer Nc}
#'   \item{Ns}{integer number of studies}
#'   \item{interactions_model}{character: which interaction model was fitted}
#'   \item{summary_additive}{MCMCsummary data.frame for additive model (if fitted)}
#'   \item{summary_lasso}{MCMCsummary data.frame for LASSO model (if fitted)}
#'   \item{samps_additive}{data.frame of combined MCMC samples, additive model}
#'   \item{samps_lasso}{data.frame of combined MCMC samples, LASSO model}
#'   \item{interaction_inclusion_prob}{named numeric: \code{mean(Ind[k])} per interaction}
#'   \item{interaction_effects}{named numeric: \code{median(gamma[k])} per interaction}
#'   \item{component_values}{validated component value matrix (NULL if not supplied)}
#'   \item{modifier_terms}{data.frame of modifier (beta) terms (NULL without modifiers)}
#'   \item{modifier_effects}{list(additive, lasso) of beta posterior summaries
#'     (NULL without modifiers)}
#'   \item{jags_data}{list passed to JAGS}
#'   \item{call}{matched call}
#' }
#'
#' @examples
#' \dontrun{
#' library(nmatools)
#'
#' # Load the bundled CBTICNMA example data
#' dat <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
#'                             package = "nmatools"))
#'
#' components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
#'                 "w",  "ns", "he", "tg", "ind", "gp", "ff", "ae")
#'
#' fit <- bcnma_bin(
#'   data         = dat,
#'   studlab      = study,
#'   treat        = treatment_component,
#'   event        = r,
#'   n            = n,
#'   components   = components,
#'   interactions = "lasso",
#'   seed         = 42
#' )
#' print(fit)
#' forest(fit)
#'
#' # Compare full CBT-I vs. sleep hygiene only
#' compare_combinations(fit,
#'   combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
#'   combo2 = c("se"))
#' }
#'
#' @export
bcnma_bin <- function(data,
                      studlab,
                      treat,
                      event,
                      n,
                      components,
                      component_separator = "+",
                      reference.group     = NULL,
                      interactions        = c("lasso", "none", "lasso_informative"),
                      active_components   = NULL,
                      lasso_prob          = BCNMA_DEFAULT_INCLUSION_PROB,
                      g                   = BCNMA_DEFAULT_SLAB_RATIO,
                      tau_prior           = NULL,
                      n.chains            = 3,
                      n.adapt             = 2000,
                      n.burnin            = 5000,
                      n.iter              = 20000,
                      n.thin              = 1,
                      seed                = NULL,
                      quiet               = FALSE,
                      component_values    = NULL,
                      modifiers           = NULL) {
  .bcnma_require_mcmc_packages("bcnma_bin")
  call         <- match.call()
  interactions <- match.arg(interactions)
  studlab_col  <- .resolve_column(deparse(substitute(studlab)), data)
  treat_col    <- .resolve_column(deparse(substitute(treat)), data)
  event_col    <- .resolve_column(deparse(substitute(event)), data)
  n_col        <- .resolve_column(deparse(substitute(n)), data)
  .validate_components(components)
  ssvs_pairs <- .candidate_interaction_pairs(interactions, active_components, components)
  component_values <- .validate_component_values(component_values, nrow(data), components)
  modifier_specs   <- .normalise_modifier_specs(modifiers, data, components)

  if (!quiet) message("Preparing data...")
  prep <- .bcnma_bin_prepare_data(data, studlab_col, treat_col, event_col, n_col,
                                  components, component_separator)
  prep <- .apply_component_values(prep, component_values)

  options <- list(
    interactions      = interactions,
    active_components = if (interactions == "lasso_informative") active_components,
    ssvs_pairs        = ssvs_pairs,
    inclusion_prob    = lasso_prob,
    slab_ratio        = g,
    eta_prior_sd      = BCNMA_DEFAULT_ETA_PRIOR_SD,
    tau_prior         = .tau_prior_spec(tau_prior, .default_tau_prior_bin()),
    effect_prior_sd   = BCNMA_EFFECT_PRIOR_SD_BIN,
    baseline_prior_sd = BCNMA_BASELINE_PRIOR_SD_BIN,
    component_values  = component_values,
    modifier_specs    = modifier_specs,
    modifier_design   = .build_modifier_design(modifier_specs, data, prep),
    reference_group   = reference.group,
    mcmc              = .mcmc_settings(n.chains, n.adapt, n.burnin, n.iter, n.thin, seed),
    quiet             = quiet
  )
  fitted <- .fit_bcnma_models(prep, options)
  if (!quiet) message("\nDone.")
  structure(.assemble_bcnma_fit(call, prep, options, fitted, sm = "OR"), class = "bcnma_bin")
}
