# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (rewritten 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: bcnma_bin(), the user-facing fit of a Bayesian random-effects
#          component NMA for binary outcomes (binomial likelihood, logit
#          link, NICE DSU TSD2 random-effects structure) with additive or
#          SSVS interaction models. Interface modelled after
#          netmeta::discomb().
# Inputs:  arm-level data.frame (one row per arm) and fitting options
# Outputs: object of class "bcnma_bin"
# Depends: R/bcnma_args.R, R/bcnma_prepare.R, R/bcnma_identifiability.R,
#          R/bcnma_modifiers.R, R/bcnma_fit.R; rjags + MCMCvis (Suggests)

#' Bayesian component network meta-analysis for binary outcomes
#'
#' Fits a Bayesian random-effects component NMA (cNMA) with a binomial
#' likelihood in JAGS. Three interaction models are available:
#' - `"none"`: fully additive (no interactions);
#' - `"ssvs"` (default): every estimable pairwise interaction gets a
#'   spike-and-slab (stochastic search variable selection, SSVS) prior with
#'   prior inclusion probability `inclusion_prob`;
#' - `"ssvs_informative"`: SSVS priors only for interactions among
#'   `active_components`; all other interactions are fixed to zero.
#'
#' The additive model is always fitted too. Before fitting, the within-study
#' contrasts are checked for identifiability: non-identifiable main effects
#' are an error, and interactions that are not estimable or are aliased are
#' fixed to zero (see `fit$identifiability`). Use [bcnma_model_code()] to
#' print the exact JAGS code.
#'
#' Requires the JAGS system library and the suggested packages rjags and
#' MCMCvis; an informative error is raised when they are missing.
#'
#' @param data                data.frame with one row per arm per study
#' @param studlab             unquoted column name for study identifier
#' @param treat               unquoted column name for the treatment string
#'   (components joined by `component_separator`)
#' @param event               unquoted column name for event count (integer)
#' @param n                   unquoted column name for arm sample size (integer)
#' @param components          character vector of component abbreviations;
#'   their order defines `d[1]`, `d[2]`, ...
#' @param component_separator character separating components in `treat`
#'   (default `"+"`)
#' @param inactive            character vector of `treat` tokens that are not
#'   components: tokens meaning "no active component" (e.g. `"placebo"`,
#'   `"waitlist"`) or labels deliberately not modelled. An arm made only of
#'   such tokens is the reference (all components absent). Any token of
#'   `treat` in neither `components` nor `inactive` is an error. Default `NULL`.
#' @param reference.group     character: reference label for printing (optional)
#' @param interactions        character: `"ssvs"` (default), `"none"` or
#'   `"ssvs_informative"`. `"lasso"` and `"lasso_informative"` are deprecated
#'   aliases of the SSVS options (one message per session).
#' @param active_components   character vector of components whose pairwise
#'   interactions get SSVS priors (`interactions = "ssvs_informative"` only)
#' @param inclusion_prob      numeric in (0, 1): prior probability that an
#'   interaction is in the slab (default 0.5)
#' @param g                   numeric: slab variance / spike variance
#'   (default 100)
#' @param eta_prior_sd        numeric: SD of the half-normal prior on the spike
#'   SD `eta` (default `1/sqrt(1000)`, i.e. precision 1000). The slab SD is
#'   `eta * sqrt(g)`; at the defaults its prior median is about 0.21 and its
#'   97.5th percentile about 0.71 (log-OR). These defaults decide what counts
#'   as a "large" interaction, so a sensitivity analysis over `eta_prior_sd`
#'   and `g` is recommended.
#' @param tau_prior           heterogeneity prior. `NULL` (default): a
#'   log-normal prior `c(meanlog = -1.67, precision = 1/1.472^2)` on the
#'   between-study variance tau^2 (JAGS node `tau2`; `tau = sqrt(tau2)` is the
#'   SD). It has the form of the Turner et al. (2015) predictive
#'   distributions, but its exact category has not been verified, so it is
#'   labelled "not from a verified published category" in the output; choose
#'   a Turner category that matches your outcome and comparison type where
#'   possible. Otherwise numeric `c(meanlog, precision)` (log-normal on
#'   tau^2), `list(family = "lognormal_tau2", meanlog, precision)` or
#'   `list(family = "half_normal_tau", sd)`.
#' @param n.chains            integer: number of MCMC chains (default 3)
#' @param n.adapt             integer: JAGS adaptation iterations (default 2000)
#' @param n.burnin            integer: burn-in iterations after adaptation
#'   (default 5000)
#' @param n.iter              integer: sampling iterations per chain (default 20000)
#' @param n.thin              integer: thinning interval (default 1)
#' @param seed                integer or NULL: seed of the JAGS random number
#'   generators. Chain c uses `.RNG.name = "base::Mersenne-Twister"` and
#'   `.RNG.seed = seed + c`, so identical seeds give identical samples. `NULL`
#'   draws one seed from R's generator; it is stored in `fit$seed` (with the
#'   per-chain `fit$inits`) for re-running.
#' @param quiet               logical: suppress progress messages (default FALSE)
#' @param component_values    optional numeric matrix or data.frame with
#'   `nrow(data)` rows in the order of `data` and column names exactly equal to
#'   `components`. Replaces the 0/1 indicators parsed from `treat` (e.g. number
#'   of sessions), so `d[k]` becomes the log-OR per unit of component k (linear
#'   dose-response through zero). SSVS interactions and modifiers act on
#'   presence (value != 0). Default `NULL` keeps the 0/1 indicators.
#' @param modifiers           optional list of arm-level effect-modifier
#'   specifications (a single specification may be passed without the outer
#'   list); see the "Effect modifiers" section.
#' @param lasso_prob          deprecated alias of `inclusion_prob`
#'
#' @section Effect modifiers:
#' Each specification is a list with `column` (arm-level column of `data`),
#' `components` (components whose effect is modified; one term per component
#' and level step), `type` (`"continuous"` or `"ordinal"`) and optionally
#' `label` (default `column`) and `prior_sd` (default 1; `beta ~ N(0,
#' prior_sd^2)` on the code scale). Continuous: `center` (default: mean over
#' arms where the component is present); the code is `(value - center) / s`
#' with `s` the SD of the covariate over those arms, so `beta` is sampled per
#' covariate SD and reported both per SD and per original unit. Ordinal:
#' `levels` (required, lowest first) and `monotone` (`"none"`,
#' `"increasing"`, `"decreasing"`); cumulative coding, so `beta_l` is the
#' increment from level l to l+1. `NA` is allowed only in arms where the
#' modified component is absent. The term `beta_q * (z[i,k,q] - z[i,1,q])` is
#' added to the consistency equation, so `d[k]` of a modified component is
#' its effect at the center (continuous) or reference level (ordinal), and
#' modifiers act on presence, not on the dose. Modifier effects are
#' identified mainly by between-study variation and are observational
#' (ecological) associations: the output reports the number of studies
#' informing each `beta` and warns when its posterior SD exceeds half its
#' prior SD (prior-dominated). For binary outcomes `exp(beta)` is a ratio of
#' ORs per unit.
#'
#' Spike-and-slab selection of a modifier: `selection = "ssvs"` (default
#' `"none"`) adds one indicator per (modifier, component), shared by all
#' ordinal steps of that modifier for that component, so that it answers "is
#' there any modification?". All its betas get a slab prior N(0, `slab_sd`^2)
#' (default 0.5) when the indicator is 1 and a spike N(0, `spike_sd`^2)
#' (default 0.01) when it is 0, on the code scale: per covariate SD for
#' continuous modifiers (standardised internally), per level step for ordinal
#' ones, in log-OR (or SMD / MD) units. `inclusion_prob` (default 0.5) is the
#' prior probability of the slab. `slab_sd` replaces `prior_sd`, and
#' `selection = "ssvs"` cannot be combined with `monotone` (a truncated prior
#' pushes the posterior away from 0). `fit$modifier_selection` reports
#' P(modification present), its Monte Carlo SE and the Bayes factor
#' (posterior / prior odds, as a bound when all draws are in one component);
#' these depend on `slab_sd` (Lindley's paradox), so report a sensitivity
#' analysis.
#'
#' @return object of class `bcnma_bin` (a list) with elements including:
#' \describe{
#'   \item{components, n_components, Ns, Ninter}{components, their number,
#'     the number of studies and of component pairs}
#'   \item{interactions_model}{`"none"`, `"ssvs"` or `"ssvs_informative"`}
#'   \item{summary_additive, summary_ssvs}{MCMCsummary data.frames}
#'   \item{samps_additive, samps_ssvs}{combined-chain MCMC samples (`d[k]`,
#'     `ORd[k]`, `tau`, `tau2`; SSVS also `gamma[m]`, `Ind[m]`, `eta`, indexed
#'     by [place()])}
#'   \item{interaction_inclusion_prob, interaction_effects}{named numeric:
#'     `mean(Ind[m])` and `median(gamma[m])` per pair (0 when fixed)}
#'   \item{identifiability}{design ranks and `interaction_status` per pair}
#'   \item{model_code, model_settings}{exact JAGS code and the settings
#'     (priors) that produced it}
#'   \item{seed, inits, mcmc_settings}{reproducibility information}
#'   \item{component_values, modifier_terms, modifier_effects}{doses and
#'     effect-modifier output (NULL when unused)}
#'   \item{jags_data, prep, call}{data passed to JAGS (additive model),
#'     prepared arm-level data and the matched call}
#' }
#'
#' @examples
#' \dontrun{
#' library(nmatools)
#'
#' # Load the bundled CBTICNMA example data
#' dat <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
#'                             package = "nmatools"))
#' names(dat)[1] <- "study"   # BOM in the first column name
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
#'   inactive     = "dt",
#'   interactions = "ssvs",
#'   seed         = 42
#' )
#' print(fit)
#' forest(fit)
#' bcnma_model_code(fit, "ssvs")
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
                      inactive            = NULL,
                      reference.group     = NULL,
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
  .bcnma_require_mcmc_packages("bcnma_bin")
  call         <- match.call()
  interactions <- .normalise_interactions(match.arg(interactions))
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
                                  components, component_separator, as.character(inactive))
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
