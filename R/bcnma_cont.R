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
#' @param tau_prior           numeric \code{c(mean, precision)}: parameters of the
#'   lognormal prior on tau. Default \code{c(-1.0, 1/0.8^2)} is appropriate for
#'   SMD heterogeneity in psychological intervention trials.
#' @param n.chains            integer: MCMC chains (default 3)
#' @param n.adapt             integer: JAGS adaptation iterations (default 2000)
#' @param n.burnin            integer: burn-in iterations (default 5000)
#' @param n.iter              integer: sampling iterations per chain (default 20000)
#' @param n.thin              integer: thinning interval (default 1)
#' @param seed                integer or NULL: random seed
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
                       lasso_prob          = 0.5,
                       g                   = 100,
                       tau_prior           = c(-1.0, 1 / 0.8^2),
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
  cl <- match.call()

  # ---- Input validation ----
  sm           <- match.arg(sm)
  pooled_sd    <- match.arg(pooled_sd)
  interactions <- match.arg(interactions)

  studlab_col <- gsub('"', '', deparse(substitute(studlab)))
  treat_col   <- gsub('"', '', deparse(substitute(treat)))
  mean_col    <- gsub('"', '', deparse(substitute(mean)))
  sd_col      <- gsub('"', '', deparse(substitute(sd)))
  n_col       <- gsub('"', '', deparse(substitute(n)))

  for (col in c(studlab_col, treat_col, mean_col, sd_col, n_col)) {
    if (!col %in% names(data)) {
      stop(sprintf("Column '%s' not found in data.", col))
    }
  }

  if (!is.character(components) || length(components) < 2) {
    stop("'components' must be a character vector of length >= 2.")
  }

  if (interactions == "lasso_informative") {
    if (is.null(active_components)) {
      stop("'active_components' must be specified when interactions = 'lasso_informative'.")
    }
    unknown <- setdiff(active_components, components)
    if (length(unknown) > 0) {
      stop("'active_components' contains names not in 'components': ",
           paste(unknown, collapse = ", "))
    }
  }

  component_values <- .validate_component_values(component_values, nrow(data), components)
  modifier_specs   <- .normalise_modifier_specs(modifiers, data, components)

  # ---- Data preparation ----
  if (!quiet) message("Preparing data...")
  prep <- .bcnma_cont_prepare_data(
    data, studlab_col, treat_col, mean_col, sd_col, n_col,
    components, component_separator, sm, pooled_sd
  )
  prep   <- .apply_component_values(prep, component_values)
  modifier_design <- .build_modifier_design(modifier_specs, data, prep)
  Ns     <- prep$Ns
  Nc     <- prep$Nc
  Ninter <- Nc * (Nc - 1L) / 2L

  # ---- Determine included interactions ----
  if (interactions == "lasso_informative") {
    included <- all.interactions(active_components, Nc, components)
    if (length(included) == 0) {
      stop("No pairwise interactions found among 'active_components'.")
    }
  } else {
    included <- seq_len(Ninter)
  }

  # ---- Seed ----
  if (!is.null(seed)) set.seed(seed)

  # ---- Common JAGS data ----
  jags_data_base <- list(
    y  = prep$y,
    prec = prep$prec,
    xA = prep$xA,
    xB = prep$xB,
    na = prep$na,
    Ns = Ns,
    Nc = Nc
  )
  # Only wA is added (no Q): the continuous prior block has no ORbeta loop,
  # and JAGS rejects unused data. Nothing is added without modifiers.
  if (!is.null(modifier_design)) {
    jags_data_base <- c(jags_data_base, list(wA = modifier_design$wA))
  }
  modifier_priors <- modifier_design$priors
  mcmc <- list(n.chains = n.chains, n.adapt = n.adapt,
               n.burnin = n.burnin, n.iter  = n.iter, n.thin = n.thin)

  # ---- Results containers ----
  samps_lasso      <- NULL
  summary_lasso    <- NULL

  # =====================================================================
  # MODEL 1: Additive
  # =====================================================================
  if (!quiet) message("\n--- Fitting additive model (no interactions) ---")

  model_add  <- .jags_model_additive_cont(Nc, tau_prior, modifier_priors = modifier_priors)
  params_add <- .with_modifier_params(c("d", "tau"), modifier_design)
  fit_add    <- .bcnma_fit_model(model_add, jags_data_base, params_add, mcmc, quiet)
  summary_additive <- fit_add$summary
  samps_additive   <- fit_add$samps

  # =====================================================================
  # MODEL 2: SSVS / LASSO
  # =====================================================================
  if (interactions != "none") {
    if (!quiet) {
      model_label <- if (interactions == "lasso") "LASSO (equiprobable)" else "LASSO (informative)"
      message(sprintf("\n--- Fitting %s interaction model ---", model_label))
      message(sprintf("  %d active interactions out of %d total", length(included), Ninter))
    }

    # Presence, not dose products, so gamma stays the co-presence effect
    component_presence <- (prep$xA != 0) * 1L
    interactions_arr <- .build_interaction_array(component_presence, Ns, prep$max_arms, Nc)
    jags_data_ssvs   <- c(jags_data_base, list(interactions = interactions_arr))

    model_ssvs <- .jags_model_ssvs_cont(
      Nc, Ninter,
      included        = included,
      prob_active     = lasso_prob,
      g               = g,
      tau_prior       = tau_prior,
      modifier_priors = modifier_priors
    )

    params_ssvs <- .with_modifier_params(c("d", "gamma", "Ind", "tau", "eta"), modifier_design)
    fit_ssvs    <- .bcnma_fit_model(model_ssvs, jags_data_ssvs, params_ssvs, mcmc, quiet)
    summary_lasso <- fit_ssvs$summary
    samps_lasso   <- fit_ssvs$samps

    interaction_summary <- .extract_interaction_summary(samps_lasso, Ninter, components)
    inter_incl_prob <- interaction_summary$inclusion_prob
    inter_effects   <- interaction_summary$effects

  } else {
    inter_incl_prob <- NULL
    inter_effects   <- NULL
  }

  if (!quiet) message("\nDone.")

  # ---- Build return object ----
  result <- c(
    list(
      call                       = cl,
      components                 = components,
      n_components               = Nc,
      Ns                         = Ns,
      Ninter                     = Ninter,
      sm                         = sm,
      pooled_sd                  = pooled_sd,
      sigma_pooled               = prep$sigma_pooled,
      interactions_model         = interactions,
      active_components          = if (interactions == "lasso_informative") active_components else NULL,
      included_interactions      = included,
      reference.group            = reference.group,
      summary_additive           = summary_additive,
      summary_lasso              = summary_lasso,
      samps_additive             = samps_additive,
      samps_lasso                = samps_lasso,
      interaction_inclusion_prob = inter_incl_prob,
      interaction_effects        = inter_effects,
      jags_data                  = jags_data_base,
      mcmc_settings              = mcmc
    ),
    .modifier_results(component_values, modifier_specs, modifier_design,
                      samps_additive, samps_lasso, exponentiate = FALSE)
  )
  structure(result, class = "bcnma_cont")
}
