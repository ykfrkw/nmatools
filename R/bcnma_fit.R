# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: fitting engine shared by bcnma_bin() and bcnma_cont(): build the
#          JAGS data (within-study contrasts) and model code, fit the
#          additive model and, when requested, the SSVS interaction model,
#          and assemble the fitted object.
# Inputs:  `prep` (R/bcnma_prepare*.R, after .apply_component_values()) and
#          the `options` list assembled by the user-facing function
# Outputs: list that bcnma_bin() / bcnma_cont() turn into the S3 object
# Depends: R/bcnma_jags_models.R, R/bcnma_jags_run.R, R/bcnma_modifiers*.R

#' Settings that fully determine the model code
#'
#' @param prep    prepared data (likelihood, ...)
#' @param options list from the user-facing function
#' @return list(likelihood, effect_prior_sd, baseline_prior_sd, tau_prior,
#'   modifier_priors, ssvs)
#' @noRd
.bcnma_model_settings <- function(prep, options) {
  list(
    likelihood        = prep$likelihood,
    effect_prior_sd   = options$effect_prior_sd,
    baseline_prior_sd = options$baseline_prior_sd,
    tau_prior         = options$tau_prior,
    modifier_priors   = options$modifier_design$priors,
    ssvs              = list(inclusion_prob = options$inclusion_prob,
                             slab_ratio     = options$slab_ratio,
                             eta_prior_sd   = options$eta_prior_sd)
  )
}

#' Arm k minus arm 1 of an arm-level `[study, arm, column]` array
#'
#' Unused arm slots (`k > na[i]`) are set to 0; JAGS never reads them.
#' @noRd
.within_study_contrast <- function(arm_array, n_arms) {
  contrast <- arm_array
  for (study_index in seq_along(n_arms)) {
    arms <- seq_len(n_arms[study_index])
    reference_values <- arm_array[study_index, 1, ]
    contrast[study_index, , ] <- 0
    for (arm in arms) {
      contrast[study_index, arm, ] <- arm_array[study_index, arm, ] - reference_values
    }
  }
  contrast
}

#' JAGS data of one model
#'
#' @param prep            prepared data
#' @param model           "additive" or "ssvs"
#' @param modifier_design output of .build_modifier_design() or NULL
#' @param ssvs_pairs      integer place() indices of the SSVS interactions
#' @return named list passed to rjags::jags.model()
#' @noRd
.build_jags_data <- function(prep, model, modifier_design = NULL, ssvs_pairs = integer(0)) {
  jags_data <- c(
    prep$outcome,
    list(Ns = prep$n_studies, na = prep$n_arms, Nc = prep$n_components,
         x = .within_study_contrast(prep$x_arm, prep$n_arms))
  )
  if (!is.null(modifier_design)) {
    jags_data$z <- .within_study_contrast(modifier_design$code_arm, prep$n_arms)
  }
  if (model == "ssvs") {
    co_presence    <- .build_interaction_array(prep$x_arm)
    jags_data$Nint <- length(ssvs_pairs)
    jags_data$xint <- .within_study_contrast(co_presence[, , ssvs_pairs, drop = FALSE],
                                             prep$n_arms)
  }
  jags_data
}

#' Monitored nodes of a model
#' @noRd
.monitored_nodes <- function(model, likelihood, has_modifiers) {
  nodes <- c("d", if (likelihood == "binomial") "ORd", "tau", "tau2")
  if (model == "ssvs") nodes <- c(nodes, "gamma", "Ind", "eta")
  if (has_modifiers) nodes <- c(nodes, "beta")
  nodes
}

#' Fit one model: code, data, MCMC
#'
#' @return list(model_code, jags_data, summary, samples)
#' @noRd
.fit_one_model <- function(model, prep, options, model_settings) {
  model_code <- .build_jags_model_code(model, model_settings)
  jags_data  <- .build_jags_data(prep, model, options$modifier_design, options$ssvs_pairs)
  monitored  <- .monitored_nodes(model, prep$likelihood, !is.null(options$modifier_design))
  posterior  <- .bcnma_run_jags(model_code, jags_data, monitored, options$mcmc, options$quiet)
  if (model == "ssvs") posterior <- .rename_interaction_nodes(posterior, options$ssvs_pairs)
  c(list(model_code = model_code, jags_data = jags_data), posterior)
}

#' Fit the additive model and, unless interactions = "none", the SSVS model
#'
#' @param prep    prepared data
#' @param options list(interactions, ssvs_pairs, inclusion_prob, slab_ratio,
#'   eta_prior_sd, tau_prior, effect_prior_sd, baseline_prior_sd,
#'   modifier_design, mcmc, quiet)
#' @return list(model_settings, additive, ssvs) (ssvs NULL when not fitted)
#' @noRd
.fit_bcnma_models <- function(prep, options) {
  model_settings <- .bcnma_model_settings(prep, options)
  if (!options$quiet) message("\n--- Fitting additive model (no interactions) ---")
  additive <- .fit_one_model("additive", prep, options, model_settings)
  if (options$interactions == "none") {
    return(list(model_settings = model_settings, additive = additive, ssvs = NULL))
  }
  if (!options$quiet) {
    message(sprintf("\n--- Fitting SSVS interaction model (%s) ---", options$interactions))
    message(sprintf("  %d modelled interactions out of %d pairs", length(options$ssvs_pairs),
                    prep$n_components * (prep$n_components - 1L) / 2L))
  }
  ssvs <- .fit_one_model("ssvs", prep, options, model_settings)
  list(model_settings = model_settings, additive = additive, ssvs = ssvs)
}

#' Elements common to bcnma_bin and bcnma_cont objects
#'
#' @param call    matched call
#' @param prep    prepared data
#' @param options fitting options
#' @param fitted  output of .fit_bcnma_models()
#' @param sm      effect measure ("OR", "SMD", "MD")
#' @return named list
#' @noRd
.assemble_bcnma_fit <- function(call, prep, options, fitted, sm) {
  pair_count <- prep$n_components * (prep$n_components - 1L) / 2L
  interaction_summary <- if (is.null(fitted$ssvs)) NULL else
    .extract_interaction_summary(fitted$ssvs$samples, prep$components)
  c(
    list(
      call                       = call,
      components                 = prep$components,
      n_components               = prep$n_components,
      Ns                         = prep$n_studies,
      Ninter                     = pair_count,
      sm                         = sm,
      interactions_model         = options$interactions,
      active_components          = options$active_components,
      included_interactions      = options$ssvs_pairs,
      reference.group            = options$reference_group,
      summary_additive           = fitted$additive$summary,
      summary_ssvs               = fitted$ssvs$summary,
      samps_additive             = fitted$additive$samples,
      samps_ssvs                 = fitted$ssvs$samples,
      interaction_inclusion_prob = interaction_summary$inclusion_prob,
      interaction_effects        = interaction_summary$effects,
      jags_data                  = fitted$additive$jags_data,
      model_code                 = list(additive = fitted$additive$model_code,
                                        ssvs     = fitted$ssvs$model_code),
      model_settings             = fitted$model_settings,
      identifiability            = options$identifiability,
      prep                       = prep,
      mcmc_settings              = options$mcmc[c("n.chains", "n.adapt", "n.burnin",
                                                  "n.iter", "n.thin")],
      seed                       = options$mcmc$seed,
      inits                      = options$mcmc$inits
    ),
    .modifier_results(options$component_values, options$modifier_specs,
                      options$modifier_design, fitted$additive$samples,
                      fitted$ssvs$samples, exponentiate = sm == "OR")
  )
}
