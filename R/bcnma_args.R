# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: argument checks and option assembly shared by bcnma_bin() and
#          bcnma_cont(): column names, components, interaction model and the
#          pairs it models, MCMC settings and heterogeneity prior.
# Inputs:  arguments of the user-facing fitting functions
# Outputs: validated values / the `options` list read by R/bcnma_fit.R
# Depends: R/bcnma_constants.R, R/bcnma_interactions.R

#' Column name from a quoted or unquoted argument
#'
#' @param argument_text character from deparse(substitute(argument))
#' @param data          user data.frame
#' @return character column name
#' @noRd
.resolve_column <- function(argument_text, data) {
  column <- gsub('"', "", argument_text)
  if (!column %in% names(data)) stop(sprintf("Column '%s' not found in data.", column))
  column
}

.validate_components <- function(components) {
  is_valid <- is.character(components) && length(components) >= 2
  if (!is_valid) stop("'components' must be a character vector of length >= 2.")
  invisible(components)
}

#' Pairs (place() indices) given SSVS priors
#'
#' @param interactions      interaction model
#' @param active_components character or NULL
#' @param components        all components
#' @return integer vector (empty for "none")
#' @noRd
.candidate_interaction_pairs <- function(interactions, active_components, components) {
  pair_count <- length(components) * (length(components) - 1L) / 2L
  if (interactions == "none") return(integer(0))
  if (interactions != "lasso_informative") return(seq_len(pair_count))
  if (is.null(active_components)) {
    stop("'active_components' must be specified when interactions = 'lasso_informative'.")
  }
  unknown <- setdiff(active_components, components)
  if (length(unknown) > 0) {
    stop("'active_components' contains names not in 'components': ",
         paste(unknown, collapse = ", "))
  }
  pairs <- all.interactions(active_components, length(components), components)
  if (length(pairs) == 0) {
    stop("No pairwise interactions found among 'active_components'. ",
         "Need at least 2 active components.")
  }
  pairs
}

#' Heterogeneity prior as a self-describing list
#'
#' Accepted forms of the user's `tau_prior`:
#' * `NULL`: the outcome-specific default `default_prior`;
#' * numeric `c(meanlog, precision)`: log-normal prior on tau^2;
#' * `list(family = "lognormal_tau2", meanlog, precision)`: the same;
#' * `list(family = "half_normal_tau", sd)`: half-normal prior on tau.
#'
#' @param tau_prior     user value (see above)
#' @param default_prior spec list used when `tau_prior` is NULL
#' @return list(family, parameters, label, source)
#' @noRd
.tau_prior_spec <- function(tau_prior, default_prior) {
  if (is.null(tau_prior)) return(default_prior)
  if (is.numeric(tau_prior)) {
    tau_prior <- list(family = "lognormal_tau2", meanlog = tau_prior[1],
                      precision = tau_prior[2], is_valid_length = length(tau_prior) == 2)
  }
  if (!is.list(tau_prior) || !isTRUE(tau_prior$family %in% BCNMA_TAU_PRIOR_FAMILIES)) {
    stop("'tau_prior' must be NULL, c(meanlog, precision), or a list with family ",
         "\"lognormal_tau2\" (meanlog, precision) or \"half_normal_tau\" (sd).")
  }
  if (tau_prior$family == "half_normal_tau") {
    return(.half_normal_tau_prior(tau_prior$sd, source = "user-supplied"))
  }
  .lognormal_tau2_prior(tau_prior$meanlog, tau_prior$precision, source = "user-supplied",
                        is_valid_length = !isFALSE(tau_prior$is_valid_length))
}

#' Log-normal prior on tau^2 as a spec list
#' @noRd
.lognormal_tau2_prior <- function(meanlog, precision, source, is_valid_length = TRUE) {
  is_valid <- is_valid_length && is.numeric(meanlog) && is.numeric(precision) &&
    length(meanlog) == 1 && length(precision) == 1 && precision > 0
  if (!is_valid) stop("A log-normal tau2 prior needs one meanlog and one precision > 0.")
  list(family = "lognormal_tau2", meanlog = meanlog, precision = precision, source = source,
       label = sprintf("log-normal(meanlog %s, precision %s) on tau2 [%s]",
                       .format_number(meanlog), .format_number(precision), source))
}

#' Half-normal prior on tau as a spec list
#' @noRd
.half_normal_tau_prior <- function(prior_sd, source) {
  is_valid <- is.numeric(prior_sd) && length(prior_sd) == 1 && prior_sd > 0
  if (!is_valid) stop("A half-normal tau prior needs one sd > 0.")
  list(family = "half_normal_tau", sd = prior_sd, source = source,
       label = sprintf("half-normal(SD %s) on tau [%s]", .format_number(prior_sd), source))
}

#' MCMC settings with reproducible per-chain RNG initial values
#' @noRd
.mcmc_settings <- function(n.chains, n.adapt, n.burnin, n.iter, n.thin, seed) {
  seed <- .resolve_seed(seed)
  list(n.chains = n.chains, n.adapt = n.adapt, n.burnin = n.burnin,
       n.iter = n.iter, n.thin = n.thin, seed = seed,
       inits = .build_chain_inits(seed, n.chains))
}
