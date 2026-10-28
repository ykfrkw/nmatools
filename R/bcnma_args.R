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
#' @param tau_prior numeric c(meanlog, precision) of a log-normal prior on tau^2
#' @param label     character: where the default comes from
#' @return list(family, meanlog, precision, label)
#' @noRd
.tau_prior_spec <- function(tau_prior, label) {
  is_valid <- is.numeric(tau_prior) && length(tau_prior) == 2 && tau_prior[2] > 0
  if (!is_valid) stop("'tau_prior' must be c(meanlog, precision) with precision > 0.")
  list(family = "lognormal_tau2", meanlog = tau_prior[1], precision = tau_prior[2],
       label = sprintf("log-normal(%s, precision %s) on tau2 (%s)",
                       .format_number(tau_prior[1]), .format_number(tau_prior[2]), label))
}

#' MCMC settings with reproducible per-chain RNG initial values
#' @noRd
.mcmc_settings <- function(n.chains, n.adapt, n.burnin, n.iter, n.thin, seed) {
  seed <- .resolve_seed(seed)
  list(n.chains = n.chains, n.adapt = n.adapt, n.burnin = n.burnin,
       n.iter = n.iter, n.thin = n.thin, seed = seed,
       inits = .build_chain_inits(seed, n.chains))
}
