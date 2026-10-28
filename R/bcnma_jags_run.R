# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: run one JAGS model: check that the optional rjags / MCMCvis
#          packages are installed, build reproducible per-chain RNG initial
#          values, compile, burn in, sample and return the posterior summary
#          and the combined-chain samples.
# Inputs:  model code (R/bcnma_jags_models.R), JAGS data, MCMC settings
# Outputs: list(summary, samples)
# Depends: rjags, MCMCvis (Suggests; checked by .bcnma_require_mcmc_packages())

#' Stop early when the optional MCMC packages are missing
#'
#' rjags and MCMCvis are Suggests (JAGS is a system library that most
#' nmatools users never need), so every fitting entry point checks for them
#' before any data preparation.
#'
#' @param caller character: name of the calling function, for the message
#' @return invisible TRUE; otherwise an error naming what to install
#' @noRd
.bcnma_require_mcmc_packages <- function(caller) {
  is_missing <- !vapply(BCNMA_MCMC_PACKAGES, requireNamespace, logical(1),
                        quietly = TRUE)
  if (!any(is_missing)) return(invisible(TRUE))
  missing_packages <- BCNMA_MCMC_PACKAGES[is_missing]
  stop(sprintf(
    paste0("%s() needs the optional package(s) %s. Install JAGS ",
           "(https://mcmc-jags.sourceforge.io/) first, then ",
           "install.packages(c(%s))."),
    caller, paste0("'", missing_packages, "'", collapse = ", "),
    paste0('"', missing_packages, '"', collapse = ", ")
  ), call. = FALSE)
}

#' Seed for the JAGS random number generators
#'
#' A NULL seed is replaced by one draw from R's generator so that every fit
#' carries the seed needed to reproduce it.
#' @param seed integer or NULL
#' @return integer seed
#' @noRd
.resolve_seed <- function(seed) {
  if (is.null(seed)) return(sample.int(BCNMA_MAX_SEED, size = 1))
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("'seed' must be a single integer or NULL.")
  }
  as.integer(seed)
}

#' Per-chain initial values that fix the JAGS RNG
#'
#' JAGS ignores R's set.seed(); reproducibility needs .RNG.name / .RNG.seed
#' in the initial values of every chain, with a different seed per chain so
#' that the chains are not copies of each other.
#' @param seed     integer from .resolve_seed()
#' @param n_chains integer number of chains
#' @return list of n_chains lists
#' @noRd
.build_chain_inits <- function(seed, n_chains) {
  lapply(seq_len(n_chains), function(chain_index) {
    list(.RNG.name = BCNMA_RNG_NAME, .RNG.seed = seed + chain_index)
  })
}

#' Compile, burn in and sample one JAGS model
#'
#' @param model_code   character JAGS model
#' @param jags_data    list of data passed to JAGS
#' @param monitored    character vector of monitored nodes
#' @param mcmc         list(n.chains, n.adapt, n.burnin, n.iter, n.thin, inits)
#' @param quiet        logical: suppress progress messages
#' @return list(summary = MCMCsummary data.frame, samples = combined-chain
#'   data.frame); vector nodes of length 1 keep their index (`beta[1]`)
#' @noRd
.bcnma_run_jags <- function(model_code, jags_data, monitored, mcmc, quiet) {
  model_connection <- textConnection(model_code)
  # close() rather than closeAllConnections(): the latter also closes
  # connections owned by the caller (sinks, test reporters).
  on.exit(close(model_connection))
  jags_object <- rjags::jags.model(
    model_connection,
    data     = jags_data,
    inits    = mcmc$inits,
    n.chains = mcmc$n.chains,
    n.adapt  = mcmc$n.adapt,
    quiet    = quiet
  )
  if (mcmc$n.burnin > 0) {
    if (!quiet) message("  Burn-in: ", mcmc$n.burnin, " iterations...")
    stats::update(jags_object, n.iter = mcmc$n.burnin)
  }
  if (!quiet) message("  Sampling: ", mcmc$n.iter, " iterations x ", mcmc$n.chains, " chains...")
  raw_samples <- rjags::coda.samples(jags_object, monitored,
                                     n.iter = mcmc$n.iter, thin = mcmc$n.thin)
  posterior <- list(
    summary = MCMCvis::MCMCsummary(raw_samples),
    samples = as.data.frame(do.call(rbind, lapply(raw_samples, as.matrix))),
    dic     = .sample_dic(jags_object, mcmc, quiet)
  )
  .index_length_one_vectors(posterior, jags_data)
}

#' Posterior mean deviance and pD per observed node (Plummer's pD)
#'
#' rjags::dic.samples() runs n.iter further iterations and needs at least two
#' chains; with one chain DIC is not available (NULL).
#' @return list(deviance, penalty), named by node (e.g. "r[3,2]"), or NULL
#' @noRd
.sample_dic <- function(jags_object, mcmc, quiet) {
  if (mcmc$n.chains < 2) return(NULL)
  if (!quiet) message("  DIC: ", mcmc$n.iter, " further iterations...")
  dic <- rjags::dic.samples(jags_object, n.iter = mcmc$n.iter, thin = mcmc$n.thin,
                            type = "pD", progress.bar = if (quiet) "none" else "text")
  list(deviance = dic$deviance, penalty = dic$penalty)
}

#' Restore the index of length-1 vector nodes
#'
#' coda drops the index of length-1 vector nodes (`beta` instead of
#' `beta[1]`); renaming keeps downstream code able to address `node[1]`.
#' @noRd
.index_length_one_vectors <- function(posterior, jags_data) {
  node_length <- c(beta  = if (is.null(jags_data$z)) 0 else dim(jags_data$z)[3],
                   gamma = if (is.null(jags_data$Nint)) 0 else jags_data$Nint,
                   Ind   = if (is.null(jags_data$Nint)) 0 else jags_data$Nint)
  for (node in names(node_length)) {
    if (node_length[[node]] != 1) next
    colnames(posterior$samples)[colnames(posterior$samples) == node] <- paste0(node, "[1]")
    rownames(posterior$summary)[rownames(posterior$summary) == node] <- paste0(node, "[1]")
  }
  posterior
}

#' Rename `gamma[m]` / `Ind[m]` of the SSVS model to the place() pair index
#'
#' Only estimable, selected pairs enter the JAGS model (m = 1..Nint); outputs
#' are indexed by the pair index of place() so that users can look them up.
#' @param posterior  list(summary, samples) from .bcnma_run_jags()
#' @param pair_index integer place() index of JAGS interaction m
#' @noRd
.rename_interaction_nodes <- function(posterior, pair_index) {
  rename <- function(node_names) {
    for (node in c("gamma", "Ind")) {
      jags_names <- paste0(node, "[", seq_along(pair_index), "]")
      matched <- match(node_names, jags_names)
      node_names[!is.na(matched)] <- paste0(node, "[", pair_index[matched[!is.na(matched)]], "]")
    }
    node_names
  }
  colnames(posterior$samples) <- rename(colnames(posterior$samples))
  rownames(posterior$summary) <- rename(rownames(posterior$summary))
  posterior
}
