# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# JAGS building blocks for arm-level effect modifiers (beta terms), shared by
# the binary and continuous model builders in R/bcnma_jags_models*.R, and the
# common compile / burn-in / sample runner used by bcnma_bin() and
# bcnma_cont(), with the guard that the optional rjags / MCMCvis packages are
# installed (both are Suggests of nmatools; JAGS is a system library).
# Without modifiers every block returns "" so that the model strings stay
# byte-identical to bcnma 0.1.0 (see
# tests/testthat/test-bcnma-backward-compat.R).

# Continuation indent of the consistency equation in each model family.
BCNMA_INDENT_BIN  <- "                      "
BCNMA_INDENT_CONT <- "                    "

# Optional (Suggests) packages needed to fit a model.
BCNMA_MCMC_PACKAGES <- c("rjags", "MCMCvis")

#' Effect-modifier lines for the consistency equation
#'
#' Like xB, the comparator is always arm 1: `beta[q] * (wA[i,k,q] - wA[i,1,q])`.
#'
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#' @param indent          character continuation indent of the model family
#' @return character ("" without modifier terms)
#' @noRd
.jags_modifier_lines <- function(modifier_priors, indent = BCNMA_INDENT_BIN) {
  if (is.null(modifier_priors) || nrow(modifier_priors) == 0) return("")
  term_index <- seq_len(nrow(modifier_priors))
  A_terms <- paste0("beta[", term_index, "] * wA[i,k,", term_index, "]")
  B_terms <- paste0("beta[", term_index, "] * wA[i,1,", term_index, "]")
  separator <- paste0(" +\n", indent)
  paste0(
    indent, '+ (', paste(A_terms, collapse = separator), ')\n',
    indent, '- (', paste(B_terms, collapse = separator), ')\n'
  )
}

#' Prior block for beta (effect-modifier) terms
#'
#' `truncation` ("" / "T(0,)" / "T(,0)") implements monotone ordinal increments.
#'
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#' @param scale_label     character: effect scale named in the comment
#' @param add_odds_ratio  logical: also define `ORbeta[q] = exp(beta[q])` (binary)
#' @return character ("" without modifier terms)
#' @noRd
.jags_modifier_prior_block <- function(modifier_priors, scale_label = "log-OR",
                                       add_odds_ratio = TRUE) {
  if (is.null(modifier_priors) || nrow(modifier_priors) == 0) return("")
  truncation_suffix <- ifelse(modifier_priors$truncation == "", "",
                              paste0(" ", modifier_priors$truncation))
  prior_lines <- sprintf("  beta[%d] ~ dnorm(0, %.6f)%s",
                         seq_len(nrow(modifier_priors)),
                         modifier_priors$precision, truncation_suffix)
  paste0(
    '\n',
    '  ## Effect modifiers: beta[q] = change in component ', scale_label, ' per code unit\n',
    paste(prior_lines, collapse = "\n"), '\n',
    if (add_odds_ratio) '  for (q in 1:Q) { ORbeta[q] <- exp(beta[q]) }\n' else ''
  )
}

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

#' Compile, burn in and sample one JAGS model
#'
#' @param model_string character JAGS model
#' @param jags_data    list of data passed to JAGS
#' @param params       character vector of monitored nodes
#' @param mcmc         list(n.chains, n.adapt, n.burnin, n.iter, n.thin)
#' @param quiet        logical: suppress progress messages
#' @return list(summary = MCMCsummary data.frame, samps = combined-chain data.frame)
#' @noRd
.bcnma_run_jags <- function(model_string, jags_data, params, mcmc, quiet) {
  model_connection <- textConnection(model_string)
  # close() rather than closeAllConnections(): the latter also closes
  # connections owned by the caller (sinks, test reporters).
  on.exit(close(model_connection))
  jags_object <- rjags::jags.model(
    model_connection,
    data     = jags_data,
    n.chains = mcmc$n.chains,
    n.adapt  = mcmc$n.adapt,
    quiet    = quiet
  )
  if (mcmc$n.burnin > 0) {
    if (!quiet) message("  Burn-in: ", mcmc$n.burnin, " iterations...")
    stats::update(jags_object, n.iter = mcmc$n.burnin)
  }
  if (!quiet) message("  Sampling: ", mcmc$n.iter, " iterations x ", mcmc$n.chains, " chains...")
  samples_raw <- rjags::coda.samples(jags_object, params,
                                     n.iter = mcmc$n.iter, thin = mcmc$n.thin)
  list(
    summary = MCMCvis::MCMCsummary(samples_raw),
    samps   = as.data.frame(do.call(rbind, lapply(samples_raw, as.matrix)))
  )
}

#' Monitored nodes for a model, with beta added when modifiers are present
#' @noRd
.with_modifier_params <- function(params, modifier_design) {
  if (is.null(modifier_design)) params else c(params, "beta")
}

#' Fit one model and restore the index of a single beta term
#' @noRd
.bcnma_fit_model <- function(model_string, jags_data, params, mcmc, quiet) {
  .index_single_beta(.bcnma_run_jags(model_string, jags_data, params, mcmc, quiet))
}
