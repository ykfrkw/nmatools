# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# JAGS model string builders for continuous-outcome Bayesian cNMA (normal
# arm-level likelihood; SMD or MD scale), used by bcnma_cont(). Split from
# R/bcnma_jags_models.R (binary models and shared helpers) when bcnma was
# merged into nmatools; the model strings themselves are unchanged.

# ============================================================================
# Continuous outcome models (Normal arm-level likelihood; SMD or MD scale)
# ============================================================================

#' Build JAGS model string: additive cNMA for continuous outcomes
#'
#' Random-effects Bayesian cNMA with normal arm-level likelihood. Component
#' effects \code{d[k]} are on the SMD (or MD) scale. Within-arm precision
#' (\code{prec[i,k]} = 1 / SE^2) is treated as known and supplied as data.
#'
#' @param Nc        integer number of components
#' @param tau_prior numeric c(meanlog, precision) of the dlnorm prior on tau2
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#'                  for no effect modifiers (then wA and Q are not needed in the data)
#' @return character JAGS model string
#' @noRd
.jags_model_additive_cont <- function(Nc, tau_prior = c(-1.0, 1 / 0.8^2),
                                      modifier_priors = NULL) {
  cs <- .jags_component_sum(Nc)

  paste0(
    'model {\n',
    '  for (i in 1:Ns) {\n',
    '    w[i,1]     <- 0\n',
    '    theta[i,1] <- 0\n',
    '    for (k in 1:na[i]) { y[i,k] ~ dnorm(mu[i,k], prec[i,k]) }\n',
    '    mu[i,1] <- u[i]\n',
    '    for (k in 2:na[i]) {\n',
    '      mu[i,k]    <- u[i] + theta[i,k]\n',
    '      theta[i,k] ~ dnorm(md[i,k], precd[i,k])\n',
    '      md[i,k]    <- mean[i,k] + sw[i,k]\n',
    '      w[i,k]     <- theta[i,k] - mean[i,k]\n',
    '      sw[i,k]    <- sum(w[i,1:(k-1)]) / (k-1)\n',
    '      precd[i,k] <- prec_het * 2 * (k-1) / k\n',
    '      ## Consistency: arm k effect minus reference arm effect\n',
    '      mean[i,k]  <- (', cs$A, ')\n',
    '                    - (', cs$B, ')\n',
    .jags_modifier_lines(modifier_priors, BCNMA_INDENT_CONT),
    '    }\n',
    '  }\n',
    '\n',
    '  ## Baseline means\n',
    '  for (i in 1:Ns) { u[i] ~ dnorm(0, 0.001) }\n',
    '\n',
    '  ## Heterogeneity: log-normal prior on the variance tau2\n',
    '  prec_het <- 1 / tau2\n',
    '  tau      <- sqrt(tau2)\n',
    '  tau2     ~ dlnorm(', sprintf("%.6f", tau_prior[1]), ', ', sprintf("%.6f", tau_prior[2]), ')\n',
    '\n',
    '  ## Component SMD (or MD) effects\n',
    '  for (k in 1:Nc) { d[k] ~ dnorm(0, 0.001) }\n',
    .jags_modifier_prior_block(modifier_priors, "SMD (or MD)", add_odds_ratio = FALSE),
    '}'
  )
}

#' Build JAGS model string: SSVS cNMA for continuous outcomes
#'
#' Normal arm-level likelihood with spike-and-slab priors on pairwise
#' interaction terms. Component effects \code{d[k]} and interaction effects
#' \code{gamma[m]} are on the SMD (or MD) scale.
#'
#' @param Nc          integer number of components
#' @param Ninter      integer number of pairwise interactions
#' @param included    integer vector of interaction indices to model
#' @param prob_active numeric prior P(included)
#' @param g           numeric spike-slab variance ratio
#' @param tau_prior   numeric c(meanlog, precision) of the dlnorm prior on tau2
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#'                  for no effect modifiers (then wA and Q are not needed in the data)
#' @return character JAGS model string
#' @noRd
.jags_model_ssvs_cont <- function(Nc, Ninter,
                                   included    = seq_len(Ninter),
                                   prob_active = 0.5,
                                   g           = 100,
                                   tau_prior   = c(-1.0, 1 / 0.8^2),
                                   modifier_priors = NULL) {
  cs   <- .jags_component_sum(Nc)
  is_  <- .jags_interaction_sum(Ninter)
  ssvs <- .jags_ssvs_prior_block(Ninter, included, prob_active)

  paste0(
    'model {\n',
    '  for (i in 1:Ns) {\n',
    '    w[i,1]     <- 0\n',
    '    theta[i,1] <- 0\n',
    '    for (k in 1:na[i]) { y[i,k] ~ dnorm(mu[i,k], prec[i,k]) }\n',
    '    mu[i,1] <- u[i]\n',
    '    for (k in 2:na[i]) {\n',
    '      mu[i,k]    <- u[i] + theta[i,k]\n',
    '      theta[i,k] ~ dnorm(md[i,k], precd[i,k])\n',
    '      md[i,k]    <- mean[i,k] + sw[i,k]\n',
    '      w[i,k]     <- theta[i,k] - mean[i,k]\n',
    '      sw[i,k]    <- sum(w[i,1:(k-1)]) / (k-1)\n',
    '      precd[i,k] <- prec_het * 2 * (k-1) / k\n',
    '      ## Consistency with interactions\n',
    '      mean[i,k]  <- (', cs$A, ')\n',
    '                    - (', cs$B, ')\n',
    '                    + (', is_$A, ')\n',
    '                    - (', is_$B, ')\n',
    .jags_modifier_lines(modifier_priors, BCNMA_INDENT_CONT),
    '    }\n',
    '  }\n',
    '\n',
    '  ## Baseline means\n',
    '  for (i in 1:Ns) { u[i] ~ dnorm(0, 0.001) }\n',
    '\n',
    '  ## Heterogeneity: log-normal prior on the variance tau2\n',
    '  prec_het <- 1 / tau2\n',
    '  tau      <- sqrt(tau2)\n',
    '  tau2     ~ dlnorm(', sprintf("%.6f", tau_prior[1]), ', ', sprintf("%.6f", tau_prior[2]), ')\n',
    '\n',
    '  ## Component SMD (or MD) effects\n',
    '  for (k in 1:Nc) { d[k] ~ dnorm(0, 0.001) }\n',
    .jags_modifier_prior_block(modifier_priors, "SMD (or MD)", add_odds_ratio = FALSE),
    '\n',
    '  ## Spike-and-slab scale (learned from data)\n',
    '  zeta      <- pow(eta, -2)\n',
    '  eta       ~ dnorm(0, 1000)I(0,)\n',
    '  tauCov[1] <- zeta\n',
    sprintf('  tauCov[2] <- zeta * %.6f\n', 1 / g),
    '\n',
    '  ## SSVS: Ind[k] = 1 => included (slab), Ind[k] = 0 => not included (spike)\n',
    ssvs, '\n',
    '}'
  )
}
