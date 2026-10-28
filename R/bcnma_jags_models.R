# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Dynamic JAGS model string builders for Bayesian cNMA.
# Models use 3D component arrays (xA, xB) instead of per-component matrices,
# making them reusable for any number of components.
# Effect-modifier blocks (beta terms) come from R/bcnma_jags_modifiers.R;
# continuous-outcome builders are in R/bcnma_jags_models_cont.R.

# ---- Internal helpers ----

# Build the component main-effect sum for the consistency equation.
# Returns strings for arm k ("A") and reference arm ("B").
# e.g. for Nc=3: "d[1]*xA[i,k,1] + d[2]*xA[i,k,2] + d[3]*xA[i,k,3]"
.jags_component_sum <- function(Nc) {
  A_terms <- paste0("d[", 1:Nc, "] * xA[i,k,", 1:Nc, "]")
  B_terms <- paste0("d[", 1:Nc, "] * xB[i,",   1:Nc, "]")
  list(
    A = paste(A_terms, collapse = " +\n                      "),
    B = paste(B_terms, collapse = " +\n                      ")
  )
}

# Build the interaction effect sum for the consistency equation.
# Returns strings for arm k ("A") and reference arm ("B").
.jags_interaction_sum <- function(Ninter) {
  A_terms <- paste0("gamma[", 1:Ninter, "] * interactions[i,k,", 1:Ninter, "]")
  B_terms <- paste0("gamma[", 1:Ninter, "] * interactions[i,1,",  1:Ninter, "]")
  list(
    A = paste(A_terms, collapse = " +\n                      "),
    B = paste(B_terms, collapse = " +\n                      ")
  )
}

# Build the SSVS/LASSO prior block for gamma interaction terms.
# - included:     integer vector of interaction indices to model stochastically
# - not_included: remaining indices forced to gamma = 0
# - prob_active:  prior P(included) for active interactions
# Uses explicit per-index JAGS code for JAGS compatibility.
.jags_ssvs_prior_block <- function(Ninter, included, prob_active) {
  not_included <- setdiff(seq_len(Ninter), included)

  lines <- character(Ninter)
  for (k in seq_len(Ninter)) {
    if (k %in% included) {
      lines[k] <- sprintf(
        paste0(
          "  ## interaction %d: stochastic (SSVS)\n",
          "  IndA[%d] ~ dcat(Pind_active[])\n",
          "  Ind[%d]  <- IndA[%d] - 1\n",
          "  gamma[%d] ~ dnorm(0, tauCov[IndA[%d]])"
        ),
        k, k, k, k, k, k
      )
    } else {
      lines[k] <- sprintf(
        paste0(
          "  ## interaction %d: excluded (fixed to 0)\n",
          "  gamma[%d] <- 0\n",
          "  Ind[%d]   <- 0"
        ),
        k, k, k
      )
    }
  }

  paste0(
    paste(lines, collapse = "\n"),
    "\n\n",
    sprintf(
      "  Pind_active[1] <- %.4f  ## P(not included)\n  Pind_active[2] <- %.4f  ## P(included)",
      1 - prob_active, prob_active
    )
  )
}

# ---- Public JAGS model builders ----

#' Build JAGS model string: additive cNMA (no interactions)
#'
#' Random-effects Bayesian cNMA with binomial likelihood and log-normal
#' heterogeneity prior. Component effects are additive (no interaction terms).
#'
#' @param Nc        integer number of components
#' @param tau_prior numeric c(meanlog, precision) of the dlnorm prior on tau2
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#'                  for no effect modifiers (then wA and Q are not needed in the data)
#' @return character JAGS model string
#' @noRd
.jags_model_additive <- function(Nc, tau_prior = c(-1.67, 1 / 1.472^2),
                                 modifier_priors = NULL) {
  cs <- .jags_component_sum(Nc)

  paste0(
    'model {\n',
    '  for (i in 1:Ns) {\n',
    '    w[i,1]     <- 0\n',
    '    theta[i,1] <- 0\n',
    '    for (k in 1:na[i]) { r[i,k] ~ dbin(p[i,k], n[i,k]) }\n',
    '    logit(p[i,1]) <- u[i]\n',
    '    for (k in 2:na[i]) {\n',
    '      logit(p[i,k]) <- u[i] + theta[i,k]\n',
    '      theta[i,k]    ~ dnorm(md[i,k], precd[i,k])\n',
    '      md[i,k]       <- mean[i,k] + sw[i,k]\n',
    '      w[i,k]        <- theta[i,k] - mean[i,k]\n',
    '      sw[i,k]       <- sum(w[i,1:(k-1)]) / (k-1)\n',
    '      precd[i,k]    <- prec * 2 * (k-1) / k\n',
    '      ## Consistency: arm k effect minus reference arm effect\n',
    '      mean[i,k]     <- (', cs$A, ')\n',
    '                      - (', cs$B, ')\n',
    .jags_modifier_lines(modifier_priors, BCNMA_INDENT_BIN),
    '    }\n',
    '  }\n',
    '\n',
    '  ## Baseline log-odds\n',
    '  for (i in 1:Ns) { u[i] ~ dnorm(0, 0.01) }\n',
    '\n',
    '  ## Heterogeneity: log-normal prior on the variance tau2 (Turner et al. 2015)\n',
    '  prec <- 1 / tau2\n',
    '  tau  <- sqrt(tau2)\n',
    '  tau2 ~ dlnorm(', sprintf("%.6f", tau_prior[1]), ', ', sprintf("%.6f", tau_prior[2]), ')\n',
    '\n',
    '  ## Component log-OR effects\n',
    '  for (k in 1:Nc) { d[k]   ~ dnorm(0, 0.01) }\n',
    '  for (k in 1:Nc) { ORd[k] <- exp(d[k]) }\n',
    .jags_modifier_prior_block(modifier_priors),
    '}'
  )
}

#' Build JAGS model string: SSVS cNMA (with pairwise interaction penalisation)
#'
#' Random-effects Bayesian cNMA with spike-and-slab (SSVS) priors on all
#' pairwise interaction terms. The spike forces near-zero interactions to zero;
#' the slab allows large interactions to be estimated freely. The scale is
#' estimated from data via the global parameter eta.
#'
#' For `interactions = "lasso"`: all Ninter terms are modelled stochastically.
#' For `interactions = "lasso_informative"`: only `included` terms are
#' stochastic; the remainder are fixed to gamma = 0.
#'
#' @param Nc          integer number of components
#' @param Ninter      integer number of pairwise interactions (= Nc*(Nc-1)/2)
#' @param included    integer vector of interaction indices to model (default all)
#' @param prob_active numeric prior P(included) for active interactions (default 0.5)
#' @param g           numeric spike-slab variance ratio (slab variance = spike variance * g)
#' @param tau_prior   numeric c(meanlog, precision) of the dlnorm prior on tau2
#' @param modifier_priors data.frame (precision, truncation) per beta term, or NULL
#'                  for no effect modifiers (then wA and Q are not needed in the data)
#' @return character JAGS model string
#' @noRd
.jags_model_ssvs <- function(Nc, Ninter,
                              included    = seq_len(Ninter),
                              prob_active = 0.5,
                              g           = 100,
                              tau_prior   = c(-1.67, 1 / 1.472^2),
                              modifier_priors = NULL) {
  cs    <- .jags_component_sum(Nc)
  is_   <- .jags_interaction_sum(Ninter)
  ssvs  <- .jags_ssvs_prior_block(Ninter, included, prob_active)

  paste0(
    'model {\n',
    '  for (i in 1:Ns) {\n',
    '    w[i,1]     <- 0\n',
    '    theta[i,1] <- 0\n',
    '    for (k in 1:na[i]) { r[i,k] ~ dbin(p[i,k], n[i,k]) }\n',
    '    logit(p[i,1]) <- u[i]\n',
    '    for (k in 2:na[i]) {\n',
    '      logit(p[i,k]) <- u[i] + theta[i,k]\n',
    '      theta[i,k]    ~ dnorm(md[i,k], precd[i,k])\n',
    '      md[i,k]       <- mean[i,k] + sw[i,k]\n',
    '      w[i,k]        <- theta[i,k] - mean[i,k]\n',
    '      sw[i,k]       <- sum(w[i,1:(k-1)]) / (k-1)\n',
    '      precd[i,k]    <- prec * 2 * (k-1) / k\n',
    '      ## Consistency with interactions\n',
    '      mean[i,k]     <- (', cs$A, ')\n',
    '                      - (', cs$B, ')\n',
    '                      + (', is_$A, ')\n',
    '                      - (', is_$B, ')\n',
    .jags_modifier_lines(modifier_priors, BCNMA_INDENT_BIN),
    '    }\n',
    '  }\n',
    '\n',
    '  ## Baseline log-odds\n',
    '  for (i in 1:Ns) { u[i] ~ dnorm(0, 0.01) }\n',
    '\n',
    '  ## Heterogeneity: log-normal prior on the variance tau2 (Turner et al. 2015)\n',
    '  prec <- 1 / tau2\n',
    '  tau  <- sqrt(tau2)\n',
    '  tau2 ~ dlnorm(', sprintf("%.6f", tau_prior[1]), ', ', sprintf("%.6f", tau_prior[2]), ')\n',
    '\n',
    '  ## Component log-OR effects\n',
    '  for (k in 1:Nc) { d[k]   ~ dnorm(0, 0.01) }\n',
    '  for (k in 1:Nc) { ORd[k] <- exp(d[k]) }\n',
    .jags_modifier_prior_block(modifier_priors),
    '\n',
    '  ## Spike-and-slab scale (learned from data)\n',
    '  ## tauCov[1] = spike precision (near 0 effect when NOT included)\n',
    sprintf('  ## tauCov[2] = slab precision (g = %.0f; allows large effect when included)\n', g),
    '  zeta      <- pow(eta, -2)\n',
    '  eta       ~ dnorm(0, 1000)I(0,)\n',
    '  tauCov[1] <- zeta\n',
    sprintf('  tauCov[2] <- zeta * %.6f  ## slab = spike / g\n', 1 / g),
    '\n',
    '  ## SSVS: Ind[k] = 1 => included (slab), Ind[k] = 0 => not included (spike)\n',
    ssvs, '\n',
    '}'
  )
}
