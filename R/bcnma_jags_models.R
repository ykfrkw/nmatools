# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (rewritten 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: assemble the JAGS code of every bcnma model from a few small
#          blocks, written in NICE DSU TSD2 notation (Dias et al.) so that a
#          reader of TSD2 can check them line by line:
#            likelihood (binomial / normal)  x  linear predictor
#            (additive / ssvs / standard)  +  priors.
#          Within-study contrasts are passed as data (x, xint, z: arm k minus
#          arm 1), so the consistency equation is one line of inprod() terms.
#          Optional blocks (interactions, modifiers) are added only when
#          present because JAGS cannot loop over an empty range of a data
#          array.
# Inputs:  `model_settings` list built by .bcnma_model_settings()
#          (R/bcnma_fit.R)
# Outputs: character JAGS model code; bcnma_model_code() prints it
# Depends: base R only

#' JAGS code of one bcnma model
#'
#' @param model          "additive", "ssvs" or "standard"
#' @param model_settings list(likelihood, effect_prior_sd, baseline_prior_sd,
#'   tau_prior, modifier_priors, ssvs) from .bcnma_model_settings()
#' @return character JAGS model code
#' @noRd
.build_jags_model_code <- function(model, model_settings) {
  paste0(
    "model {\n",
    .jags_study_loop(model, model_settings),
    .jags_heterogeneity_block(model_settings$tau_prior),
    .jags_effect_block(model, model_settings),
    .jags_modifier_block(model_settings$modifier_priors, model),
    if (model == "ssvs") .jags_ssvs_block(model_settings$ssvs) else "",
    "}\n"
  )
}

# Study loop: likelihood, random effects with the TSD2 multi-arm correction,
# and the consistency equation of the chosen model.
.jags_study_loop <- function(model, model_settings) {
  paste0(
    "  for (i in 1:Ns) {\n",
    "    w[i,1]     <- 0\n",
    "    delta[i,1] <- 0\n",
    sprintf("    u[i] ~ dnorm(0, %s)    # study baseline (arm 1)\n",
            .format_precision(model_settings$baseline_prior_sd)),
    "    for (k in 1:na[i]) {\n",
    .jags_likelihood_lines(model_settings$likelihood),
    "    }\n",
    "    resdev_study[i] <- sum(dev[i,1:na[i]])\n",
    "    for (k in 2:na[i]) {\n",
    "      delta[i,k] ~ dnorm(md[i,k], precd[i,k])    # trial-specific effect\n",
    "      md[i,k]    <- mean[i,k] + sw[i,k]\n",
    "      w[i,k]     <- delta[i,k] - mean[i,k]\n",
    "      sw[i,k]    <- sum(w[i,1:(k-1)]) / (k-1)    # multi-arm correction\n",
    "      precd[i,k] <- prec * 2 * (k-1) / k\n",
    .jags_consistency_line(model, model_settings),
    "    }\n",
    "  }\n",
    "  resdev <- sum(resdev_study[])    # total residual deviance\n"
  )
}

# Residual deviance (TSD2): the binomial form uses the saturated
# log-likelihood (data loglik_sat) because 0 * log(0) is NaN in JAGS when an
# arm has 0 or n events; it equals TSD2's 2 * (r log(r / rhat) + ...).
.jags_likelihood_lines <- function(likelihood) {
  if (likelihood == "binomial") {
    return(paste0(
      "      r[i,k] ~ dbin(p[i,k], n[i,k])\n",
      "      logit(p[i,k]) <- u[i] + delta[i,k]\n",
      "      dev[i,k] <- -2 * (logdensity.bin(r[i,k], p[i,k], n[i,k]) - loglik_sat[i,k])\n"
    ))
  }
  paste0(
    "      y[i,k] ~ dnorm(theta[i,k], pow(se[i,k], -2))\n",
    "      theta[i,k] <- u[i] + delta[i,k]\n",
    "      dev[i,k] <- pow(y[i,k] - theta[i,k], 2) / pow(se[i,k], 2)\n"
  )
}

# mean[i,k]: expected effect of arm k vs arm 1 of study i.
.jags_consistency_line <- function(model, model_settings) {
  if (model == "standard") {
    return("      mean[i,k]  <- dcomb[t[i,k]] - dcomb[t[i,1]]\n")
  }
  terms <- "inprod(d[], x[i,k,])"
  if (model == "ssvs") terms <- c(terms, "inprod(gamma[], xint[i,k,])")
  if (!is.null(model_settings$modifier_priors)) terms <- c(terms, "inprod(beta[], z[i,k,])")
  paste0("      mean[i,k]  <- ", paste(terms, collapse = "\n                    + "), "\n")
}

# The stochastic node is the parameter the prior is defined on: tau2 for
# log-normal priors (the form of Turner et al. 2015), tau for half-normal priors.
.jags_heterogeneity_block <- function(tau_prior) {
  prior_lines <- if (tau_prior$family == "half_normal_tau") {
    paste0(sprintf("  tau  ~ dnorm(0, %s) T(0,)\n", .format_precision(tau_prior$sd)),
           "  tau2 <- pow(tau, 2)\n")
  } else {
    paste0(sprintf("  tau2 ~ dlnorm(%s, %s)\n", .format_number(tau_prior$meanlog),
                   .format_number(tau_prior$precision)),
           "  tau  <- sqrt(tau2)\n")
  }
  paste0("\n  # Heterogeneity: ", tau_prior$label, "\n", prior_lines, "  prec <- 1 / tau2\n")
}

# Component effects d[j] (or one node per combination for the standard model).
.jags_effect_block <- function(model, model_settings) {
  effect_precision <- .format_precision(model_settings$effect_prior_sd)
  if (model == "standard") {
    return(paste0(
      "\n  # One effect per observed combination; combination 1 is the reference\n",
      "  dcomb[1] <- 0\n",
      sprintf("  for (c in 2:Ncomb) { dcomb[c] ~ dnorm(0, %s) }\n", effect_precision)
    ))
  }
  paste0(
    "\n  # Component effects (per unit of component value)\n",
    sprintf("  for (j in 1:Nc) { d[j] ~ dnorm(0, %s) }\n", effect_precision),
    if (model_settings$likelihood == "binomial") "  for (j in 1:Nc) { ORd[j] <- exp(d[j]) }\n"
    else ""
  )
}

# beta[q]: change in the effect of a modified component per code unit. One
# line per term because priors (and monotone truncation) differ by term.
.jags_modifier_block <- function(modifier_priors, model) {
  if (is.null(modifier_priors) || model == "standard") return("")
  truncation <- ifelse(modifier_priors$truncation == "", "",
                       paste0(" ", modifier_priors$truncation))
  prior_lines <- sprintf("  beta[%d] ~ dnorm(0, %s)%s\n", seq_len(nrow(modifier_priors)),
                         .format_number(modifier_priors$precision), truncation)
  paste0("\n  # Effect modifiers\n", paste(prior_lines, collapse = ""))
}

# Spike-and-slab (George & McCulloch 1993): Ind[m] = 1 puts gamma[m] in the
# slab (SD eta * sqrt(g)), Ind[m] = 0 in the spike (SD eta).
.jags_ssvs_block <- function(ssvs_settings) {
  paste0(
    "\n  # SSVS interactions\n",
    "  for (m in 1:Nint) {\n",
    "    IndA[m]  ~ dcat(p_incl[])\n",
    "    Ind[m]   <- IndA[m] - 1\n",
    "    gamma[m] ~ dnorm(0, prec_gamma[IndA[m]])\n",
    "  }\n",
    sprintf("  p_incl[1] <- %s\n", .format_number(1 - ssvs_settings$inclusion_prob)),
    sprintf("  p_incl[2] <- %s\n", .format_number(ssvs_settings$inclusion_prob)),
    sprintf("  eta ~ dnorm(0, %s) T(0,)\n", .format_precision(ssvs_settings$eta_prior_sd)),
    "  prec_gamma[1] <- pow(eta, -2)    # spike\n",
    sprintf("  prec_gamma[2] <- pow(eta, -2) / %s    # slab = g x spike variance\n",
            .format_number(ssvs_settings$slab_ratio))
  )
}

# Numbers are printed with up to 6 significant digits so that the code reads
# like hand-written JAGS (0.01, not 0.010000).
.format_number <- function(value) as.character(signif(value, 6))
.format_precision <- function(prior_sd) .format_number(1 / prior_sd^2)

#' Print the JAGS code of a fitted bcnma model
#'
#' Shows the exact model text that was compiled (stored in
#' `fit$model_code`). For `model = "standard"` the full-interaction
#' (standard NMA) model of [check_additivity()], one effect per observed
#' combination, is built
#' from the fit.
#'
#' @param fit   a `bcnma_bin` or `bcnma_cont` object
#' @param model `"additive"`, `"ssvs"` or `"standard"`
#' @return the model code, invisibly
#' @export
bcnma_model_code <- function(fit, model = c("additive", "ssvs", "standard")) {
  model <- match.arg(model)
  model_code <- fit$model_code[[model]]
  if (is.null(model_code) && model == "standard") {
    model_code <- .build_jags_model_code("standard", fit$model_settings)
  }
  if (is.null(model_code)) stop("No '", model, "' model in this fit.")
  cat(model_code)
  invisible(model_code)
}
