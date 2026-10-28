# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: model fit statistics of every bcnma model (NICE DSU TSD2 / Dias
#          et al.): the posterior mean of the total residual deviance
#          (`resdev`, compared with the number of data points), the effective
#          number of parameters pD and DIC from rjags::dic.samples(), and the
#          posterior mean residual deviance of every arm (used by
#          check_additivity()).
# Inputs:  `prep`, the fitted model list (samples with resdev, dic)
# Outputs: model_fit rows, per-arm deviance tables, console line
# Depends: base R

#' Saturated log-likelihood of each arm (`[study, arm]` matrix)
#'
#' Binomial: log dbinom(r | n, r / n); normal: log dnorm(y | y, se). The
#' residual deviance of an arm is -2 x (log-likelihood - this).
#' @noRd
.saturated_loglik <- function(prep) {
  outcome <- prep$outcome
  if (prep$likelihood == "binomial") {
    return(stats::dbinom(outcome$r, outcome$n, outcome$r / outcome$n, log = TRUE))
  }
  stats::dnorm(outcome$y, outcome$y, outcome$se, log = TRUE)
}

#' Posterior mean residual deviance and pD contribution of every arm
#'
#' @param dic  list(deviance, penalty) named by observed node, or NULL
#' @param prep prepared data
#' @return data.frame(study, arm, treat, residual_deviance, pD), or NULL
#' @noRd
.arm_residual_deviance <- function(dic, prep) {
  if (is.null(dic)) return(NULL)
  node_index <- do.call(rbind, lapply(strsplit(gsub("^.*\\[|\\]$", "", names(dic$deviance)),
                                               ","), as.integer))
  saturated  <- .saturated_loglik(prep)[node_index]
  arm_table  <- prep$arm_table
  arm_row    <- match(paste(node_index[, 1], node_index[, 2]),
                      paste(arm_table$study_index, arm_table$arm))
  arm_deviance <- data.frame(
    study = arm_table$study[arm_row], arm = node_index[, 2], treat = arm_table$treat[arm_row],
    residual_deviance = as.numeric(dic$deviance) + 2 * saturated,
    pD = as.numeric(dic$penalty), stringsAsFactors = FALSE)
  arm_deviance[order(arm_row), ]
}

#' One row of the model fit table
#'
#' @param model_name     "additive", "ssvs" or "standard"
#' @param fitted_model   list with samples (resdev) and dic, or NULL
#' @param prep           prepared data
#' @return data.frame(model, mean_resdev, n_datapoints, pD, DIC) or NULL
#' @noRd
.model_fit_row <- function(model_name, fitted_model, prep) {
  if (is.null(fitted_model)) return(NULL)
  dic <- fitted_model$dic
  mean_deviance <- if (is.null(dic)) NA_real_ else sum(dic$deviance)
  effective_parameters <- if (is.null(dic)) NA_real_ else sum(dic$penalty)
  data.frame(model = model_name, mean_resdev = mean(fitted_model$samples$resdev),
             n_datapoints = sum(prep$n_arms), pD = effective_parameters,
             DIC = mean_deviance + effective_parameters, stringsAsFactors = FALSE)
}

#' Console line with the fit statistics of one model
#' @noRd
.print_model_fit <- function(model_fit, model_name) {
  if (is.null(model_fit)) return(invisible(NULL))
  row <- model_fit[model_fit$model == model_name, , drop = FALSE]
  if (nrow(row) == 0) return(invisible(NULL))
  dic_text <- if (is.na(row$DIC)) "pD / DIC need >= 2 chains" else
    sprintf("pD %.1f, DIC %.1f", row$pD, row$DIC)
  cat(sprintf("Model fit: mean residual deviance %.1f (%d data points), %s\n",
              row$mean_resdev, as.integer(row$n_datapoints), dic_text))
}
