# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: check the additivity assumption of a fitted bcnma model against
#          the standard (full-interaction) NMA: same likelihood, same
#          random-effects structure and multi-arm correction, but one effect
#          dcomb[c] per observed combination of components. Compares model
#          fit (resdev, pD, DIC), per-combination effects (standard vs
#          additive) and per-arm residual deviance.
# Inputs:  fitted bcnma_bin / bcnma_cont object (without modifiers)
# Outputs: object of class "bcnma_additivity" (list of tables)
# Depends: R/bcnma_jags_models.R, R/bcnma_jags_run.R, R/bcnma_model_fit.R

#' Combinations observed in the arms, and each arm's combination index
#'
#' The reference (index 1) is the combination with no component when it is
#' observed, otherwise the most frequent combination.
#' @param prep prepared data
#' @return list(labels, values (matrix combination x component), arm_index
#'   (`[study, arm]` matrix), n_arms, n_studies, reference_is_inactive)
#' @noRd
.observed_combinations <- function(prep) {
  arm_table  <- prep$arm_table
  arm_values <- t(vapply(seq_len(nrow(arm_table)), function(row) {
    prep$x_arm[arm_table$study_index[row], arm_table$arm[row], ]
  }, numeric(prep$n_components)))
  arm_values <- matrix(arm_values, ncol = prep$n_components)
  arm_key    <- apply(arm_values, 1, paste, collapse = "|")
  arm_counts <- table(arm_key)
  is_empty   <- rowSums(arm_values != 0) == 0
  reference_key <- if (any(is_empty)) arm_key[is_empty][1] else names(which.max(arm_counts))
  keys <- c(reference_key, setdiff(unique(arm_key), reference_key))
  values <- arm_values[match(keys, arm_key), , drop = FALSE]
  colnames(values) <- prep$components
  arm_index <- matrix(NA_integer_, prep$n_studies, prep$max_arms)
  arm_index[cbind(arm_table$study_index, arm_table$arm)] <- match(arm_key, keys)
  list(labels = apply(values, 1, .combination_label, components = prep$components),
       values = values, arm_index = arm_index,
       n_arms = as.integer(arm_counts[keys]),
       n_studies = vapply(keys, function(key) {
         length(unique(arm_table$study[arm_key == key]))
       }, integer(1), USE.NAMES = FALSE),
       reference_is_inactive = any(is_empty))
}

.combination_label <- function(values, components) {
  is_present <- values != 0
  if (!any(is_present)) return("(no component)")
  labels <- ifelse(values[is_present] == 1, components[is_present],
                   paste0(components[is_present], "=", values[is_present]))
  paste(labels, collapse = "+")
}

#' Check additivity against the standard (full-interaction) NMA
#'
#' Fits the standard random-effects NMA with one node per observed
#' combination (`dcomb`, reference = the combination with no component if
#' observed, otherwise the most frequent one) using the same likelihood,
#' priors, heterogeneity prior, multi-arm correction and MCMC settings (and
#' seed) as `fit`, and compares it with the additive (and SSVS) models of
#' `fit`. Print the standard model with `bcnma_model_code(fit, "standard")`.
#'
#' @param fit a `bcnma_bin` or `bcnma_cont` object fitted without `modifiers`
#' @param quiet logical: suppress progress messages (default `TRUE`)
#' @param ... unused
#' @return object of class `bcnma_additivity`, a list with
#' \describe{
#'   \item{model_fit}{mean residual deviance, data points, pD and DIC of the
#'     additive, SSVS (if fitted) and standard models}
#'   \item{combinations}{per combination: arms, studies, standard and additive
#'     posterior median and 95% CrI vs the reference, and `is_discrepant`
#'     (the two 95% CrIs do not overlap)}
#'   \item{arm_deviance}{per arm posterior mean residual deviance under the
#'     standard and additive models and their difference (NA with one chain)}
#'   \item{reference}{label of the reference combination}
#' }
#' @export
check_additivity <- function(fit, quiet = TRUE, ...) {
  if (!inherits(fit, c("bcnma_bin", "bcnma_cont"))) {
    stop("'fit' must be a bcnma_bin or bcnma_cont object.")
  }
  if (!is.null(fit$modifier_terms)) {
    stop("check_additivity() compares models without effect modifiers; refit without ",
         "'modifiers' to check additivity.", call. = FALSE)
  }
  prep <- fit$prep
  combinations <- .observed_combinations(prep)
  standard <- .fit_standard_model(fit, prep, combinations, quiet)
  model_fit <- rbind(fit$model_fit, .model_fit_row("standard", standard, prep))
  result <- list(
    model_fit    = model_fit,
    combinations = .compare_combination_effects(fit, standard, combinations),
    arm_deviance = .compare_arm_deviance(fit$arm_deviance$additive, standard$arm_deviance),
    reference    = combinations$labels[1],
    standard_summary = standard$summary
  )
  structure(result, class = "bcnma_additivity")
}

.fit_standard_model <- function(fit, prep, combinations, quiet) {
  model_code <- .build_jags_model_code("standard", fit$model_settings)
  jags_data  <- c(prep$outcome, list(Ns = prep$n_studies, na = prep$n_arms,
                                     t = combinations$arm_index,
                                     Ncomb = length(combinations$labels)))
  if (prep$likelihood == "binomial") jags_data$loglik_sat <- .saturated_loglik(prep)
  mcmc <- c(fit$mcmc_settings, list(inits = fit$inits))
  if (!quiet) message("--- Fitting standard (full-interaction) NMA model ---")
  posterior <- .bcnma_run_jags(model_code, jags_data, c("dcomb", "tau", "tau2", "resdev"),
                               mcmc, quiet)
  posterior$arm_deviance <- .arm_residual_deviance(posterior$dic, prep)
  posterior
}

# Posterior median and 95% CrI of each combination vs the reference.
.compare_combination_effects <- function(fit, standard, combinations) {
  additive_draws <- as.matrix(fit$samps_additive[paste0("d[", seq_len(fit$n_components), "]")])
  value_contrast <- sweep(combinations$values, 2, combinations$values[1, ])
  rows <- lapply(seq_along(combinations$labels)[-1], function(combination) {
    standard_q <- stats::quantile(standard$samples[[paste0("dcomb[", combination, "]")]],
                                  BCNMA_CREDIBLE_PROBS, names = FALSE)
    additive_q <- stats::quantile(additive_draws %*% value_contrast[combination, ],
                                  BCNMA_CREDIBLE_PROBS, names = FALSE)
    data.frame(combination = combinations$labels[combination],
               n_arms = combinations$n_arms[combination],
               n_studies = combinations$n_studies[combination],
               standard_median = standard_q[2], standard_lower = standard_q[1],
               standard_upper = standard_q[3], additive_median = additive_q[2],
               additive_lower = additive_q[1], additive_upper = additive_q[3],
               is_discrepant = standard_q[1] > additive_q[3] || additive_q[1] > standard_q[3],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.compare_arm_deviance <- function(additive_deviance, standard_deviance) {
  if (is.null(additive_deviance) || is.null(standard_deviance)) return(NULL)
  arm_deviance <- data.frame(
    study = standard_deviance$study, arm = standard_deviance$arm,
    treat = standard_deviance$treat,
    resdev_standard = standard_deviance$residual_deviance,
    resdev_additive = additive_deviance$residual_deviance[
      match(paste(standard_deviance$study, standard_deviance$arm),
            paste(additive_deviance$study, additive_deviance$arm))],
    stringsAsFactors = FALSE)
  arm_deviance$difference <- arm_deviance$resdev_additive - arm_deviance$resdev_standard
  arm_deviance[order(-arm_deviance$difference), ]
}

#' @export
print.bcnma_additivity <- function(x, digits = 2, ...) {
  cat("Additivity check: additive cNMA vs standard (full-interaction) NMA\n")
  cat("Reference combination:", x$reference, "\n\n")
  print(x$model_fit, digits = digits + 2, row.names = FALSE)
  n_discrepant <- sum(x$combinations$is_discrepant)
  cat(sprintf("\nCombinations whose standard and additive 95%% CrIs do not overlap: %d of %d\n",
              n_discrepant, nrow(x$combinations)))
  if (n_discrepant > 0) {
    print(x$combinations[x$combinations$is_discrepant, ], digits = digits, row.names = FALSE)
  }
  if (!is.null(x$arm_deviance)) {
    cat("\nArms with the largest residual deviance increase under additivity:\n")
    print(utils::head(x$arm_deviance, 5), digits = digits, row.names = FALSE)
  }
  invisible(x)
}
