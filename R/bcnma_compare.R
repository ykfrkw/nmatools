# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: compare_combinations() generic and its methods for bcnma_bin and
#          bcnma_cont. Both share .combination_effect_samples(): posterior
#          draws of combination A minus combination B (main effects per unit
#          of component value, SSVS interactions on presence, effect
#          modifiers at `at`), plus a predictive interval for a new study and
#          flags for combinations or pairs that no arm has observed.
# Inputs:  fitted bcnma object, two combinations
# Outputs: printed estimate; list of quantiles and draws (invisible)
# Depends: R/bcnma_compare_helpers.R, R/bcnma_modifiers_summary.R

#' Compare posterior treatment effects for two component combinations
#'
#' Computes the posterior distribution of combination A vs. combination B,
#' accounting for main effects, (optionally) pairwise interaction terms
#' estimated by the SSVS model, and effect modifiers. For
#' \code{bcnma_bin} objects the result is an OR (or log-OR); for
#' \code{bcnma_cont} objects it is on the SMD (or MD) scale.
#'
#' @param x            fitted \code{bcnma_bin} or \code{bcnma_cont} object
#' @param combo1       combination A: character vector of component names,
#'   integer indices (value 1 each), or a named numeric vector (names =
#'   components, values = component values, e.g. doses; main effects are
#'   linear through zero, d * dose, while interactions and modifiers act on
#'   presence; as in
#'   \code{component_values})
#' @param combo2       combination B, same forms as \code{combo1}. Use
#'   \code{character(0)} or \code{integer(0)} to compare against "no
#'   components" (baseline).
#' @param model        character: \code{"ssvs"} (default) or \code{"additive"}
#'   (\code{"lasso"} is a deprecated alias of \code{"ssvs"})
#' @param exponentiate logical (\code{bcnma_bin} only): return OR (\code{TRUE},
#'   default) or log-OR (\code{FALSE})
#' @param probs        numeric vector: quantiles to return (default 95% CrI + median)
#' @param at           named list: modifier label -> covariate value
#'   (continuous) or level (ordinal) at which modified components are
#'   evaluated, for both combinations. Modifiers not named are evaluated at
#'   their center / reference level. Ignored (with a warning) when the model
#'   has no effect modifiers.
#' @param ...          further arguments (unused)
#'
#' The predictive interval for a new study adds one draw of the
#' between-study heterogeneity, N(0, tau^2), to every posterior draw of the
#' effect (common heterogeneity across contrasts); its draws use R's RNG
#' seeded with `x$seed` (the global RNG state is restored). A combination
#' that no arm of the data contains is flagged "extrapolated under
#' additivity", and pairs of its components that never occur together in an
#' arm are listed: such comparisons rest on the additivity assumption.
#'
#' @return named list (invisible):
#' \describe{
#'   \item{estimate}{named numeric: posterior quantiles}
#'   \item{prediction}{named numeric: quantiles of the predictive
#'     distribution for a new study (NULL when tau2 was not sampled)}
#'   \item{is_extrapolated}{logical c(combo1, combo2): combination observed in
#'     no arm}
#'   \item{never_copresent_pairs}{character: component pairs in either
#'     combination that never occur together in an arm}
#'   \item{combo1}{character: combination A component names}
#'   \item{combo2}{character: combination B component names (or "baseline")}
#'   \item{model}{character: model used}
#'   \item{exponentiate}{logical (\code{bcnma_bin} only)}
#'   \item{sm}{character effect measure (\code{bcnma_cont} only)}
#'   \item{n_mcmc}{integer: number of posterior samples used}
#'   \item{modifier_codes}{numeric: code of each modifier term used (NULL if none)}
#'   \item{effect_samples}{numeric vector: full posterior sample (for further use)}
#'   \item{prediction_samples}{numeric vector: predictive draws (or NULL)}
#' }
#'
#' @examples
#' \dontrun{
#' # Full CBT-I package vs. sleep hygiene only
#' compare_combinations(fit,
#'   combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
#'   combo2 = c("se"))
#'
#' # Single component vs. baseline
#' compare_combinations(fit, combo1 = "sr", combo2 = character(0))
#'
#' # Two sessions of 'th' delivered by an expert therapist vs. baseline
#' compare_combinations(fit, combo1 = c(th = 2, se = 1),
#'                      at = list(proficiency = "expert"))
#' }
#' @export
compare_combinations <- function(x, ...) UseMethod("compare_combinations")

#' @rdname compare_combinations
#' @export
compare_combinations.bcnma_bin <- function(x,
                                        combo1,
                                        combo2       = character(0),
                                        model        = c("ssvs", "additive", "lasso"),
                                        exponentiate = TRUE,
                                        probs        = c(0.025, 0.5, 0.975),
                                        at           = NULL,
                                        ...) {
  model <- .normalise_model_name(match.arg(model))
  if (!inherits(x, "bcnma_bin")) stop("'x' must be a bcnma_bin object.")

  core <- .combination_effect_samples(x, combo1, combo2, model, at)
  back_transform <- if (exponentiate) exp else identity
  effect     <- back_transform(core$effect)
  prediction <- .predictive_effect_samples(core$effect, core$tau2, x$seed)
  if (!is.null(prediction)) prediction <- back_transform(prediction)

  .print_comparison_header(core)
  estimate <- stats::quantile(effect, probs)
  cat(sprintf("  %s: %.3f [%.3f; %.3f]\n", if (exponentiate) "OR" else "log-OR",
              estimate[2], estimate[1], estimate[3]))
  comparison <- .finish_comparison(x, core, effect, prediction, probs)
  invisible(c(comparison, list(exponentiate = exponentiate)))
}

#' @rdname compare_combinations
#' @export
compare_combinations.bcnma_cont <- function(x,
                                             combo1,
                                             combo2 = character(0),
                                             model  = c("ssvs", "additive", "lasso"),
                                             probs  = c(0.025, 0.5, 0.975),
                                             at     = NULL,
                                             ...) {
  model <- .normalise_model_name(match.arg(model))
  if (!inherits(x, "bcnma_cont")) stop("'x' must be a bcnma_cont object.")

  core <- .combination_effect_samples(x, combo1, combo2, model, at)
  prediction <- .predictive_effect_samples(core$effect, core$tau2, x$seed)

  .print_comparison_header(core)
  estimate <- stats::quantile(core$effect, probs)
  cat(sprintf("  %s: %.3f [%.3f; %.3f]\n", x$sm, estimate[2], estimate[1], estimate[3]))
  comparison <- .finish_comparison(x, core, core$effect, prediction, probs)
  invisible(c(comparison, list(sm = x$sm)))
}

# ---- Shared internals ----

# Posterior draws of combination A minus combination B on the linear-predictor
# scale (log-OR, SMD or MD), plus the labels and modifier codes used.
.combination_effect_samples <- function(x, combo1, combo2, model, at) {
  components <- x$components
  values1 <- .combo_to_values(combo1, components)
  values2 <- .combo_to_values(combo2, components)
  if (all(values1 == 0)) stop("'combo1' must contain at least one component.")

  if (model == "ssvs" && is.null(x$samps_ssvs)) {
    warning("SSVS samples not available; falling back to additive model.")
    model <- "additive"
  }
  samples <- if (model == "ssvs") x$samps_ssvs else x$samps_additive
  if (is.null(samples)) stop("No MCMC samples available for model = '", model, "'.")

  effect   <- numeric(nrow(samples))
  present1 <- which(values1 != 0)
  present2 <- which(values2 != 0)

  # ---- Main effects (d per unit of component value) ----
  # Added then subtracted in this order so that 0/1 input reproduces the
  # bcnma 0.1.0 results bit for bit.
  for (j in present1) effect <- effect + .sample_column(samples, "d", j) * values1[[j]]
  for (j in present2) effect <- effect - .sample_column(samples, "d", j) * values2[[j]]

  # ---- Interaction effects (SSVS model only; defined on presence) ----
  # Pairs fixed to 0 have no gamma column and contribute nothing.
  if (model == "ssvs") {
    for (pair in all.interactions(present1, x$n_components)) {
      column <- paste0("gamma[", pair, "]")
      if (column %in% colnames(samples)) effect <- effect + samples[[column]]
    }
    for (pair in all.interactions(present2, x$n_components)) {
      column <- paste0("gamma[", pair, "]")
      if (column %in% colnames(samples)) effect <- effect - samples[[column]]
    }
  }

  # ---- Effect modifiers (same covariate values for both combinations) ----
  modifier_codes <- NULL
  if (!is.null(x$modifier_terms)) {
    modifier_codes <- .modifier_codes_at(x, at)
    terms <- x$modifier_terms
    for (q in terms$q) {
      component_index   <- match(terms$component[q], components)
      presence_contrast <- (values1[[component_index]] != 0) - (values2[[component_index]] != 0)
      weight <- modifier_codes[q] * presence_contrast
      if (weight != 0) effect <- effect + .sample_column(samples, "beta", q) * weight
    }
  } else if (!is.null(at)) {
    warning("'at' ignored: the model has no effect modifiers.")
  }

  list(
    effect         = effect,
    tau2           = samples$tau2,
    values1        = values1,
    values2        = values2,
    model          = model,
    modifier_codes = modifier_codes,
    combo1_names   = .combo_labels(combo1, values1, components),
    combo2_names   = if (length(combo2) == 0) "baseline" else
      .combo_labels(combo2, values2, components)
  )
}

.print_comparison_header <- function(core) {
  cat(sprintf("Comparison: [%s] vs. [%s] | model: %s\n",
              paste(core$combo1_names, collapse = "+"),
              paste(core$combo2_names, collapse = "+"),
              core$model))
}

# Predictive interval, extrapolation flags and the returned list, shared by
# both methods (effect and prediction are on the reported scale).
.finish_comparison <- function(x, core, effect, prediction, probs) {
  prediction_quantiles <- if (is.null(prediction)) NULL else stats::quantile(prediction, probs)
  if (!is.null(prediction_quantiles)) {
    cat(sprintf("  Predictive interval (new study): [%.3f; %.3f]\n",
                prediction_quantiles[1], prediction_quantiles[length(probs)]))
  }
  flags <- .combination_flags(x, core$values1, core$values2)
  .print_comparison_notes(flags, core)
  list(
    estimate              = stats::quantile(effect, probs),
    prediction            = prediction_quantiles,
    combo1                = core$combo1_names,
    combo2                = core$combo2_names,
    model                 = core$model,
    n_mcmc                = length(effect),
    modifier_codes        = core$modifier_codes,
    is_extrapolated       = flags$is_extrapolated,
    never_copresent_pairs = flags$never_copresent_pairs,
    effect_samples        = effect,
    prediction_samples    = prediction
  )
}
