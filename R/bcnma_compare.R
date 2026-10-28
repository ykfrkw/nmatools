# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# compare_combinations() generic and its methods for bcnma_bin and bcnma_cont.
# Both methods share .combination_effect_samples(): posterior draws of
# combination A minus combination B (main effects per unit of component
# value, SSVS interactions on presence, effect modifiers at `at`). Moved out of
# methods.R / methods_cont.R of bcnma (2026-10-28) when component values and effect
# modifiers were added.

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
#'   components, values = component values, e.g. doses, as in
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
#' @return named list (invisible):
#' \describe{
#'   \item{estimate}{named numeric: posterior quantiles}
#'   \item{combo1}{character: combination A component names}
#'   \item{combo2}{character: combination B component names (or "baseline")}
#'   \item{model}{character: model used}
#'   \item{exponentiate}{logical (\code{bcnma_bin} only)}
#'   \item{sm}{character effect measure (\code{bcnma_cont} only)}
#'   \item{n_mcmc}{integer: number of posterior samples used}
#'   \item{modifier_codes}{numeric: code of each modifier term used (NULL if none)}
#'   \item{effect_samples}{numeric vector: full posterior sample (for further use)}
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

  core   <- .combination_effect_samples(x, combo1, combo2, model, at)
  effect <- core$effect
  if (exponentiate) effect <- exp(effect)

  .print_comparison_header(core)
  cat(sprintf("  %s: ", if (exponentiate) "OR" else "log-OR"))
  est <- stats::quantile(effect, probs)
  cat(sprintf("%.3f [%.3f; %.3f]\n", est[2], est[1], est[3]))

  invisible(list(
    estimate       = est,
    combo1         = core$combo1_names,
    combo2         = core$combo2_names,
    model          = core$model,
    exponentiate   = exponentiate,
    n_mcmc         = length(effect),
    modifier_codes = core$modifier_codes,
    effect_samples = effect
  ))
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

  core   <- .combination_effect_samples(x, combo1, combo2, model, at)
  effect <- core$effect

  .print_comparison_header(core)
  est <- stats::quantile(effect, probs)
  cat(sprintf("  %s: %.3f [%.3f; %.3f]\n", x$sm, est[2], est[1], est[3]))

  invisible(list(
    estimate       = est,
    combo1         = core$combo1_names,
    combo2         = core$combo2_names,
    model          = core$model,
    sm             = x$sm,
    n_mcmc         = length(effect),
    modifier_codes = core$modifier_codes,
    effect_samples = effect
  ))
}

# ---- Shared internals ----

# Posterior draws of combination A minus combination B on the linear-predictor
# scale (log-OR, SMD or MD), plus the labels and modifier codes used.
.combination_effect_samples <- function(x, combo1, combo2, model, at) {
  comps <- x$components
  Nc    <- x$n_components

  values1 <- .combo_to_values(combo1, comps)
  values2 <- .combo_to_values(combo2, comps)
  if (all(values1 == 0)) stop("'combo1' must contain at least one component.")

  if (model == "ssvs" && is.null(x$samps_ssvs)) {
    warning("SSVS samples not available; falling back to additive model.")
    model <- "additive"
  }
  samps <- if (model == "ssvs") x$samps_ssvs else x$samps_additive
  if (is.null(samps)) stop("No MCMC samples available for model = '", model, "'.")

  effect   <- numeric(nrow(samps))
  present1 <- which(values1 != 0)
  present2 <- which(values2 != 0)

  # ---- Main effects (d per unit of component value) ----
  # Added then subtracted in this order so that 0/1 input reproduces the
  # bcnma 0.1.0 results bit for bit.
  for (j in present1) effect <- effect + .sample_column(samps, "d", j) * values1[[j]]
  for (j in present2) effect <- effect - .sample_column(samps, "d", j) * values2[[j]]

  # ---- Interaction effects (SSVS model only; defined on presence) ----
  if (model == "ssvs") {
    for (k in all.interactions(present1, Nc)) {
      col <- paste0("gamma[", k, "]")
      if (col %in% colnames(samps)) effect <- effect + samps[[col]]
    }
    for (k in all.interactions(present2, Nc)) {
      col <- paste0("gamma[", k, "]")
      if (col %in% colnames(samps)) effect <- effect - samps[[col]]
    }
  }

  # ---- Effect modifiers (same covariate values for both combinations) ----
  modifier_codes <- NULL
  if (!is.null(x$modifier_terms)) {
    modifier_codes <- .modifier_codes_at(x, at)
    terms <- x$modifier_terms
    for (q in terms$q) {
      component_index   <- match(terms$component[q], comps)
      presence_contrast <- (values1[[component_index]] != 0) - (values2[[component_index]] != 0)
      weight <- modifier_codes[q] * presence_contrast
      if (weight != 0) effect <- effect + .sample_column(samps, "beta", q) * weight
    }
  } else if (!is.null(at)) {
    warning("'at' ignored: the model has no effect modifiers.")
  }

  list(
    effect         = effect,
    model          = model,
    modifier_codes = modifier_codes,
    combo1_names   = .combo_labels(combo1, values1, comps),
    combo2_names   = if (length(combo2) == 0) "baseline" else .combo_labels(combo2, values2, comps)
  )
}

.print_comparison_header <- function(core) {
  cat(sprintf("Comparison: [%s] vs. [%s] | model: %s\n",
              paste(core$combo1_names, collapse = "+"),
              paste(core$combo2_names, collapse = "+"),
              core$model))
}

# Convert a combination (names, indices, or named values) into a numeric
# vector of component values aligned with `comps`.
.combo_to_values <- function(combo, comps) {
  values <- stats::setNames(numeric(length(comps)), comps)
  if (length(combo) == 0) return(values)
  if (is.numeric(combo) && !is.null(names(combo))) {
    unknown <- setdiff(names(combo), comps)
    if (length(unknown) > 0 || anyNA(combo)) {
      stop("Named numeric combination must use known component names and no NA. Unknown: ",
           paste(unknown, collapse = ", "))
    }
    values[names(combo)] <- combo
    return(values)
  }
  if (is.character(combo)) {
    unknown <- setdiff(combo, comps)
    if (length(unknown) > 0) {
      stop("Unknown components: ", paste(unknown, collapse = ", "),
           "\nAvailable: ", paste(comps, collapse = ", "))
    }
    values[comps %in% combo] <- 1
    return(values)
  }
  idx <- as.integer(combo)
  if (any(idx < 1 | idx > length(comps))) {
    stop("Component indices out of range [1, ", length(comps), "]")
  }
  values[idx] <- 1
  values
}

# Labels for printing; non-unit values are shown as "comp=value".
.combo_labels <- function(combo, values, comps) {
  if (is.character(combo)) return(combo)
  present <- which(values != 0)
  unname(ifelse(values[present] == 1, comps[present], paste0(comps[present], "=", values[present])))
}

# Fetch "<prefix>[index]" from the sample data frame or fail loudly.
.sample_column <- function(samps, prefix, index) {
  col <- paste0(prefix, "[", index, "]")
  if (!col %in% colnames(samps)) stop("Column '", col, "' not found in MCMC samples.")
  samps[[col]]
}
