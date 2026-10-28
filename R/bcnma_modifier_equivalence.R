# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: modifier_equivalence(): a region-of-practical-equivalence (ROPE)
#          summary of an effect modifier. The quantity is the difference in
#          a component's effect between the highest and the lowest modifier
#          value (continuous: beta per unit x range; ordinal: the sum of the
#          increments), compared with a pre-specified margin.
# Inputs:  fitted bcnma object with modifiers, label, margin
# Outputs: data.frame, one row per modified component (printed)
# Depends: R/bcnma_modifiers_summary.R (BCNMA_CREDIBLE_PROBS)

#' Equivalence (ROPE) summary of an effect modifier
#'
#' For each component modified by the modifier `label`, computes the
#' posterior of the difference in the component effect between the highest
#' and the lowest modifier value: for a continuous modifier `beta_unit * (to -
#' from)`, by default over the observed range of the covariate in arms where
#' the component is present; for an ordinal modifier the sum of the
#' increments from the lowest to the highest level (the largest absolute
#' difference between any two levels is reported too). The difference is
#' compared with a margin that must be fixed in advance.
#'
#' The difference is identified mainly by between-study variation in the
#' covariate: it is an observational (ecological) association, and
#' "equivalent" means "no modification larger than the margin was found",
#' not that the component works identically in every setting.
#'
#' @param fit          fitted `bcnma_bin` or `bcnma_cont` object with modifiers
#' @param label        character: modifier label (as in `fit$modifier_terms`)
#' @param component    character: components to report (default: all
#'   components modified by `label`)
#' @param margin       positive number on the analysis scale (log-OR for
#'   `bcnma_bin`, SMD / MD for `bcnma_cont`); no default, so that the margin is
#'   pre-specified
#' @param model        `"additive"` (default) or `"ssvs"`
#' @param threshold    posterior probability needed for a verdict (default 0.9)
#' @param margin_ratio binary outcomes only: the margin as a ratio of ORs
#'   (e.g. 1.25); `margin = abs(log(margin_ratio))`
#' @param from,to      continuous modifiers: covariate values to compare
#'   (default: observed minimum and maximum)
#' @return data.frame (invisibly), one row per component: `difference_median`,
#'   `difference_lower`, `difference_upper` (95% CrI), `p_inside` = P(|diff| <
#'   margin), `p_above` = P(diff > margin), `p_below` = P(diff < -margin),
#'   `max_abs_difference_*` (ordinal) and `verdict`: "equivalent",
#'   "modification (higher at top)", "modification (lower at top)" or
#'   "inconclusive"
#' @export
modifier_equivalence <- function(fit, label, component = NULL, margin = NULL,
                                 model = c("additive", "ssvs"), threshold = 0.9,
                                 margin_ratio = NULL, from = NULL, to = NULL) {
  model  <- match.arg(model)
  margin <- .resolve_equivalence_margin(fit, margin, margin_ratio)
  terms  <- fit$modifier_terms
  if (is.null(terms) || !label %in% terms$label) {
    stop("'label' must be one of the modifier labels: ", paste(unique(terms$label),
                                                               collapse = ", "))
  }
  samples <- fit[[paste0("samps_", model)]]
  if (is.null(samples)) stop("No samples for model = '", model, "'.")
  label_terms <- terms[terms$label == label, ]
  components  <- if (is.null(component)) unique(label_terms$component) else component
  rows <- lapply(components, function(component_name) {
    component_terms <- label_terms[label_terms$component == component_name, ]
    if (nrow(component_terms) == 0) stop("Component '", component_name, "' is not modified by '",
                                         label, "'.")
    difference <- .modifier_difference_draws(component_terms, samples, from, to)
    .equivalence_row(component_name, difference, margin, threshold)
  })
  result <- cbind(modifier = label, do.call(rbind, rows), margin = margin,
                  stringsAsFactors = FALSE)
  .print_equivalence(result, model)
  invisible(result)
}

.resolve_equivalence_margin <- function(fit, margin, margin_ratio) {
  if (!is.null(margin_ratio)) {
    if (!is.null(margin)) stop("Give 'margin' or 'margin_ratio', not both.")
    if (fit$sm != "OR") stop("'margin_ratio' is for binary outcomes (OR) only.")
    margin <- abs(log(margin_ratio))
  }
  is_valid <- is.numeric(margin) && length(margin) == 1 && margin > 0
  if (!is_valid) {
    stop("'margin' (or 'margin_ratio') is required and must be positive: pre-specify the ",
         "smallest modification that would matter, on the analysis scale.", call. = FALSE)
  }
  margin
}

#' Draws of top minus bottom effect difference (and the largest difference)
#' @noRd
.modifier_difference_draws <- function(component_terms, samples, from, to) {
  beta_draws <- as.matrix(samples[paste0("beta[", component_terms$q, "]")])
  if (component_terms$type[1] == "continuous") {
    lowest  <- if (is.null(from)) component_terms$value_min else from
    highest <- if (is.null(to)) component_terms$value_max else to
    per_unit <- beta_draws[, 1] / component_terms$scale
    return(list(difference = per_unit * (highest - lowest), max_abs_difference = NULL))
  }
  cumulative <- if (ncol(beta_draws) == 1) beta_draws else t(apply(beta_draws, 1, cumsum))
  level_effects <- cbind(0, cumulative)
  list(difference = rowSums(beta_draws),
       max_abs_difference = apply(level_effects, 1, function(effects) diff(range(effects))))
}

.equivalence_row <- function(component, difference, margin, threshold) {
  draws <- difference$difference
  quantiles <- stats::quantile(draws, BCNMA_CREDIBLE_PROBS, names = FALSE)
  p_inside <- mean(abs(draws) < margin)
  p_above  <- mean(draws > margin)
  p_below  <- mean(draws < -margin)
  verdict <- if (p_inside > threshold) "equivalent" else
    if (p_above > threshold) "modification (higher at top)" else
      if (p_below > threshold) "modification (lower at top)" else "inconclusive"
  max_abs <- difference$max_abs_difference
  max_quantiles <- if (is.null(max_abs)) rep(NA_real_, 3) else
    stats::quantile(max_abs, BCNMA_CREDIBLE_PROBS, names = FALSE)
  data.frame(component = component, difference_median = quantiles[2],
             difference_lower = quantiles[1], difference_upper = quantiles[3],
             p_inside = p_inside, p_above = p_above, p_below = p_below,
             max_abs_difference_median = max_quantiles[2],
             max_abs_difference_upper = max_quantiles[3],
             p_max_abs_inside = if (is.null(max_abs)) NA_real_ else mean(max_abs < margin),
             verdict = verdict, stringsAsFactors = FALSE)
}

.print_equivalence <- function(result, model) {
  cat(sprintf("Equivalence of modifier '%s' (%s model; margin %s on the analysis scale)\n",
              result$modifier[1], model, .format_number(result$margin[1])))
  print(result[, c("component", "difference_median", "difference_lower", "difference_upper",
                   "p_inside", "p_above", "p_below", "verdict")],
        digits = 3, row.names = FALSE)
  cat("Caveat: the difference is identified mainly by between-study variation in the",
      "covariate; it is an\nobservational (ecological) association, not a randomised",
      "comparison.\n")
}
