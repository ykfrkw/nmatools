# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: posterior summaries, printing and compare_combinations() codes
#          of arm-level effect modifiers (design in R/bcnma_modifier_design.R).
#          beta is sampled per code unit (per covariate SD for continuous
#          modifiers); summaries back-transform it to the original unit and
#          flag terms whose posterior is dominated by the prior.
# Inputs:  combined MCMC samples, the modifier term table, fitted objects
# Outputs: data.frames of beta summaries, console tables, code vectors
# Depends: R/bcnma_constants.R

# Posterior SD above this fraction of the prior SD: the data add little.
BCNMA_PRIOR_DOMINANCE_RATIO <- 0.5
BCNMA_CREDIBLE_PROBS <- c(0.025, 0.5, 0.975)

#' Posterior summary of beta per modifier term
#'
#' @param samples      data.frame of combined MCMC samples (or NULL)
#' @param terms        modifier term table (or NULL)
#' @param exponentiate logical: add ratio-of-ORs columns (binary outcomes)
#' @return data.frame: per original unit (continuous) or per step (ordinal)
#'   median / lower / upper, the same per covariate SD (continuous only),
#'   posterior and prior SD, prior dominance flag and studies informing beta
#' @noRd
.summarise_modifier_effects <- function(samples, terms, exponentiate = TRUE) {
  if (is.null(samples) || is.null(terms)) return(NULL)
  rows <- lapply(terms$q, function(q) {
    draws_per_code <- samples[[paste0("beta[", q, "]")]]
    is_continuous  <- terms$type[q] == "continuous"
    per_unit <- stats::quantile(draws_per_code / terms$scale[q], BCNMA_CREDIBLE_PROBS,
                                names = FALSE)
    per_sd <- if (is_continuous) {
      stats::quantile(draws_per_code, BCNMA_CREDIBLE_PROBS, names = FALSE)
    } else rep(NA_real_, 3)
    posterior_sd <- stats::sd(draws_per_code)
    row <- data.frame(
      term = terms$term[q], median = per_unit[2], lower = per_unit[1], upper = per_unit[3],
      median_per_sd = per_sd[2], lower_per_sd = per_sd[1], upper_per_sd = per_sd[3],
      posterior_sd = posterior_sd, prior_sd = terms$prior_sd[q],
      is_prior_dominated = posterior_sd > BCNMA_PRIOR_DOMINANCE_RATIO * terms$prior_sd[q],
      n_studies_contrast = terms$n_studies_contrast[q], stringsAsFactors = FALSE)
    if (exponentiate) {
      row$ratio_of_OR       <- exp(per_unit[2])
      row$ratio_of_OR_lower <- exp(per_unit[1])
      row$ratio_of_OR_upper <- exp(per_unit[3])
    }
    row
  })
  do.call(rbind, rows)
}

#' Warn when the data barely update a beta prior
#' @noRd
.warn_prior_dominated <- function(effects, model_label) {
  if (is.null(effects) || !any(effects$is_prior_dominated)) return(invisible(NULL))
  warning(sprintf(paste0("%s model: posterior SD of beta exceeds %s x its prior SD for %s; ",
                         "these modifier effects are mostly prior."),
                  model_label, BCNMA_PRIOR_DOMINANCE_RATIO,
                  paste(effects$term[effects$is_prior_dominated], collapse = ", ")),
          call. = FALSE)
}

#' Print the beta table with its interpretation caveat
#'
#' @param effects data.frame from .summarise_modifier_effects() (or NULL)
#' @param digits  integer decimals
#' @param sm      effect measure: "OR" (log-OR scale, plus ratio of ORs) or
#'   "SMD" / "MD"
#' @noRd
.print_modifier_table <- function(effects, digits, sm = "OR") {
  if (is.null(effects)) return(invisible(NULL))
  is_binary <- sm == "OR"
  cat(sprintf("\n  Effect modifiers (beta: change in component %s)\n",
              if (is_binary) "log-OR" else sm))
  cat(sprintf("  %-28s  %-24s  %-24s%s  %7s\n", "Term", "per unit/step [95% CrI]",
              "per SD [95% CrI]", if (is_binary) "  ratio of ORs per unit   " else "",
              "studies"))
  for (row in seq_len(nrow(effects))) {
    per_unit <- .format_interval(effects$median[row], effects$lower[row], effects$upper[row],
                                 digits)
    per_sd <- if (is.na(effects$median_per_sd[row])) "-" else
      .format_interval(effects$median_per_sd[row], effects$lower_per_sd[row],
                       effects$upper_per_sd[row], digits)
    ratio <- if (is_binary) {
      paste0("  ", formatC(.format_interval(effects$ratio_of_OR[row],
                                            effects$ratio_of_OR_lower[row],
                                            effects$ratio_of_OR_upper[row], digits),
                           width = -24))
    } else ""
    flag <- if (effects$is_prior_dominated[row]) " (prior-dominated)" else ""
    cat(sprintf("  %-28s  %-24s  %-24s%s  %7d%s\n", effects$term[row], per_unit, per_sd,
                ratio, as.integer(effects$n_studies_contrast[row]), flag))
  }
  cat("  Caveat: beta is identified mainly by between-study variation in the covariate",
      "('studies' = studies whose\n  within-study contrast informs it); it is an",
      "observational association open to ecological bias.\n")
  invisible(NULL)
}

#' Print the modifier term table (used by summary methods)
#' @noRd
.print_modifier_terms <- function(modifier_terms) {
  if (is.null(modifier_terms)) return(invisible(NULL))
  cat("\nModifier terms (d[k] of a modified component = effect at center / reference level;",
      "code = (value - center) / scale):\n")
  term_columns <- c("q", "term", "type", "level_from", "level_to", "center", "scale",
                    "monotone", "prior_sd", "n_studies_contrast", "n_distinct_contrasts")
  print(modifier_terms[, term_columns], row.names = FALSE)
  invisible(NULL)
}

#' Modifier-related elements of a fitted object
#'
#' @param component_values validated matrix or NULL
#' @param modifier_specs   normalised specs (possibly empty list)
#' @param modifier_design  output of .build_modifier_design() or NULL
#' @param samps_additive,samps_ssvs combined MCMC samples (ssvs may be NULL)
#' @param exponentiate     logical: add ratio-of-ORs columns (binary outcomes)
#' @return named list appended to the bcnma_bin / bcnma_cont object
#' @noRd
.modifier_results <- function(component_values, modifier_specs, modifier_design,
                              samps_additive, samps_ssvs, exponentiate) {
  modifier_terms <- modifier_design$terms
  modifier_effects <- if (!is.null(modifier_terms)) list(
    additive = .summarise_modifier_effects(samps_additive, modifier_terms, exponentiate),
    ssvs     = .summarise_modifier_effects(samps_ssvs, modifier_terms, exponentiate)
  )
  .warn_prior_dominated(modifier_effects$additive, "Additive")
  .warn_prior_dominated(modifier_effects$ssvs, "SSVS")
  list(
    component_values = component_values,
    modifier_specs   = if (length(modifier_specs) > 0) modifier_specs else NULL,
    modifier_terms   = modifier_terms,
    modifier_effects = modifier_effects
  )
}

# ---- Modifier codes for compare_combinations() ----

#' Code value of every modifier term at user-chosen covariate values
#'
#' @param x  fitted object with modifier_terms / modifier_specs
#' @param at named list: modifier label -> covariate value or ordinal level.
#'           Labels not given are evaluated at center / reference level (code 0).
#' @return numeric vector of length Q, on the code scale of beta
#' @noRd
.modifier_codes_at <- function(x, at) {
  terms  <- x$modifier_terms
  labels <- unique(terms$label)
  unknown <- setdiff(names(at), labels)
  if (length(unknown) > 0) {
    stop("'at' names not among modifier labels: ", paste(unknown, collapse = ", "))
  }
  defaulted <- setdiff(labels, names(at))
  if (length(defaulted) > 0) {
    message("Modifiers evaluated at center (continuous) / reference level (ordinal): ",
            paste(defaulted, collapse = ", "))
  }
  specs <- x$modifier_specs
  names(specs) <- vapply(specs, function(spec) spec$label, character(1))
  vapply(terms$q, function(q) {
    label <- terms$label[q]
    if (!label %in% names(at)) return(0)
    value <- at[[label]]
    if (terms$type[q] == "continuous") {
      return((as.numeric(value) - terms$center[q]) / terms$scale[q])
    }
    levels <- specs[[label]]$levels
    level_index <- match(as.character(value), levels)
    if (is.na(level_index)) {
      stop(sprintf("'at$%s' must be one of: %s", label, paste(levels, collapse = ", ")))
    }
    as.numeric(level_index >= match(terms$level_to[q], levels))
  }, numeric(1))
}
