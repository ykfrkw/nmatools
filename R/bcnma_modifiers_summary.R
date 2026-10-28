# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Posterior summaries, printing and compare_combinations() codes for
# arm-level effect modifiers (see R/bcnma_modifiers.R for the design). Shared
# by bcnma_bin (log-OR scale) and bcnma_cont (SMD / MD scale).

#' Posterior median and 95% CrI of beta per modifier term
#'
#' @param samps        data.frame of combined MCMC samples (or NULL)
#' @param terms        modifier term table (or NULL)
#' @param exponentiate logical: add OR columns (binary outcomes only)
#' @noRd
.summarise_modifier_effects <- function(samps, terms, exponentiate = TRUE) {
  if (is.null(samps) || is.null(terms)) return(NULL)
  rows <- lapply(terms$q, function(q) {
    draws <- samps[[paste0("beta[", q, "]")]]
    quantiles <- stats::quantile(draws, c(0.025, 0.5, 0.975), names = FALSE)
    row <- data.frame(term = terms$term[q], median = quantiles[2], lower = quantiles[1],
                      upper = quantiles[3], stringsAsFactors = FALSE)
    if (exponentiate) {
      row$OR       <- exp(quantiles[2])
      row$OR_lower <- exp(quantiles[1])
      row$OR_upper <- exp(quantiles[3])
    }
    row
  })
  do.call(rbind, rows)
}

#' Print the beta table
#'
#' @param effects data.frame from .summarise_modifier_effects() (or NULL)
#' @param digits  integer decimals
#' @param scale   character: "OR" (binary; prints log-OR and OR) or the
#'                continuous effect measure ("SMD" / "MD")
#' @noRd
.print_modifier_table <- function(effects, digits, scale = "OR") {
  if (is.null(effects)) return(invisible(NULL))
  format_interval <- function(m, lo, hi) sprintf("%.*f [%.*f; %.*f]", digits, m, digits, lo, digits, hi)
  if (scale != "OR") {
    cat(sprintf("\n  Effect modifiers (beta: change in component %s per code unit)\n", scale))
    cat(sprintf("  %-32s  %-28s\n", "Term", paste0(scale, " [95% CrI]")))
    for (row in seq_len(nrow(effects))) {
      cat(sprintf("  %-32s  %-28s\n", effects$term[row],
                  format_interval(effects$median[row], effects$lower[row], effects$upper[row])))
    }
    return(invisible(NULL))
  }
  cat("\n  Effect modifiers (beta: change in component log-OR per code unit)\n")
  cat(sprintf("  %-32s  %-26s  %-26s\n", "Term", "log-OR [95% CrI]", "OR [95% CrI]"))
  for (row in seq_len(nrow(effects))) {
    cat(sprintf("  %-32s  %-26s  %-26s\n", effects$term[row],
                format_interval(effects$median[row], effects$lower[row], effects$upper[row]),
                format_interval(effects$OR[row], effects$OR_lower[row], effects$OR_upper[row])))
  }
  invisible(NULL)
}

#' Print the modifier term table (used by summary methods)
#' @noRd
.print_modifier_terms <- function(modifier_terms) {
  if (is.null(modifier_terms)) return(invisible(NULL))
  cat("\nModifier terms (d[k] of a modified component = effect at center / reference level):\n")
  term_columns <- c("q", "term", "type", "level_from", "level_to", "center", "monotone")
  print(modifier_terms[, term_columns], row.names = FALSE)
  invisible(NULL)
}

#' Modifier-related elements of a fitted object
#'
#' @param component_values validated matrix or NULL
#' @param modifier_specs   normalised specs (possibly empty list)
#' @param modifier_design  output of .build_modifier_design() or NULL
#' @param samps_additive,samps_ssvs combined MCMC samples (ssvs may be NULL)
#' @param exponentiate     logical: add OR columns (binary outcomes)
#' @return named list appended to the bcnma_bin / bcnma_cont object
#' @noRd
.modifier_results <- function(component_values, modifier_specs, modifier_design,
                              samps_additive, samps_ssvs, exponentiate) {
  modifier_terms <- modifier_design$terms
  list(
    component_values = component_values,
    modifier_specs   = if (length(modifier_specs) > 0) modifier_specs else NULL,
    modifier_terms   = modifier_terms,
    modifier_effects = if (is.null(modifier_terms)) NULL else list(
      additive = .summarise_modifier_effects(samps_additive, modifier_terms, exponentiate),
      ssvs     = .summarise_modifier_effects(samps_ssvs, modifier_terms, exponentiate)
    )
  )
}

# ---- Modifier codes for compare_combinations() ----

#' Code value of every modifier term at user-chosen covariate values
#'
#' @param x  fitted object with modifier_terms / modifier_specs
#' @param at named list: modifier label -> covariate value or ordinal level.
#'           Labels not given are evaluated at center / reference level (code 0).
#' @return numeric vector of length Q
#' @noRd
.modifier_codes_at <- function(x, at) {
  terms  <- x$modifier_terms
  labels <- unique(terms$label)
  unknown <- setdiff(names(at), labels)
  if (length(unknown) > 0) stop("'at' names not among modifier labels: ", paste(unknown, collapse = ", "))
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
    if (terms$type[q] == "continuous") return(as.numeric(value) - terms$center[q])
    level_index <- match(as.character(value), specs[[label]]$levels)
    if (is.na(level_index)) stop(sprintf("'at$%s' must be one of: %s", label,
                                         paste(specs[[label]]$levels, collapse = ", ")))
    as.numeric(level_index >= match(terms$level_to[q], specs[[label]]$levels))
  }, numeric(1))
}
