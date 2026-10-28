# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: summaries of spike-and-slab selection of effect modifiers
#          (modifier spec `selection = "ssvs"`): per (modifier, component)
#          the posterior probability that a modification is present, its
#          Monte Carlo SE and the Bayes factor (posterior odds / prior odds),
#          reported as a bound when no draw (or every draw) is in the slab.
# Inputs:  modifier term table, fitted models (samples, summary)
# Outputs: data.frames in fit$modifier_selection; console table
# Depends: R/bcnma_constants.R

#' Selection summaries of the additive and SSVS models
#' @noRd
.modifier_selection_results <- function(terms, fitted) {
  if (is.null(terms) || all(is.na(terms$selection_group))) return(NULL)
  list(additive = .summarise_modifier_selection(terms, fitted$additive),
       ssvs     = .summarise_modifier_selection(terms, fitted$ssvs))
}

#' P(modification present), MCSE and Bayes factor per selection group
#'
#' @param terms        modifier term table (selection_group, inclusion_prob)
#' @param fitted_model list(samples, summary) or NULL
#' @return data.frame(group, modifier, component, prior_prob, posterior_prob,
#'   mcse, bayes_factor, bayes_factor_bound) where the bound is "", ">" or "<"
#' @noRd
.summarise_modifier_selection <- function(terms, fitted_model) {
  if (is.null(fitted_model)) return(NULL)
  groups <- terms[!is.na(terms$selection_group) & !duplicated(terms$selection_group), ]
  draw_count <- nrow(fitted_model$samples)
  rows <- lapply(seq_len(nrow(groups)), function(row) {
    node <- paste0("IndM[", groups$selection_group[row], "]")
    posterior_prob <- mean(fitted_model$samples[[node]])
    n_eff <- suppressWarnings(as.numeric(fitted_model$summary[node, "n.eff"]))
    if (is.na(n_eff) || n_eff <= 0) n_eff <- draw_count
    bayes_factor <- .selection_bayes_factor(posterior_prob, groups$inclusion_prob[row],
                                            draw_count)
    data.frame(group = groups$selection_group[row], modifier = groups$label[row],
               component = groups$component[row], prior_prob = groups$inclusion_prob[row],
               posterior_prob = posterior_prob,
               mcse = sqrt(posterior_prob * (1 - posterior_prob) / n_eff),
               bayes_factor = bayes_factor$value, bayes_factor_bound = bayes_factor$bound,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Bayes factor for "modification present" = posterior odds / prior odds
#'
#' With every draw in the spike (or slab) the posterior odds are 0 (or
#' infinite); the probability is then replaced by 0.5 / n (or 1 - 0.5 / n)
#' and the result is reported as an upper (or lower) bound.
#' @noRd
.selection_bayes_factor <- function(posterior_prob, prior_prob, draw_count) {
  bound <- ""
  if (posterior_prob <= 0) {
    posterior_prob <- 0.5 / draw_count
    bound <- "<"
  } else if (posterior_prob >= 1) {
    posterior_prob <- 1 - 0.5 / draw_count
    bound <- ">"
  }
  posterior_odds <- posterior_prob / (1 - posterior_prob)
  prior_odds     <- prior_prob / (1 - prior_prob)
  list(value = posterior_odds / prior_odds, bound = bound)
}

#' Print the selection table with the slab-sensitivity note
#' @noRd
.print_modifier_selection <- function(selection) {
  if (is.null(selection)) return(invisible(NULL))
  cat("\n  Effect-modifier selection (SSVS; one indicator per modifier x component)\n")
  cat(sprintf("  %-28s  %7s  %18s  %12s\n", "Modifier:component", "prior P",
              "P(present) [MCSE]", "Bayes factor"))
  for (row in seq_len(nrow(selection))) {
    cat(sprintf("  %-28s  %7.2f  %10.3f [%.3f]  %12s\n",
                paste0(selection$modifier[row], ":", selection$component[row]),
                selection$prior_prob[row], selection$posterior_prob[row], selection$mcse[row],
                paste0(selection$bayes_factor_bound[row],
                       formatC(selection$bayes_factor[row], digits = 3, format = "g"))))
  }
  cat("  Note: P(present) and the Bayes factor depend on slab_sd (Lindley's paradox: a",
      "wider slab favours\n  'no modification'); report a sensitivity analysis over",
      "slab_sd.\n")
  invisible(NULL)
}
