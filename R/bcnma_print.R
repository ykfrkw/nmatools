# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (merged print methods 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: print() and summary() methods of bcnma_bin and bcnma_cont
#          objects. Both classes share one implementation; they differ only
#          in the effect measure (log-OR + OR, or SMD / MD).
# Inputs:  fitted bcnma_bin / bcnma_cont objects
# Outputs: console output; the object, invisibly
# Depends: R/bcnma_modifiers_summary.R (modifier tables),
#          R/bcnma_interactions.R (summarise_interactions()),
#          R/bcnma_identifiability.R (.print_identifiability_note())

BCNMA_RULE_WIDTH   <- 62
BCNMA_LABEL_WIDTH  <- 16
BCNMA_TOP_INTERACTIONS <- 10
BCNMA_SELECTION_THRESHOLD <- 0.5

#' Print and summarise a fitted bcnma model
#'
#' `print()` shows the component effects with 95% credible intervals for the
#' additive and/or SSVS model (log-OR and OR for `bcnma_bin`, SMD or MD for
#' `bcnma_cont`), the heterogeneity (tau and tau2), the SSVS scale and
#' selected interactions and, when the model has effect modifiers, the
#' modifier (beta) table. `summary()` adds the call, MCMC settings and the
#' modifier term definitions.
#'
#' @param x,object fitted `bcnma_bin` or `bcnma_cont` object
#' @param digits   integer: decimals to print
#' @param model    character: `"both"` (default), `"additive"` or `"ssvs"`
#'   (`"lasso"` is a deprecated alias of `"ssvs"`)
#' @param ...      further arguments (unused)
#' @return the object, invisibly
#' @name print.bcnma_bin
#' @export print.bcnma_bin
#' @export
print.bcnma_bin <- function(x, digits = 2, model = c("both", "additive", "ssvs", "lasso"), ...) {
  .print_bcnma(x, digits, .normalise_model_name(match.arg(model)))
}

#' @rdname print.bcnma_bin
#' @export print.bcnma_cont
#' @export
print.bcnma_cont <- function(x, digits = 2, model = c("both", "additive", "ssvs", "lasso"), ...) {
  .print_bcnma(x, digits, .normalise_model_name(match.arg(model)))
}

#' @rdname print.bcnma_bin
#' @export summary.bcnma_bin
#' @export
summary.bcnma_bin <- function(object, digits = 3, ...) .summary_bcnma(object, digits)

#' @rdname print.bcnma_bin
#' @export summary.bcnma_cont
#' @export
summary.bcnma_cont <- function(object, digits = 3, ...) .summary_bcnma(object, digits)

.print_bcnma <- function(x, digits, model) {
  .print_header(x)
  if (model %in% c("both", "additive") && !is.null(x$summary_additive)) {
    .print_model_section(x, "additive", "Additive model", digits)
  }
  if (model %in% c("both", "ssvs") && !is.null(x$summary_ssvs)) {
    .print_model_section(x, "ssvs", sprintf("SSVS model (%s)", x$interactions_model), digits)
    .print_ssvs_scale(x, digits)
    .print_selected_interactions(x)
  }
  invisible(x)
}

.print_header <- function(x) {
  is_binary <- x$sm == "OR"
  cat("Bayesian Component NMA (bcnma)", if (!is_binary) "— continuous outcome", "\n")
  cat(strrep("-", BCNMA_RULE_WIDTH), "\n", sep = "")
  .cat_field("Components", x$n_components)
  .cat_field("Studies", x$Ns)
  .cat_field("Effect measure", if (is_binary) "OR (log-OR scale in d[k])" else x$sm)
  .cat_field("Interactions", x$interactions_model)
  .cat_field("Tau prior", x$model_settings$tau_prior$label)
  if (!is.null(x$reference.group)) .cat_field("Reference", x$reference.group)
  if (!is.null(x$component_values)) {
    .cat_field("Component values", sprintf("user-supplied (d[k] = %s per unit)",
                                           if (is_binary) "log-OR" else x$sm))
  }
  if (!is.null(x$modifier_terms)) .cat_field("Modifier terms", nrow(x$modifier_terms))
  cat("\n")
}

.cat_field <- function(label, value) {
  cat(sprintf("%-*s: %s\n", BCNMA_LABEL_WIDTH, label, paste(value, collapse = " ")))
}

.print_model_section <- function(x, model, title, digits) {
  cat("Component effects — ", title, "\n", sep = "")
  cat(strrep("-", BCNMA_RULE_WIDTH), "\n", sep = "")
  .print_effect_table(x[[paste0("summary_", model)]], x$components, x$sm, digits)
  .print_modifier_table(x$modifier_effects[[model]], digits, x$sm)
  .print_convergence(x$convergence[[model]])
  .print_model_fit(x$model_fit, model)
  cat("\n")
}

.format_interval <- function(estimate, lower, upper, digits) {
  sprintf("%.*f [%.*f; %.*f]", digits, estimate, digits, lower, digits, upper)
}

# Component rows (d[j]; OR also exp(d[j])) and the heterogeneity rows.
.print_effect_table <- function(posterior_summary, components, sm, digits) {
  is_binary <- sm == "OR"
  effect_label <- if (is_binary) "log-OR [95% CrI]" else paste0(sm, " [95% CrI]")
  cat(sprintf("  %-*s  %-26s%s\n", BCNMA_LABEL_WIDTH, "Component", effect_label,
              if (is_binary) "  OR [95% CrI]" else ""))
  for (component_index in seq_along(components)) {
    row <- posterior_summary[paste0("d[", component_index, "]"), ]
    interval <- .format_interval(row[["50%"]], row[["2.5%"]], row[["97.5%"]], digits)
    odds_ratio <- if (is_binary) {
      paste0("  ", .format_interval(exp(row[["50%"]]), exp(row[["2.5%"]]),
                                    exp(row[["97.5%"]]), digits))
    } else ""
    cat(sprintf("  %-*s  %-26s%s\n", BCNMA_LABEL_WIDTH, components[component_index],
                interval, odds_ratio))
  }
  .print_heterogeneity_rows(posterior_summary, digits)
}

# tau (SD) and tau2 (variance); log-normal priors are placed on tau2.
.print_heterogeneity_rows <- function(posterior_summary, digits) {
  heterogeneity_rows <- c("tau (SD)" = "tau", "tau2 (variance)" = "tau2")
  for (label in names(heterogeneity_rows)) {
    node <- heterogeneity_rows[[label]]
    if (!node %in% rownames(posterior_summary)) next
    row <- posterior_summary[node, ]
    cat(sprintf("  %-*s  %s\n", BCNMA_LABEL_WIDTH, label,
                .format_interval(row[["50%"]], row[["2.5%"]], row[["97.5%"]], digits)))
  }
}

# Spike SD eta and implied slab SD eta * sqrt(g): prior settings and posterior.
.print_ssvs_scale <- function(x, digits) {
  ssvs_settings <- x$model_settings$ssvs
  eta_draws <- x$samps_ssvs$eta
  if (is.null(eta_draws)) return(invisible(NULL))
  slab_sd <- stats::quantile(eta_draws * sqrt(ssvs_settings$slab_ratio), c(0.025, 0.5, 0.975))
  cat(sprintf(paste0("SSVS scale: eta ~ half-normal(SD %s), g = %s, inclusion prob %s; ",
                     "posterior slab SD (eta*sqrt(g)) %s\n"),
              .format_number(ssvs_settings$eta_prior_sd),
              .format_number(ssvs_settings$slab_ratio),
              .format_number(ssvs_settings$inclusion_prob),
              .format_interval(slab_sd[2], slab_sd[1], slab_sd[3], digits)))
}

.print_selected_interactions <- function(x) {
  .print_identifiability_note(x)
  interaction_table <- summarise_interactions(x, threshold = BCNMA_SELECTION_THRESHOLD)
  selected <- utils::head(interaction_table[interaction_table$selected, ],
                          BCNMA_TOP_INTERACTIONS)
  cat(sprintf("Interactions with inclusion prob >= %s: %d / %d\n",
              BCNMA_SELECTION_THRESHOLD, sum(interaction_table$selected), x$Ninter))
  if (nrow(selected) == 0) return(invisible(NULL))
  is_binary <- x$sm == "OR"
  effect_label <- if (is_binary) "median OR" else paste0("median ", x$sm)
  cat(sprintf("  %-20s  %10s  %8s  %8s\n", "Pair", effect_label, "P(incl)", "MCSE"))
  for (row in seq_len(nrow(selected))) {
    effect <- if (is_binary) selected$OR_interaction[row] else selected$median_gamma[row]
    cat(sprintf("  %-20s  %10.3f  %8.3f  %8.3f\n", selected$component_pair[row], effect,
                selected$inclusion_prob[row], selected$inclusion_prob_mcse[row]))
  }
}

.summary_bcnma <- function(object, digits) {
  cat("=== Bayesian cNMA Summary ===\n\nCall:\n")
  print(object$call)
  mcmc <- object$mcmc_settings
  cat("\nData:\n")
  .cat_field("  Studies", object$Ns)
  .cat_field("  Components", object$n_components)
  .cat_field("  Pairs", object$Ninter)
  .cat_field("  MCMC", sprintf("%d chains, adapt %d, burn-in %d, iter %d, thin %d, seed %s",
                               mcmc$n.chains, mcmc$n.adapt, mcmc$n.burnin, mcmc$n.iter,
                               mcmc$n.thin, object$seed))
  cat("\n")
  .print_bcnma(object, digits, model = "both")
  .print_modifier_terms(object$modifier_terms)
  cat("\nPriors:\n")
  print(object$priors, row.names = FALSE, right = FALSE)
  if (identical(object$interactions_model, "ssvs_informative")) {
    cat("\nActive components:", paste(object$active_components, collapse = ", "), "\n")
  }
  invisible(object)
}
