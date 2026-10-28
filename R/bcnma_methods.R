# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# S3 methods and standalone functions for bcnma objects:
#   print.bcnma       — component effects table
#   summary.bcnma     — full results including interactions
#   forest.bcnma      — forest plot of component OR
#   plot_interactions — LASSO interaction overview
# compare_combinations() lives in R/bcnma_compare.R.

# ---- print ----

#' Print and summarise a fitted bcnma_bin model
#'
#' \code{print} shows the component effects (log-OR and OR with 95% CrI) for
#' the additive and/or LASSO model, selected interactions and, when the model
#' has effect modifiers, the modifier (beta) table. \code{summary} adds the
#' call, MCMC settings and the modifier term definitions.
#'
#' @param x,object fitted \code{bcnma_bin} object
#' @param digits   integer: decimals to print
#' @param model    character: \code{"both"} (default), \code{"additive"} or \code{"lasso"}
#' @param ...      further arguments (unused)
#' @return the object, invisibly
#' @name print.bcnma_bin
#' @export print.bcnma_bin
#' @export
print.bcnma_bin <- function(x, digits = 2, model = c("both", "additive", "lasso"), ...) {
  model <- match.arg(model)

  cat("Bayesian Component NMA (bcnma)\n")
  cat(rep("-", 55), "\n", sep = "")
  cat("Components    :", x$n_components, "\n")
  cat("Studies       :", x$Ns, "\n")
  cat("Effect measure: OR (log-OR scale in d[k])\n")
  cat("Interactions  :", x$interactions_model, "\n")
  cat("Tau prior     :", x$model_settings$tau_prior$label, "\n")
  if (!is.null(x$reference.group)) cat("Reference     :", x$reference.group, "\n")
  if (!is.null(x$component_values)) cat("Component values: user-supplied (d[k] = log-OR per unit)\n")
  if (!is.null(x$modifier_terms)) cat("Modifier terms:", nrow(x$modifier_terms), "\n")
  cat("\n")

  show_add  <- model %in% c("both", "additive") && !is.null(x$summary_additive)
  show_lass <- model %in% c("both", "lasso")    && !is.null(x$summary_lasso)

  if (show_add) {
    cat("Component effects \u2014 Additive model\n")
    cat(rep("-", 55), "\n", sep = "")
    .print_component_table(x$summary_additive, x$components, digits)
    .print_modifier_table(x$modifier_effects$additive, digits)
  }

  if (show_lass) {
    cat("\nComponent effects \u2014 LASSO model (", x$interactions_model, ")\n", sep = "")
    cat(rep("-", 55), "\n", sep = "")
    .print_component_table(x$summary_lasso, x$components, digits)
    .print_modifier_table(x$modifier_effects$lasso, digits)

    # Top selected interactions
    n_selected <- sum(x$interaction_inclusion_prob >= 0.5, na.rm = TRUE)
    cat(sprintf("\nInteractions with inclusion prob >= 0.5: %d / %d\n",
                n_selected, x$Ninter))
    if (n_selected > 0) {
      top <- summarise_interactions(x, threshold = 0.5, top_n = 10)
      top_sel <- top[top$selected, ]
      if (nrow(top_sel) > 0) {
        cat(sprintf("  %-20s  %6s  %8s  %8s\n",
                    "Pair", "median OR", "P(incl)", "selected"))
        for (i in seq_len(nrow(top_sel))) {
          cat(sprintf("  %-20s  %6.3f  %8.3f  %s\n",
                      top_sel$component_pair[i],
                      top_sel$OR_interaction[i],
                      top_sel$inclusion_prob[i],
                      if (top_sel$selected[i]) "*" else ""))
        }
      }
    }
  }

  invisible(x)
}

# Internal: print component effects table from MCMCsummary
.print_component_table <- function(smry, components, digits) {
  Nc <- length(components)
  # Find d[k] rows (log-OR)
  d_rows  <- smry[paste0("d[",   seq_len(Nc), "]"), , drop = FALSE]
  or_rows <- smry[paste0("ORd[", seq_len(Nc), "]"), , drop = FALSE]

  fmt <- function(m, lo, hi) sprintf("%.*f [%.*f; %.*f]",
                                      digits, m, digits, lo, digits, hi)

  cat(sprintf("  %-8s  %-26s  %-26s\n", "Comp", "log-OR [95% CrI]", "OR [95% CrI]"))
  cat("  ", rep("-", 62), "\n", sep = "")
  for (k in seq_len(Nc)) {
    cat(sprintf("  %-8s  %-26s  %-26s\n",
                components[k],
                fmt(d_rows[k, "50%"], d_rows[k, "2.5%"], d_rows[k, "97.5%"]),
                fmt(or_rows[k, "50%"], or_rows[k, "2.5%"], or_rows[k, "97.5%"])
    ))
  }
  .print_heterogeneity_rows(smry, digits, label_width = 8)
}

# Internal: tau (SD) and tau2 (variance) rows; the prior is placed on tau2
.print_heterogeneity_rows <- function(smry, digits, label_width) {
  fmt <- function(m, lo, hi) sprintf("%.*f [%.*f; %.*f]",
                                      digits, m, digits, lo, digits, hi)
  heterogeneity_rows <- c("tau (SD)" = "tau", "tau2 (variance)" = "tau2")
  for (label in names(heterogeneity_rows)) {
    node <- heterogeneity_rows[[label]]
    if (!node %in% rownames(smry)) next
    cat(sprintf("  %-*s  %s\n", label_width, label,
                fmt(smry[node, "50%"], smry[node, "2.5%"], smry[node, "97.5%"])))
  }
}

# ---- summary ----

#' @rdname print.bcnma_bin
#' @export summary.bcnma_bin
#' @export
summary.bcnma_bin <- function(object, digits = 3, ...) {
  cat("=== Bayesian cNMA Summary ===\n\n")

  cat("Call:\n")
  print(object$call)
  cat("\n")

  cat("Data:\n")
  cat("  Studies      :", object$Ns, "\n")
  cat("  Components   :", object$n_components, "\n")
  cat("  Interactions :", object$Ninter, "pairs total\n")
  cat("  MCMC chains  :", object$mcmc_settings$n.chains,
      "  iter:", object$mcmc_settings$n.iter,
      "  thin:", object$mcmc_settings$n.thin, "\n\n")

  print(object, digits = digits, model = "both")
  .print_modifier_terms(object$modifier_terms)

  cat("\nInteraction model: ", object$interactions_model, "\n")
  if (object$interactions_model == "lasso_informative") {
    cat("Active components:", paste(object$active_components, collapse = ", "), "\n")
    cat("Modelled interactions:", length(object$included_interactions),
        "/ fixed to 0:", object$Ninter - length(object$included_interactions), "\n")
  }

  invisible(object)
}

# ---- forest ----

#' Forest plot of component OR effects
#'
#' @param x      bcnma object
#' @param model  character: `"additive"`, `"lasso"`, or `"both"` (default)
#' @param ref_line numeric: reference line position (default 1 for OR)
#' @param xlab   character: x-axis label
#' @param title  character: plot title
#' @param ...    additional arguments (unused)
#' @return ggplot object (invisible)
#' @export forest.bcnma_bin
#' @export
forest.bcnma_bin <- function(x,
                          model    = c("both", "additive", "lasso"),
                          ref_line = 1,
                          xlab     = "Odds Ratio vs. no component",
                          title    = "Component effects",
                          ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  model <- match.arg(model)

  Nc     <- x$n_components
  comps  <- x$components

  extract_df <- function(smry, label) {
    or_rows <- smry[paste0("ORd[", seq_len(Nc), "]"), , drop = FALSE]
    data.frame(
      component = factor(comps, levels = rev(comps)),
      median    = or_rows[, "50%"],
      lo        = or_rows[, "2.5%"],
      hi        = or_rows[, "97.5%"],
      model     = label,
      stringsAsFactors = FALSE
    )
  }

  dfs <- list()
  if (model %in% c("additive", "both") && !is.null(x$summary_additive)) {
    dfs[["Additive"]] <- extract_df(x$summary_additive, "Additive")
  }
  if (model %in% c("lasso", "both") && !is.null(x$summary_lasso)) {
    lbl <- if (x$interactions_model == "lasso") "LASSO" else "LASSO (informative)"
    dfs[[lbl]] <- extract_df(x$summary_lasso, lbl)
  }

  if (length(dfs) == 0) stop("No model results available to plot.")
  df <- do.call(rbind, dfs)

  dodge <- if (length(dfs) > 1) ggplot2::position_dodge(width = 0.5) else ggplot2::position_identity()

  p <- ggplot2::ggplot(df, ggplot2::aes(x = median, y = component,
                                         xmin = lo, xmax = hi,
                                         colour = model, shape = model)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(height = 0.25), position = dodge) +
    ggplot2::geom_point(size = 3, position = dodge) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = xlab, y = NULL, title = title,
                  colour = "Model", shape = "Model") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  print(p)
  invisible(p)
}

#' Plot LASSO interaction overview: coefficient vs. inclusion probability
#'
#' Scatter plot of median gamma (x-axis) vs. mean P(Ind=1) (y-axis) for all
#' pairwise interactions, labelling the top interactions by |gamma|.
#'
#' @param x       bcnma object
#' @param top_n   integer: number of interaction pairs to label (default 10)
#' @param threshold numeric: draw horizontal reference line at this inclusion prob (default 0.5)
#' @return ggplot object (invisible)
#' @export
plot_interactions <- function(x, top_n = 10, threshold = 0.5) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (is.null(x$interaction_effects)) {
    stop("No interaction results. Run bcnma_bin() with interactions = 'lasso' or 'lasso_informative'.")
  }

  df <- summarise_interactions(x)

  top_labels <- utils::head(df[order(abs(df$median_gamma), decreasing = TRUE), ], top_n)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = median_gamma, y = inclusion_prob,
                                         label = component_pair)) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "red") +
    ggplot2::geom_point(ggplot2::aes(colour = selected), size = 2, alpha = 0.7) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#d62728", "FALSE" = "grey60"),
                                  labels = c("TRUE" = paste0(">= ", threshold),
                                             "FALSE" = paste0("< ", threshold)),
                                  name = "Selected") +
    ggplot2::labs(
      x     = "Median interaction coefficient (log-OR scale)",
      y     = "Inclusion probability P(Ind = 1)",
      title = "LASSO interaction overview"
    ) +
    ggplot2::theme_bw(base_size = 12)

  # Add labels for top interactions (requires ggrepel if available)
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(
      data = top_labels,
      ggplot2::aes(label = component_pair),
      size = 3, max.overlaps = 20
    )
  } else {
    p <- p + ggplot2::geom_text(
      data = top_labels,
      ggplot2::aes(label = component_pair),
      size = 3, vjust = -0.8
    )
  }

  print(p)
  invisible(p)
}
