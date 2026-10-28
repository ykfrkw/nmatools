# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11
#
# File description:
# S3 methods for bcnma_cont objects (continuous outcomes, SMD/MD scale).
# Mirrors the structure of R/bcnma_methods.R for bcnma_bin (binary outcomes).
# compare_combinations() lives in R/bcnma_compare.R.

# ---- print ----

#' Print and summarise a fitted bcnma_cont model
#'
#' \code{print} shows the component effects (SMD or MD with 95% CrI) for
#' the additive and/or LASSO model, selected interactions and, when the model
#' has effect modifiers, the modifier (beta) table. \code{summary} adds the
#' call, MCMC settings and the modifier term definitions.
#'
#' @param x,object fitted \code{bcnma_cont} object
#' @param digits   integer: decimals to print
#' @param model    character: \code{"both"} (default), \code{"additive"} or \code{"lasso"}
#' @param ...      further arguments (unused)
#' @return the object, invisibly
#' @name print.bcnma_cont
#' @export print.bcnma_cont
#' @export
print.bcnma_cont <- function(x, digits = 2,
                              model = c("both", "additive", "lasso"), ...) {
  model <- match.arg(model)

  cat("Bayesian Component NMA \u2014 Continuous outcomes (bcnma_cont)\n")
  cat(rep("-", 60), "\n", sep = "")
  cat("Components    :", x$n_components, "\n")
  cat("Studies       :", x$Ns, "\n")
  cat("Effect measure:", x$sm, "\n")
  cat("Interactions  :", x$interactions_model, "\n")
  cat("Tau prior     :", x$model_settings$tau_prior$label, "\n")
  if (!is.null(x$reference.group)) cat("Reference     :", x$reference.group, "\n")
  if (!is.null(x$component_values)) {
    cat("Component values: user-supplied (d[k] = ", x$sm, " per unit)\n", sep = "")
  }
  if (!is.null(x$modifier_terms)) cat("Modifier terms:", nrow(x$modifier_terms), "\n")
  cat("\n")

  show_add  <- model %in% c("both", "additive") && !is.null(x$summary_additive)
  show_lass <- model %in% c("both", "lasso")    && !is.null(x$summary_lasso)

  if (show_add) {
    cat("Component effects \u2014 Additive model\n")
    cat(rep("-", 60), "\n", sep = "")
    .print_cont_table(x$summary_additive, x$components, x$sm, digits)
    .print_modifier_table(x$modifier_effects$additive, digits, x$sm)
  }

  if (show_lass) {
    cat(sprintf("\nComponent effects \u2014 LASSO model (%s)\n", x$interactions_model))
    cat(rep("-", 60), "\n", sep = "")
    .print_cont_table(x$summary_lasso, x$components, x$sm, digits)
    .print_modifier_table(x$modifier_effects$lasso, digits, x$sm)

    n_selected <- sum(x$interaction_inclusion_prob >= 0.5, na.rm = TRUE)
    cat(sprintf("\nInteractions with inclusion prob >= 0.5: %d / %d\n",
                n_selected, x$Ninter))
    if (n_selected > 0) {
      top <- summarise_interactions(x, threshold = 0.5, top_n = 10)
      top_sel <- top[top$selected, ]
      if (nrow(top_sel) > 0) {
        cat(sprintf("  %-20s  %8s  %8s\n", "Pair",
                    paste0("median ", x$sm), "P(incl)"))
        for (i in seq_len(nrow(top_sel))) {
          cat(sprintf("  %-20s  %8.3f  %8.3f\n",
                      top_sel$component_pair[i],
                      top_sel$median_gamma[i],
                      top_sel$inclusion_prob[i]))
        }
      }
    }
  }

  invisible(x)
}

# Internal: print component table for continuous models (no ORd column)
.print_cont_table <- function(smry, components, sm, digits) {
  Nc     <- length(components)
  d_rows <- smry[paste0("d[", seq_len(Nc), "]"), , drop = FALSE]

  fmt <- function(m, lo, hi) sprintf("%.*f [%.*f; %.*f]",
                                      digits, m, digits, lo, digits, hi)

  cat(sprintf("  %-8s  %-28s\n", "Comp", paste0(sm, " [95% CrI]")))
  cat("  ", rep("-", 38), "\n", sep = "")
  for (k in seq_len(Nc)) {
    cat(sprintf("  %-8s  %-28s\n",
                components[k],
                fmt(d_rows[k, "50%"], d_rows[k, "2.5%"], d_rows[k, "97.5%"])))
  }
  .print_heterogeneity_rows(smry, digits, label_width = 8)
}

# ---- summary ----

#' @rdname print.bcnma_cont
#' @export summary.bcnma_cont
#' @export
summary.bcnma_cont <- function(object, digits = 3, ...) {
  cat("=== Bayesian cNMA Summary (Continuous Outcomes) ===\n\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")
  cat("Data:\n")
  cat("  Studies      :", object$Ns, "\n")
  cat("  Components   :", object$n_components, "\n")
  cat("  Effect measure:", object$sm, "\n")
  cat("  Interactions :", object$Ninter, "pairs total\n")
  cat("  MCMC chains  :", object$mcmc_settings$n.chains,
      "  iter:", object$mcmc_settings$n.iter, "\n\n")

  print(object, digits = digits, model = "both")
  .print_modifier_terms(object$modifier_terms)
  invisible(object)
}

# ---- forest ----

#' Forest plot of component SMD (or MD) effects
#'
#' @param x      bcnma_cont object
#' @param model  character: \code{"additive"}, \code{"lasso"}, or \code{"both"}
#' @param ref_line numeric: reference line (default 0 for SMD/MD)
#' @param xlab   character: x-axis label
#' @param title  character: plot title
#' @param ...    additional arguments (unused)
#' @return ggplot object (invisible)
#' @export forest.bcnma_cont
#' @export
forest.bcnma_cont <- function(x,
                               model    = c("both", "additive", "lasso"),
                               ref_line = 0,
                               xlab     = NULL,
                               title    = "Component effects",
                               ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  model <- match.arg(model)
  if (is.null(xlab)) xlab <- paste0(x$sm, " vs. no component")

  Nc    <- x$n_components
  comps <- x$components

  extract_df <- function(smry, label) {
    d_rows <- smry[paste0("d[", seq_len(Nc), "]"), , drop = FALSE]
    data.frame(
      component = factor(comps, levels = rev(comps)),
      median    = d_rows[, "50%"],
      lo        = d_rows[, "2.5%"],
      hi        = d_rows[, "97.5%"],
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
    ggplot2::labs(x = xlab, y = NULL, title = title,
                  colour = "Model", shape = "Model") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  print(p)
  invisible(p)
}
