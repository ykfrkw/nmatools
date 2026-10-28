# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (merged plot methods 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: plots of fitted bcnma objects: forest() methods (component
#          effects of the additive and SSVS models) and plot_interactions()
#          (median gamma vs inclusion probability).
# Inputs:  fitted bcnma_bin / bcnma_cont objects
# Outputs: ggplot objects (printed, returned invisibly)
# Depends: ggplot2 (Imports); ggrepel (Suggests, optional labels)

#' Forest plot of component effects
#'
#' One row per component: posterior median and 95% CrI of the OR
#' (`bcnma_bin`, log axis) or SMD / MD (`bcnma_cont`) against "component
#' absent", for the additive and/or SSVS model.
#'
#' @param x        fitted `bcnma_bin` or `bcnma_cont` object
#' @param model    character: `"both"` (default), `"additive"` or `"ssvs"`
#'   (`"lasso"` is a deprecated alias of `"ssvs"`)
#' @param ref_line numeric: reference line (default 1 for OR, 0 for SMD / MD)
#' @param xlab     character: x-axis label
#' @param title    character: plot title
#' @param ...      additional arguments (unused)
#' @return ggplot object (invisible)
#' @name forest.bcnma_bin
#' @export forest.bcnma_bin
#' @export
forest.bcnma_bin <- function(x, model = c("both", "additive", "ssvs", "lasso"), ref_line = 1,
                             xlab = "Odds Ratio vs. no component",
                             title = "Component effects", ...) {
  .forest_bcnma(x, .normalise_model_name(match.arg(model)), ref_line, xlab, title,
                is_ratio = TRUE)
}

#' @rdname forest.bcnma_bin
#' @export forest.bcnma_cont
#' @export
forest.bcnma_cont <- function(x, model = c("both", "additive", "ssvs", "lasso"), ref_line = 0,
                              xlab = NULL, title = "Component effects", ...) {
  if (is.null(xlab)) xlab <- paste0(x$sm, " vs. no component")
  .forest_bcnma(x, .normalise_model_name(match.arg(model)), ref_line, xlab, title,
                is_ratio = FALSE)
}

.forest_bcnma <- function(x, model, ref_line, xlab, title, is_ratio) {
  model_labels <- c(additive = "Additive",
                    ssvs = if (identical(x$interactions_model, "ssvs")) "SSVS" else
                      "SSVS (informative)")
  shown_models <- names(model_labels)[names(model_labels) %in%
                                        c(model, if (model == "both") c("additive", "ssvs"))]
  shown_models <- shown_models[!vapply(shown_models, function(name) {
    is.null(x[[paste0("summary_", name)]])
  }, logical(1))]
  if (length(shown_models) == 0) stop("No model results available to plot.")

  plot_data <- do.call(rbind, lapply(shown_models, function(name) {
    .forest_rows(x[[paste0("summary_", name)]], x$components, model_labels[[name]], is_ratio)
  }))
  dodge <- if (length(shown_models) > 1) ggplot2::position_dodge(width = 0.5) else
    ggplot2::position_identity()
  forest_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = median, y = component, xmin = lower, xmax = upper, colour = model, shape = model)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(width = 0.25, orientation = "y", position = dodge) +
    ggplot2::geom_point(size = 3, position = dodge) +
    ggplot2::labs(x = xlab, y = NULL, title = title, colour = "Model", shape = "Model") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  if (is_ratio) forest_plot <- forest_plot + ggplot2::scale_x_log10()
  print(forest_plot)
  invisible(forest_plot)
}

# Median and 95% CrI of d[j] (exponentiated for OR) as plot rows.
.forest_rows <- function(posterior_summary, components, model_label, is_ratio) {
  effect_rows <- posterior_summary[paste0("d[", seq_along(components), "]"), , drop = FALSE]
  transform_scale <- if (is_ratio) exp else identity
  data.frame(
    component = factor(components, levels = rev(components)),
    median    = transform_scale(effect_rows[, "50%"]),
    lower     = transform_scale(effect_rows[, "2.5%"]),
    upper     = transform_scale(effect_rows[, "97.5%"]),
    model     = model_label,
    stringsAsFactors = FALSE
  )
}

#' Plot SSVS interaction overview: coefficient vs. inclusion probability
#'
#' Scatter plot of median gamma (x-axis) vs. mean P(Ind = 1) (y-axis) for all
#' pairwise interactions, labelling the top interactions by |gamma|.
#' Interactions fixed to 0 (not modelled, not estimable or aliased) appear at
#' (0, 0) and are never "selected".
#'
#' @param x         fitted bcnma object with an SSVS model
#' @param top_n     integer: number of interaction pairs to label (default 10)
#' @param threshold numeric: horizontal reference line at this inclusion
#'   probability (default 0.5)
#' @return ggplot object (invisible)
#' @export
plot_interactions <- function(x, top_n = 10, threshold = 0.5) {
  if (is.null(x$interaction_effects)) {
    stop("No interaction results. Fit with interactions = 'ssvs' or 'ssvs_informative'.")
  }
  interaction_table <- summarise_interactions(x, threshold = threshold)
  top_labels <- utils::head(interaction_table, top_n)
  overview_plot <- ggplot2::ggplot(interaction_table, ggplot2::aes(
    x = median_gamma, y = inclusion_prob, label = component_pair)) +
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed", colour = "red") +
    ggplot2::geom_point(ggplot2::aes(colour = selected), size = 2, alpha = 0.7) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#d62728", "FALSE" = "grey60"),
                                 labels = c("TRUE" = paste0(">= ", threshold),
                                            "FALSE" = paste0("< ", threshold)),
                                 name = "Selected") +
    ggplot2::labs(x = sprintf("Median interaction coefficient (%s scale)",
                              if (x$sm == "OR") "log-OR" else x$sm),
                  y = "Inclusion probability P(Ind = 1)", title = "SSVS interaction overview") +
    ggplot2::theme_bw(base_size = 12)
  label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(data = top_labels, ggplot2::aes(label = component_pair),
                             size = 3, max.overlaps = 20)
  } else {
    ggplot2::geom_text(data = top_labels, ggplot2::aes(label = component_pair),
                       size = 3, vjust = -0.8)
  }
  overview_plot <- overview_plot + label_layer
  print(overview_plot)
  invisible(overview_plot)
}
