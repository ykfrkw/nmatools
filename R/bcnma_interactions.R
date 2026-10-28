# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (restructured 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: pairwise component interactions: the arm-level co-presence array
#          used by the SSVS model, the (i, j) <-> index mapping (place(),
#          which.place(), all.interactions()) and interaction summaries
#          (summarise_interactions()).
# Inputs:  component value arrays / fitted bcnma objects
# Outputs: interaction arrays, indices and summary data.frames
# Depends: base R only

#' Arm-level co-presence of every component pair
#'
#' Interactions are defined on component *presence*, so that with component
#' values (doses) gamma keeps its meaning as the extra effect of
#' co-presence, not of a dose product.
#'
#' @param x_arm array `[study, arm, component]` of component values
#' @return array `[study, arm, pair]` of 0/1, pairs in place() order
#' @noRd
.build_interaction_array <- function(x_arm) {
  is_present   <- x_arm != 0
  n_components <- dim(x_arm)[3]
  pair_count   <- n_components * (n_components - 1L) / 2L
  co_presence  <- array(0, dim = c(dim(x_arm)[1:2], pair_count))
  pair_index   <- 0L
  for (first in seq_len(n_components - 1L)) {
    for (second in (first + 1L):n_components) {
      pair_index <- pair_index + 1L
      co_presence[, , pair_index] <- is_present[, , first] * is_present[, , second]
    }
  }
  co_presence
}

#' Labels "a - b" of every component pair, in place() order
#' @noRd
.interaction_pair_names <- function(components) {
  pair_count <- length(components) * (length(components) - 1L) / 2L
  vapply(seq_len(pair_count), function(index) which.place(index, components)[["names"]],
         character(1))
}

# ---- Interaction indexing ----

#' Map component pair (i, j) to pairwise interaction index
#'
#' Numbering follows upper-triangle row-major order.
#' e.g., for Nc=4: (1,2)->1, (1,3)->2, (1,4)->3, (2,3)->4, (2,4)->5, (3,4)->6
#'
#' @param i          integer or character: first component (index or name)
#' @param j          integer or character: second component (index or name)
#' @param Nc         integer total number of components
#' @param components character vector of component names (required when i or j
#'   is character)
#' @return integer interaction index
#' @export
place <- function(i, j, Nc, components = NULL) {
  resolve_index <- function(component) {
    if (!is.character(component)) return(as.integer(component))
    if (is.null(components)) stop("'components' must be provided when i or j is character")
    index <- which(components == component)
    if (length(index) == 0) stop("Component '", component, "' not found in components vector")
    index
  }
  first  <- min(resolve_index(i), resolve_index(j))
  second <- max(resolve_index(i), resolve_index(j))
  if (first == second) stop("i and j must refer to different components")
  as.integer((2L * Nc - first) * (first - 1L) / 2L + second - first)
}

#' Identify component pair from an interaction index
#'
#' @param idx        integer interaction index
#' @param components character vector of component names
#' @return named character vector: c(indices = "i j", names = "comp_i - comp_j")
#' @export
which.place <- function(idx, components) {
  Nc <- length(components)
  for (first in seq_len(Nc - 1L)) {
    for (second in (first + 1L):Nc) {
      if (place(first, second, Nc) != idx) next
      return(c(indices = paste(first, second),
               names   = paste(components[first], "-", components[second])))
    }
  }
  stop(sprintf("Interaction index %d not found for Nc = %d", idx, Nc))
}

#' Get all pairwise interaction indices for a component combination
#'
#' @param combination integer or character vector of component indices (or names)
#' @param Nc          integer total number of components
#' @param components  character vector of component names (required if
#'   combination is character)
#' @return integer vector of interaction indices; empty if fewer than 2 components
#' @export all.interactions
all.interactions <- function(combination, Nc, components = NULL) {
  if (is.character(combination)) {
    if (is.null(components)) stop("'components' must be provided when 'combination' is character")
    combination <- which(components %in% combination)
  }
  combination <- sort(unique(as.integer(combination)))
  if (length(combination) < 2L) return(integer(0))
  pairs <- utils::combn(combination, 2)
  vapply(seq_len(ncol(pairs)), function(column) {
    place(pairs[1, column], pairs[2, column], Nc)
  }, integer(1))
}

#' Summarise interaction results from a bcnma object
#'
#' @param x          bcnma object
#' @param threshold  numeric: inclusion probability threshold for "selected"
#'   label (default 0.5)
#' @param top_n      integer: how many top interactions to return (default all)
#' @return data.frame sorted by |median_gamma| descending
#' @export
summarise_interactions <- function(x, threshold = 0.5, top_n = NULL) {
  if (is.null(x$interaction_inclusion_prob)) {
    stop("No interaction results found. Fit with interactions = 'lasso' or ",
         "'lasso_informative'.")
  }
  pair_count <- x$n_components * (x$n_components - 1L) / 2L
  summary_table <- data.frame(
    index          = seq_len(pair_count),
    component_pair = .interaction_pair_names(x$components),
    median_gamma   = unname(x$interaction_effects),
    OR_interaction = exp(unname(x$interaction_effects)),
    inclusion_prob = unname(x$interaction_inclusion_prob),
    stringsAsFactors = FALSE
  )
  summary_table$selected <- summary_table$inclusion_prob >= threshold
  summary_table <- summary_table[order(abs(summary_table$median_gamma), decreasing = TRUE), ]
  rownames(summary_table) <- NULL
  if (!is.null(top_n)) summary_table <- utils::head(summary_table, top_n)
  summary_table
}

#' Per-interaction inclusion probability and median gamma
#'
#' Interactions that are not in the SSVS model (fixed to 0) have no columns
#' in the samples and are reported as 0.
#'
#' @param samples    data.frame of combined MCMC samples from the SSVS model
#' @param components character vector of component names
#' @return list(inclusion_prob, effects), both named numeric vectors
#' @noRd
.extract_interaction_summary <- function(samples, components) {
  pair_names <- .interaction_pair_names(components)
  column_summary_or_zero <- function(prefix, index, summary_function) {
    column <- paste0(prefix, "[", index, "]")
    if (!column %in% colnames(samples)) return(0)
    summary_function(samples[[column]], na.rm = TRUE)
  }
  inclusion_prob <- vapply(seq_along(pair_names), function(index) {
    column_summary_or_zero("Ind", index, mean)
  }, numeric(1))
  effects <- vapply(seq_along(pair_names), function(index) {
    column_summary_or_zero("gamma", index, stats::median)
  }, numeric(1))
  list(inclusion_prob = stats::setNames(inclusion_prob, pair_names),
       effects        = stats::setNames(effects, pair_names))
}
