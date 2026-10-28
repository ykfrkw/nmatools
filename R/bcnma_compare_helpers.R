# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: helpers of compare_combinations(): conversion of user
#          combinations to component values, labels, predictive draws for a
#          new study, and flags for combinations / component pairs that no
#          arm of the data contains.
# Inputs:  combinations as names, indices or named values; fitted objects
# Outputs: value vectors, labels, draws, flag lists
# Depends: base R; .observed_combinations() (R/bcnma_additivity.R)

#' Predictive draws for a new study: effect + N(0, tau^2)
#'
#' Seeded with the fit's seed so that the interval is reproducible; the
#' caller's RNG state is restored.
#' @param effect numeric posterior draws of the comparison (linear scale)
#' @param tau2   numeric posterior draws of tau^2, or NULL
#' @param seed   integer seed or NULL
#' @return numeric draws, or NULL when tau2 is not available
#' @noRd
.predictive_effect_samples <- function(effect, tau2, seed) {
  if (is.null(tau2)) return(NULL)
  has_global_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_global_seed) saved_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (has_global_seed) assign(".Random.seed", saved_seed, envir = globalenv())
  })
  if (!is.null(seed)) set.seed(seed)
  effect + stats::rnorm(length(effect), mean = 0, sd = sqrt(tau2))
}

#' Combinations and component pairs that no arm contains
#'
#' @return list(is_extrapolated = c(combo1, combo2), never_copresent_pairs)
#' @noRd
.combination_flags <- function(x, values1, values2) {
  no_flags <- list(is_extrapolated = c(combo1 = NA, combo2 = NA),
                   never_copresent_pairs = character(0))
  if (is.null(x$prep)) return(no_flags)
  observed_presence <- .observed_combinations(x$prep)$values != 0
  is_observed <- function(values) {
    is_present <- values != 0
    if (!any(is_present)) return(TRUE)
    any(apply(observed_presence, 1, function(pattern) all(pattern == is_present)))
  }
  arm_presence <- .build_interaction_array(x$prep$x_arm)
  is_copresent <- apply(arm_presence != 0, 3, any)
  pair_indices <- unique(c(all.interactions(which(values1 != 0), x$n_components),
                           all.interactions(which(values2 != 0), x$n_components)))
  never_pairs <- pair_indices[!is_copresent[pair_indices]]
  list(is_extrapolated = c(combo1 = !is_observed(values1), combo2 = !is_observed(values2)),
       never_copresent_pairs = .interaction_pair_names(x$components)[never_pairs])
}

.print_comparison_notes <- function(flags, core) {
  combo_labels <- list(combo1 = core$combo1_names, combo2 = core$combo2_names)
  for (combo in names(which(flags$is_extrapolated))) {
    cat(sprintf("  Note: [%s] is not observed in any arm: extrapolated under additivity.\n",
                paste(combo_labels[[combo]], collapse = "+")))
  }
  if (length(flags$never_copresent_pairs) > 0) {
    cat("  Note: component pairs never co-present in an arm:",
        paste(flags$never_copresent_pairs, collapse = ", "), "\n")
  }
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
