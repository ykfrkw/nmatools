# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: MCMC convergence diagnostics of every fitted bcnma model: the
#          largest Rhat and smallest effective sample size per parameter
#          group (d, gamma, tau2, beta, eta), with a warning when they fail
#          the thresholds, and the Monte Carlo standard error of SSVS
#          inclusion probabilities.
# Inputs:  MCMCsummary tables (columns Rhat, n.eff) and combined samples
# Outputs: data.frames stored in fit$convergence; console lines
# Depends: R/bcnma_constants.R

BCNMA_RHAT_THRESHOLD <- 1.05
BCNMA_MIN_EFFECTIVE_SIZE <- 400
BCNMA_DIAGNOSED_GROUPS <- c("d", "gamma", "tau2", "beta", "eta")

#' Largest Rhat and smallest n.eff per parameter group
#'
#' @param posterior_summary MCMCsummary data.frame (rows "d[1]", "tau2", ...)
#' @return data.frame(group, n_parameters, max_rhat, min_n_eff); Rhat is NA
#'   with one chain
#' @noRd
.convergence_by_group <- function(posterior_summary) {
  node_group <- sub("\\[.*$", "", rownames(posterior_summary))
  groups <- intersect(BCNMA_DIAGNOSED_GROUPS, node_group)
  rows <- lapply(groups, function(group) {
    group_rows <- posterior_summary[node_group == group, , drop = FALSE]
    rhat  <- suppressWarnings(as.numeric(group_rows[["Rhat"]]))
    n_eff <- suppressWarnings(as.numeric(group_rows[["n.eff"]]))
    data.frame(group = group, n_parameters = nrow(group_rows),
               max_rhat  = if (all(is.na(rhat))) NA_real_ else max(rhat, na.rm = TRUE),
               min_n_eff = if (all(is.na(n_eff))) NA_real_ else min(n_eff, na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Warn once per model when convergence thresholds are not met
#' @noRd
.warn_convergence <- function(convergence, model_label) {
  if (is.null(convergence)) return(invisible(NULL))
  has_high_rhat <- !is.na(convergence$max_rhat) & convergence$max_rhat > BCNMA_RHAT_THRESHOLD
  has_low_n_eff <- !is.na(convergence$min_n_eff) &
    convergence$min_n_eff < BCNMA_MIN_EFFECTIVE_SIZE
  if (!any(has_high_rhat | has_low_n_eff)) return(invisible(NULL))
  problems <- c(
    if (any(has_high_rhat)) sprintf("Rhat > %s for %s", BCNMA_RHAT_THRESHOLD,
                                    paste(convergence$group[has_high_rhat], collapse = ", ")),
    if (any(has_low_n_eff)) sprintf("n.eff < %s for %s", BCNMA_MIN_EFFECTIVE_SIZE,
                                    paste(convergence$group[has_low_n_eff], collapse = ", "))
  )
  warning(sprintf("%s model convergence: %s. Run longer chains (n.iter) or more burn-in.",
                  model_label, paste(problems, collapse = "; ")), call. = FALSE)
}

#' Monte Carlo standard error of each inclusion probability mean(Ind[m])
#'
#' MCSE = sqrt(p (1 - p) / n.eff); n.eff of a constant indicator is
#' undefined, so the number of draws is used instead.
#' @param samples           combined SSVS samples
#' @param posterior_summary MCMCsummary of the SSVS model
#' @param pair_count        number of component pairs
#' @return numeric vector, one per pair (NA for pairs not in the model)
#' @noRd
.inclusion_prob_mcse <- function(samples, posterior_summary, pair_count) {
  vapply(seq_len(pair_count), function(pair_index) {
    node <- paste0("Ind[", pair_index, "]")
    if (!node %in% colnames(samples)) return(NA_real_)
    inclusion_prob <- mean(samples[[node]])
    n_eff <- suppressWarnings(as.numeric(posterior_summary[node, "n.eff"]))
    if (is.na(n_eff) || n_eff <= 0) n_eff <- nrow(samples)
    sqrt(inclusion_prob * (1 - inclusion_prob) / n_eff)
  }, numeric(1))
}

#' One console line per model with the worst Rhat / n.eff
#' @noRd
.print_convergence <- function(convergence) {
  if (is.null(convergence) || nrow(convergence) == 0) return(invisible(NULL))
  worst_rhat  <- suppressWarnings(max(convergence$max_rhat, na.rm = TRUE))
  worst_n_eff <- suppressWarnings(min(convergence$min_n_eff, na.rm = TRUE))
  cat(sprintf("Convergence: max Rhat %s, min n.eff %s (over %s; thresholds Rhat <= %s, ",
              if (is.finite(worst_rhat)) sprintf("%.3f", worst_rhat) else "NA (1 chain)",
              if (is.finite(worst_n_eff)) sprintf("%.0f", worst_n_eff) else "NA",
              paste(convergence$group, collapse = ", "), BCNMA_RHAT_THRESHOLD),
      sprintf("n.eff >= %s)\n", BCNMA_MIN_EFFECTIVE_SIZE), sep = "")
}
