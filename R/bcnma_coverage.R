# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: reporting aids: component_coverage() (how much evidence each
#          component and component pair has) and the table of priors used
#          by a fit (fit$priors).
# Inputs:  fitted bcnma objects / model settings
# Outputs: data.frames
# Depends: .build_interaction_array(), .interaction_pair_names()
#          (R/bcnma_interactions.R), .count_studies_with_contrast()
#          (R/bcnma_identifiability.R)

#' Evidence available for each component and component pair
#'
#' Counts, from the data of a fit, how many studies and arms contain each
#' component (and each pair of components together), and in how many studies
#' the component (pair) differs between arms, i.e. has a within-study
#' contrast that informs its effect. Useful for reporting (PRISMA-NMA
#' network geometry) and for judging which interactions the data can
#' support.
#'
#' @param fit a `bcnma_bin` or `bcnma_cont` object
#' @return list of two data.frames:
#' \describe{
#'   \item{components}{component, n_studies (studies with the component in
#'     at least one arm), n_arms, n_studies_contrast}
#'   \item{pairs}{component_pair, n_studies, n_arms (co-present),
#'     n_studies_contrast, status (identifiability, see
#'     `fit$identifiability`)}
#' }
#' @export
component_coverage <- function(fit) {
  if (!inherits(fit, c("bcnma_bin", "bcnma_cont"))) {
    stop("'fit' must be a bcnma_bin or bcnma_cont object.")
  }
  prep <- fit$prep
  is_present  <- (prep$x_arm != 0) * 1
  co_presence <- .build_interaction_array(prep$x_arm)
  pairs <- .coverage_table(co_presence, prep$n_arms)
  pairs <- cbind(component_pair = .interaction_pair_names(prep$components), pairs,
                 stringsAsFactors = FALSE)
  pairs$status <- fit$identifiability$interaction_status$status
  list(
    components = cbind(component = prep$components, .coverage_table(is_present, prep$n_arms),
                       stringsAsFactors = FALSE),
    pairs = pairs
  )
}

# Studies / arms where each slice of a `[study, arm, slice]` 0/1 array is 1,
# and studies where it varies between arms.
.coverage_table <- function(presence_array, n_arms) {
  slice_count <- dim(presence_array)[3]
  counts <- lapply(seq_len(slice_count), function(slice) {
    presence <- matrix(presence_array[, , slice], nrow = dim(presence_array)[1])
    data.frame(n_studies = sum(rowSums(presence) > 0), n_arms = sum(presence),
               n_studies_contrast = .count_studies_with_contrast(presence, n_arms))
  })
  do.call(rbind, counts)
}

#' Priors used by a fit, one row per parameter
#'
#' @param model_settings list from .bcnma_model_settings()
#' @param modifier_terms modifier term table or NULL
#' @param scale_label    "log-OR", "SMD" or "MD"
#' @return data.frame(parameter, prior, source)
#' @noRd
.prior_table <- function(model_settings, modifier_terms, scale_label) {
  ssvs <- model_settings$ssvs
  rows <- list(
    c("d[j] (component effects)",
      sprintf("normal(0, SD %s) on the %s scale", .format_number(model_settings$effect_prior_sd),
              scale_label), "package default"),
    c("u[i] (study baselines)",
      sprintf("normal(0, SD %s)", .format_number(model_settings$baseline_prior_sd)),
      "package default"),
    c("tau / tau2 (heterogeneity)", model_settings$tau_prior$label,
      model_settings$tau_prior$source),
    c("Ind[m] (SSVS inclusion)", sprintf("categorical, P(slab) = %s",
                                         .format_number(ssvs$inclusion_prob)), "SSVS model only"),
    c("eta (spike SD)", sprintf("half-normal(SD %s); slab SD = eta * sqrt(%s)",
                                .format_number(ssvs$eta_prior_sd),
                                .format_number(ssvs$slab_ratio)), "SSVS model only")
  )
  if (!is.null(modifier_terms)) {
    rows <- c(rows, lapply(modifier_terms$q, function(q) {
      c(sprintf("beta[%d] (%s)", q, modifier_terms$term[q]),
        sprintf("normal(0, SD %s) per %s%s", .format_number(modifier_terms$prior_sd[q]),
                if (modifier_terms$type[q] == "continuous") "covariate SD" else "level step",
                if (modifier_terms$monotone[q] == "none") "" else ", truncated (monotone)"),
        "modifier specification")
    }))
  }
  table <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(table) <- c("parameter", "prior", "source")
  table
}
