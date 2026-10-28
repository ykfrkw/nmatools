# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11 (restructured 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: continuous-outcome data preparation for bcnma_cont(): arm means
#          y[i, k] and their standard errors se[i, k] on the SMD or MD scale
#          (MD means centred on arm 1 of each study).
# Inputs:  user data.frame and column names (resolved by bcnma_cont())
# Outputs: a `prep` list (R/bcnma_prepare.R) with `likelihood = "normal"`
#          and `outcome = list(y, se)`
# Depends: .parse_arm_structure(), .arm_matrix() (R/bcnma_prepare.R)

#' Prepare arm-level continuous data for Bayesian cNMA
#'
#' With `sm = "SMD"` each study's arm means and standard errors are divided
#' by the study's pooled SD, so every `d[j]` is on the SMD scale. With
#' `sm = "MD"` the raw scale is kept, but each study's means are centred on
#' the observed mean of arm 1: an exact location shift that the study
#' baseline `u[i]` absorbs, so that the baseline prior does not need to cover
#' the outcome's absolute level (e.g. a mean of 360 minutes).
#'
#' @param data,studlab_col,treat_col,components,component_separator see
#'   .parse_arm_structure()
#' @param mean_col,sd_col,n_col character: arm mean, SD and sample size columns
#' @param sm        character: "SMD" or "MD"
#' @param pooled_sd character: "within" (pool all arms) or "control" (arm 1)
#' @return `prep` list with `outcome = list(y, se)`, `sm`, the per-study
#'   `sigma_pooled` and `median_pooled_sd` (scales the MD priors)
#' @noRd
.bcnma_cont_prepare_data <- function(data, studlab_col, treat_col,
                                     mean_col, sd_col, n_col,
                                     components, component_separator,
                                     sm, pooled_sd) {
  prep <- .parse_arm_structure(data, studlab_col, treat_col, components,
                               component_separator)
  arm_mean <- .arm_matrix(as.numeric(data[[mean_col]]), prep)
  arm_sd   <- .arm_matrix(as.numeric(data[[sd_col]]), prep)
  arm_size <- .arm_matrix(floor(as.numeric(data[[n_col]])), prep)
  standard_error <- arm_sd / sqrt(arm_size)
  sigma_pooled   <- .pooled_sd_by_study(arm_sd, arm_size, prep$n_arms, pooled_sd,
                                        prep$studies)

  if (sm == "SMD") {
    arm_mean       <- arm_mean / sigma_pooled
    standard_error <- standard_error / sigma_pooled
  } else {
    arm_mean <- arm_mean - arm_mean[, 1]
  }

  prep$likelihood       <- "normal"
  prep$outcome          <- list(y = arm_mean, se = standard_error)
  prep$sm               <- sm
  prep$sigma_pooled     <- sigma_pooled
  prep$median_pooled_sd <- stats::median(sigma_pooled)
  prep
}

#' Pooled SD of each study
#'
#' @param arm_sd,arm_size `[study, arm]` matrices
#' @param n_arms   integer arms per study
#' @param pooled_sd "within" (all arms, df-weighted) or "control" (arm 1)
#' @param studies  study labels for messages
#' @return numeric vector, one SD per study
#' @noRd
.pooled_sd_by_study <- function(arm_sd, arm_size, n_arms, pooled_sd, studies) {
  sigma_pooled <- vapply(seq_along(n_arms), function(study_index) {
    arms <- seq_len(n_arms[study_index])
    if (pooled_sd == "control") return(arm_sd[study_index, 1])
    degrees_of_freedom <- arm_size[study_index, arms] - 1
    sqrt(sum(degrees_of_freedom * arm_sd[study_index, arms]^2) / sum(degrees_of_freedom))
  }, numeric(1))
  .replace_invalid_pooled_sd(sigma_pooled, studies)
}

.replace_invalid_pooled_sd <- function(sigma_pooled, studies) {
  is_invalid <- is.na(sigma_pooled) | sigma_pooled <= 0
  for (study in studies[is_invalid]) {
    warning(sprintf("Study '%s': pooled SD is non-positive or NA; skipping standardisation.",
                    study))
  }
  sigma_pooled[is_invalid] <- 1
  sigma_pooled
}
