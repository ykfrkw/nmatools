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
#' by the study's pooled SD and multiplied by Hedges' small-sample
#' correction J, so every contrast of `y` is Hedges' g and every `d[j]` is on
#' the SMD scale. The pooled SD is treated as known (its sampling error is
#' ignored, as in arm-based SMD models). With `sm = "MD"` the raw scale is
#' kept, but each study's means are centred on the observed mean of arm 1: an
#' exact location shift that the study baseline `u[i]` absorbs, so that the
#' baseline prior does not need to cover the outcome's absolute level (e.g. a
#' mean of 360 minutes).
#'
#' @param data,studlab_col,treat_col,components,component_separator,inactive
#'   see .parse_arm_structure()
#' @param mean_col,sd_col,n_col character: arm mean, SD and sample size columns
#' @param sm        character: "SMD" or "MD"
#' @param pooled_sd character: "within" (pool all arms) or "control" (the SD
#'   of arm 1, i.e. the first row of the study in `data`)
#' @return `prep` list with `outcome = list(y, se)`, `sm`, the per-study
#'   `sigma_pooled`, `hedges_j` (1 for MD) and `median_pooled_sd` (scales the
#'   MD priors)
#' @noRd
.bcnma_cont_prepare_data <- function(data, studlab_col, treat_col,
                                     mean_col, sd_col, n_col,
                                     components, component_separator,
                                     sm, pooled_sd, inactive = character(0)) {
  prep <- .parse_arm_structure(data, studlab_col, treat_col, components,
                               component_separator, inactive)
  arm_mean <- .arm_matrix(as.numeric(data[[mean_col]]), prep)
  arm_sd   <- .arm_matrix(as.numeric(data[[sd_col]]), prep)
  arm_size <- .arm_matrix(floor(as.numeric(data[[n_col]])), prep)
  .check_arm_sd_and_size(arm_sd, arm_size, prep)
  standard_error <- arm_sd / sqrt(arm_size)
  pooled <- .pooled_sd_by_study(arm_sd, arm_size, prep$n_arms, pooled_sd)

  hedges_j <- rep(1, prep$n_studies)
  if (sm == "SMD") {
    hedges_j       <- .hedges_correction(pooled$degrees_of_freedom)
    arm_mean       <- arm_mean * hedges_j / pooled$sd
    standard_error <- standard_error * hedges_j / pooled$sd
  } else {
    arm_mean <- arm_mean - arm_mean[, 1]
  }

  prep$likelihood       <- "normal"
  prep$outcome          <- list(y = arm_mean, se = standard_error)
  prep$sm               <- sm
  prep$sigma_pooled     <- pooled$sd
  prep$hedges_j         <- hedges_j
  prep$median_pooled_sd <- stats::median(pooled$sd)
  prep
}

#' Stop on missing or non-positive arm SDs or sample sizes
#'
#' Every arm's standard error needs both; replacing a bad SD by 1 (the old
#' behaviour) silently put that study on a wrong scale.
#' @noRd
.check_arm_sd_and_size <- function(arm_sd, arm_size, prep) {
  arm_table  <- prep$arm_table
  arm_cells  <- cbind(arm_table$study_index, arm_table$arm)
  is_bad_arm <- is.na(arm_sd[arm_cells]) | arm_sd[arm_cells] <= 0 |
    is.na(arm_size[arm_cells]) | arm_size[arm_cells] <= 0
  if (!any(is_bad_arm)) return(invisible(NULL))
  where <- paste0(arm_table$study[is_bad_arm], " (arm ", arm_table$arm[is_bad_arm], ")")
  stop("SD and n must be positive and not NA in every arm. Problem arms: ",
       paste(where, collapse = ", "), call. = FALSE)
}

#' Pooled SD of each study and its degrees of freedom
#'
#' @param arm_sd,arm_size `[study, arm]` matrices
#' @param n_arms    integer arms per study
#' @param pooled_sd "within" (all arms, df-weighted) or "control" (arm 1)
#' @return list(sd, degrees_of_freedom), one value per study
#' @noRd
.pooled_sd_by_study <- function(arm_sd, arm_size, n_arms, pooled_sd) {
  arms_used <- lapply(n_arms, function(arm_count) {
    if (pooled_sd == "control") 1L else seq_len(arm_count)
  })
  degrees_of_freedom <- vapply(seq_along(n_arms), function(study_index) {
    sum(arm_size[study_index, arms_used[[study_index]]] - 1)
  }, numeric(1))
  sum_of_squares <- vapply(seq_along(n_arms), function(study_index) {
    arms <- arms_used[[study_index]]
    sum((arm_size[study_index, arms] - 1) * arm_sd[study_index, arms]^2)
  }, numeric(1))
  list(sd = sqrt(sum_of_squares / degrees_of_freedom), degrees_of_freedom = degrees_of_freedom)
}

#' Hedges' small-sample correction J = 1 - 3 / (4 df - 1)
#' @noRd
.hedges_correction <- function(degrees_of_freedom) {
  1 - 3 / (4 * degrees_of_freedom - 1)
}
