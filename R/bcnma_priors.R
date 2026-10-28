# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: outcome-specific default priors of bcnma_bin() / bcnma_cont():
#          component effects d, study baselines u and heterogeneity. MD
#          priors scale with the median within-study pooled SD because a
#          fixed prior (e.g. SD 31.6) is informative for outcomes measured in
#          minutes and vague for outcomes measured in points.
# Inputs:  `prep` (continuous outcomes: sm, median_pooled_sd)
# Outputs: prior SDs and heterogeneity prior spec lists
# Depends: R/bcnma_constants.R, .lognormal_tau2_prior() /
#          .half_normal_tau_prior() (R/bcnma_args.R)

#' Default heterogeneity prior for binary outcomes
#' @noRd
.default_tau_prior_bin <- function() {
  .lognormal_tau2_prior(BCNMA_TAU2_PRIOR_BIN[1], BCNMA_TAU2_PRIOR_BIN[2],
                        source = "binary default")
}

#' Default heterogeneity prior for continuous outcomes
#'
#' SMD: log-normal on tau^2. MD: half-normal on tau with SD equal to the
#' median pooled SD (heterogeneity as large as the outcome SD is unlikely).
#' @noRd
.default_tau_prior_cont <- function(prep) {
  if (prep$sm == "SMD") {
    return(.lognormal_tau2_prior(BCNMA_TAU2_PRIOR_SMD[1], BCNMA_TAU2_PRIOR_SMD[2],
                                 source = "SMD default"))
  }
  .half_normal_tau_prior(BCNMA_TAU_PRIOR_SD_PER_SD_MD * prep$median_pooled_sd,
                         source = "MD default: SD = median pooled SD")
}

#' Prior SD of d[j] ("effect") or u[i] ("baseline") for continuous outcomes
#' @noRd
.location_prior_sd_cont <- function(prep, parameter = c("effect", "baseline")) {
  parameter <- match.arg(parameter)
  if (prep$sm == "SMD") {
    return(if (parameter == "effect") BCNMA_EFFECT_PRIOR_SD_SMD else BCNMA_BASELINE_PRIOR_SD_SMD)
  }
  multiplier <- if (parameter == "effect") BCNMA_EFFECT_PRIOR_SD_PER_SD_MD else
    BCNMA_BASELINE_PRIOR_SD_PER_SD_MD
  multiplier * prep$median_pooled_sd
}
