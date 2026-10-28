# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-11
#
# File description:
# Data preparation helpers for continuous-outcome Bayesian cNMA.
# Handles SMD (standardised mean difference) and MD (mean difference).

#' Prepare arm-level continuous data for Bayesian cNMA
#'
#' Converts long-format arm-level continuous data into JAGS-ready matrices.
#' When \code{sm = "SMD"}, arm means are standardised by the within-study
#' pooled SD before passing to JAGS, so all \code{d[k]} parameters are on
#' the SMD scale. When \code{sm = "MD"}, raw means are used.
#'
#' @param data               data.frame with study-arm level rows
#' @param studlab_col        character: column name for study ID
#' @param treat_col          character: column name for treatment combination
#' @param mean_col           character: column name for arm mean
#' @param sd_col             character: column name for arm SD
#' @param n_col              character: column name for arm sample size
#' @param components         character vector of component abbreviations
#' @param component_separator character: separator in treat column
#' @param sm                 character: "SMD" or "MD"
#' @param pooled_sd          character: "within" (pool all arms) or
#'                           "control" (reference arm only); used when sm="SMD"
#' @return list with JAGS-ready arrays and metadata
#' @noRd
.bcnma_cont_prepare_data <- function(data, studlab_col, treat_col,
                                     mean_col, sd_col, n_col,
                                     components, component_separator,
                                     sm, pooled_sd) {
  d <- data.frame(
    study = as.character(data[[studlab_col]]),
    treat = as.character(data[[treat_col]]),
    y     = as.numeric(data[[mean_col]]),
    sd    = as.numeric(data[[sd_col]]),
    n     = as.integer(floor(as.numeric(data[[n_col]]))),
    # Original row position survives the study re-ordering below, so that
    # row-aligned inputs (component_values, modifier columns) can be matched.
    row_id = seq_len(nrow(data))
  )

  d <- d[order(d$study), ]
  studies  <- unique(d$study)
  Ns       <- length(studies)
  Nc       <- length(components)

  # Assign arm numbers within each study
  na_vec <- integer(Ns)
  d$arm  <- 0L
  for (i in seq_along(studies)) {
    idx       <- which(d$study == studies[i])
    na_vec[i] <- length(idx)
    d$arm[idx] <- seq_along(idx)
  }
  max_arms <- max(na_vec)

  # Component indicator array: xA[study, arm, component]
  xA <- array(0L, dim = c(Ns, max_arms, Nc))
  for (i in seq_along(studies)) {
    rows <- d[d$study == studies[i], ]
    for (k in seq_len(na_vec[i])) {
      parts <- trimws(strsplit(rows$treat[k], component_separator, fixed = TRUE)[[1]])
      for (j in seq_along(components)) {
        xA[i, k, j] <- as.integer(components[j] %in% parts)
      }
    }
  }

  # Reference arm (arm 1) indicators: xB[study, component]
  xB <- xA[, 1, , drop = FALSE]
  dim(xB) <- c(Ns, Nc)

  # Build y (standardised or raw) and precision matrices
  y_mat    <- matrix(NA_real_, Ns, max_arms)
  prec_mat <- matrix(NA_real_, Ns, max_arms)
  sigma_pooled_vec <- rep(NA_real_, Ns)  # for back-transformation

  for (i in seq_along(studies)) {
    rows <- d[d$study == studies[i], ]
    ni   <- na_vec[i]

    if (sm == "SMD") {
      if (pooled_sd == "within") {
        # Pool SD across all arms (Hedges' approach)
        sigma_p <- sqrt(sum((rows$n - 1) * rows$sd^2) / (sum(rows$n) - ni))
      } else {
        # Reference arm (arm 1) SD
        sigma_p <- rows$sd[1]
      }
      if (is.na(sigma_p) || sigma_p <= 0) {
        warning(sprintf("Study '%s': pooled SD is non-positive or NA; skipping standardisation.",
                        studies[i]))
        sigma_p <- 1
      }
      sigma_pooled_vec[i]     <- sigma_p
      y_mat[i, seq_len(ni)]   <- rows$y / sigma_p
      # SE of standardised mean = SD / (sigma_p * sqrt(n))
      prec_mat[i, seq_len(ni)] <- (sigma_p * sqrt(rows$n) / rows$sd)^2

    } else {
      # MD: raw scale, SE = SD / sqrt(n)
      sigma_pooled_vec[i]     <- NA_real_
      y_mat[i, seq_len(ni)]   <- rows$y
      prec_mat[i, seq_len(ni)] <- rows$n / rows$sd^2
    }
  }

  # Warn about components not present in any arm
  comp_used <- apply(xA != 0, 3, sum)
  if (any(comp_used == 0)) {
    warning("The following components appear in no arm: ",
            paste(components[comp_used == 0], collapse = ", "))
  }

  list(
    y            = y_mat,
    prec         = prec_mat,
    xA           = xA,
    xB           = xB,
    na           = na_vec,
    Ns           = Ns,
    Nc           = Nc,
    max_arms     = max_arms,
    studies      = studies,
    components   = components,
    sm           = sm,
    sigma_pooled = sigma_pooled_vec,
    data         = d
  )
}
