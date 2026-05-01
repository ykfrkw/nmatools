#' @title Partially contextualised evidence framework
#'
#' @description Contextualises NMA effect estimates by converting them to
#'   absolute effects and classifying them into groups defined by user-supplied
#'   thresholds.  The group numbering mirrors \code{\link{min_context}()}:
#'   the bin that contains \code{abs_effect = 0} (the reference position) is
#'   Group 0; bins above are Group +1, +2, …; bins below are Group -1, -2, …
#'
#' @details
#' Two contextualisation modes are supported:
#'
#' \strong{Binary outcome} (\code{outcome_type = "binary"}):
#' The NMA log-OR (or log-RR) is converted to an absolute risk difference
#' (ARD) relative to the control event rate (\code{cer}).  When
#' \code{small_values = "undesirable"} (default) the ARD is used as-is;
#' when \code{small_values = "desirable"} the sign is flipped so that
#' positive \code{abs_effect} always means the beneficial direction.
#' Thresholds should be expressed in the same \code{abs_effect} units.
#'
#' \strong{Continuous outcome} (\code{outcome_type = "continuous"}):
#' The effect estimate (MD or SMD) is used directly (sign-flipped when
#' \code{small_values = "desirable"}).
#'
#' The bin that straddles zero (\code{findInterval(0, thresholds)}) becomes
#' Group 0.  Treatments in higher bins receive positive group numbers and
#' treatments in lower bins receive negative group numbers.
#'
#' @param x A \code{netmeta} object.
#' @param reference Reference treatment name.
#' @param thresholds Numeric vector of cut-points on the \code{abs_effect}
#'   scale (sorted automatically).  \eqn{k} thresholds produce \eqn{k+1}
#'   groups.  Example for binary: \code{c(-0.05, 0.05, 0.15)}; for
#'   continuous (SMD): \code{c(-0.2, 0.2, 0.5, 0.8)}.
#' @param cer Control event rate (0–1) for binary outcomes.  Use
#'   \code{"metaprop"} or \code{"simple"} for automatic estimation.
#' @param outcome_type \code{"binary"} or \code{"continuous"}.
#'   Inferred from \code{x$sm} when \code{NULL}.
#' @param small_values \code{"desirable"} or \code{"undesirable"} (default).
#'   When \code{"desirable"}, \code{abs_effect} is sign-flipped so that
#'   positive values indicate the beneficial direction.
#' @param common Logical. Use common-effects model? Default \code{FALSE}.
#' @param fixed Deprecated. Use \code{common} instead.
#' @param cinema Path to CINeMA report CSV or data frame (optional).
#' @param n_thresholds N-based quality thresholds \code{c(lo, hi)}.
#'   Default \code{c(100, 400)}. \code{NULL} omits the quality columns.
#' @param digits Decimal places used when formatting threshold values in the
#'   \code{"threshold_labels"} attribute.  Default \code{2}.
#' @return A data frame (compatible with \code{\link{table_min_context}()})
#'   with columns:
#'   \itemize{
#'     \item \code{treatment}: treatment name.
#'     \item \code{abs_effect}: signed absolute effect (positive = beneficial).
#'     \item \code{group}: integer group number (0 = same bin as reference;
#'       positive = more beneficial; negative = more harmful).
#'     \item \code{cinema}: CINeMA rating (if \code{cinema} provided).
#'     \item \code{n_total}, \code{n_quality}: participant count and quality
#'       tier (if \code{n_thresholds} not \code{NULL}).
#'   }
#'   Attributes: \code{"reference"}, \code{"thresholds"} (sorted),
#'   \code{"zero_bin"} (bin index of Group 0),
#'   \code{"threshold_labels"} (human-readable bin labels).
#'   Rows: reference first, then by \code{abs_effect} descending.
#' @export
#'
#' @importFrom stats setNames
part_context <- function(x,
                         reference,
                         thresholds,
                         cer          = NULL,
                         outcome_type = NULL,
                         small_values = "undesirable",
                         common       = FALSE,
                         fixed        = NULL,
                         cinema       = NULL,
                         n_thresholds = c(100, 400),
                         digits       = 2) {

  small_values <- match.arg(small_values, c("desirable", "undesirable"))

  if (!is.null(fixed)) {
    message(
      "Argument 'fixed' is deprecated; use 'common' instead."
    )
    common <- fixed
  }

  if (!is.numeric(thresholds) || length(thresholds) < 1L)
    stop("`thresholds` must be a non-empty numeric vector.")

  thresholds <- sort(thresholds)

  # Bin that straddles abs_effect = 0 → Group 0
  zero_bin <- findInterval(0, thresholds)   # integer 0 .. length(thresholds)

  # Direction: undesirable → larger value = higher group (+)
  #            desirable   → larger value = lower  group (-)
  direction <- if (small_values == "desirable") -1L else 1L

  # ---- 1. Infer outcome type ----
  if (is.null(outcome_type)) {
    outcome_type <- if (x$sm %in% c("OR", "RR", "RD")) "binary" else
      "continuous"
  }
  outcome_type <- match.arg(outcome_type, c("binary", "continuous"))

  # ---- 2. Estimate CER for binary outcomes ----
  if (outcome_type == "binary" && x$sm %in% c("OR", "RR")) {
    if (is.null(cer) || identical(cer, "metaprop")) {
      cer <- .estimate_cer(x, reference, method = "metaprop")
    } else if (identical(cer, "simple")) {
      cer <- .estimate_cer(x, reference, method = "simple")
    }
  }

  # ---- 3. Matrices ----
  te_mat <- .nm_mat(x, "TE", common)

  treatments <- x$trts

  # ---- 4. Parse CINeMA ----
  cinema_df <- if (!is.null(cinema)) parse_cinema(cinema) else NULL

  # ---- 5. Per-treatment computation ----
  abs_effects <- numeric(length(treatments))
  groups      <- integer(length(treatments))
  names(abs_effects) <- treatments
  names(groups)      <- treatments

  for (trt in treatments) {
    if (trt == reference) {
      abs_effects[trt] <- 0
      groups[trt]      <- 0L
      next
    }

    if (!trt %in% rownames(te_mat) || !reference %in% rownames(te_mat)) {
      abs_effects[trt] <- NA_real_
      groups[trt]      <- NA_integer_
      next
    }

    te <- te_mat[trt, reference]   # log-OR/RR or raw MD/SMD

    # Absolute effect: raw ARD (binary) or raw TE (continuous), no sign flip
    if (outcome_type == "binary") {
      abs_val <- if (x$sm == "OR") {
        .or_to_ar(exp(te), cer) - cer
      } else if (x$sm == "RR") {
        cer * (exp(te) - 1)
      } else {
        te   # RD: already ARD
      }
    } else {
      abs_val <- te
    }

    abs_effects[trt] <- abs_val

    # Group number: (bin_idx - zero_bin) * direction
    #   undesirable: larger abs_val → higher bin → positive group
    #   desirable:   larger abs_val → higher bin → negative group (flipped)
    bin_idx     <- findInterval(abs_val, thresholds)
    groups[trt] <- (as.integer(bin_idx) - as.integer(zero_bin)) * direction
  }

  # ---- 6. CINeMA ratings ----
  cinema_ratings <- rep(NA_character_, length(treatments))
  names(cinema_ratings) <- treatments
  if (!is.null(cinema_df)) {
    for (trt in treatments) {
      if (trt != reference)
        cinema_ratings[trt] <- get_cinema_rating(trt, reference, cinema_df)
    }
  }

  # ---- 7. N-based quality ----
  n_total   <- rep(NA_integer_,   length(treatments))
  n_quality <- rep(NA_character_, length(treatments))
  names(n_total)   <- treatments
  names(n_quality) <- treatments

  if (!is.null(n_thresholds)) {
    if (length(n_thresholds) != 2L || !is.numeric(n_thresholds))
      stop("`n_thresholds` must be a numeric vector of length 2.")
    n_thresholds <- sort(n_thresholds)

    d <- tryCatch(x$data, error = function(e) NULL)
    if (!is.null(d)) {
      t1_col <- grep("^treat1$", colnames(d), value = TRUE)
      t2_col <- grep("^treat2$", colnames(d), value = TRUE)
      n1_col <- grep("^n1$",     colnames(d), value = TRUE)
      n2_col <- grep("^n2$",     colnames(d), value = TRUE)

      for (trt in treatments) {
        if (length(t1_col) && length(n1_col) && length(n2_col)) {
          mask1 <- as.character(d[[t1_col]]) == trt
          mask2 <- as.character(d[[t2_col]]) == trt
          n_arms <- sum(d[[n1_col]][mask1], na.rm = TRUE) +
                    sum(d[[n2_col]][mask2], na.rm = TRUE)
          n_total[trt] <- as.integer(n_arms)
        }
        nt <- n_total[trt]
        if (!is.na(nt)) {
          n_quality[trt] <- if (nt >= n_thresholds[2]) "High" else
                            if (nt >= n_thresholds[1]) "Moderate" else "Low"
        }
      }
    }
  }

  # ---- 8. Build result ----
  result <- data.frame(
    treatment  = treatments,
    abs_effect = abs_effects[treatments],
    group      = groups[treatments],
    cinema     = cinema_ratings[treatments],
    stringsAsFactors = FALSE
  )

  if (!is.null(n_thresholds)) {
    result$n_total   <- n_total[treatments]
    result$n_quality <- n_quality[treatments]
  }

  attr(result, "reference")        <- reference
  attr(result, "thresholds")       <- thresholds
  attr(result, "zero_bin")         <- zero_bin
  attr(result, "direction")        <- direction
  attr(result, "threshold_labels") <- .make_threshold_labels(
    thresholds, zero_bin, direction, digits
  )

  # Sort: reference first, then by group descending (Group +N first)
  ref_row <- result[result$treatment == reference, ]
  non_ref <- result[result$treatment != reference, ]
  non_ref <- non_ref[order(non_ref$group, decreasing = TRUE,
                           na.last = TRUE), ]
  result  <- rbind(ref_row, non_ref)
  rownames(result) <- NULL
  result
}


# ---------------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------------

#' Build human-readable group labels from thresholds
#'
#' @param thresholds Sorted numeric vector of cut-points.
#' @param zero_bin   Integer index of the Group-0 bin (0-based).
#' @param direction  1L (undesirable: larger = better) or -1L (desirable).
#' @param digits     Decimal places.
#' @return Named character vector: names are "Group +N" / "Group 0" /
#'   "Group -N"; values are human-readable range labels.
#' @keywords internal
.make_threshold_labels <- function(thresholds, zero_bin,
                                   direction = 1L, digits = 2) {
  fmt    <- paste0("%+.", digits, "f")
  n      <- length(thresholds)
  n_bins <- n + 1L

  range_labels <- character(n_bins)
  range_labels[1L] <- paste0("< ", sprintf(fmt, thresholds[1L]))
  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      range_labels[i + 1L] <- paste0(
        sprintf(fmt, thresholds[i]),
        " to <",
        sprintf(fmt, thresholds[i + 1L])
      )
    }
  }
  range_labels[n_bins] <- paste0("\u2265 ", sprintf(fmt, thresholds[n]))

  # bin indices 0..(n_bins-1); group number applies direction
  bin_indices <- seq(0L, n_bins - 1L)
  group_nums  <- (bin_indices - as.integer(zero_bin)) * as.integer(direction)
  group_names <- ifelse(
    group_nums > 0L, paste0("Group +", group_nums),
    ifelse(group_nums == 0L, "Group 0", paste0("Group ", group_nums))
  )

  stats::setNames(range_labels, group_names)
}
