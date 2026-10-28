# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: decide before fitting which parameters the within-study
#          contrasts can identify. Rows of the design are the contrasts arm
#          k vs arm 1 (k >= 2) of every study; columns are the component
#          main effects followed by the pairwise interactions.
#            * A main effect with zero contrast, or aliased with other main
#              effects (rank deficiency), is an error: d[j] would be
#              identified by its prior only.
#            * An interaction never co-present in any arm, or with zero
#              within-study contrast, is "not estimable"; one aliased with
#              the main effects or with earlier interactions (QR pivot) is
#              "aliased". Both are fixed to gamma = 0 (left out of the SSVS
#              model) and never reported as selected.
# Inputs:  `prep` (x_arm, n_arms, components), candidate SSVS pairs
# Outputs: list(main_rank, design_rank, interaction_status, ssvs_pairs)
# Depends: base R (qr()), .within_study_contrast() (R/bcnma_fit.R),
#          .build_interaction_array() (R/bcnma_interactions.R)

BCNMA_INTERACTION_STATUS <- c(
  estimable          = "estimable",
  never_copresent    = "not estimable: never co-present in an arm",
  zero_contrast      = "not estimable: zero within-study contrast",
  aliased            = "not separately estimable (aliased)"
)

#' Stack the within-study contrasts (arms k >= 2) of an arm-level array
#'
#' @return matrix with one row per contrast and one column per slice
#' @noRd
.contrast_rows <- function(arm_array, n_arms) {
  contrast <- .within_study_contrast(arm_array, n_arms)
  rows <- lapply(seq_along(n_arms), function(study_index) {
    arms <- seq_len(n_arms[study_index])[-1]
    matrix(contrast[study_index, arms, ], nrow = length(arms))
  })
  do.call(rbind, rows)
}

#' Indices of columns that QR moves behind the rank (linearly dependent)
#'
#' LINPACK QR (the default of qr()) pivots only columns that are dependent
#' on earlier ones, so earlier columns are always kept.
#' @noRd
.dependent_columns <- function(design) {
  decomposition <- qr(design, tol = BCNMA_RANK_TOLERANCE)
  if (decomposition$rank == ncol(design)) return(integer(0))
  sort(decomposition$pivot[seq(decomposition$rank + 1, ncol(design))])
}

#' Stop when a component main effect is not identified by the contrasts
#' @noRd
.check_main_effects <- function(main_design, components) {
  has_zero_contrast <- colSums(abs(main_design)) < BCNMA_ZERO_TOLERANCE
  if (any(has_zero_contrast)) {
    stop("Component(s) with no within-study contrast (present in all or no arms of every ",
         "study): ", paste(components[has_zero_contrast], collapse = ", "),
         ". Their effects are not identifiable; remove or merge them.", call. = FALSE)
  }
  aliased <- .dependent_columns(main_design)
  if (length(aliased) == 0) return(invisible(NULL))
  stop("Component main effects are aliased (only combinations of them are identified): ",
       paste(components[aliased], collapse = ", "),
       ". Merge components that always occur together or add evidence.", call. = FALSE)
}

#' Classify main effects and interactions before fitting
#'
#' @param prep            prepared data (x_arm, n_arms, components)
#' @param candidate_pairs place() indices that would get SSVS priors
#' @return list(main_rank, design_rank, interaction_status (data.frame),
#'   ssvs_pairs (estimable candidate pairs))
#' @noRd
.classify_identifiability <- function(prep, candidate_pairs) {
  main_design <- .contrast_rows(prep$x_arm, prep$n_arms)
  .check_main_effects(main_design, prep$components)

  co_presence <- .build_interaction_array(prep$x_arm)
  pair_design <- .contrast_rows(co_presence, prep$n_arms)
  is_copresent <- apply(co_presence != 0, 3, any)
  has_contrast <- colSums(abs(pair_design)) > BCNMA_ZERO_TOLERANCE
  status <- ifelse(!is_copresent, BCNMA_INTERACTION_STATUS[["never_copresent"]],
                   ifelse(!has_contrast, BCNMA_INTERACTION_STATUS[["zero_contrast"]],
                          BCNMA_INTERACTION_STATUS[["estimable"]]))

  # Aliasing among candidate pairs that have a contrast, given all main effects
  is_checked <- seq_along(status) %in% candidate_pairs & has_contrast
  checked_pairs <- which(is_checked)
  full_design <- cbind(main_design, pair_design[, checked_pairs, drop = FALSE])
  dependent <- .dependent_columns(full_design) - ncol(main_design)
  status[checked_pairs[dependent]] <- BCNMA_INTERACTION_STATUS[["aliased"]]

  n_studies_contrast <- vapply(seq_along(status), function(pair_index) {
    .count_studies_with_contrast(matrix(co_presence[, , pair_index], nrow = prep$n_studies),
                                 prep$n_arms)
  }, numeric(1))
  interaction_status <- data.frame(
    index = seq_along(status), component_pair = .interaction_pair_names(prep$components),
    status = status, n_studies_contrast = n_studies_contrast,
    is_modelled = seq_along(status) %in% candidate_pairs &
      status == BCNMA_INTERACTION_STATUS[["estimable"]],
    stringsAsFactors = FALSE)
  list(main_rank = qr(main_design)$rank, design_rank = qr(full_design)$rank,
       n_design_columns = ncol(main_design) + length(candidate_pairs),
       interaction_status = interaction_status,
       ssvs_pairs = interaction_status$index[interaction_status$is_modelled])
}

#' Number of studies in which an arm-level column varies between arms
#' @noRd
.count_studies_with_contrast <- function(arm_matrix, n_arms) {
  sum(vapply(seq_along(n_arms), function(study_index) {
    values <- arm_matrix[study_index, seq_len(n_arms[study_index])]
    any(abs(values - values[1]) > BCNMA_ZERO_TOLERANCE)
  }, logical(1)))
}

#' Identifiability check run by the fitting functions
#'
#' @param prep            prepared data
#' @param candidate_pairs place() indices that would get SSVS priors
#' @param interactions    interaction model
#' @return output of .classify_identifiability()
#' @noRd
.check_identifiability <- function(prep, candidate_pairs, interactions) {
  identifiability <- .classify_identifiability(prep, candidate_pairs)
  if (interactions == "none") return(identifiability)
  .warn_unestimable_interactions(identifiability, candidate_pairs)
  if (length(identifiability$ssvs_pairs) == 0) {
    stop("None of the candidate interactions is estimable from within-study contrasts; ",
         "use interactions = \"none\".", call. = FALSE)
  }
  identifiability
}

#' One line on modelled vs fixed interactions, for print methods
#' @noRd
.print_identifiability_note <- function(x) {
  identifiability <- x$identifiability
  if (is.null(identifiability)) return(invisible(NULL))
  n_modelled <- length(identifiability$ssvs_pairs)
  cat(sprintf(paste0("\nInteraction pairs in the SSVS model: %d of %d (others fixed to 0: ",
                     "not modelled, not estimable or aliased; design rank %d of %d)\n"),
              n_modelled, x$Ninter, identifiability$design_rank,
              identifiability$n_design_columns))
  invisible(NULL)
}

#' One warning summarising interactions fixed to zero
#' @noRd
.warn_unestimable_interactions <- function(identifiability, candidate_pairs) {
  candidate_status <- identifiability$interaction_status$status[candidate_pairs]
  is_fixed <- candidate_status != BCNMA_INTERACTION_STATUS[["estimable"]]
  if (!any(is_fixed)) return(invisible(NULL))
  counts <- table(candidate_status[is_fixed])
  warning(sprintf("%d of %d candidate interactions are fixed to gamma = 0 (%s). ",
                  sum(is_fixed), length(candidate_pairs),
                  paste(sprintf("%s: %d", names(counts), counts), collapse = "; ")),
          "See fit$identifiability$interaction_status.", call. = FALSE)
}
