# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: validate the optional inputs that extend the linear predictor,
#          shared by bcnma_bin() and bcnma_cont(): non-binary component
#          values (doses) and the specifications of arm-level effect
#          modifiers of specific components. Dose main effects are linear
#          through zero (effect = d[j] * value); interactions and modifiers
#          act on presence (value != 0).
# Inputs:  `component_values` / `modifiers` arguments, user data, `prep`
# Outputs: validated matrix, updated prep (x_arm), normalised modifier specs
# Depends: R/bcnma_constants.R; the modifier design (codes) is built in
#          R/bcnma_modifier_design.R, summaries in R/bcnma_modifiers_summary.R

BCNMA_MODIFIER_TYPES   <- c("continuous", "ordinal")
BCNMA_MONOTONE_OPTIONS <- c("none", "increasing", "decreasing")
# beta ~ N(0, 1): per covariate SD (continuous, standardised internally) or
# per ordinal step, on the log-OR / SMD scale.
BCNMA_MODIFIER_PRIOR_SD <- 1
# Spike-and-slab selection of modifiers (selection = "ssvs"): slab and spike
# SD of beta on the code scale (per covariate SD / per ordinal step).
BCNMA_MODIFIER_SELECTIONS     <- c("none", "ssvs")
BCNMA_MODIFIER_SLAB_SD        <- 0.5
BCNMA_MODIFIER_SPIKE_SD       <- 0.01
BCNMA_MODIFIER_INCLUSION_PROB <- 0.5

# ---- Component values (doses) ----

#' Validate the optional component_values input
#'
#' @param component_values numeric matrix / data.frame or NULL
#' @param n_rows           integer nrow(data)
#' @param components       character vector of component names
#' @return numeric matrix (double) with colnames = components, or NULL
#' @noRd
.validate_component_values <- function(component_values, n_rows, components) {
  if (is.null(component_values)) return(NULL)
  if (is.data.frame(component_values)) {
    is_numeric_column <- vapply(component_values, is.numeric, logical(1))
    if (!all(is_numeric_column)) stop("'component_values' columns must all be numeric.")
    component_values <- as.matrix(component_values)
  }
  if (!is.matrix(component_values) || !is.numeric(component_values)) {
    stop("'component_values' must be a numeric matrix or data.frame.")
  }
  if (nrow(component_values) != n_rows) {
    stop(sprintf("'component_values' has %d rows; 'data' has %d (rows must match data).",
                 nrow(component_values), n_rows))
  }
  if (!identical(colnames(component_values), components)) {
    stop("colnames(component_values) must equal 'components' exactly (same order): ",
         paste(components, collapse = ", "))
  }
  if (anyNA(component_values)) stop("'component_values' must not contain NA.")
  storage.mode(component_values) <- "double"
  component_values
}

#' Replace the 0/1 indicators in x_arm with user-supplied values
#'
#' Rows are matched through prep$arm_table$row_id because the data
#' preparation (.parse_arm_structure()) sorts arms by study.
#' @noRd
.apply_component_values <- function(prep, component_values) {
  if (is.null(component_values)) return(prep)
  arm_rows    <- prep$arm_table
  value_array <- array(0, dim = dim(prep$x_arm))
  for (row in seq_len(nrow(arm_rows))) {
    value_array[arm_rows$study_index[row], arm_rows$arm[row], ] <-
      component_values[arm_rows$row_id[row], ]
  }
  n_disagreeing <- sum((value_array != 0) != (prep$x_arm != 0))
  if (n_disagreeing > 0) {
    warning(n_disagreeing, " arm-component cell(s) where 'component_values' (non-zero) ",
            "and the 'treat' label disagree on presence; 'component_values' is used.")
  }
  is_all_zero <- apply(value_array == 0, 3, all)
  if (any(is_all_zero)) {
    warning("'component_values' is zero in every arm for: ",
            paste(prep$components[is_all_zero], collapse = ", "))
  }
  prep$x_arm <- value_array
  prep
}

# ---- Modifier specification ----

#' Validate and fill defaults for the `modifiers` argument
#'
#' @return list of normalised specs (possibly empty)
#' @noRd
.normalise_modifier_specs <- function(modifiers, data, components) {
  if (is.null(modifiers) || length(modifiers) == 0) return(list())
  if (!is.list(modifiers)) stop("'modifiers' must be a list of modifier specifications.")
  # A single spec passed without the outer list is accepted for convenience
  if (!is.null(modifiers$column)) modifiers <- list(modifiers)
  specs  <- lapply(modifiers, .normalise_one_modifier, data = data, components = components)
  labels <- vapply(specs, function(spec) spec$label, character(1))
  if (anyDuplicated(labels)) {
    stop("Modifier labels must be unique: ", paste(labels, collapse = ", "))
  }
  specs
}

.normalise_one_modifier <- function(spec, data, components) {
  if (!is.list(spec) || is.null(spec$column)) {
    stop("Each modifier must be a list with a 'column'.")
  }
  if (!spec$column %in% names(data)) {
    stop(sprintf("Modifier column '%s' not found in data.", spec$column))
  }
  unknown <- setdiff(spec$components, components)
  if (length(spec$components) == 0 || length(unknown) > 0) {
    stop(sprintf("Modifier '%s': 'components' must name known components (unknown: %s).",
                 spec$column, paste(unknown, collapse = ", ")))
  }
  if (is.null(spec$type) || !spec$type %in% BCNMA_MODIFIER_TYPES) {
    stop(sprintf("Modifier '%s': 'type' must be \"continuous\" or \"ordinal\".", spec$column))
  }
  spec$label    <- if (is.null(spec$label)) spec$column else as.character(spec$label)
  spec$monotone <- if (is.null(spec$monotone)) "none" else
    match.arg(spec$monotone, BCNMA_MONOTONE_OPTIONS)
  spec <- .normalise_modifier_selection(spec)
  spec$prior_sd <- if (is.null(spec$prior_sd)) BCNMA_MODIFIER_PRIOR_SD else spec$prior_sd
  is_valid_prior_sd <- is.numeric(spec$prior_sd) && length(spec$prior_sd) == 1 &&
    spec$prior_sd > 0
  if (!is_valid_prior_sd) {
    stop(sprintf("Modifier '%s': 'prior_sd' must be one positive number.", spec$label))
  }
  if (spec$type == "continuous") return(.check_continuous_spec(spec, data))
  if (length(spec$levels) < 2 || anyDuplicated(spec$levels)) {
    stop(sprintf("Modifier '%s': ordinal 'levels' (>= 2 distinct, lowest first) are required.",
                 spec$label))
  }
  spec$levels <- as.character(spec$levels)
  spec
}

.check_continuous_spec <- function(spec, data) {
  if (!is.numeric(data[[spec$column]])) {
    stop(sprintf("Modifier '%s': column must be numeric.", spec$label))
  }
  if (spec$monotone != "none") {
    stop(sprintf("Modifier '%s': 'monotone' applies to ordinal modifiers only.", spec$label))
  }
  is_valid_center <- is.null(spec$center) || (is.numeric(spec$center) && length(spec$center) == 1)
  if (!is_valid_center) stop(sprintf("Modifier '%s': 'center' must be a single number.", spec$label))
  spec
}

#' Fill and check the spike-and-slab selection fields of a modifier spec
#'
#' With `selection = "ssvs"` one indicator per (modifier, component) chooses
#' between a spike N(0, spike_sd^2) and a slab N(0, slab_sd^2) for all its
#' beta terms; the slab SD then replaces `prior_sd`.
#' @noRd
.normalise_modifier_selection <- function(spec) {
  spec$selection <- if (is.null(spec$selection)) "none" else
    match.arg(spec$selection, BCNMA_MODIFIER_SELECTIONS)
  if (spec$selection == "none") return(spec)
  if (!is.null(spec$prior_sd)) {
    stop(sprintf("Modifier '%s': with selection = \"ssvs\" set 'slab_sd' (and 'spike_sd'), ",
                 spec$label), "not 'prior_sd'.", call. = FALSE)
  }
  if (spec$monotone != "none") {
    stop(sprintf("Modifier '%s': selection = \"ssvs\" cannot be combined with monotone = ",
                 spec$label),
         "\"", spec$monotone, "\". A truncated (half-normal) prior puts no mass on the ",
         "other sign and pushes the posterior away from 0, so the spike-vs-slab comparison ",
         "would no longer test 'no modification'. Use monotone = \"none\".", call. = FALSE)
  }
  spec$inclusion_prob <- if (is.null(spec$inclusion_prob)) BCNMA_MODIFIER_INCLUSION_PROB else
    spec$inclusion_prob
  spec$slab_sd  <- if (is.null(spec$slab_sd)) BCNMA_MODIFIER_SLAB_SD else spec$slab_sd
  spec$spike_sd <- if (is.null(spec$spike_sd)) BCNMA_MODIFIER_SPIKE_SD else spec$spike_sd
  is_valid <- is.numeric(spec$inclusion_prob) && length(spec$inclusion_prob) == 1 &&
    spec$inclusion_prob > 0 && spec$inclusion_prob < 1 &&
    is.numeric(spec$slab_sd) && is.numeric(spec$spike_sd) && spec$spike_sd > 0 &&
    spec$slab_sd > spec$spike_sd
  if (!is_valid) {
    stop(sprintf("Modifier '%s': need 0 < inclusion_prob < 1 and 0 < spike_sd < slab_sd.",
                 spec$label), call. = FALSE)
  }
  spec$prior_sd <- spec$slab_sd
  spec
}
