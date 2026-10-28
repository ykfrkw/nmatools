# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: non-binary component values (doses) and arm-level effect
#          modifiers of specific components, shared by bcnma_bin() and
#          bcnma_cont(). Builds the modifier code array
#            code_arm[i, k, q] = presence(component c_q in arm k) * code_q(arm k)
#          whose within-study contrast z[i,k,q] enters the JAGS linear
#          predictor as inprod(beta[], z[i,k,]).
# Inputs:  `modifiers` / `component_values` arguments, user data, `prep`
#          (x_arm, arm_table with row_id / study_index / arm, n_arms)
# Outputs: validated values, updated prep, modifier design list
# Depends: R/bcnma_constants.R; summaries / printing / compare_combinations()
#          codes live in R/bcnma_modifiers_summary.R

BCNMA_MODIFIER_TYPES     <- c("continuous", "ordinal")
BCNMA_MONOTONE_OPTIONS   <- c("none", "increasing", "decreasing")
BCNMA_DEFAULT_PRIOR_SD   <- 10   # precision 0.01 (the binary model's vague prior on d[k])

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
  if (anyDuplicated(labels)) stop("Modifier labels must be unique: ", paste(labels, collapse = ", "))
  specs
}

.normalise_one_modifier <- function(spec, data, components) {
  if (!is.list(spec) || is.null(spec$column)) stop("Each modifier must be a list with a 'column'.")
  if (!spec$column %in% names(data)) stop(sprintf("Modifier column '%s' not found in data.", spec$column))
  unknown <- setdiff(spec$components, components)
  if (length(spec$components) == 0 || length(unknown) > 0) {
    stop(sprintf("Modifier '%s': 'components' must name known components (unknown: %s).",
                 spec$column, paste(unknown, collapse = ", ")))
  }
  if (is.null(spec$type) || !spec$type %in% BCNMA_MODIFIER_TYPES) {
    stop(sprintf("Modifier '%s': 'type' must be \"continuous\" or \"ordinal\".", spec$column))
  }
  spec$label    <- if (is.null(spec$label)) spec$column else as.character(spec$label)
  spec$monotone <- if (is.null(spec$monotone)) "none" else match.arg(spec$monotone, BCNMA_MONOTONE_OPTIONS)
  spec$prior_sd <- if (is.null(spec$prior_sd)) BCNMA_DEFAULT_PRIOR_SD else spec$prior_sd
  if (!is.numeric(spec$prior_sd) || length(spec$prior_sd) != 1 || spec$prior_sd <= 0) {
    stop(sprintf("Modifier '%s': 'prior_sd' must be one positive number.", spec$label))
  }
  if (spec$type == "continuous") {
    if (!is.numeric(data[[spec$column]])) stop(sprintf("Modifier '%s': column must be numeric.", spec$label))
    if (spec$monotone != "none") stop(sprintf("Modifier '%s': 'monotone' applies to ordinal modifiers only.", spec$label))
    if (!is.null(spec$center) && (!is.numeric(spec$center) || length(spec$center) != 1)) {
      stop(sprintf("Modifier '%s': 'center' must be a single number.", spec$label))
    }
    return(spec)
  }
  if (length(spec$levels) < 2 || anyDuplicated(spec$levels)) {
    stop(sprintf("Modifier '%s': ordinal 'levels' (>= 2 distinct, lowest first) are required.", spec$label))
  }
  spec$levels <- as.character(spec$levels)
  spec
}

# ---- Modifier design array ----

#' Build code_arm, the term table and the beta priors from normalised specs
#'
#' @return NULL when there are no specs; otherwise list(code_arm, Q, terms,
#'   priors)
#' @noRd
.build_modifier_design <- function(specs, data, prep) {
  if (length(specs) == 0) return(NULL)
  arm_rows    <- prep$arm_table
  study_index <- arm_rows$study_index
  term_rows   <- list()
  code_list   <- list()
  for (spec in specs) {
    raw_values <- data[[spec$column]][arm_rows$row_id]
    for (component in spec$components) {
      component_index <- match(component, prep$components)
      is_present <- prep$x_arm[cbind(study_index, arm_rows$arm, component_index)] != 0
      .check_modifier_missing(raw_values, is_present, arm_rows, spec, component)
      coded <- if (spec$type == "continuous") {
        .code_continuous(raw_values, is_present, spec, component)
      } else {
        .code_ordinal(raw_values, is_present, spec, component)
      }
      term_rows <- c(term_rows, list(coded$terms))
      code_list <- c(code_list, coded$codes)
    }
  }
  terms   <- do.call(rbind, term_rows)
  terms$q <- seq_len(nrow(terms))
  terms   <- terms[, c("q", setdiff(names(terms), "q"))]
  code_arm <- array(0, dim = c(prep$n_studies, prep$max_arms, nrow(terms)))
  for (q in terms$q) code_arm[cbind(study_index, arm_rows$arm, q)] <- code_list[[q]]
  .check_modifier_identifiability(code_arm, prep$n_arms, terms)
  message("Note: modifier effects (beta) are identified mainly by between-study variation ",
          "in the covariate; interpret them as observational (ecological) associations.")
  priors <- data.frame(precision  = 1 / terms$prior_sd^2,
                       truncation = ifelse(terms$monotone == "increasing", "T(0,)",
                                    ifelse(terms$monotone == "decreasing", "T(,0)", "")))
  list(code_arm = code_arm, Q = nrow(terms), terms = terms, priors = priors)
}

.check_modifier_missing <- function(raw_values, is_present, arm_rows, spec, component) {
  is_missing_where_present <- is.na(raw_values) & is_present
  if (!any(is_missing_where_present)) return(invisible(NULL))
  where <- paste0(arm_rows$study[is_missing_where_present], " (arm ",
                  arm_rows$arm[is_missing_where_present], ")")
  stop(sprintf("Modifier '%s' is NA where component '%s' is present: %s",
               spec$label, component, paste(where, collapse = ", ")))
}

.modifier_term_row <- function(spec, component, term, level_from = NA, level_to = NA, center = NA) {
  data.frame(term = term, label = spec$label, component = component, type = spec$type,
             level_from = level_from, level_to = level_to, center = center,
             monotone = spec$monotone, prior_sd = spec$prior_sd, stringsAsFactors = FALSE)
}

.code_continuous <- function(raw_values, is_present, spec, component) {
  center <- spec$center
  if (is.null(center)) {
    if (!any(is_present)) stop(sprintf("Modifier '%s': component '%s' is present in no arm.", spec$label, component))
    center <- mean(raw_values[is_present])
  }
  codes <- ifelse(is_present, raw_values - center, 0)
  list(codes = list(codes),
       terms = .modifier_term_row(spec, component, paste0(spec$label, ":", component), center = center))
}

.code_ordinal <- function(raw_values, is_present, spec, component) {
  level_index <- match(as.character(raw_values), spec$levels)
  is_unknown  <- !is.na(raw_values) & is.na(level_index)
  if (any(is_unknown)) {
    stop(sprintf("Modifier '%s': values not in 'levels': %s", spec$label,
                 paste(unique(as.character(raw_values[is_unknown])), collapse = ", ")))
  }
  # Cumulative coding: term l switches on at level l+1, so beta_l is the
  # increment from level l to level l+1 (reference = lowest level).
  step_index <- seq_len(length(spec$levels) - 1L)
  codes <- lapply(step_index, function(l) {
    as.numeric(is_present & !is.na(level_index) & level_index >= l + 1L)
  })
  level_from <- spec$levels[step_index]
  level_to   <- spec$levels[step_index + 1L]
  terms <- .modifier_term_row(spec, component,
                              sprintf("%s(%s->%s):%s", spec$label, level_from, level_to, component),
                              level_from = level_from, level_to = level_to)
  list(codes = codes, terms = terms)
}

.check_modifier_identifiability <- function(code_arm, n_arms, terms) {
  for (q in terms$q) {
    contrasts <- unlist(lapply(seq_along(n_arms), function(study_index) {
      arms <- seq_len(n_arms[study_index])
      code_arm[study_index, arms, q] - code_arm[study_index, 1, q]
    }))
    if (all(abs(contrasts) < BCNMA_ZERO_TOLERANCE)) {
      warning(sprintf("Modifier term '%s' is not identifiable: its within-study contrast is zero ",
                      terms$term[q]),
              "in every study (e.g. the same level in all arms). Its posterior is the prior.")
    }
  }
}
