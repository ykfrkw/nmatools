# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: build the effect-modifier design from normalised specs: one term
#          q per (modifier, component[, ordinal step]) with arm-level code
#            code_arm[i, k, q] = presence(component c_q in arm k) * code_q(arm k)
#          whose within-study contrast z[i,k,q] enters the JAGS linear
#          predictor as inprod(beta[], z[i,k,]). Continuous covariates are
#          standardised ((value - center) / SD over arms where the component
#          is present) so that one default prior fits every covariate;
#          ordinal covariates use cumulative step codes.
# Inputs:  normalised specs (R/bcnma_modifiers.R), user data, `prep`
# Outputs: list(code_arm, Q, terms, priors)
# Depends: R/bcnma_constants.R, .count_studies_with_contrast()
#          (R/bcnma_identifiability.R)

#' Build code_arm, the term table and the beta priors from normalised specs
#'
#' @return NULL when there are no specs; otherwise list(code_arm, Q, terms,
#'   priors). `terms` has one row per beta with its center / scale (codes are
#'   (value - center) / scale), prior SD and the number of studies whose
#'   within-study contrast informs it.
#' @noRd
.build_modifier_design <- function(specs, data, prep) {
  if (length(specs) == 0) return(NULL)
  arm_rows  <- prep$arm_table
  term_rows <- list()
  code_list <- list()
  for (spec in specs) {
    raw_values <- data[[spec$column]][arm_rows$row_id]
    for (component in spec$components) {
      component_index <- match(component, prep$components)
      arm_cells  <- cbind(arm_rows$study_index, arm_rows$arm, component_index)
      is_present <- prep$x_arm[arm_cells] != 0
      .check_modifier_missing(raw_values, is_present, arm_rows, spec, component)
      code_term <- if (spec$type == "continuous") .code_continuous else .code_ordinal
      coded <- code_term(raw_values, is_present, spec, component)
      term_rows <- c(term_rows, list(coded$terms))
      code_list <- c(code_list, coded$codes)
    }
  }
  terms   <- do.call(rbind, term_rows)
  terms$q <- seq_len(nrow(terms))
  # One SSVS indicator per (modifier, component), shared by its ordinal steps
  group_key <- paste(terms$label, terms$component, sep = ":")
  is_selected <- terms$selection == "ssvs"
  terms$selection_group <- NA_integer_
  terms$selection_group[is_selected] <- match(group_key[is_selected],
                                              unique(group_key[is_selected]))
  terms   <- terms[, c("q", setdiff(names(terms), "q"))]
  code_arm <- array(0, dim = c(prep$n_studies, prep$max_arms, nrow(terms)))
  for (q in terms$q) {
    code_arm[cbind(arm_rows$study_index, arm_rows$arm, q)] <- code_list[[q]]
  }
  terms <- .add_modifier_information(terms, code_arm, prep$n_arms)
  message("Note: modifier effects (beta) are identified mainly by between-study variation ",
          "in the covariate; interpret them as observational (ecological) associations.")
  truncation <- ifelse(terms$monotone == "increasing", "T(0,)",
                       ifelse(terms$monotone == "decreasing", "T(,0)", ""))
  priors <- data.frame(precision = 1 / terms$prior_sd^2, truncation = truncation,
                       selection_group = terms$selection_group,
                       inclusion_prob = terms$inclusion_prob,
                       slab_sd = terms$prior_sd, spike_sd = terms$spike_sd)
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

.modifier_term_row <- function(spec, component, term, level_from = NA, level_to = NA,
                               center = NA, scale = 1) {
  data.frame(term = term, label = spec$label, component = component, type = spec$type,
             level_from = level_from, level_to = level_to, center = center, scale = scale,
             monotone = spec$monotone, prior_sd = spec$prior_sd, selection = spec$selection,
             inclusion_prob = if (is.null(spec$inclusion_prob)) NA else spec$inclusion_prob,
             spike_sd = if (is.null(spec$spike_sd)) NA else spec$spike_sd,
             stringsAsFactors = FALSE)
}

# Code = (value - center) / scale, with scale = SD of the covariate over arms
# where the component is present, so that beta is per covariate SD.
.code_continuous <- function(raw_values, is_present, spec, component) {
  if (!any(is_present)) {
    stop(sprintf("Modifier '%s': component '%s' is present in no arm.", spec$label, component))
  }
  center <- if (is.null(spec$center)) mean(raw_values[is_present]) else spec$center
  scale  <- stats::sd(raw_values[is_present])
  # A constant covariate cannot be standardised (and its beta is not
  # identified; .add_modifier_information() reports it).
  if (is.na(scale) || scale < BCNMA_ZERO_TOLERANCE) scale <- 1
  codes <- ifelse(is_present, (raw_values - center) / scale, 0)
  list(codes = list(codes),
       terms = .modifier_term_row(spec, component, paste0(spec$label, ":", component),
                                  center = center, scale = scale))
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
  codes <- lapply(step_index, function(step) {
    as.numeric(is_present & !is.na(level_index) & level_index >= step + 1L)
  })
  level_from <- spec$levels[step_index]
  level_to   <- spec$levels[step_index + 1L]
  term_names <- sprintf("%s(%s->%s):%s", spec$label, level_from, level_to, component)
  list(codes = codes,
       terms = .modifier_term_row(spec, component, term_names,
                                  level_from = level_from, level_to = level_to))
}

#' Studies informing each beta, and a warning for terms no study informs
#'
#' `n_studies_contrast`: studies where the term's code differs between arms;
#' `n_distinct_contrasts`: distinct non-zero contrast values among them. With
#' fewer than 2 distinct values beta cannot be separated from the main
#' effect by between-study variation.
#' @noRd
.add_modifier_information <- function(terms, code_arm, n_arms) {
  contrast_values <- lapply(terms$q, function(q) {
    unlist(lapply(seq_along(n_arms), function(study_index) {
      arms <- seq_len(n_arms[study_index])
      code_arm[study_index, arms, q] - code_arm[study_index, 1, q]
    }))
  })
  terms$n_studies_contrast <- vapply(terms$q, function(q) {
    .count_studies_with_contrast(matrix(code_arm[, , q], nrow = dim(code_arm)[1]), n_arms)
  }, numeric(1))
  terms$n_distinct_contrasts <- vapply(contrast_values, function(values) {
    length(unique(round(values[abs(values) > BCNMA_ZERO_TOLERANCE], 8)))
  }, numeric(1))
  for (q in terms$q[terms$n_studies_contrast == 0]) {
    warning(sprintf("Modifier term '%s' is not identifiable: its within-study contrast is zero ",
                    terms$term[q]),
            "in every study (e.g. the same level in all arms). Its posterior is the prior.")
  }
  terms
}
