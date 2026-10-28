# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09 (restructured 2026-10-28)
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: turn long-format arm-level data (one row per arm) into the arrays
#          used by every bcnma model: arms sorted and numbered within study,
#          the component value array x_arm[i, k, j] parsed from `treat`, and
#          the binomial outcome matrices r[i, k], n[i, k].
# Inputs:  user data.frame and column names (resolved by bcnma_bin())
# Outputs: a `prep` list (see .parse_arm_structure()); the continuous
#          outcome is added by R/bcnma_prepare_cont.R
# Depends: base R only

#' Sort arms by study, number them and parse the component strings
#'
#' Arm 1 of each study (the first row of that study in `data`, after a
#' stable sort by study) is the within-study comparator of every model.
#'
#' @param data                data.frame, one row per arm
#' @param studlab_col         character: study column
#' @param treat_col           character: treatment (component string) column
#' @param components          character vector of component names
#' @param component_separator character separating components in `treat`
#' @return list(arm_table, x_arm, n_arms, max_arms, studies, n_studies,
#'   components, n_components); `arm_table` keeps `row_id` (row of `data`)
#'   so that row-aligned inputs (component values, modifiers) can be matched.
#' @noRd
.parse_arm_structure <- function(data, studlab_col, treat_col, components,
                                 component_separator = "+") {
  arm_table <- data.frame(
    study  = as.character(data[[studlab_col]]),
    treat  = as.character(data[[treat_col]]),
    row_id = seq_len(nrow(data)),
    stringsAsFactors = FALSE
  )
  arm_table <- arm_table[order(arm_table$study), ]
  studies   <- unique(arm_table$study)
  arm_table$study_index <- match(arm_table$study, studies)
  arm_table$arm <- stats::ave(arm_table$row_id, arm_table$study_index, FUN = seq_along)
  n_arms <- as.integer(tabulate(arm_table$study_index, nbins = length(studies)))

  tokens <- lapply(strsplit(arm_table$treat, component_separator, fixed = TRUE), trimws)
  x_arm  <- array(0, dim = c(length(studies), max(n_arms), length(components)))
  for (row in seq_len(nrow(arm_table))) {
    is_present <- components %in% tokens[[row]]
    x_arm[arm_table$study_index[row], arm_table$arm[row], ] <- as.numeric(is_present)
  }
  .warn_unused_components(x_arm, components)

  list(arm_table = arm_table, x_arm = x_arm, n_arms = n_arms, max_arms = max(n_arms),
       studies = studies, n_studies = length(studies),
       components = components, n_components = length(components))
}

.warn_unused_components <- function(x_arm, components) {
  is_unused <- apply(x_arm == 0, 3, all)
  if (!any(is_unused)) return(invisible(NULL))
  warning("The following components appear in no arm: ",
          paste(components[is_unused], collapse = ", "))
}

#' Arm-level values of one data column as a `[study, arm]` matrix
#' @noRd
.arm_matrix <- function(values, structure) {
  arm_table <- structure$arm_table
  value_matrix <- matrix(NA_real_, structure$n_studies, structure$max_arms)
  value_matrix[cbind(arm_table$study_index, arm_table$arm)] <- values[arm_table$row_id]
  value_matrix
}

#' Prepare arm-level binary data for Bayesian cNMA
#'
#' @param data,studlab_col,treat_col,components,component_separator see
#'   .parse_arm_structure()
#' @param event_col character: event count column
#' @param n_col     character: sample size column
#' @return `prep` list: .parse_arm_structure() plus `likelihood = "binomial"`
#'   and `outcome = list(r, n)` (the JAGS outcome data)
#' @noRd
.bcnma_bin_prepare_data <- function(data, studlab_col, treat_col, event_col, n_col,
                                    components, component_separator = "+") {
  prep <- .parse_arm_structure(data, studlab_col, treat_col, components,
                               component_separator)
  events      <- floor(as.numeric(data[[event_col]]))
  sample_size <- floor(as.numeric(data[[n_col]]))
  prep$likelihood <- "binomial"
  prep$outcome <- list(r = .arm_matrix(events, prep), n = .arm_matrix(sample_size, prep))
  prep
}
