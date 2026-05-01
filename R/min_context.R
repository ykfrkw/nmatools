#' @title Minimally contextualised evidence framework
#'
#' @description Classifies NMA interventions into evidence groups based on
#'   statistical comparisons vs a reference treatment, then optionally
#'   cross-classifies by CINeMA confidence rating or N-based quality.
#'
#' @details
#' Groups are assigned iteratively (starting from the reference = Group 0):
#' \itemize{
#'   \item Group 0: No significant difference from reference (p \eqn{\ge} alpha).
#'   \item Group +1: Significantly better than reference (p < alpha, favourable
#'     direction).
#'   \item Group -1: Significantly worse than reference (p < alpha, unfavourable
#'     direction).
#'   \item Group +2: Subset of Group +1 that is significantly better than
#'     \emph{every other} member of Group +1 (p < alpha for each pairwise
#'     comparison). These are removed from Group +1.
#'   \item Group -2: Analogous for negative side.
#'   \item The process repeats up to \code{max_iter = 10} levels.
#' }
#' The reference treatment is always assigned Group 0.
#'
#' When \code{n_thresholds} is supplied, the function also computes an
#' \code{n_quality} column based on the total number of participants in all
#' trials that include each treatment:
#' \itemize{
#'   \item \code{"Low"}      — N < \code{n_thresholds[1]}
#'   \item \code{"Moderate"} — \code{n_thresholds[1]} ≤ N < \code{n_thresholds[2]}
#'   \item \code{"High"}     — N ≥ \code{n_thresholds[2]}
#' }
#' The default thresholds are \code{c(100, 400)}.
#'
#' @param x A \code{netmeta} object.
#' @param cinema Path to a CINeMA report CSV or a data frame (optional).
#' @param n_thresholds Numeric vector of length 2 \code{c(low_threshold,
#'   high_threshold)} used to compute an \code{n_quality} column from
#'   participant counts. Default \code{c(100, 400)}.
#'   Set to \code{NULL} to omit the \code{n_quality} column entirely.
#' @param reference Reference treatment name. Defaults to
#'   \code{x$reference.group}.
#' @param common Logical. Use common-effects model? Default \code{FALSE} = random-effects model.
#' @param fixed Deprecated. Use \code{common} instead.
#' @param small_values Are small values desirable? \code{"desirable"} or
#'   \code{"undesirable"} (default \code{"undesirable"}). Affects the direction
#'   of group assignment.
#' @param alpha Significance threshold (default 0.05).
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{treatment}: treatment name.
#'     \item \code{group}: numeric group assignment.
#'     \item \code{cinema}: CINeMA confidence rating (or \code{NA} if
#'       \code{cinema} not provided).
#'     \item \code{n_total}: total participants across all trials including
#'       this treatment (only when \code{n_thresholds} is not \code{NULL}).
#'     \item \code{n_quality}: quality rating derived from \code{n_total}
#'       (only when \code{n_thresholds} is not \code{NULL}).
#'   }
#'   Rows are sorted by \code{group} descending.
#' @export
#'
#' @importFrom stats setNames
min_context <- function(x,
                        cinema       = NULL,
                        n_thresholds = c(100, 400),
                        reference    = NULL,
                        common       = FALSE,
                        fixed        = NULL,
                        small_values = "undesirable",
                        alpha        = 0.05) {

  small_values <- match.arg(small_values, c("desirable", "undesirable"))

  if (!is.null(fixed)) {
    message("Argument 'fixed' is deprecated in min_context(); use 'common' instead.")
    common <- fixed
  }

  # ---- 1. Setup ----
  if (is.null(reference)) reference <- x$reference.group
  if (is.null(reference)) reference <- x$trts[1]

  te_mat   <- .nm_mat(x, "TE",   common)
  pval_mat <- .nm_mat(x, "pval", common)

  treatments <- x$trts

  # ---- 2. Parse CINeMA ----
  cinema_df <- NULL
  if (!is.null(cinema)) cinema_df <- parse_cinema(cinema)

  # ---- 3. Determine favourable direction for reference comparison ----
  # te_mat[trt, reference]: positive means trt > ref on log/raw scale
  # small_values = "desirable": LOWER is better (e.g., SMD, dropout),
  #   so negative TE vs reference is favourable
  # small_values = "undesirable": HIGHER is better (e.g., remission OR > 1),
  #   so positive TE vs reference is favourable
  is_favorable_vs_ref <- function(trt) {
    te <- te_mat[trt, reference]
    if (small_values == "desirable") te < 0 else te > 0
  }

  # ---- 4. Initial classification vs reference ----
  groups <- stats::setNames(rep(0L, length(treatments)), treatments)

  sig_vs_ref <- sapply(treatments, function(trt) {
    if (trt == reference) return(FALSE)
    pval_mat[trt, reference] < alpha
  })

  for (trt in treatments) {
    if (trt == reference) {
      groups[trt] <- 0L
    } else if (!sig_vs_ref[trt]) {
      groups[trt] <- 0L
    } else if (is_favorable_vs_ref(trt)) {
      groups[trt] <- 1L
    } else {
      groups[trt] <- -1L
    }
  }

  # ---- 5. Iterative elevation for positive groups ----
  max_iter <- 10L

  for (level in seq_len(max_iter)) {
    in_level <- names(groups)[groups == level]
    if (length(in_level) < 2) break  # can't elevate if fewer than 2

    # A treatment can be elevated to level+1 if it beats ALL other treatments
    # in the current level (pairwise p < alpha in the favourable direction)
    can_elevate <- sapply(in_level, function(trt) {
      others <- setdiff(in_level, trt)
      if (length(others) == 0) return(FALSE)
      all(sapply(others, function(other) {
        pv <- pval_mat[trt, other]
        te <- te_mat[trt, other]
        # favorable: trt better than other
        pv < alpha && (if (small_values == "desirable") te < 0 else te > 0)
      }))
    })

    to_elevate <- names(can_elevate)[can_elevate]
    if (length(to_elevate) == 0) break

    groups[to_elevate] <- level + 1L
  }

  # ---- 6. Iterative lowering for negative groups ----
  for (level in seq_len(max_iter)) {
    in_level <- names(groups)[groups == -level]
    if (length(in_level) < 2) break

    can_lower <- sapply(in_level, function(trt) {
      others <- setdiff(in_level, trt)
      if (length(others) == 0) return(FALSE)
      all(sapply(others, function(other) {
        pv <- pval_mat[trt, other]
        te <- te_mat[trt, other]
        # "worse" direction: trt worse than other
        pv < alpha && (if (small_values == "desirable") te > 0 else te < 0)
      }))
    })

    to_lower <- names(can_lower)[can_lower]
    if (length(to_lower) == 0) break

    groups[to_lower] <- -(level + 1L)
  }

  # ---- 7. Look up CINeMA ratings ----
  cinema_ratings <- rep(NA_character_, length(treatments))
  names(cinema_ratings) <- treatments

  if (!is.null(cinema_df)) {
    for (trt in treatments) {
      if (trt != reference) {
        cinema_ratings[trt] <- get_cinema_rating(trt, reference, cinema_df)
      }
    }
  }

  # ---- 7b. Compute N-based quality ----
  n_total   <- rep(NA_integer_, length(treatments))
  n_quality <- rep(NA_character_, length(treatments))
  names(n_total)   <- treatments
  names(n_quality) <- treatments

  if (!is.null(n_thresholds)) {
    if (length(n_thresholds) != 2L || !is.numeric(n_thresholds))
      stop("`n_thresholds` must be a numeric vector of length 2.")
    n_thresholds <- sort(n_thresholds)

    d <- tryCatch(x$data, error = function(e) NULL)

    if (!is.null(d)) {
      # Identify N columns: n1/n2 (pairwise) or n (arm-based)
      t1_col <- grep("^treat1$", colnames(d), value = TRUE)
      t2_col <- grep("^treat2$", colnames(d), value = TRUE)
      n1_col <- grep("^n1$",     colnames(d), value = TRUE)
      n2_col <- grep("^n2$",     colnames(d), value = TRUE)

      for (trt in treatments) {
        if (length(t1_col) && length(n1_col) && length(n2_col)) {
          # Pairwise data: each row = one comparison; sum n1 where treat1==trt
          # and n2 where treat2==trt
          mask1 <- as.character(d[[t1_col]]) == trt
          mask2 <- as.character(d[[t2_col]]) == trt
          n_arms <- sum(d[[n1_col]][mask1], na.rm = TRUE) +
                    sum(d[[n2_col]][mask2], na.rm = TRUE)
          n_total[trt] <- as.integer(n_arms)
        }
        # Map to quality label
        nt <- n_total[trt]
        if (!is.na(nt)) {
          n_quality[trt] <- if (nt >= n_thresholds[2]) {
            "High"
          } else if (nt >= n_thresholds[1]) {
            "Moderate"
          } else {
            "Low"
          }
        }
      }
    }
  }

  # ---- 8. Build result data frame ----
  result <- data.frame(
    treatment = treatments,
    group     = groups[treatments],
    cinema    = cinema_ratings[treatments],
    stringsAsFactors = FALSE
  )

  if (!is.null(n_thresholds)) {
    result$n_total   <- n_total[treatments]
    result$n_quality <- n_quality[treatments]
  }

  result <- result[order(result$group, decreasing = TRUE), ]
  rownames(result) <- NULL
  attr(result, "reference") <- reference
  result
}


#' @title Tabulate minimally contextualised framework results
#'
#' @description Creates a cross-tabulation of treatment groups by CINeMA
#'   confidence rating or N-based quality.
#'
#' @details
#' Rows represent combined quality levels; columns represent group numbers.
#'
#' For \code{quality_col = "cinema"}: rows are \code{"High/Moderate"} and
#' \code{"Low/Very low"} (plus \code{"No CINeMA"} if any ratings are missing).
#'
#' For \code{quality_col = "n_quality"}: rows are \code{"High/Moderate"} and
#' \code{"Low"} (plus \code{"No data"} if N information is unavailable).
#'
#' Column names follow the pattern \code{"Group +1"}, \code{"Group 0"},
#' \code{"Group -1"}, etc., sorted from highest to lowest group.
#'
#' @param df Output of \code{\link{min_context}()}.
#' @param quality_col Name of the quality column to cross-tabulate by.
#'   \code{"cinema"} (default) or \code{"n_quality"}.
#' @param file Optional output path (\code{.docx} or \code{.xlsx}).
#'   If \code{NULL}, returns a data frame.
#' @return A cross-tabulation data frame (rows = quality levels, cols = groups),
#'   or writes to file and invisibly returns the data frame.
#' @export
#'
#' @importFrom flextable flextable set_table_properties save_as_docx
#' @importFrom officer prop_section page_size
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
table_min_context <- function(df, quality_col = "cinema", file = NULL) {
  if (!all(c("treatment", "group") %in% colnames(df)))
    stop("`df` must have columns 'treatment' and 'group'.")
  if (!quality_col %in% colnames(df))
    stop("Column '", quality_col, "' not found in `df`. ",
         "For n_quality, call min_context() with n_thresholds != NULL.")

  # Groups: columns, sorted high → low
  groups <- sort(unique(df$group), decreasing = TRUE)
  format_group <- function(g) {
    if (g > 0) paste0("Group +", g) else if (g == 0) "Group 0" else paste0("Group ", g)
  }
  group_col_names <- sapply(groups, format_group)

  # Quality rows and first-column label depend on quality_col
  if (quality_col == "cinema") {
    combined_levels <- list(
      "High/Moderate" = c("High", "Moderate"),
      "Low/Very low"  = c("Low", "Very low")
    )
    no_data_label  <- "No CINeMA"
    quality_header <- "CINeMA"
  } else {
    combined_levels <- list(
      "n \u2267 400"        = c("High"),
      "100 \u2266 n < 400"  = c("Moderate"),
      "n < 100"             = c("Low")
    )
    no_data_label  <- "No data"
    quality_header <- "n"
  }

  # Reference treatment: cinema is always NA (no self-comparison in CINeMA)
  # → force it into the best quality bucket
  ref_trt <- attr(df, "reference")  # NULL for old outputs without attribute

  # Add a no-data row only if non-reference treatments have missing quality
  non_ref_mask <- if (!is.null(ref_trt)) df$treatment != ref_trt else
    rep(TRUE, nrow(df))
  has_no_data <- any(is.na(df[[quality_col]][non_ref_mask]))
  if (has_no_data) combined_levels[[no_data_label]] <- NA_character_

  best_bucket <- names(combined_levels)[1]

  # Build table
  rows <- lapply(names(combined_levels), function(qlabel) {
    qvals <- combined_levels[[qlabel]]
    row_vals <- sapply(groups, function(g) {
      sub_df <- df[df$group == g, ]
      if (length(qvals) == 1L && is.na(qvals)) {
        # No-data bucket: exclude reference (placed in best bucket instead)
        trts <- sub_df$treatment[
          is.na(sub_df[[quality_col]]) &
            (is.null(ref_trt) | sub_df$treatment != ref_trt)
        ]
      } else {
        mask <- !is.na(sub_df[[quality_col]]) &
                  tolower(sub_df[[quality_col]]) %in% tolower(qvals)
        # Best bucket: include reference even when its quality is NA
        if (qlabel == best_bucket && !is.null(ref_trt)) {
          ref_in_group <- sub_df$treatment == ref_trt &
                            is.na(sub_df[[quality_col]])
          mask <- mask | ref_in_group
        }
        trts <- sub_df$treatment[mask]
      }
      paste(trts, collapse = ", ")
    })
    stats::setNames(
      c(list(qlabel), as.list(row_vals)),
      c(quality_header, group_col_names)
    )
  })

  result_df <- do.call(
    rbind,
    lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE)
  )

  if (is.null(file)) return(result_df)

  ext <- tolower(tools::file_ext(file))

  if (ext == "docx") {
    ft <- flextable::flextable(result_df)
    ft <- flextable::set_table_properties(ft, layout = "autofit")
    pr_section <- officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      type = "continuous"
    )
    flextable::save_as_docx(ft, path = file, pr_section = pr_section)
    message("Saved min_context table (docx): ", file)
  } else if (ext == "xlsx") {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "min_context")
    openxlsx::writeData(wb, sheet = 1, result_df)
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    message("Saved min_context table (xlsx): ", file)
  } else {
    stop("Unsupported file extension: ", ext, ". Use .docx or .xlsx")
  }

  invisible(result_df)
}


#' @title Tabulate min_context results across multiple outcomes
#'
#' @description Creates a wide table where each row is an outcome and each
#'   column is a group level (+2, +1, 0, -1, -2, etc.). Cells list the
#'   treatment names assigned to that group for that outcome.
#'   Unlike \code{\link{table_min_context}()}, this function does not
#'   cross-classify by CINeMA or N-quality.
#'
#' @param outcome_list A named list where each element is a data frame
#'   returned by \code{\link{min_context}()}. Names are used as row labels
#'   (outcome names). Must have at least one element with a non-empty name.
#' @param sep Separator between treatment names within a cell.
#'   Default \code{"\n"} (newline). Use \code{", "} for comma-separated.
#' @param file Optional output path (\code{.docx} or \code{.xlsx}).
#'   If \code{NULL}, returns a data frame.
#' @return A data frame (rows = outcomes, cols = groups), or writes to file
#'   and invisibly returns the data frame.
#' @export
#'
#' @importFrom flextable flextable set_table_properties save_as_docx
#' @importFrom officer prop_section page_size
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
table_min_context_multi <- function(outcome_list, sep = "\n", file = NULL) {
  if (!is.list(outcome_list) || length(outcome_list) == 0)
    stop("`outcome_list` must be a non-empty named list.")
  if (is.null(names(outcome_list)) || any(nchar(names(outcome_list)) == 0))
    stop("`outcome_list` must have non-empty names for each element.")

  # Collect all group levels across all outcomes, sorted high -> low
  all_groups <- sort(
    unique(unlist(lapply(outcome_list, function(df) df$group))),
    decreasing = TRUE
  )

  format_group <- function(g) {
    if (g > 0) paste0("Group +", g) else if (g == 0) "Group 0" else
      paste0("Group ", g)
  }
  group_col_names <- sapply(all_groups, format_group)

  # Build one row per outcome
  rows <- lapply(names(outcome_list), function(outcome_name) {
    df <- outcome_list[[outcome_name]]
    row_vals <- sapply(all_groups, function(g) {
      trts <- df$treatment[df$group == g]
      paste(trts, collapse = sep)
    })
    stats::setNames(
      c(list(outcome_name), as.list(row_vals)),
      c("Outcome", group_col_names)
    )
  })

  result_df <- do.call(
    rbind,
    lapply(rows, as.data.frame, stringsAsFactors = FALSE)
  )

  if (is.null(file)) return(result_df)

  ext <- tolower(tools::file_ext(file))

  if (ext == "docx") {
    ft <- flextable::flextable(result_df)
    ft <- flextable::set_table_properties(ft, layout = "autofit")
    pr_section <- officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      type = "continuous"
    )
    flextable::save_as_docx(ft, path = file, pr_section = pr_section)
    message("Saved multi-outcome min_context table (docx): ", file)
  } else if (ext == "xlsx") {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "min_context_multi")
    openxlsx::writeData(wb, sheet = 1, result_df)
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    message("Saved multi-outcome min_context table (xlsx): ", file)
  } else {
    stop("Unsupported file extension: ", ext, ". Use .docx or .xlsx")
  }

  invisible(result_df)
}
