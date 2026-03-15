#' Plot Transitivity Assessment
#'
#' For each specified continuous covariate, produces a strip-and-box plot of
#' study-level covariate values grouped by direct treatment comparison.
#' Helps assess whether the distribution of effect modifiers differs across
#' the comparisons in the network (transitivity assumption).
#'
#' Each study is assigned to all pairwise comparisons it directly contributes
#' to (i.e., all pairs among the treatments in that study). For a 3-arm study
#' with arms A, B, C, the study is included in A vs B, A vs C, and B vs C.
#'
#' @param data Arm-level data frame (same as passed to [netmetawrap()]).
#' @param studlab Unquoted or quoted column name for study labels.
#' @param treat Unquoted or quoted column name for treatment.
#' @param covariate_cols Character vector of column names to visualize.
#'   Only numeric columns are plotted; others are skipped with a message.
#' @param outcome Name used for the output sub-directory and file names.
#'   Default `"transitivity"`.
#' @param path Base output directory. Default `"./outputs"`.
#' @param n_min_pair Minimum number of studies per comparison to include
#'   that comparison in the plot. Default `2L`.
#' @param trim Trim whitespace from PDFs via magick. Default `TRUE`.
#' @param trim_fuzz Fuzz parameter for [magick::image_trim()]. Default `30L`.
#'
#' @return Invisibly, a data frame with one row per study × comparison,
#'   containing the covariate values used for plotting.
#'
#' @examples
#' \dontrun{
#' d <- load_w2i()
#'
#' plot_transitivity(
#'   data           = d,
#'   studlab        = id,
#'   treat          = t,
#'   covariate_cols = c("rob", "indirectness"),
#'   outcome        = "remission_lt",
#'   path           = "./outputs"
#' )
#' }
#' @export
plot_transitivity <- function(data, studlab, treat,
                               covariate_cols,
                               outcome     = "transitivity",
                               path        = "./outputs",
                               n_min_pair  = 2L,
                               trim        = TRUE,
                               trim_fuzz   = 30L) {
  studlab <- .nse_col(substitute(studlab))
  treat   <- .nse_col(substitute(treat))

  missing_cols <- setdiff(covariate_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  output_dir <- file.path(path, outcome)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Build study-level data (one row per study) ────────────────────────────────
  # For multi-arm studies, covariate values are aggregated per column:
  #   - Multiple non-NA values → simple mean (handles arm-level variation)
  #   - Exactly one non-NA value → use that value
  #   - All NA → NA
  study_list <- split(data, data[[studlab]])
  study_df <- do.call(rbind, lapply(study_list, function(s) {
    row <- data.frame(studlab = s[[studlab]][1L], stringsAsFactors = FALSE)
    for (cov in covariate_cols) {
      vals    <- suppressWarnings(as.numeric(s[[cov]]))
      non_na  <- vals[!is.na(vals)]
      row[[cov]] <- if (length(non_na) == 0L) NA_real_ else mean(non_na)
    }
    row$treats <- I(list(sort(unique(as.character(s[[treat]])))))
    row
  }))
  rownames(study_df) <- NULL

  # ── Expand: one row per study × comparison pair ───────────────────────────────
  expanded_rows <- lapply(seq_len(nrow(study_df)), function(i) {
    trts <- unlist(study_df$treats[[i]])
    if (length(trts) < 2L) return(NULL)
    pairs <- utils::combn(trts, 2L, function(x) paste(x[1L], "vs", x[2L]),
                          simplify = FALSE)
    base  <- study_df[i, c("studlab", covariate_cols), drop = FALSE]
    lapply(pairs, function(p) cbind(base, comparison = p, stringsAsFactors = FALSE))
  })
  expanded <- do.call(
    rbind,
    Filter(Negate(is.null), unlist(expanded_rows, recursive = FALSE))
  )

  if (is.null(expanded) || nrow(expanded) == 0L) {
    message("plot_transitivity: no multi-arm or multi-treatment studies found.")
    return(invisible(NULL))
  }

  # ── Filter comparisons with too few studies ───────────────────────────────────
  counts <- table(expanded$comparison)
  keep   <- names(counts)[counts >= n_min_pair]
  if (length(keep) == 0L) {
    message("plot_transitivity: no comparison has \u2265 ", n_min_pair, " studies.")
    return(invisible(NULL))
  }
  expanded            <- expanded[expanded$comparison %in% keep, ]
  expanded$comparison <- factor(expanded$comparison, levels = sort(unique(expanded$comparison)))

  # ── One PDF per numeric covariate ─────────────────────────────────────────────
  for (cov in covariate_cols) {
    cov_vals <- expanded[[cov]]
    if (!is.numeric(cov_vals)) {
      message("plot_transitivity: skipping non-numeric covariate '", cov, "'.")
      next
    }

    fname    <- gsub("[^A-Za-z0-9_-]", "_", cov)
    out_file <- file.path(output_dir,
                          paste0("transitivity_", outcome, "_", fname, ".pdf"))

    n_comps <- nlevels(expanded$comparison)
    pw      <- max(6, n_comps * 1.8)
    ph      <- 5.5

    # Capture for use inside .save_plot expr
    comp_fac   <- expanded$comparison
    comp_cols  <- grDevices::rainbow(n_comps, alpha = 0.5)
    comp_bords <- grDevices::rainbow(n_comps)
    max_label_chars <- max(nchar(levels(comp_fac)))
    bottom_mar <- max(4L, ceiling(max_label_chars / 4L))

    .save_plot(
      file      = out_file,
      width     = pw,
      height    = ph,
      trim      = trim,
      trim_fuzz = trim_fuzz,
      expr      = {
        graphics::par(mar = c(bottom_mar, 4, 3, 1))
        graphics::boxplot(
          cov_vals ~ comp_fac,
          col     = comp_cols,
          border  = comp_bords,
          outline = FALSE,
          ylab    = cov,
          xlab    = "",
          main    = paste0("Transitivity: ", cov),
          las     = 2
        )
        graphics::stripchart(
          cov_vals ~ comp_fac,
          vertical = TRUE,
          method   = "jitter",
          pch      = 16,
          col      = comp_bords,
          add      = TRUE
        )
      }
    )
  }

  message("plot_transitivity: plots saved to ", output_dir)
  invisible(expanded)
}
