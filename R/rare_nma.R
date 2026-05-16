# rare_nma.R — Rare-event diagnostics and 4-method sensitivity panel for NMA
#
# Mirrors pmatools/R/rare_events.R for network meta-analysis.
# References:
#   Efthimiou O, Rücker G, Schwarzer G, Higgins JPT, Egger M, Salanti G (2019).
#   Network meta-analysis of rare events using the Mantel-Haenszel method.
#   Statistics in Medicine 38(16):2992–3012.
#
#   Tsujimoto Y et al. (2024). Methodological recommendations for rare-event
#   meta-analysis. Research Synthesis Methods. PMC9805041.

# ── Internal: convert df_pw back to per-arm long table ────────────────────────

# meta::pairwise() output contains, for each unique (studlab, treat) arm, the
# same (n, event) duplicated across every contrast that arm participates in.
# We deduplicate on (studlab, treat) to recover per-arm counts.
.df_pw_to_arms <- function(df_pw) {
  rows1 <- data.frame(
    studlab = as.character(df_pw$studlab),
    treat   = as.character(df_pw$treat1),
    n       = suppressWarnings(as.numeric(df_pw$n1)),
    event   = suppressWarnings(as.numeric(df_pw$event1)),
    stringsAsFactors = FALSE
  )
  rows2 <- data.frame(
    studlab = as.character(df_pw$studlab),
    treat   = as.character(df_pw$treat2),
    n       = suppressWarnings(as.numeric(df_pw$n2)),
    event   = suppressWarnings(as.numeric(df_pw$event2)),
    stringsAsFactors = FALSE
  )
  arms <- rbind(rows1, rows2)
  arms <- arms[is.finite(arms$n) & is.finite(arms$event) & arms$n > 0, ,
               drop = FALSE]
  arms <- arms[!duplicated(arms[, c("studlab", "treat")]), , drop = FALSE]
  arms
}

# ── Diagnostics ───────────────────────────────────────────────────────────────

# Compute rare-event diagnostics from a pairwise data frame.
# Returns an object of class "nma_rare_diagnostics".
.rare_nma_diagnostics <- function(df_pw,
                                  event_rate_threshold = 0.01,
                                  zero_study_fraction_threshold = 0.50) {
  arms <- .df_pw_to_arms(df_pw)
  if (nrow(arms) == 0L) {
    stop("No complete arm data available for rare-event diagnostics.")
  }

  # Per-study summaries
  by_study <- split(arms, arms$studlab)
  zero_arm_in_study <- vapply(by_study, function(d) any(d$event == 0L),
                              logical(1L))
  all_zero_in_study <- vapply(by_study, function(d) all(d$event == 0L),
                              logical(1L))
  k_studies        <- length(by_study)
  zero_cell_k      <- sum(zero_arm_in_study)
  double_zero_k    <- sum(all_zero_in_study)
  single_zero_k    <- zero_cell_k - double_zero_k
  both_arms_events_k <- sum(vapply(by_study,
                                   function(d) all(d$event > 0L),
                                   logical(1L)))

  # Per-arm-treatment summaries
  by_treat <- split(arms, arms$treat)
  events_by_treat <- vapply(by_treat, function(d) sum(d$event), numeric(1L))
  n_by_treat      <- vapply(by_treat, function(d) sum(d$n),     numeric(1L))
  rate_by_treat   <- ifelse(n_by_treat > 0, events_by_treat / n_by_treat,
                            NA_real_)

  total_events <- sum(arms$event)
  total_n      <- sum(arms$n)
  event_rate_overall <- if (total_n > 0) total_events / total_n else NA_real_

  one_arm_total_zero <- any(events_by_treat == 0)
  zero_study_fraction <- mean(zero_arm_in_study)

  # rare_flow logic — mirrors pmatools .rare_diagnostics_from_wide()
  rare_rate_flag <- isTRUE(event_rate_overall < event_rate_threshold) ||
    any(rate_by_treat < event_rate_threshold, na.rm = TRUE)
  sparse_zero_pattern <- any(zero_arm_in_study) &&
    is.finite(event_rate_overall) && event_rate_overall < 0.05
  high_zero_fraction <- zero_study_fraction >= zero_study_fraction_threshold &&
    is.finite(event_rate_overall) && event_rate_overall < 0.05
  few_informative <- both_arms_events_k <= 1L && any(zero_arm_in_study) &&
    is.finite(event_rate_overall) && event_rate_overall < 0.10

  rare_flow <- rare_rate_flag || one_arm_total_zero ||
    sparse_zero_pattern || high_zero_fraction || few_informative

  recommendation <- if (rare_flow) {
    paste(
      "Rare-event NMA workflow recommended.",
      "Primary analysis switches to common-effect Mantel-Haenszel NMA",
      "(no continuity correction). A 4-method sensitivity panel",
      "(MH / NCH / IV-FE-CC / IV-RE-CC) is also produced."
    )
  } else {
    "Rare-event workflow not triggered; standard NMA workflow applies."
  }

  structure(
    list(
      rare_flow            = isTRUE(rare_flow),
      rare_rate_flag       = isTRUE(rare_rate_flag),
      one_arm_total_zero   = isTRUE(one_arm_total_zero),
      sparse_zero_pattern  = isTRUE(sparse_zero_pattern),
      high_zero_fraction   = isTRUE(high_zero_fraction),
      few_informative      = isTRUE(few_informative),
      k                    = k_studies,
      n_treatments         = length(by_treat),
      total_events         = total_events,
      total_n              = total_n,
      event_rate_overall   = event_rate_overall,
      events_by_treat      = events_by_treat,
      n_by_treat           = n_by_treat,
      rate_by_treat        = rate_by_treat,
      zero_cell_k          = zero_cell_k,
      single_zero_k        = single_zero_k,
      double_zero_k        = double_zero_k,
      both_arms_events_k   = both_arms_events_k,
      zero_study_fraction  = zero_study_fraction,
      event_rate_threshold = event_rate_threshold,
      zero_study_fraction_threshold = zero_study_fraction_threshold,
      recommendation       = recommendation
    ),
    class = "nma_rare_diagnostics"
  )
}

#' @export
format.nma_rare_diagnostics <- function(x, ...) {
  pct <- function(p) if (is.finite(p)) sprintf("%.2f%%", 100 * p) else "NA"
  per_treat <- paste(
    sprintf("    %-30s %6d / %8d (%s)",
            names(x$events_by_treat),
            as.integer(x$events_by_treat),
            as.integer(x$n_by_treat),
            vapply(x$rate_by_treat, pct, character(1L))),
    collapse = "\n"
  )
  c(
    "Rare-event NMA diagnostics",
    "==========================",
    sprintf("  rare_flow            : %s", x$rare_flow),
    sprintf("  Studies (k)          : %d", x$k),
    sprintf("  Treatments           : %d", x$n_treatments),
    sprintf("  Total events / N     : %d / %d (overall rate = %s)",
            as.integer(x$total_events), as.integer(x$total_n),
            pct(x$event_rate_overall)),
    sprintf("  Zero-arm studies     : %d / %d (%.1f%%)",
            x$zero_cell_k, x$k, 100 * x$zero_study_fraction),
    sprintf("    single-zero        : %d", x$single_zero_k),
    sprintf("    double-zero        : %d", x$double_zero_k),
    sprintf("  Studies with events in all arms : %d", x$both_arms_events_k),
    sprintf("  Treatment with 0 total events?  : %s", x$one_arm_total_zero),
    "",
    "  Per-treatment event rates:",
    per_treat,
    "",
    "  Triggered flags:",
    sprintf("    rare_rate_flag       : %s", x$rare_rate_flag),
    sprintf("    one_arm_total_zero   : %s", x$one_arm_total_zero),
    sprintf("    sparse_zero_pattern  : %s", x$sparse_zero_pattern),
    sprintf("    high_zero_fraction   : %s", x$high_zero_fraction),
    sprintf("    few_informative      : %s", x$few_informative),
    "",
    sprintf("  Threshold: event rate < %s; zero-arm fraction >= %s",
            pct(x$event_rate_threshold),
            pct(x$zero_study_fraction_threshold)),
    "",
    "  Recommendation:",
    paste0("    ", strwrap(x$recommendation, width = 76, exdent = 4))
  )
}

#' @export
print.nma_rare_diagnostics <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# ── Sensitivity panel ─────────────────────────────────────────────────────────

# Method specifications for the 4-method sensitivity panel.
.rare_nma_method_specs <- function() {
  list(
    list(id     = "MH",
         label  = "Mantel-Haenszel, no CC (common-effect)",
         method = "MH",     incr = 0,   cc.pooled = FALSE,
         common = TRUE,     random = FALSE,
         note   = "Recommended primary for rare events (Efthimiou 2019)."),
    list(id     = "NCH",
         label  = "Non-central hypergeometric (Breslow approx.)",
         method = "NCH",    incr = 0,   cc.pooled = FALSE,
         common = TRUE,     random = FALSE,
         note   = "Slightly better coverage when ALL studies have low event rates; biased if rates are mixed/high."),
    list(id     = "IV_FE_CC",
         label  = "Inverse-variance, fixed-effect, CC = 0.5",
         method = "Inverse", incr = 0.5, cc.pooled = TRUE,
         common = TRUE,      random = FALSE,
         note   = "Reference comparator; suboptimal for rare events."),
    list(id     = "IV_RE_CC",
         label  = "Inverse-variance, random-effects, CC = 0.5",
         method = "Inverse", incr = 0.5, cc.pooled = TRUE,
         common = FALSE,     random = TRUE,
         note   = "Reference comparator; tau often estimated as 0 with sparse data.")
  )
}

# Fit a single rare-event NMA model. Returns NULL on failure.
.fit_one_rare_nma <- function(df_pw, spec, sm, reference.group, small.values) {
  args <- list(
    df_pw,
    sm              = sm,
    reference.group = reference.group,
    small.values    = small.values,
    sort            = TRUE,
    method          = spec$method,
    incr            = spec$incr,
    cc.pooled       = spec$cc.pooled,
    common          = spec$common,
    random          = spec$random,
    warn            = FALSE,
    warn.deprecated = FALSE
  )
  err <- NULL
  obj <- tryCatch(
    suppressWarnings(do.call(netmeta::netmetabin, args)),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  list(spec = spec, fit = obj, error = err)
}

# Run the 4-method sensitivity panel and return a tidy tibble:
#   one row per (method × non-reference treatment) with TE, seTE, lower, upper
#   on the back-transformed scale (OR / RR).
.run_rare_nma_sensitivity <- function(df_pw, sm, reference.group,
                                      small.values) {
  specs <- .rare_nma_method_specs()
  fits  <- lapply(specs, .fit_one_rare_nma, df_pw = df_pw, sm = sm,
                  reference.group = reference.group,
                  small.values = small.values)

  rows <- lapply(fits, function(z) {
    spec <- z$spec
    obj  <- z$fit
    if (is.null(obj)) {
      return(data.frame(
        method_id   = spec$id,
        method_label = spec$label,
        treat       = NA_character_,
        estimate    = NA_real_,
        ci_low      = NA_real_,
        ci_high     = NA_real_,
        TE          = NA_real_,
        seTE        = NA_real_,
        model       = if (isTRUE(spec$random)) "random" else "common",
        status      = z$error %||% "Method failed",
        note        = spec$note,
        stringsAsFactors = FALSE
      ))
    }
    use_random <- isTRUE(spec$random)
    TE_mat   <- if (use_random) obj$TE.random   else obj$TE.common
    se_mat   <- if (use_random) obj$seTE.random else obj$seTE.common
    lo_mat   <- if (use_random) obj$lower.random else obj$lower.common
    hi_mat   <- if (use_random) obj$upper.random else obj$upper.common
    if (is.null(TE_mat) || !(reference.group %in% colnames(TE_mat))) {
      return(data.frame(
        method_id   = spec$id,
        method_label = spec$label,
        treat       = NA_character_,
        estimate    = NA_real_,
        ci_low      = NA_real_,
        ci_high     = NA_real_,
        TE          = NA_real_,
        seTE        = NA_real_,
        model       = if (use_random) "random" else "common",
        status      = "Reference group not in fit",
        note        = spec$note,
        stringsAsFactors = FALSE
      ))
    }
    trts <- setdiff(rownames(TE_mat), reference.group)
    data.frame(
      method_id    = spec$id,
      method_label = spec$label,
      treat        = trts,
      estimate     = exp(TE_mat[trts, reference.group]),
      ci_low       = exp(lo_mat[trts, reference.group]),
      ci_high      = exp(hi_mat[trts, reference.group]),
      TE           = TE_mat[trts, reference.group],
      seTE         = se_mat[trts, reference.group],
      model        = if (use_random) "random" else "common",
      status       = "",
      note         = spec$note,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# ── Sensitivity forest plot ───────────────────────────────────────────────────

#' Plot the 4-method rare-event NMA sensitivity panel
#'
#' Creates a faceted forest plot: for each non-reference treatment, the
#' MH-NMA (primary), NCH-NMA, IV-FE-CC and IV-RE-CC estimates are stacked.
#'
#' @param method_table Tidy `data.frame` returned by the internal
#'   `.run_rare_nma_sensitivity()` (saved as `method_table_{outcome}.rds` by
#'   [netmetawrap()] when the rare-event workflow is triggered).
#' @param sm Effect measure label (`"OR"` or `"RR"`); used as the
#'   right-column heading and for back-transformation labelling.
#' @param reference.group Reference treatment label (used for x-axis legend).
#' @param file Optional output PDF path. When supplied, the plot is written
#'   to this file at an A4-friendly size.
#' @param title Optional plot title.
#' @param ... Passed through to [meta::forest()].
#'
#' @return Invisibly `NULL`. Side-effect: draws on the active device or
#'   writes a PDF to `file`.
#' @seealso [netmetawrap()]
#' @export
plot_rare_nma_sensitivity <- function(method_table,
                                      sm = "OR",
                                      reference.group = NULL,
                                      file = NULL,
                                      title = NULL,
                                      ...) {
  stopifnot(is.data.frame(method_table))
  treats <- unique(stats::na.omit(method_table$treat))
  if (length(treats) == 0L) {
    warning("method_table contains no valid treatment estimates; skipping plot.")
    return(invisible(NULL))
  }

  # Build one metagen object per treatment (row stack of methods).
  build_one <- function(trt) {
    sub <- method_table[!is.na(method_table$treat) &
                          method_table$treat == trt, , drop = FALSE]
    sub <- sub[order(match(sub$method_id, c("MH", "NCH", "IV_FE_CC", "IV_RE_CC"))), ]
    valid <- is.finite(sub$TE) & is.finite(sub$seTE) & sub$seTE > 0
    if (!any(valid)) return(NULL)
    studlab <- ifelse(sub$method_id == "MH",
                      paste0(sub$method_id, " (primary)"),
                      sub$method_id)
    meta::metagen(
      TE         = sub$TE[valid],
      seTE       = sub$seTE[valid],
      studlab    = studlab[valid],
      sm         = sm,
      common     = FALSE,
      random     = FALSE,
      backtransf = TRUE,
      warn       = FALSE,
      method.tau = "DL"
    )
  }

  draw_panel <- function() {
    n_trt <- length(treats)
    op <- graphics::par(mfrow = c(n_trt, 1L), mar = c(2, 2, 3, 2))
    on.exit(graphics::par(op), add = TRUE)
    for (trt in treats) {
      m <- build_one(trt)
      if (is.null(m)) {
        graphics::plot.new()
        graphics::title(main = sprintf("%s vs %s — no valid estimates",
                                       trt, reference.group %||% "reference"))
        next
      }
      smlab <- sprintf("%s vs %s", trt, reference.group %||% "reference")
      tryCatch(
        meta::forest(
          m,
          smlab     = smlab,
          leftcols  = "studlab",
          leftlabs  = "Method",
          rightcols = c("effect", "ci"),
          rightlabs = c(sm, "95% CI"),
          spacing   = 0.9,
          fs.study  = 9,
          fs.heading = 10,
          ...
        ),
        error = function(e) {
          graphics::plot.new()
          graphics::title(main = paste0(smlab, " — forest failed: ",
                                        conditionMessage(e)))
        }
      )
    }
    if (!is.null(title)) {
      graphics::mtext(title, side = 3, line = -1.5, outer = TRUE, cex = 1.1)
    }
  }

  if (is.null(file)) {
    draw_panel()
    return(invisible(NULL))
  }

  height <- max(4, length(treats) * 2.2)
  grDevices::pdf(file, width = 9, height = height)
  tryCatch(draw_panel(), finally = grDevices::dev.off())
  invisible(file)
}

# ── Convenience: tidy method_table for xlsx export ────────────────────────────

# Reorder/format columns for human-readable Excel output.
.format_rare_method_table <- function(method_table, reference.group) {
  if (nrow(method_table) == 0L) return(method_table)
  out <- method_table
  out$comparison <- ifelse(
    is.na(out$treat),
    NA_character_,
    sprintf("%s vs %s", out$treat, reference.group %||% "reference")
  )
  out$estimate_ci <- ifelse(
    is.finite(out$estimate) & is.finite(out$ci_low) & is.finite(out$ci_high),
    sprintf("%.2f [%.2f; %.2f]", out$estimate, out$ci_low, out$ci_high),
    NA_character_
  )
  cols <- c("method_id", "method_label", "model", "comparison",
            "estimate", "ci_low", "ci_high", "estimate_ci",
            "TE", "seTE", "status", "note")
  out[, intersect(cols, names(out)), drop = FALSE]
}

# Null-coalescing operator (defined for stand-alone sourcing of this file).
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}
