# =============================================================================
# ROB-MEN background plot builder (spec-13, phase 1)
# =============================================================================
# Pure helper used by module_C_robmen.R's async pipeline (phase 2). Kept in a
# standalone file (no shiny / DT / plotly imports) so the function can be
# unit-tested via tests/testthat/test-robmen-bg-plots.R without loading the
# full Shiny app.
#
# Responsibilities
# ----------------
# Given one pairwise comparison and its study-level subset, write a forest
# PNG (always, when k >= 1) and — only if k >= 10 (Egger's threshold) — a
# contour-enhanced funnel PNG. Return a single-row metadata list describing
# what was produced and any error encountered.
#
# Inputs
# ------
# comp          : list / data.frame row with at least $comp_key (character).
#                 Used purely for labelling and filename construction; the
#                 caller is responsible for filtering the studies to the
#                 right t1/t2 pair before calling this helper.
# study_subset  : data.frame with columns studlab, y (TE), se (seTE).
#                 Optional: rob (used in subtitle if present).
# sm            : summary measure passed to meta::metagen (default "MD").
#                 The helper does NOT back-transform — y / se are taken on
#                 whatever scale the caller uses, matching netmeta's stored
#                 internal scale (log scale for OR/RR/HR).
# run_id        : integer / character used to disambiguate filenames across
#                 re-runs. Defaults to 0L for tests.
# out_dir       : directory to write PNGs into. Defaults to tempdir().
# width/height/res : png() parameters; defaults aim for ~800x500 px @ 100 DPI.
#
# Output
# ------
# A list with fields
#   comp_key    : character — echo of comp$comp_key
#   forest_path : character — path to written forest PNG, or NA on failure
#   funnel_path : character — path to written funnel PNG, or NA when k < 10
#                 or on failure
#   k           : integer  — number of valid studies after NA / SE filter
#   error       : character — NA on success, otherwise an error message
#
# Error isolation
# ---------------
# Each plot is wrapped in tryCatch(); a metagen() or forest() failure does
# not propagate, instead it returns a row with forest_path = NA_character_
# and error filled in. This is what spec-13 S4 ("Error isolation") relies on:
# the async pipeline iterates over comparisons via lapply() and one bad
# subset must not abort the rest.
# =============================================================================

build_robmen_plots <- function(comp,
                               study_subset,
                               sm      = "MD",
                               run_id  = 0L,
                               out_dir = tempdir(),
                               width   = 800,
                               height  = 500,
                               res     = 100) {

  comp_key <- if (is.list(comp) || is.data.frame(comp)) {
    as.character(comp[["comp_key"]])
  } else {
    as.character(comp)
  }
  if (length(comp_key) == 0 || is.na(comp_key) || !nzchar(comp_key))
    comp_key <- "unknown"

  empty <- list(comp_key    = comp_key,
                forest_path = NA_character_,
                funnel_path = NA_character_,
                k           = 0L,
                error       = NA_character_)

  # ---- input validation ----------------------------------------------------
  if (is.null(study_subset) ||
      (!is.data.frame(study_subset) && !is.list(study_subset)) ||
      NROW(study_subset) == 0)
    return(empty)

  required <- c("studlab", "y", "se")
  miss <- setdiff(required, names(study_subset))
  if (length(miss) > 0)
    return(modifyList(empty, list(
      error = paste0("Missing required column(s): ",
                     paste(miss, collapse = ", "))
    )))

  ok_rows <- !is.na(study_subset$y) &
             !is.na(study_subset$se) &
             study_subset$se > 0
  sub <- study_subset[ok_rows, , drop = FALSE]
  k   <- nrow(sub)
  if (k == 0)
    return(modifyList(empty, list(
      error = "No valid studies (y/se NA or se <= 0)"
    )))

  # Ensure output directory exists. tempdir() always does; user-supplied
  # might not.
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  safe_label  <- gsub("[^A-Za-z0-9_-]+", "_", comp_key)
  forest_path <- file.path(
    out_dir, paste0("robmen_forest_", run_id, "_", safe_label, ".png"))
  funnel_path <- file.path(
    out_dir, paste0("robmen_funnel_", run_id, "_", safe_label, ".png"))

  # ---- metagen object (shared by both plots) -------------------------------
  m <- tryCatch(
    meta::metagen(TE      = sub$y,
                  seTE    = sub$se,
                  studlab = sub$studlab,
                  sm      = sm,
                  common  = FALSE,
                  random  = TRUE),
    error = function(e) e)

  if (inherits(m, "error"))
    return(modifyList(empty, list(
      k     = k,
      error = paste0("metagen() failed: ", conditionMessage(m))
    )))

  # ---- forest plot ---------------------------------------------------------
  forest_err <- tryCatch({
    grDevices::png(forest_path,
                   width = width, height = height, res = res)
    on_exit_close <- TRUE
    tryCatch(
      meta::forest(m,
                   leftcols  = c("studlab"),
                   rightcols = c("effect", "ci"),
                   smlab     = paste("Forest plot:", comp_key)),
      finally = { grDevices::dev.off(); on_exit_close <<- FALSE })
    NULL
  }, error = function(e) {
    # Make sure the device is closed even if forest() errored before finally
    if (length(grDevices::dev.list()) > 0)
      try(grDevices::dev.off(), silent = TRUE)
    if (file.exists(forest_path))
      try(file.remove(forest_path), silent = TRUE)
    e
  })

  if (inherits(forest_err, "error"))
    return(modifyList(empty, list(
      k     = k,
      error = paste0("forest() failed: ", conditionMessage(forest_err))
    )))

  # ---- funnel plot (only when k >= 10, Egger threshold) --------------------
  funnel_actual <- NA_character_
  if (k >= 10) {
    funnel_err <- tryCatch({
      grDevices::png(funnel_path,
                     width = width, height = height, res = res)
      tryCatch(
        meta::funnel(m,
                     contour = c(0.9, 0.95, 0.99),
                     col.contour = c("#dddddd", "#bbbbbb", "#999999")),
        finally = grDevices::dev.off())
      NULL
    }, error = function(e) {
      if (length(grDevices::dev.list()) > 0)
        try(grDevices::dev.off(), silent = TRUE)
      if (file.exists(funnel_path))
        try(file.remove(funnel_path), silent = TRUE)
      e
    })
    if (!inherits(funnel_err, "error"))
      funnel_actual <- funnel_path
    # NB: funnel failure does NOT propagate to the row's $error — we still
    # have a usable forest plot, and funnel is a secondary diagnostic.
    # Caller can detect missing funnel via funnel_path == NA.
  }

  list(comp_key    = comp_key,
       forest_path = forest_path,
       funnel_path = funnel_actual,
       k           = k,
       error       = NA_character_)
}
