# =============================================================================
# generate_gui_screenshots.R
# -----------------------------------------------------------------------------
# Reproducible shinytest2 pipeline that launches the nmatools Shiny app
# (inst/app), drives it through the full CINeMA + ROB-MEN workflow, and saves
# ~16 PNG screenshots into docs/manual/images/ for the user manual.
#
# Run from the repository root:
#   NOT_CRAN=true Rscript docs/manual/generate_gui_screenshots.R
#
# Requirements: shinytest2, chromote, and a local Chrome/Chromium install.
# The app forces an en_US.UTF-8 CTYPE locale at startup; we mirror that here so
# the UTF-8 navbar labels (①-⑥, en-dash, ▶) match exactly.
# =============================================================================

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

library(shinytest2)

options(chromote.headless = "new")

# ---- Paths ------------------------------------------------------------------
IMG_DIR <- "docs/manual/images"
if (!dir.exists(IMG_DIR)) dir.create(IMG_DIR, recursive = TRUE)

# Remove any previous captures so shinytest2's get_screenshot (which refuses to
# overwrite an existing file) has clean targets.
old <- list.files(IMG_DIR, pattern = "^gui_.*\\.png$", full.names = TRUE)
if (length(old)) file.remove(old)

# ---- Helpers ----------------------------------------------------------------
# shot(): capture a screenshot into IMG_DIR. If `selector` is given, capture
# that element only (focused/unclipped); otherwise capture the full page. Falls
# back to a full-page shot if the selector cannot be resolved.
shot <- function(app, name, selector = NULL, delay = 0.4) {
  path <- file.path(IMG_DIR, name)
  if (file.exists(path)) file.remove(path)
  if (delay > 0) Sys.sleep(delay)
  if (is.null(selector)) {
    app$get_screenshot(path)
  } else {
    ok <- tryCatch({ app$get_screenshot(path, selector = selector); TRUE },
                   error = function(e) {
                     message("  selector '", selector, "' failed (",
                             conditionMessage(e), ") -> full-page fallback")
                     FALSE
                   })
    if (!ok) {
      if (file.exists(path)) file.remove(path)
      app$get_screenshot(path)
    }
  }
  message("  saved ", name)
  invisible(path)
}

# Switch navbar tab by its exact UTF-8 label (navset input id = main_navbar).
go_tab <- function(app, label, idle = 8000, settle = 0.8) {
  app$set_inputs(main_navbar = label)
  app$wait_for_idle(timeout = idle)
  Sys.sleep(settle)
}

# wrap_section(): group the DOM siblings from the (last) heading matching
# `heading_re` down through the block containing element `end_id` into a new
# <div id=wrap_id>, so that block can be captured with a single focused,
# viewport-independent selector screenshot. Returns TRUE on success.
# shinytest2's get_screenshot ignores chromote's `cliprect` (it forces
# selector="html"), so an injected wrapper is the reliable way to crop a region
# that has no id of its own. This only mutates the live browser DOM, never the
# app source.
wrap_section <- function(app, wrap_id, heading_re, end_id) {
  js <- sprintf("(function(){
    var end = document.getElementById('%s');
    var hs = document.querySelectorAll('h4'); var h = null;
    for (var i=0;i<hs.length;i++){ if(/%s/.test(hs[i].textContent)) h = hs[i]; }
    if(!h||!end) return false;
    var endBlock = end;
    while(endBlock && endBlock.parentNode !== h.parentNode) endBlock = endBlock.parentNode;
    if(!endBlock) return false;
    var nodes=[]; var node=h;
    while(node){ nodes.push(node); if(node===endBlock) break; node=node.nextElementSibling; }
    var wrap=document.createElement('div'); wrap.id='%s';
    h.parentNode.insertBefore(wrap, h);
    nodes.forEach(function(n){ wrap.appendChild(n); });
    return true;
  })()", end_id, heading_re, wrap_id)
  isTRUE(tryCatch(app$get_js(js), error = function(e) FALSE))
}

# ---- Launch -----------------------------------------------------------------
message("Launching app ...")
app <- AppDriver$new(
  app_dir      = "inst/app",
  name         = "nmatools-gui",
  width        = 1440,
  height       = 1000,
  load_timeout = 60000,
  seed         = 42
)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
app$wait_for_idle(timeout = 15000)

# =============================================================================
# 1. Configuration tab, nothing loaded
# =============================================================================
message("[01] config empty")
shot(app, "gui_01_config_empty.png")

# =============================================================================
# 2. Load SLEEPI demo data -> Configuration fills in
# =============================================================================
message("[02] load demo")
app$click("module_a-load_demo")
app$wait_for_idle(timeout = 30000)
Sys.sleep(1)
shot(app, "gui_02_config_demo_loaded.png")

# =============================================================================
# 3. NMA Settings section (reference / model / tau + ROB-MEN Bayesian settings)
# =============================================================================
message("[03] nma settings")
shot(app, "gui_03_config_nma_settings.png", selector = "#module_a-nma_settings_ui")

# =============================================================================
# 4. ROB-MEN MCMC settings + Pairwise Data Preview table
# =============================================================================
message("[04] pairwise preview")
shot(app, "gui_04_config_robmen_settings.png", selector = "#module_a-data_preview")

# =============================================================================
# 5. Run CINeMA + ROB-MEN analysis, wait for completion
# =============================================================================
message("[05] run analysis (this takes ~20-40s: Bayesian Egger / JAGS MCMC) ...")
app$click("module_a-run_analysis")
# Long idle wait covering the full Bayesian pipeline.
app$wait_for_idle(timeout = 120000, duration = 3000)

# Robust completion poll: switch to Report and wait until the summary table
# (combined_dt) actually renders a data row (outputs on hidden tabs are
# suspended, so it only renders once Report is active).
go_tab(app, "Report", idle = 60000, settle = 1)
report_ready <- FALSE
for (i in seq_len(30)) {
  html <- tryCatch(app$get_html("#module_d-combined_dt"), error = function(e) "")
  if (!is.null(html) && nchar(html) > 400 && grepl("<td", html)) {
    report_ready <- TRUE
    break
  }
  Sys.sleep(3)
  app$wait_for_idle(timeout = 15000)
}
message("  report_ready = ", report_ready, " after poll")
Sys.sleep(1)

# gui_05 documents the completed Configuration state (full page: settings +
# loaded data + run control), captured after the pipeline has finished.
go_tab(app, "Configuration", idle = 15000, settle = 1)
shot(app, "gui_05_config_run_done.png")

# =============================================================================
# 6. Domain 1 - Within-study bias
# =============================================================================
message("[06] D1 within-study bias")
go_tab(app, "① Within-study bias", idle = 20000, settle = 1.5)
shot(app, "gui_06_d1_withinstudy.png", delay = 1.2)

# =============================================================================
# 7. Domain 2 - Reporting bias (embedded ROB-MEN analysis, top of tab)
# =============================================================================
message("[07] D2 reporting bias (ROB-MEN embed)")
go_tab(app, "② Reporting bias", idle = 30000, settle = 1.5)
shot(app, "gui_07_d2_robmen.png", delay = 1.2)

# =============================================================================
# 8. Domain 2 - Final Ratings section (per-comparison overrides)
# =============================================================================
message("[08] D2 final ratings")
# Wrap the "Domain 2 ... Final Ratings" heading + bulk buttons + per-comparison
# override table into one div so we can capture the whole section, focused.
d2_wrapped <- wrap_section(app, "gui08_wrap", "Final Ratings",
                           "module_b-d2_override_ui")
Sys.sleep(0.4)
shot(app, "gui_08_d2_robmen_final.png",
     selector = if (d2_wrapped) "#gui08_wrap" else "#module_b-d2_override_ui",
     delay = 0.4)

# =============================================================================
# 9-12. Domains 3-6
# =============================================================================
message("[09] D3 indirectness")
go_tab(app, "③ Indirectness", idle = 20000, settle = 1.2)
shot(app, "gui_09_d3_indirectness.png", delay = 1)

message("[10] D4 imprecision")
go_tab(app, "④ Imprecision", idle = 20000, settle = 1.2)
shot(app, "gui_10_d4_imprecision.png", delay = 1)

message("[11] D5 heterogeneity")
go_tab(app, "⑤ Heterogeneity", idle = 20000, settle = 1.2)
shot(app, "gui_11_d5_heterogeneity.png", delay = 1)

message("[12] D6 incoherence")
go_tab(app, "⑥ Incoherence", idle = 20000, settle = 1.2)
shot(app, "gui_12_d6_incoherence.png", delay = 1)

# =============================================================================
# 13-16. Report tab: summary table, network graph, forest plot, export
# =============================================================================
message("[13-16] Report tab")
go_tab(app, "Report", idle = 30000, settle = 2)

message("[13] report summary table")
shot(app, "gui_13_report_summary.png", selector = "#module_d-combined_dt", delay = 1)

message("[14] report network graph")
shot(app, "gui_14_report_netgraph.png", selector = "#module_d-netgraph_plot_ui", delay = 1.5)

message("[15] report forest plot")
shot(app, "gui_15_report_forest.png", selector = "#module_d-forest_plot_ui", delay = 1.5)

message("[16] report export / bundle")
shot(app, "gui_16_report_export.png",
     selector = "div[style*='border:1px solid #d4d4d8']", delay = 0.6)

# ---- Teardown & manifest ----------------------------------------------------
app$stop()

expected <- c(
  "gui_01_config_empty.png", "gui_02_config_demo_loaded.png",
  "gui_03_config_nma_settings.png", "gui_04_config_robmen_settings.png",
  "gui_05_config_run_done.png", "gui_06_d1_withinstudy.png",
  "gui_07_d2_robmen.png", "gui_08_d2_robmen_final.png",
  "gui_09_d3_indirectness.png", "gui_10_d4_imprecision.png",
  "gui_11_d5_heterogeneity.png", "gui_12_d6_incoherence.png",
  "gui_13_report_summary.png", "gui_14_report_netgraph.png",
  "gui_15_report_forest.png", "gui_16_report_export.png"
)
paths <- file.path(IMG_DIR, expected)
present <- file.exists(paths)

message("\n===== Screenshot manifest =====")
for (i in seq_along(expected)) {
  sz <- if (present[i]) sprintf("%6.1f KB", file.size(paths[i]) / 1024) else "MISSING"
  message(sprintf("  %-34s %s", expected[i], sz))
}

stopifnot(all(present))
message("\nAll 16 screenshots generated successfully.")
