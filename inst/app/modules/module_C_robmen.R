# =============================================================================
# Module C: ROB-MEN Analysis
# =============================================================================
# REFERENCES:
#   Chiocchia V et al. ROB-MEN: a tool to assess risk of bias due to missing
#   evidence in network meta-analysis. BMC Med. 2021;19:304.
#   https://doi.org/10.1186/s12916-021-02166-3
#
#   Chiocchia V et al. Semi-automated assessment of the risk of bias due to
#   missing evidence in NMA: a guidance paper for the ROB-MEN web-application.
#   BMC Med Res Methodol. 2023;23:223.
#   https://doi.org/10.1186/s12874-023-02038-9
#
# OVERVIEW:
#   ROB-MEN evaluates whether MISSING studies (unpublished / selectively
#   unreported) bias each NMA estimate. Two tables:
#
#   PAIRWISE COMPARISONS TABLE — assessed per comparison:
#     Within-study assessment (Groups A & B):
#       Qualitative judgment using ROB-ME Step 2 signalling questions.
#       Output: "No bias detected" | "Some concerns" | "Suspected bias"
#
#     Across-study assessment (all groups):
#       Qualitative conditions (grey literature search, novel agent bias,
#       prior pub bias evidence) + quantitative (Egger's test if k>=10).
#       Output: "No bias detected" | "Some concerns" | "Suspected bias"
#
#     Pairwise overall judgement:
#       EITHER component = "Suspected bias" -> overall = "Suspected bias"
#       BOTH = "No bias detected" -> "No bias detected"
#       otherwise -> "Some concerns"
#       For Group C (unobserved): = across-study assessment only
#
#   ROB-MEN TABLE — assessed per NMA estimate (Table 5, Chiocchia 2021):
#     (1) % contribution from biased comparisons (auto-computed)
#     (2) Evaluation of contribution (col 3):
#           "No substantial" | "Balanced" | "Favouring one treatment"
#     (3) Small-study effects (col 7):
#           "No evidence" | "Evidence reinforcing" | "Evidence not reinforcing"
#     Final rating: Low risk | Some concerns | High risk
#
# ALGORITHM NOTE — 15 pp threshold (Chiocchia 2021):
#   "Balanced"               = substantial bias in BOTH directions (difference <=15 pp)
#   "Favouring one treatment" = bias favouring one treatment exceeds other by >15 pp
#
# LICENSE NOTE:
#   This module is an INDEPENDENT IMPLEMENTATION of the ROB-MEN methodology
#   described in the above peer-reviewed publications.
#   No source code from the ROB-MEN web application (github.com/esm-ispm-unibe-ch/rob-men,
#   licensed under GNU AGPL v3) has been copied or derived here.
#   The algorithms, decision rules, and thresholds are taken from the published
#   papers only.  If this tool is distributed as a network service, ensure
#   compliance with applicable licenses of its dependencies (shiny, netmeta,
#   meta, etc.).
# =============================================================================

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(meta)
library(netmeta)

# --------------------------------------------------------------------------
# CONSTANTS
# --------------------------------------------------------------------------
# ROBMEN_COLOURS defined in utils.R

WITHIN_CHOICES  <- c(
  "(select)" = "",
  "No bias detected",
  "Some concerns",
  "Suspected bias"
)
ACROSS_CHOICES  <- c(
  "(select)" = "",
  "No bias detected",
  "Some concerns",
  "Suspected bias",
  "Insufficient data"
)
OVERALL_PW_CHOICES <- c(
  "(auto)" = "",
  "No bias detected",
  "Some concerns",
  "Suspected bias"
)
CONTRIB_CHOICES <- c(
  "(select)"                    = "",
  "No substantial"              = "No substantial contribution from bias",
  "Substantial \u2013 balanced" = "Substantial contribution from bias \u2013 balanced",
  "Substantial \u2013 favouring one" = "Substantial contribution from bias \u2013 favouring one treatment"
)
# Chiocchia 2021: SSE direction matters for the final rating algorithm (Table 5).
# "Reinforcing biased contribution" = bias and SSE reinforce each other (High risk).
SSE_CHOICES <- c(
  "(select)"                    = "",
  "No evidence of SSE"          = "No evidence of small-study effects",
  "Evidence \u2013 reinforcing" = "Evidence of small-study effects \u2013 reinforcing biased contribution",
  "Evidence \u2013 not reinforcing" = "Evidence of small-study effects \u2013 not reinforcing biased contribution"
)
ROBMEN_CHOICES  <- c("(auto)" = "", "Low risk", "Some concerns", "High risk")

BIAS_BG <- c(
  "No bias detected"  = "#E2EFDA",
  "No bias"           = "#E2EFDA",
  "Some concerns"     = "#FFF2CC",
  "Suspected bias"    = "#FFE0E0",
  "High risk"         = "#FFE0E0",
  "Insufficient data" = "#F5F5F5",
  "(select)"          = "white"
)

# --------------------------------------------------------------------------
# HELPERS
# --------------------------------------------------------------------------

# Safe input ID: replace non-alphanumeric chars
safe_id <- function(x) gsub("[^a-zA-Z0-9]", "_", x)

# %||% / get_contrib_matrix defined in utils.R

# Pairwise overall judgement rule (Chiocchia 2021 / 2023 guidance)
# Short-circuit logic:
#   1. If within-study = "Suspected bias" → overall = "Suspected bias"
#      (across-study assessment is not needed)
#   2. Else check across-study:
#      If across = "Suspected bias" → overall = "Suspected bias"
#      Else → "No bias detected"
is_suspected <- function(r) {
  as.character(r) %in% c("Suspected bias", "High risk")  # backward compat
}
compute_overall_pw <- function(within_r, across_r) {
  if (is_suspected(within_r))  return("Suspected bias")
  if (is_suspected(across_r))  return("Suspected bias")
  "No bias detected"
}

# ROB-ME Step 2 decision logic (Page & Sterne, BMJ 2023)
# q1: "no" | "unclear" | "yes"
# q2: "no" | "unclear" | "yes"  (only relevant when q1 == "yes")
robme_derive_rating <- function(q1, q2 = "") {
  if (!nzchar(q1)) return("")
  if (q1 == "no")      return("No bias")
  if (q1 == "unclear") return("Some concerns")
  # q1 == "yes"
  if (q2 == "yes")     return("High risk")
  if (q2 == "no")      return("Some concerns")
  "Some concerns"  # q2 unclear or not yet answered
}

# Final ROB-MEN rating per NMA estimate (Chiocchia 2021, Table 5 / 2023, Table 2).
#
# Table 5 logic (mixed / only-direct estimates):
#   No substantial contribution from bias  + no SSE           -> Low risk
#   No substantial contribution from bias  + SSE present      -> Some concerns
#   Substantial (balanced)                 + no SSE           -> Low risk
#   Substantial (balanced)                 + SSE present      -> Some concerns
#   Substantial (favouring NMA result)     + no SSE           -> Some concerns
#   Substantial (favouring NMA result)     + SSE not in dir   -> Some concerns
#   Substantial (favouring NMA result)     + SSE in direction -> High risk
#
# Note: for "only indirect" estimates col 4 (indirect-evidence bias) would
# also be incorporated; reviewers can override the auto-computed rating manually.
compute_overall_robmen <- function(contrib_eval, sse_eval) {
  BALANCED <- "Substantial contribution from bias \u2013 balanced"
  FAV_ONE  <- "Substantial contribution from bias \u2013 favouring one treatment"
  FAV_NMA  <- "Substantial contribution from bias \u2013 favouring NMA result"  # backward compat
  SSE_IN   <- "Evidence of small-study effects \u2013 reinforcing biased contribution"
  SSE_IN2  <- "Small-study effects \u2013 in direction of NMA result"  # backward compat

  no_contrib <- !(contrib_eval %in% c(BALANCED, FAV_ONE, FAV_NMA))
  balanced   <- contrib_eval == BALANCED
  fav_one    <- contrib_eval %in% c(FAV_ONE, FAV_NMA)
  no_sse     <- is.na(sse_eval) || !nzchar(sse_eval) ||
                sse_eval == "No evidence of small-study effects"
  sse_in_dir <- !is.na(sse_eval) && sse_eval %in% c(SSE_IN, SSE_IN2)

  if      (no_contrib && no_sse)      "Low risk"
  else if (no_contrib && !no_sse)     "Some concerns"
  else if (balanced   && no_sse)      "Low risk"
  else if (balanced   && !no_sse)     "Some concerns"
  else if (fav_one    && !sse_in_dir) "Some concerns"
  else if (fav_one    && sse_in_dir)  "High risk"
  else                                "Some concerns"
}

# Style badge for a rating value
rating_badge <- function(val, bg_map = BIAS_BG) {
  bg <- bg_map[as.character(val)]
  if (is.na(bg)) bg <- "white"
  if (nzchar(as.character(val)) && as.character(val) != "(select)") {
    tags$span(style = paste0("background:", bg,
      "; padding:2px 6px; border-radius:3px; font-size:0.85em;"), val)
  } else NULL
}

# Build one <tr> for the Pairwise Comparisons Table
# Disabled cell helper (grey read-only text badge)
disabled_cell <- function(label, title = "") {
  tags$td(style = "padding:4px 8px;",
    tags$span(
      style = paste0("background:#e9ecef; color:#6c757d; padding:4px 8px;",
                     " border:1px solid #ced4da; border-radius:4px;",
                     " display:inline-block; font-size:0.85em;"),
      title = title, label
    )
  )
}

# Build one <tr> for the Pairwise Comparisons Table
# Groups (Chiocchia 2021):
#   Group A (is_group_a = TRUE, default): observed for this outcome — direct evidence exists
#   Group B (is_group_b = TRUE): observed for OTHER outcomes — studies exist but didn't report this outcome
#   Group C (is_group_c = TRUE): unobserved — no studies at all for this comparison
make_pw_row <- function(ns, ck, t1, t2, n_direct, across_default,
                        within_default = "", n_total = NA_integer_,
                        bg = "white", is_group_c = FALSE, is_group_b = FALSE) {
  sid <- safe_id(ck)

  # ---- "Reporting this outcome" cell: k (editable) / N (editable), auto-filled ----
  reporting_cell <- if (is_group_b || is_group_c) {
    tags$td(style = "padding:4px 8px; text-align:center; color:#6c757d;",
      tags$span(style = paste0("background:#e9ecef; padding:3px 8px;",
                               " border:1px solid #ced4da; border-radius:4px;",
                               " font-size:0.85em;"),
                "0 (—)")
    )
  } else {
    tags$td(style = "padding:4px 8px;",
      div(style = "display:flex; align-items:center; gap:3px; white-space:nowrap;",
        title = "Studies reporting this outcome: k (trials) and N (participants) — auto-filled from NMA data, editable",
        numericInput(ns(paste0("n_direct_", sid)), label = NULL,
                     value = if (!is.na(n_direct)) n_direct else NA,
                     min = 0, step = 1, width = "60px"),
        tags$span("("),
        numericInput(ns(paste0("n_total_", sid)), label = NULL,
                     value = if (!is.na(n_total)) n_total else NA,
                     min = 0, step = 1, width = "80px"),
        tags$span(")")
      )
    )
  }

  # ---- "Total identified in the SR" cell: k_sr (editable) / N_sr (editable) ----
  # Defaults to same as reporting_cell; increase if SR contains studies for this
  # comparison that did not report this outcome (those are Group B candidates).
  sr_total_cell <- tags$td(style = "padding:4px 8px;",
    div(style = "display:flex; align-items:center; gap:3px; white-space:nowrap;",
      title = "All studies in SR for this comparison (k and N), including those not reporting this outcome — auto-filled from data, editable",
      numericInput(ns(paste0("n_sr_k_", sid)), label = NULL,
                   value = if (!is_group_b && !is_group_c && !is.na(n_direct)) n_direct else NA,
                   min = 0, step = 1, width = "60px"),
      tags$span("("),
      numericInput(ns(paste0("n_sr_n_", sid)), label = NULL,
                   value = if (!is_group_b && !is_group_c && !is.na(n_total)) n_total else NA,
                   min = 0, step = 1, width = "80px"),
      tags$span(")")
    )
  )

  # ---- within-study bias cell ----
  # Group A: selectInput + ROB-ME button (full assessment)
  # Group B: selectInput only (ROB-ME Q1 is implicitly "Yes"; user assesses Q2 direction)
  # Group C: disabled "Not applicable" (no studies at all -> nothing to assess)
  within_cell <- if (is_group_c) {
    disabled_cell("Not applicable",
      title = "Group C (unobserved): no studies exist for this comparison — within-study bias does not apply")
  } else if (is_group_b) {
    tags$td(style = "padding:4px 8px;",
      selectInput(ns(paste0("within_", sid)), label = NULL,
                  choices = WITHIN_CHOICES,
                  selected = if (nzchar(within_default)) within_default else "",  # nolint
                  width = "140px")
    )
  } else {
    tags$td(style = "padding:4px 8px;",
      div(style = "display:flex; gap:4px; align-items:flex-start;",
        selectInput(ns(paste0("within_", sid)), label = NULL,
                    choices = WITHIN_CHOICES,
                    selected = if (nzchar(within_default)) within_default else "",  # nolint
                    width = "130px"),
        actionButton(ns(paste0("robme_q_", sid)),
                     label = "ROB-ME",
                     class = "btn btn-xs btn-outline-info",
                     style = "margin-top:2px; white-space:nowrap; font-size:0.75em;",
                     title = "Open ROB-ME Step 2 helper (Q1: studies missing? Q2: selective omission?)")
      )
    )
  }

  # ---- across-study bias cell ----
  # Group A: selectInput + Funnel button (Egger's test available)
  # Group B: disabled "Not applicable" (no data for this outcome -> Egger's not applicable)
  # Group C: selectInput only, qualitative assessment (no Funnel button)
  across_cell <- if (is_group_b) {
    disabled_cell("Not applicable",
      title = "Group B: no direct studies for this outcome — statistical tests (Egger's) not applicable")
  } else if (is_group_c) {
    tags$td(style = "padding:4px 8px;",
      div(
        selectInput(ns(paste0("across_", sid)), label = NULL,
                    choices = ACROSS_CHOICES,
                    selected = if (!is.null(across_default) && !is.na(across_default))
                                 across_default else "",
                    width = "130px")
      )
    )
  } else {
    tags$td(style = "padding:4px 8px;",
      div(style = "display:flex; gap:4px; align-items:flex-start;",
        selectInput(ns(paste0("across_", sid)), label = NULL,
                    choices = ACROSS_CHOICES,
                    selected = if (!is.null(across_default) && !is.na(across_default))
                                 across_default else "",
                    width = "130px"),
        actionButton(ns(paste0("funnel_btn_", sid)),
                     label = tagList(icon("chart-bar"), " Funnel"),
                     class = "btn btn-xs btn-outline-secondary",
                     style = "margin-top:2px; white-space:nowrap; font-size:0.75em;",
                     title = "Show contour-enhanced funnel plot, Egger's test, trim-and-fill")
      )
    )
  }

  # ---- Direction of bias cell ----
  # Used in pct_biased() when Egger's sign is unavailable (k < 10).
  # Values "t1"/"t2" map to the canonical alphabetical t1/t2 of this comparison.
  bias_dir_choices <- setNames(
    c("", "t1", "t2"),
    c("(unknown)",
      paste0("Favours ", t1),
      paste0("Favours ", t2))
  )
  bias_dir_cell <- if (is_group_c) {
    disabled_cell("N/A",
      title = "Group C (unobserved): no studies — direction not applicable")
  } else {
    tags$td(style = "padding:4px 6px;",
      selectInput(ns(paste0("bias_dir_", sid)), label = NULL,
                  choices = bias_dir_choices, selected = "", width = "130px")
    )
  }

  tags$tr(style = paste0("background:", bg, ";"),
    tags$td(style = "padding:4px 8px; white-space:nowrap;", strong(ck)),
    reporting_cell,
    sr_total_cell,
    within_cell,
    across_cell,
    tags$td(style = "padding:4px 8px;",
      selectInput(ns(paste0("ov_pw_", sid)), label = NULL,
                  choices = OVERALL_PW_CHOICES, selected = "", width = "150px")
    ),
    bias_dir_cell
  )
}

# Build one <tr> for the ROB-MEN Table.
make_robmen_row <- function(ns, comp, te, lo, hi, pct_t1, pct_t2,
                            nmr_te = NA_real_, nmr_lo = NA_real_, nmr_hi = NA_real_,
                            bg = "white") {
  sid <- safe_id(comp)
  te_str <- if (!is.na(te)) {
    paste0(round(te, 2), " [", round(lo, 2), ", ", round(hi, 2), "]")
  } else "—"

  # NMR treatment effect at the smallest observed variance (from netmetaregression).
  nmr_str <- if (!is.na(nmr_te)) {
    ci_part <- if (!is.na(nmr_lo) && !is.na(nmr_hi))
      paste0(" [", round(nmr_lo, 2), ", ", round(nmr_hi, 2), "]")
    else ""
    tags$span(
      title = paste0("NMR treatment effect at the smallest observed variance.",
                     " Compare to unadjusted NMA estimate to assess small-study effects."),
      style = "color:#495057;",
      paste0(round(nmr_te, 2), ci_part)
    )
  } else {
    tags$span(style = "color:#aaa; font-style:italic;", "—")
  }

  # 15 pp threshold: highlight if |pct_t1 - pct_t2| > 15 (Chiocchia 2021)
  diff_pp    <- abs(pct_t1 - pct_t2)
  thresh_hit <- !is.na(diff_pp) && diff_pp > 15
  pct_cell_style <- function(pct) {
    base <- "padding:4px 8px; text-align:center;"
    if (thresh_hit && pct == max(pct_t1, pct_t2))
      paste0(base, " background:#fff3cd; font-weight:bold;")
    else
      base
  }
  pct_badge <- function(pct) {
    val_str <- paste0(sprintf("%.1f", pct), "%")
    if (thresh_hit && pct == max(pct_t1, pct_t2))
      tagList(val_str, tags$span(title = paste0("Difference = ", round(diff_pp, 1),
        " pp > 15 pp threshold (Chiocchia 2021): may be 'Substantial contribution'"),
        style = "margin-left:4px; color:#856404;", HTML("&#9888;")))
    else
      val_str
  }

  tags$tr(style = paste0("background:", bg, ";"),
    tags$td(style = "padding:4px 8px; white-space:nowrap;", strong(comp)),
    tags$td(style = pct_cell_style(pct_t1), pct_badge(pct_t1)),
    tags$td(style = pct_cell_style(pct_t2), pct_badge(pct_t2)),
    tags$td(style = "padding:4px 6px;",
      selectInput(ns(paste0("contrib_eval_", sid)), label = NULL,
                  choices = CONTRIB_CHOICES, selected = "", width = "160px")
    ),
    tags$td(style = "padding:4px 8px; text-align:center; white-space:nowrap;", te_str),
    tags$td(style = "padding:4px 8px; text-align:center; white-space:nowrap;", nmr_str),
    tags$td(style = "padding:4px 6px;",
      selectInput(ns(paste0("sse_eval_", sid)), label = NULL,
                  choices = SSE_CHOICES, selected = "", width = "160px")
    ),
    tags$td(style = "padding:4px 6px;",
      selectInput(ns(paste0("ov_robmen_", sid)), label = NULL,
                  choices = ROBMEN_CHOICES, selected = "", width = "120px")
    )
  )
}

# =============================================================================
# UI FUNCTION
# =============================================================================
moduleC_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Custom JS: enable/disable across-study cells reactively (no shinyjs needed)
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('robmen_toggle_across', function(msg) {
        var sel = document.getElementById(msg.id);
        if (!sel) return;
        var td = sel.closest('td') || sel.parentElement;
        if (msg.disabled) {
          sel.disabled = true;
          td.style.opacity = '0.38';
          td.title = 'Not needed: within-study bias is already Suspected bias';
        } else {
          sel.disabled = false;
          td.style.opacity = '1';
          td.title = '';
        }
      });
    "))),
    h3("Step 3: ROB-MEN Analysis"),
    p("Assesses risk of bias due to", strong("missing evidence"), "in NMA (Chiocchia et al.",
      tags$a("BMC Med 2021", href = "https://doi.org/10.1186/s12916-021-02166-3",
             target = "_blank"), ")."),
    tags$details(
      tags$summary(style = "cursor:pointer; font-size:0.9em; color:#444; margin-bottom:6px;",
                   HTML("&#9432; Group classification &amp; algorithm overview (click to expand)")),
      div(style = "font-size:0.9em; color:#444; margin-bottom:8px;",
        p(style = "margin-bottom:4px;", "Comparisons are classified into three groups:"),
        tags$ul(
          tags$li(strong("Group A (Observed):"), " has direct evidence for this outcome → both Components assessed"),
          tags$li(strong("Group B (Other outcomes):"), " studies exist but did NOT report this outcome → Component 1 only (within-study selective non-reporting)"),
          tags$li(strong("Group C (Unobserved):"), " no studies at all → Component 2 qualitative only")
        )
      ),
      div(style = paste0("background:#f8f9fa; border:1px solid #dee2e6; border-radius:6px;",
                         " padding:10px 14px; margin-bottom:12px; font-size:0.87em;"),
        strong("Algorithm flow (Chiocchia et al. 2021):"),
        div(style = "display:flex; flex-wrap:wrap; align-items:center; gap:6px; margin-top:8px;",
          tags$span(style = "background:#6c3483; color:white; padding:3px 8px; border-radius:3px;",
                    HTML("① Component 1<br><small>Within-study bias<br>(selective non-reporting)</small>")),
          tags$span(style = "color:#6c3483; font-size:1.2em; font-weight:bold;", "+"),
          tags$span(style = "background:#6c3483; color:white; padding:3px 8px; border-radius:3px;",
                    HTML("② Component 2<br><small>Across-study bias<br>(small-study effects)</small>")),
          tags$span(style = "color:#6c3483; font-size:1.2em; font-weight:bold;", "→"),
          tags$span(style = "background:#6c3483; color:white; padding:3px 8px; border-radius:3px;",
                    HTML("③ Pairwise overall<br><small>judgement per comparison<br>(per Group)</small>")),
          tags$span(style = "color:#6c3483; font-size:1.2em; font-weight:bold;", "→"),
          tags$span(style = "background:#4a235a; color:white; padding:3px 8px; border-radius:3px;",
                    HTML("④ % Biased contribution<br><small>via contribution matrix<br>(15 pp threshold)</small>")),
          tags$span(style = "color:#6c3483; font-size:1.2em; font-weight:bold;", "→"),
          tags$span(style = "background:#4a235a; color:white; padding:3px 8px; border-radius:3px;",
                    HTML("⑤ ROB-MEN final rating<br><small>Low risk / Some concerns<br>/ High risk</small>"))
        ),
        tags$br(),
        div(style = "color:#666; margin-top:4px;",
          HTML("① ② ③: <b>Tab 1 — Pairwise Assessment</b>&emsp;|&emsp;④ ⑤: <b>Tab 2 — ROB-MEN Final Rating</b>"),
          tags$br(),
          tags$small(
            "Tab 1 pairwise: \u2460 OR \u2461 = Suspected bias \u2192 ",
            em("Suspected bias"),
            " | Both = No bias detected \u2192 ", em("No bias detected"),
            " | all others \u2192 ", em("Some concerns"),
            HTML(" &mdash; "),
            "Tab 2 (Table 5): No contribution \u2192 Low | Balanced+no small-study effects \u2192 Low | ",
            "Favouring one treatment+small-study effects reinforcing \u2192 High | otherwise \u2192 Some concerns"
          )
        )
      )
    ),

    p(style = "font-size:0.9em; color:#555; margin-bottom:8px;",
      icon("info-circle"), " ROB-MEN analysis runs automatically after CINeMA completes.",
      " Results appear below once the contribution matrix is available.",
      tags$br(),
      tags$small("Mark indirect comparisons as Group B in Tab 1 if you know",
                 " studies exist for that comparison but did not report this outcome.")),

    hr(),
    uiOutput(ns("robmen_main_ui"))
  )
}

# =============================================================================
# SERVER FUNCTION
# =============================================================================
moduleC_server <- function(id, processed_data, cinema_module,
                           nma_settings = NULL, run_trigger = NULL,
                           go_to_cinema = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Data accessors
    # ------------------------------------------------------------------
    pairwise_data <- reactive({
      res <- processed_data()
      req(!is.null(res), !is.null(res$data))
      res$data
    })

    cinema_res <- reactive({
      cr <- cinema_module$cinema_results()
      validate(need(!is.null(cr),
        "Please run CINeMA analysis in Module B first."))
      cr
    })

    nma_net <- reactive({ cinema_res()$net })

    # ------------------------------------------------------------------
    # Auto-trigger: fire Egger's test when the NMA net object changes.
    # We track only the net structure (treatments + study labels), NOT the
    # full cinema_results() list.  This prevents re-triggering when only
    # Domain 2 ratings change (e.g. after set_robmen / ROB-MEN sync),
    # which would cause robmen_main_ui to re-render and reset the active tab.
    # ------------------------------------------------------------------
    auto_egger_trigger <- reactiveVal(0)
    last_net_key       <- reactiveVal(NULL)

    observeEvent(cinema_res(), {
      cr <- tryCatch(cinema_res(), error = function(e) NULL)
      if (is.null(cr) || is.null(cr$net)) return()
      # Fingerprint the net structure — changes only when NMA is re-run
      new_key <- list(
        trts     = sort(cr$net$trts),
        studlab  = sort(cr$net$studlab),
        sm       = cr$net$sm,
        n_studies = cr$net$k
      )
      if (!identical(new_key, last_net_key())) {
        last_net_key(new_key)
        df <- tryCatch(pairwise_data(), error = function(e) NULL)
        if (!is.null(df) && nrow(df) > 0) {
          auto_egger_trigger(auto_egger_trigger() + 1)
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ------------------------------------------------------------------
    # All pairwise comparisons
    # ------------------------------------------------------------------
    direct_comps_df <- reactive({
      df <- pairwise_data()
      df %>%
        mutate(
          t1c      = pmin(t1, t2),
          t2c      = pmax(t1, t2),
          comp_key = paste(t1c, t2c, sep = ":")
        ) %>%
        group_by(comp_key) %>%
        summarise(t1 = first(t1c), t2 = first(t2c),
                  n_direct = n(),
                  n_total  = if (all(is.na(n))) NA_integer_
                             else as.integer(sum(n, na.rm = TRUE)),
                  .groups = "drop") %>%
        arrange(comp_key)
    })

    indirect_comps_df <- reactive({
      net      <- nma_net()
      trts     <- sort(net$trts)
      dir_keys <- direct_comps_df()$comp_key
      expand.grid(t1 = trts, t2 = trts, stringsAsFactors = FALSE) %>%
        filter(t1 < t2) %>%
        mutate(comp_key = paste(t1, t2, sep = ":"),
               n_direct = 0L) %>%
        filter(!comp_key %in% dir_keys) %>%
        arrange(comp_key)
    })

    # ------------------------------------------------------------------
    # Group B/C classification (Chiocchia 2021):
    #   group_b_keys_rv: comparisons "observed for other outcomes"
    #     — studies for these comparisons exist in the SR but did NOT
    #       report the current outcome of interest.
    #     — within-study bias CAN be assessed (ROB-ME Q1 = Yes by definition)
    #     — across-study bias: not applicable (no outcome data → no Egger's)
    #   group_c: everything else in indirect_comps_df = "truly unobserved"
    #     — no studies at all for this comparison
    #     — only qualitative across-study judgment applies
    # ------------------------------------------------------------------
    group_b_keys_rv <- reactiveVal(character(0))

    group_b_comps_df <- reactive({
      ic   <- tryCatch(indirect_comps_df(), error = function(e) NULL)
      if (is.null(ic)) return(data.frame(comp_key=character(), t1=character(),
                                          t2=character(), n_direct=integer(),
                                          stringsAsFactors=FALSE))
      keys <- group_b_keys_rv()
      ic %>% filter(comp_key %in% keys)
    })

    group_c_comps_df <- reactive({
      ic   <- tryCatch(indirect_comps_df(), error = function(e) NULL)
      if (is.null(ic)) return(data.frame(comp_key=character(), t1=character(),
                                          t2=character(), n_direct=integer(),
                                          stringsAsFactors=FALSE))
      keys <- group_b_keys_rv()
      ic %>% filter(!comp_key %in% keys)
    })

    # Toggle a comparison between Group B and Group C
    grpb_observers_created <- reactiveVal(FALSE)
    observe({
      ic <- tryCatch(indirect_comps_df(), error = function(e) NULL)
      if (is.null(ic) || grpb_observers_created()) return()
      grpb_observers_created(TRUE)
      for (i in seq_len(nrow(ic))) {
        local({
          ck_i  <- ic$comp_key[i]
          sid_i <- safe_id(ck_i)
          observeEvent(input[[paste0("grpb_toggle_", sid_i)]], {
            keys <- group_b_keys_rv()
            if (ck_i %in% keys) group_b_keys_rv(setdiff(keys, ck_i))
            else                group_b_keys_rv(c(keys, ck_i))
          }, ignoreNULL = TRUE, ignoreInit = TRUE)
        })
      }
    })

    # ------------------------------------------------------------------
    # Egger's test (button-triggered OR auto-triggered after CINeMA)
    # ------------------------------------------------------------------
    run_egger_counter <- reactiveVal(0)
    observeEvent(auto_egger_trigger(), {
      run_egger_counter(run_egger_counter() + 1)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    egger_df <- eventReactive(run_egger_counter(), {
      df    <- pairwise_data()
      comps <- direct_comps_df()

      # Minimum SE across the full dataset (for NMR prediction)
      v_min  <- min(df$se^2, na.rm = TRUE)
      se_min <- sqrt(v_min)

      results <- lapply(seq_len(nrow(comps)), function(i) {
        sub <- df %>%
          filter((t1 == comps$t1[i] & t2 == comps$t2[i]) |
                 (t1 == comps$t2[i] & t2 == comps$t1[i]))
        n_s <- nrow(sub)

        if (n_s < 2) {
          return(data.frame(
            comp_key    = comps$comp_key[i],
            n_s         = n_s,
            n_few       = TRUE,
            egger_p     = NA_real_,
            egger_bias  = NA_real_,
            egger_eff   = NA_real_,
            nmr_te      = NA_real_,
            nmr_lo      = NA_real_,
            nmr_hi      = NA_real_,
            across_auto = "Insufficient data",
            stringsAsFactors = FALSE
          ))
        }
        # Chiocchia 2021: Egger's test recommended only when ≥ 10 studies.
        # Flag comparisons with 2–9 studies so the user is warned.
        n_few_flag <- (n_s < 10)

        # Normalise y to canonical direction: positive = t2c (alphabetically larger)
        sub <- sub %>%
          mutate(y = ifelse(t1 == comps$t1[i] & t2 == comps$t2[i], y, -y))

        eg <- tryCatch({
          # Egger's weighted regression (linreg):
          #   y_i/se_i = bias + eff * (1/se_i)
          # where bias = intercept (asymmetry), eff = effect at SE=0.
          # NMR_TE at se_min = eff + bias * se_min  (linear combination of coefs).
          # 95% CI via vcov of lm fit:
          #   Var(NMR_TE) = Var(eff) + se_min^2*Var(bias) + 2*se_min*Cov(eff,bias)
          y_std <- sub$y / sub$se
          x_std <- 1    / sub$se
          fit   <- lm(y_std ~ x_std)
          vcv   <- vcov(fit)
          b_int <- coef(fit)[1]   # bias (intercept)
          b_eff <- coef(fit)[2]   # eff  (slope = TE at SE=0)

          nmr    <- b_eff + b_int * se_min
          var_nm <- vcv[2,2] + se_min^2 * vcv[1,1] + 2 * se_min * vcv[1,2]
          se_nm  <- if (!is.na(var_nm) && var_nm > 0) sqrt(var_nm) else NA_real_
          t_crit <- qt(0.975, df = n_s - 2)
          nmr_lo <- if (!is.na(se_nm)) nmr - t_crit * se_nm else NA_real_
          nmr_hi <- if (!is.na(se_nm)) nmr + t_crit * se_nm else NA_real_

          # Egger p-value via metabias (for across_auto rating)
          m  <- metagen(TE = sub$y, seTE = sub$se, studlab = sub$studlab,
                        sm = "MD", common = FALSE, random = TRUE)
          mb <- metabias(m, method = "linreg")
          p_v <- {
            pv <- mb$p.value
            if (is.null(pv) || length(pv) == 0) NA_real_ else as.numeric(pv)[1]
          }

          list(p = p_v, bias = b_int, eff = b_eff,
               nmr_te = nmr, nmr_lo = nmr_lo, nmr_hi = nmr_hi)
        }, error = function(e) {
          list(p = NA_real_, bias = NA_real_, eff = NA_real_,
               nmr_te = NA_real_, nmr_lo = NA_real_, nmr_hi = NA_real_)
        })

        across_auto <- if (length(eg$p) == 0 || is.na(eg$p)) "Insufficient data"
        else if (eg$p >= 0.10) "No bias detected"
        else if (eg$p >= 0.05) "Some concerns"
        else                   "Suspected bias"

        data.frame(
          comp_key    = comps$comp_key[i],
          n_s         = n_s,
          n_few       = n_few_flag,
          egger_p     = eg$p,
          egger_bias  = eg$bias,
          egger_eff   = eg$eff,
          nmr_te      = eg$nmr_te,
          nmr_lo      = eg$nmr_lo,
          nmr_hi      = eg$nmr_hi,
          across_auto = across_auto,
          stringsAsFactors = FALSE
        )
      })
      bind_rows(results)
    })

    # ------------------------------------------------------------------
    # NOTE: within-study bias (Component 1) is NOT auto-populated.
    # Chiocchia 2021: Component 1 assesses SELECTIVE NON-REPORTING OF
    # OUTCOMES — whether any studies identified in the SR failed to report
    # the outcome of interest, possibly because of direction/magnitude/p-value.
    # This requires a qualitative judgment using ROB-ME Step 2
    # (Page & Sterne, BMJ 2023); it cannot be proxied from RoB 2 scores.
    #   - NOT RoB 2 Domain 3 (missing participant data — dropouts)
    #   - NOT RoB 2 Domain 5 (selection of reported result — within-study)
    # Default for Group A/B: "Not assessed" (user must click ROB-ME button
    # or select rating directly).
    # ------------------------------------------------------------------

    # ------------------------------------------------------------------
    # ROB-ME Step 2 Helper: per-comparison modal with Q1 + Q2
    # Decision logic (Page & Sterne, BMJ 2023):
    #   Q1=No            → No bias
    #   Q1=Unclear       → Some concerns
    #   Q1=Yes + Q2=No   → Some concerns
    #   Q1=Yes + Q2=Yes  → High risk
    #   Q1=Yes + Q2=?    → Some concerns
    # ------------------------------------------------------------------
    robme_observers_created <- reactiveVal(FALSE)

    observe({
      dc <- tryCatch(direct_comps_df(), error = function(e) NULL)
      if (is.null(dc) || robme_observers_created()) return()
      robme_observers_created(TRUE)

      for (i in seq_len(nrow(dc))) {
        local({
          ck_i  <- dc$comp_key[i]
          sid_i <- safe_id(ck_i)
          t1_i  <- dc$t1[i]   # capture t1/t2 in local scope to avoid closure bug
          t2_i  <- dc$t2[i]

          # Show modal on ROB-ME button click
          observeEvent(input[[paste0("robme_q_", sid_i)]], {
            showModal(modalDialog(
              title = tagList(icon("clipboard-check"),
                              paste(" ROB-ME Step 2:", ck_i)),
              size  = "l",
              easyClose = TRUE,

              div(class = "alert alert-info",
                  style = "font-size:0.9em;",
                  strong("ROB-ME Step 2 — Within-study bias (Selective non-reporting)"),
                  br(),
                  "Page MJ, Sterne JAC et al. BMJ 2023. Assesses whether any eligible",
                  " study failed to report the outcome of interest, likely because of",
                  " the direction, magnitude or statistical significance of the result."),

              # Forest plot for reference
              h5(style = "margin-top:12px;",
                 icon("chart-bar"), " Forest plot — studies included in this comparison"),
              p(style = "color:#666; font-size:0.85em; margin-bottom:4px;",
                "Use this to check which studies reported the outcome.",
                " Studies absent from your SR screen but eligible may indicate selective non-reporting."),
              plotlyOutput(ns(paste0("robme_forest_", sid_i)), height = "280px"),

              hr(),

              # Warning about missing outcome studies
              uiOutput(ns(paste0("robme_missing_warn_", sid_i))),

              selectInput(
                ns(paste0("robme_q1_", sid_i)),
                label = tagList(
                  strong("Q1:"),
                  " Were there any eligible studies for this comparison that were",
                  " identified in the systematic review but did",
                  strong(" NOT"), " report results for the target outcome?"),
                choices = c(
                  "(select)" = "",
                  "No — all identified studies reported the outcome" = "no",
                  "Unclear — uncertain whether any studies are missing" = "unclear",
                  "Yes — at least one identified study did not report results" = "yes"
                ),
                selected = ""
              ),

              uiOutput(ns(paste0("robme_q2_ui_", sid_i))),

              hr(),
              uiOutput(ns(paste0("robme_preview_", sid_i))),

              footer = tagList(
                actionButton(ns(paste0("robme_apply_", sid_i)),
                             "Apply to Within-study column",
                             class = "btn btn-primary",
                             icon  = icon("check")),
                modalButton("Cancel")
              )
            ))
          }, ignoreNULL = TRUE, ignoreInit = TRUE)

          # Forest plot inside ROB-ME modal
          output[[paste0("robme_forest_", sid_i)]] <- renderPlotly({
            df_pw <- tryCatch(pairwise_data(), error = function(e) NULL)
            req(!is.null(df_pw))

            sub <- df_pw %>%
              filter((t1 == t1_i & t2 == t2_i) |
                     (t1 == t2_i & t2 == t1_i)) %>%
              mutate(
                # Canonicalise direction: positive = t2_i (alphabetically larger)
                y_can = ifelse(t1 == t1_i & t2 == t2_i, y, -y),
                ci_lo = y_can - 1.96 * se,
                ci_hi = y_can + 1.96 * se,
                rob   = as.character(rob)
              ) %>%
              arrange(y_can)

            validate(need(nrow(sub) >= 1,
                          "No studies found for this comparison."))

            # Pooled estimate (inverse-variance weighted)
            w      <- 1 / sub$se^2
            pool   <- sum(sub$y_can * w) / sum(w)
            pool_se <- sqrt(1 / sum(w))
            pool_lo <- pool - 1.96 * pool_se
            pool_hi <- pool + 1.96 * pool_se

            rob_col <- c("low" = "#5cb85c", "some concerns" = "#f0ad4e",
                         "high" = "#d9534f")

            # Study rows (include n if available)
            has_n <- "n" %in% names(sub) && any(!is.na(sub$n))
            study_df <- sub %>%
              mutate(
                y_pos   = seq_len(n()),
                col     = rob_col[rob],
                col     = ifelse(is.na(col), "#888888", col),
                label   = if (has_n) paste0(studlab, " (n=", n, ")") else studlab,
                tip     = paste0(studlab,
                                 if (has_n) paste0(" (n=", n, ")") else "",
                                 "<br>Effect: ", round(y_can, 3),
                                 " [", round(ci_lo, 3), ", ", round(ci_hi, 3), "]",
                                 "<br>ROB: ", rob)
              )

            pool_y <- nrow(sub) + 1.5

            p <- plot_ly() %>%
              # CI lines (one segment per study)
              add_segments(data = study_df,
                           x = ~ci_lo, xend = ~ci_hi,
                           y = ~y_pos, yend = ~y_pos,
                           color = I("grey50"),
                           line  = list(width = 2),
                           hoverinfo = "none",
                           showlegend = FALSE) %>%
              # Point estimates coloured by ROB
              add_markers(data = study_df,
                          x = ~y_can, y = ~y_pos,
                          color = ~rob,
                          colors = c("low"           = "#5cb85c",
                                     "some concerns" = "#f0ad4e",
                                     "high"          = "#d9534f"),
                          marker = list(size = 9),
                          text   = ~tip, hoverinfo = "text") %>%
              # Pooled: diamond
              add_polygons(
                x = c(pool_lo, pool, pool_hi, pool,  pool_lo),
                y = c(pool_y,  pool_y + 0.4, pool_y, pool_y - 0.4, pool_y),
                fillcolor = "steelblue",
                line = list(color = "steelblue"),
                hoverinfo = "text",
                text = paste0("Pooled: ", round(pool, 3),
                              " [", round(pool_lo, 3), ", ", round(pool_hi, 3), "]"),
                showlegend = FALSE
              ) %>%
              # Null line
              add_segments(x = 0, xend = 0,
                           y = 0, yend = pool_y + 1,
                           line = list(color = "grey", dash = "dot", width = 1),
                           hoverinfo = "none", showlegend = FALSE) %>%
              layout(
                xaxis = list(title = paste0("Effect size (", ck_i, ")"),
                             zeroline = FALSE),
                yaxis = list(
                  tickvals = c(study_df$y_pos, pool_y),
                  ticktext = c(study_df$label, "Pooled"),
                  autorange = "reversed",
                  title = ""
                ),
                margin = list(l = 120, r = 20, t = 10, b = 40),
                plot_bgcolor  = "white",
                paper_bgcolor = "white"
              )

            p
          })

          # Missing outcome studies warning
          output[[paste0("robme_missing_warn_", sid_i)]] <- renderUI({
            df_pw <- tryCatch(pairwise_data(), error = function(e) NULL)
            if (is.null(df_pw)) return(NULL)

            n_reported <- df_pw %>%
              filter((t1 == t1_i & t2 == t2_i) |
                     (t1 == t2_i & t2 == t1_i)) %>%
              pull(studlab) %>%
              unique() %>%
              length()

            div(
              class = "alert alert-warning",
              style = "font-size:0.87em; margin-bottom:10px;",
              strong(icon("exclamation-triangle"), " Note on Q1:"),
              br(),
              paste0("This dataset contains ", n_reported,
                     " study/studies that reported outcomes for this comparison."),
              br(),
              "Q1 asks about studies identified in your ",
              strong("systematic review search"), " that did ",
              strong("NOT"), " report this outcome at all. ",
              "These studies are ", strong("not"), " in the dataset above — ",
              "they would have been excluded at screening (e.g., PRISMA flow: 'outcome not reported').",
              br(),
              "If your PRISMA flow shows studies excluded for 'outcome not reported', ",
              "those are the missing studies Q1 is referring to.",
              br(),
              span(style = "color:#856404;",
                   strong("Please check your PRISMA flow / screening records before answering 'No'."))
            )
          })

          # Q2: shown only when Q1 = "yes"
          output[[paste0("robme_q2_ui_", sid_i)]] <- renderUI({
            q1 <- input[[paste0("robme_q1_", sid_i)]]
            if (is.null(q1) || q1 != "yes") return(NULL)
            selectInput(
              ns(paste0("robme_q2_", sid_i)),
              label = tagList(
                strong("Q2:"),
                " Is there a clear indication that results are missing because of",
                " their", strong(" direction, magnitude or statistical significance"),
                " (p-value)?"),
              choices = c(
                "(select)" = "",
                "No / Probably no — missing for other reasons" = "no",
                "Unclear"                                       = "unclear",
                "Yes / Probably yes — results omitted due to their value" = "yes"
              ),
              selected = ""
            )
          })

          # Preview derived rating
          output[[paste0("robme_preview_", sid_i)]] <- renderUI({
            q1  <- input[[paste0("robme_q1_", sid_i)]] %||% ""
            q2  <- input[[paste0("robme_q2_", sid_i)]] %||% ""
            if (!nzchar(q1)) return(NULL)
            rating <- robme_derive_rating(q1, q2)
            bg <- c("No bias" = "#d4edda", "Some concerns" = "#fff3cd",
                    "High risk" = "#f8d7da")[rating]
            if (is.na(bg)) bg <- "white"
            div(style = paste0("padding:8px 12px; background:", bg,
                               "; border-radius:4px;"),
                strong("Derived rating: "), strong(rating))
          })

          # Apply button: fill within_ and close modal
          observeEvent(input[[paste0("robme_apply_", sid_i)]], {
            q1     <- input[[paste0("robme_q1_", sid_i)]] %||% ""
            q2     <- input[[paste0("robme_q2_", sid_i)]] %||% ""
            rating <- robme_derive_rating(q1, q2)
            if (nzchar(rating)) {
              # Map legacy robme_derive_rating labels to new Chiocchia 2021 terms
              rating_map <- c(
                "No bias"       = "No bias detected",
                "Some concerns" = "Some concerns",
                "High risk"     = "Suspected bias"
              )
              mapped_rating <- rating_map[rating]
              if (is.na(mapped_rating)) mapped_rating <- rating
              updateSelectInput(session, paste0("within_", sid_i), selected = mapped_rating)
            }
            removeModal()
          }, ignoreNULL = TRUE, ignoreInit = TRUE)

          # Funnel plot button — showModal (same pattern as ROB-ME)
          observeEvent(input[[paste0("funnel_btn_", sid_i)]], {
            showModal(modalDialog(
              title    = tagList(icon("chart-bar"),
                                 paste(" Contour-enhanced funnel plot & Egger's test:", ck_i)),
              size     = "l",
              easyClose = TRUE,
              uiOutput(ns(paste0("modal_egger_", sid_i))),
              plotlyOutput(ns(paste0("modal_funnel_", sid_i)), height = "420px"),
              uiOutput(ns(paste0("modal_trimfill_note_", sid_i))),
              hr(),
              div(style = "font-size:0.85em; color:#444;",
                tags$b("Conditions suggesting bias:"),
                tags$ul(style = "margin-bottom:6px;",
                  tags$li("Failure to search grey literature / unpublished studies"),
                  tags$li("Analysis based on few early trials of a novel agent",
                          " (early evidence often overestimates efficacy)"),
                  tags$li("Previous evidence showed publication bias for this comparison"),
                  tags$li(HTML("Egger's p &lt; 0.05 with \u2265 10 studies (suggests funnel asymmetry)"))
                ),
                tags$b("Conditions suggesting no bias:"),
                tags$ul(style = "margin-bottom:0;",
                  tags$li("Unpublished studies available and consistent with published results"),
                  tags$li("Tradition of prospective trial registration in this field"),
                  tags$li("Funnel plot symmetric with no statistically significant Egger test")
                )
              ),
              footer   = modalButton("Close")
            ))
          }, ignoreNULL = TRUE, ignoreInit = TRUE)
        })
      }
    })

    # ------------------------------------------------------------------
    # NMA estimates: reference vs all other treatments
    # ------------------------------------------------------------------
    nma_estimates <- reactive({
      net <- nma_net()
      cr  <- cinema_res()
      mt  <- cr$model_type

      # Use ALL pairwise comparisons from CINeMA (alphabetical t1 < t2 ordering),
      # not just ref vs others, so ROB-MEN covers every comparison in Domain 2.
      merged <- cr$merged
      if (!is.null(merged) && nrow(merged) > 0) {
        comp_labels <- merged$comparison  # "T1 vs T2" canonical labels from CINeMA
      } else {
        ref  <- net$reference.group
        trts <- setdiff(sort(net$trts), ref)
        comp_labels <- paste(ref, trts, sep = " vs ")
      }

      lapply(comp_labels, function(comp) {
        parts <- strsplit(comp, " vs ", fixed = TRUE)[[1]]
        if (length(parts) != 2) return(NULL)
        t1 <- parts[1]; t2 <- parts[2]
        if (mt == "random") {
          te <- tryCatch(net$TE.random[t1, t2],    error = function(e) NA_real_)
          lo <- tryCatch(net$lower.random[t1, t2], error = function(e) NA_real_)
          hi <- tryCatch(net$upper.random[t1, t2], error = function(e) NA_real_)
        } else {
          te <- tryCatch(net$TE.common[t1, t2],    error = function(e) NA_real_)
          lo <- tryCatch(net$lower.common[t1, t2], error = function(e) NA_real_)
          hi <- tryCatch(net$upper.common[t1, t2], error = function(e) NA_real_)
        }
        ev <- "indirect"
        if (!is.null(cr$nsplit)) {
          lbls <- cr$nsplit$comparison
          if (any(lbls == paste(t1, t2, sep = ":") |
                  lbls == paste(t2, t1, sep = ":"))) ev <- "mixed"
        }
        data.frame(comparison = comp,
                   t1 = t1, t2 = t2, te = te, lo = lo, hi = hi,
                   evidence_type = ev, stringsAsFactors = FALSE)
      }) %>% bind_rows()
    })

    # ------------------------------------------------------------------
    # % contribution from biased comparisons (reactive — updates on input change)
    # Algorithm: sum contributions from comparisons rated "Suspected substantial bias"
    #   Direction (Chiocchia 2021):
    #     Egger bias > 0 → small studies favour the active treatment (t2 of NMA comparison)
    #     Threshold: |pct_t1 - pct_t2| > 15 pp → Substantial contribution
    # ------------------------------------------------------------------
    pct_biased <- reactive({
      ne  <- tryCatch(nma_estimates(), error = function(e) NULL)
      eg  <- tryCatch(egger_df(),      error = function(e) NULL)
      cr  <- tryCatch(cinema_res(),    error = function(e) NULL)
      if (is.null(ne) || is.null(eg)) {
        return(data.frame(comparison = character(0),
                          pct_fav_t1 = numeric(0),
                          pct_fav_t2 = numeric(0)))
      }
      cm <- get_contrib_matrix(cr$contrib)

      lapply(seq_len(nrow(ne)), function(i) {
        pct_t1 <- 0; pct_t2 <- 0

        if (!is.null(cm)) {
          key1    <- paste(ne$t1[i], ne$t2[i], sep = ":")
          key2    <- paste(ne$t2[i], ne$t1[i], sep = ":")
          row_idx <- which(rownames(cm) %in% c(key1, key2))

          if (length(row_idx) > 0) {
            contribs <- cm[row_idx[1], ]
            contribs <- contribs[contribs > 0.001]

            for (col_nm in names(contribs)) {
              parts <- strsplit(col_nm, ":")[[1]]
              if (length(parts) != 2) next
              ck  <- paste(pmin(parts[1], parts[2]),
                           pmax(parts[1], parts[2]), sep = ":")
              sid <- safe_id(ck)

              # Get pairwise overall judgement
              ov_val <- input[[paste0("ov_pw_", sid)]]
              if (is.null(ov_val) || !nzchar(ov_val)) {
                # Auto-compute from within/across inputs
                w_val <- input[[paste0("within_", sid)]]
                a_val <- input[[paste0("across_", sid)]]
                if (is.null(w_val) || !nzchar(w_val)) w_val <- "No bias detected"
                if (is.null(a_val) || !nzchar(a_val)) {
                  eg_row <- eg %>% filter(comp_key == ck)
                  a_val  <- if (nrow(eg_row) > 0 && !is.na(eg_row$across_auto[1]))
                              eg_row$across_auto[1] else "Insufficient data"
                }
                ov_val <- compute_overall_pw(w_val, a_val)
              }

              if (!is.null(ov_val) && ov_val %in% c("Suspected bias", "Suspected substantial bias")) {
                pct  <- contribs[col_nm] * 100
                # Direction (Chiocchia 2021):
                # Egger bias computed on y canonicalised to positive = parts[2] is better.
                # Positive Egger bias → small studies favour parts[2].
                # Check whether parts[2] aligns with ne$t2[i] (trt) or ne$t1[i] (ref).
                eg_row   <- eg %>% filter(comp_key == ck)
                bias_sgn <- if (nrow(eg_row) > 0 && !is.na(eg_row$egger_bias[1]))
                              sign(eg_row$egger_bias[1]) else 0L
                # Fallback: use manually entered direction when Egger's sign is unavailable
                if (bias_sgn == 0) {
                  dir_manual <- input[[paste0("bias_dir_", sid)]]
                  if (!is.null(dir_manual) && nzchar(dir_manual)) {
                    # "t1" = parts[1] favored; "t2" = parts[2] favored
                    # Consistent with Egger sign: positive → parts[2] favored
                    bias_sgn <- if (dir_manual == "t2") 1L else -1L
                  }
                }
                if (bias_sgn != 0) {
                  favored <- if (bias_sgn > 0) parts[2] else parts[1]
                  if (favored == ne$t2[i]) pct_t2 <- pct_t2 + pct
                  else                     pct_t1 <- pct_t1 + pct
                }
              }
            }
          }
        }

        data.frame(comparison = ne$comparison[i],
                   pct_fav_t1 = round(pct_t1, 1),
                   pct_fav_t2 = round(pct_t2, 1),
                   stringsAsFactors = FALSE)
      }) %>% bind_rows()
    })

    # ------------------------------------------------------------------
    # Network-level SSE assessment via NMR (Chiocchia 2023, §2.4)
    # Uses netmetaregression(net, var.covar = "seTE") to estimate adjusted TE.
    # If adjusted 95% CI overlaps unadjusted CI → no evidence of SSE.
    # Direction: NMA favours t2 (te > 0) AND adjusted < unadjusted → SSE in direction.
    # Returns data.frame: comparison, nmr_te, nmr_lo, nmr_hi, nma_te, sse_auto
    # ------------------------------------------------------------------
    network_sse_df <- reactive({
      net <- tryCatch(nma_net(),      error = function(e) NULL)
      cr  <- tryCatch(cinema_res(),   error = function(e) NULL)
      ne  <- tryCatch(nma_estimates(), error = function(e) NULL)
      if (is.null(net) || is.null(cr) || is.null(ne) || nrow(ne) == 0) return(NULL)
      tryCatch({
        nmr_result <- tryCatch(
          netmetaregression(net, var.covar = "seTE"),
          error = function(e)
            tryCatch(netmetaregression(net, var.covar = "se"),
                     error = function(e2) NULL)
        )
        if (is.null(nmr_result)) return(NULL)

        mt <- cr$model_type

        # SSE labels matching SSE_CHOICES exactly
        SSE_IN  <- "Evidence of small-study effects \u2013 reinforcing biased contribution"
        SSE_NOT <- "Evidence of small-study effects \u2013 not reinforcing biased contribution"

        lapply(seq_len(nrow(ne)), function(i) {
          t1 <- ne$t1[i]; t2 <- ne$t2[i]
          comp   <- ne$comparison[i]
          nmr_te <- nmr_lo <- nmr_hi <- NA_real_
          nma_te <- nma_lo <- nma_hi <- NA_real_
          tryCatch({
            if (mt == "random") {
              nmr_te <- nmr_result$TE.random[t1, t2]
              nmr_lo <- nmr_result$lower.random[t1, t2]
              nmr_hi <- nmr_result$upper.random[t1, t2]
              nma_te <- net$TE.random[t1, t2]
              nma_lo <- net$lower.random[t1, t2]
              nma_hi <- net$upper.random[t1, t2]
            } else {
              nmr_te <- nmr_result$TE.fixed[t1, t2]
              nmr_lo <- nmr_result$lower.fixed[t1, t2]
              nmr_hi <- nmr_result$upper.fixed[t1, t2]
              nma_te <- net$TE.fixed[t1, t2]
              nma_lo <- net$lower.fixed[t1, t2]
              nma_hi <- net$upper.fixed[t1, t2]
            }
          }, error = function(e) {})

          # CI overlap test requires all four bounds; if NMR CI is unavailable
          # (e.g. tau²=0 boundary, insufficient df), fall back to "No evidence".
          ci_available <- !anyNA(c(nmr_lo, nmr_hi, nma_lo, nma_hi))
          overlaps     <- ci_available && nmr_hi >= nma_lo && nma_hi >= nmr_lo

          sse_auto <- if (!ci_available || anyNA(c(nmr_te, nma_te)) || overlaps) {
            "No evidence of small-study effects"
          } else {
            # NMR CI does NOT overlap NMA CI -> small-study effects present.
            # Direction: if NMR is less extreme than NMA (closer to null) in the
            # same direction NMA favours -> small studies are inflating the NMA
            # estimate -> reinforcing the biased contribution.
            nma_favors_t2 <- !is.na(nma_te) && nma_te > 0
            sse_shrinks    <- !is.na(nmr_te) && abs(nmr_te) < abs(nma_te)
            if (sse_shrinks && !is.na(nma_te) && sign(nmr_te) == sign(nma_te))
              SSE_IN   # NMR closer to 0 -> small studies inflate NMA -> reinforcing
            else if (is.na(nma_te) || is.na(nmr_te))
              "No evidence of small-study effects"
            else
              SSE_NOT
          }

          data.frame(comparison = comp,
                     nmr_te   = round(nmr_te, 3),
                     nmr_lo   = round(nmr_lo, 3),
                     nmr_hi   = round(nmr_hi, 3),
                     nma_te   = round(nma_te, 3),
                     sse_auto = sse_auto,
                     stringsAsFactors = FALSE)
        }) %>% bind_rows()
      }, error = function(e) NULL)
    })

    # ------------------------------------------------------------------
    # Set-all observers — Pairwise Table
    # Applies to Group A (within+across) + Group B (within only) + Group C (across only)
    # ------------------------------------------------------------------
    set_all_pw_col <- function(col_prefix, val) {
      comps <- tryCatch(
        bind_rows(direct_comps_df(), indirect_comps_df()),
        error = function(e) NULL)
      if (is.null(comps)) return()
      b_keys <- group_b_keys_rv()
      for (i in seq_len(nrow(comps))) {
        ck  <- comps$comp_key[i]
        sid <- safe_id(ck)
        is_b <- ck %in% b_keys
        # Skip across_ for Group B (not applicable), skip within_ for Group C
        if (col_prefix == "across" && is_b) next
        if (col_prefix == "within" && !ck %in% c(direct_comps_df()$comp_key, b_keys)) next
        updateSelectInput(session, paste0(col_prefix, "_", sid), selected = val)
      }
    }
    observeEvent(input$set_all_within_no, set_all_pw_col("within", "No bias detected"))
    observeEvent(input$set_all_across_no, set_all_pw_col("across", "No bias detected"))

    # --- Column header info modals ---
    observeEvent(input$info_within, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), " Within-study bias — Assessment guide"),
        easyClose = TRUE, footer = modalButton("Close"),
        tags$b("Did any study selectively NOT report this outcome?"),
        tags$ul(
          tags$li(strong("Group A:"), " click ROB-ME → answer Q1 (any eligible studies",
            " not reporting this outcome?) and Q2 (omission due to result",
            " direction/p-value?) → rating auto-derived."),
          tags$li(strong("Group B:"), " Q1 = Yes by definition. Set rating based on whether",
            " omission appears outcome-selective",
            " (Q2 = Yes/Unclear → Some concerns or Suspected bias)."),
          tags$li(strong("Group C:"), " Not applicable (no studies).")
        ),
        tags$hr(),
        tags$small("Note: this is NOT RoB 2 Domain 3 (attrition) or Domain 5.",
          " It targets cross-study selective outcome reporting (Page & Sterne BMJ 2023).")
      ))
    })

    observeEvent(input$info_across, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), " Across-study bias — Assessment guide"),
        easyClose = TRUE, footer = modalButton("Close"),
        tags$b("Conditions suggesting bias:"),
        tags$ul(
          tags$li("Failure to search grey literature / unpublished studies"),
          tags$li("Analysis based on few early trials of a novel agent",
                  " (early evidence often overestimates efficacy)"),
          tags$li("Previous evidence showed publication bias for this comparison"),
          tags$li(HTML("Egger's p &lt; 0.05 with \u2265 10 studies (suggests funnel asymmetry)"))
        ),
        tags$b("Conditions suggesting no bias:"),
        tags$ul(
          tags$li("Unpublished studies available and consistent with published results"),
          tags$li("Tradition of prospective trial registration in this field"),
          tags$li("Funnel plot symmetric with no statistically significant Egger test")
        ),
        tags$hr(),
        tags$small(HTML("Note: Egger's test recommended only for \u2265 10 studies (Chiocchia 2021)."),
          " Direction of suspected bias should reflect larger benefits seen in smaller studies.")
      ))
    })

    observeEvent(input$info_contrib, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), " Evaluation of contribution — Decision guide"),
        easyClose = TRUE, footer = modalButton("Close"),
        tags$ul(
          tags$li(strong("No substantial contribution from bias:"),
                  " neither treatment has \u226515 pp more than the other"),
          tags$li(strong("Substantial – balanced:"),
                  " substantial bias exists but roughly equal in both directions (biases cancel out)"),
          tags$li(strong("Substantial – favouring one treatment:"),
                  " one treatment has \u226515 pp more biased contribution than the other")
        ),
        tags$small("Note: 15 pp threshold is a guideline \u2014 apply consistently across all estimates.")
      ))
    })

    observeEvent(input$info_sse, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), " Small-study effects — Decision guide"),
        easyClose = TRUE, footer = modalButton("Close"),
        tags$ul(
          tags$li(strong("No evidence:"),
                  " adjusted estimate similar to unadjusted, CIs overlap well"),
          tags$li(strong("Evidence reinforcing biased contribution:"),
                  " adjusted estimate differs notably AND shifts in the same direction as the biased",
                  " contribution (\u24a4a) \u2192 risk of High risk"),
          tags$li(strong("Evidence NOT reinforcing:"),
                  " adjusted estimate differs but in opposite direction \u2192 biases counteract",
                  " \u2192 Some concerns at most")
        )
      ))
    })

    observeEvent(input$info_robmen_alg, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), " ROB-MEN final rating algorithm (Table 5, Chiocchia 2021)"),
        easyClose = TRUE, footer = modalButton("Close"),
        tags$table(class = "table table-bordered table-sm",
          style = "font-size:0.95em;",
          tags$thead(tags$tr(
            tags$th("\u24a4a Contribution"),
            tags$th("\u24a4b Small-study effects"),
            tags$th("Result")
          )),
          tags$tbody(
            tags$tr(tags$td("No substantial / Balanced"), tags$td("None"),
                    tags$td(strong(style = "color:#155724;", "Low risk"))),
            tags$tr(tags$td("No substantial / Balanced"), tags$td("Present"),
                    tags$td(strong(style = "color:#856404;", "Some concerns"))),
            tags$tr(tags$td("Favouring one treatment"), tags$td("None / opposite dir."),
                    tags$td(strong(style = "color:#856404;", "Some concerns"))),
            tags$tr(tags$td("Favouring one treatment"), tags$td("Reinforcing"),
                    tags$td(strong(style = "color:#721c24;", "High risk")))
          )
        )
      ))
    })

    # ------------------------------------------------------------------
    # Auto-compute pairwise overall judgements
    # Group A: combine within + across
    # Group B: within only (across = "Insufficient data" since no outcome data)
    # Group C: across only (within = "Not applicable" → treat as "No bias" in formula)
    # ------------------------------------------------------------------
    observeEvent(input$calc_overall_pw, {
      comps <- tryCatch(
        bind_rows(
          direct_comps_df()   %>% mutate(grp = "A"),
          indirect_comps_df() %>% mutate(grp = if_else(
            comp_key %in% group_b_keys_rv(), "B", "C"))
        ),
        error = function(e) NULL)
      eg <- tryCatch(egger_df(), error = function(e) NULL)
      if (is.null(comps)) return()

      for (i in seq_len(nrow(comps))) {
        ck  <- comps$comp_key[i]
        sid <- safe_id(ck)
        grp <- comps$grp[i]

        w_val <- input[[paste0("within_", sid)]]
        a_val <- input[[paste0("across_", sid)]]

        if (grp == "C") {
          # Group C: within = not applicable → use "No bias detected" in formula
          # across = user's qualitative choice
          w_val <- "No bias detected"
          if (is.null(a_val) || !nzchar(a_val)) a_val <- "Insufficient data"
        } else if (grp == "B") {
          # Group B: across = "Insufficient data" (no Egger's applicable)
          if (is.null(w_val) || !nzchar(w_val)) w_val <- "No bias detected"
          a_val <- "Insufficient data"
        } else {
          # Group A: both components
          if (is.null(w_val) || !nzchar(w_val)) w_val <- "No bias detected"
          if (is.null(a_val) || !nzchar(a_val)) {
            eg_row <- if (!is.null(eg)) eg %>% filter(comp_key == ck) else data.frame()
            a_val  <- if (nrow(eg_row) > 0 && !is.na(eg_row$across_auto[1]))
                        eg_row$across_auto[1] else "Insufficient data"
          }
        }
        # normalize legacy labels
        if (!is.null(a_val) && a_val == "High risk") a_val <- "Suspected bias"
        if (!is.null(w_val) && w_val == "High risk") w_val <- "Suspected bias"
        if (!is.null(a_val) && a_val == "No bias") a_val <- "No bias detected"
        if (!is.null(w_val) && w_val == "No bias") w_val <- "No bias detected"
        updateSelectInput(session, paste0("ov_pw_", sid),
                          selected = compute_overall_pw(w_val, a_val))
      }
    })

    # ------------------------------------------------------------------
    # Reactive auto-update: within → grey across + auto pairwise overall
    # Fires whenever any within_* or across_* input changes.
    # Short-circuit: if within = "Suspected bias", across is not needed.
    # ------------------------------------------------------------------
    observe({
      comps <- tryCatch(
        bind_rows(
          direct_comps_df()   %>% mutate(grp = "A"),
          indirect_comps_df() %>% mutate(grp = if_else(
            comp_key %in% group_b_keys_rv(), "B", "C"))
        ),
        error = function(e) NULL)
      if (is.null(comps) || nrow(comps) == 0) return()

      for (i in seq_len(nrow(comps))) {
        ck  <- comps$comp_key[i]
        sid <- safe_id(ck)
        grp <- comps$grp[i]

        w_val <- input[[paste0("within_", sid)]]
        a_val <- input[[paste0("across_", sid)]]

        # Normalize legacy labels
        if (!is.null(w_val) && w_val %in% c("High risk", "No bias"))
          w_val <- if (w_val == "High risk") "Suspected bias" else "No bias detected"
        if (!is.null(a_val) && a_val %in% c("High risk", "No bias"))
          a_val <- if (a_val == "High risk") "Suspected bias" else "No bias detected"

        # Short-circuit: within = Suspected bias → grey out across, set overall
        across_full_id <- session$ns(paste0("across_", sid))
        if (grp == "A" && !is.null(w_val) && w_val == "Suspected bias") {
          session$sendCustomMessage("robmen_toggle_across",
                                    list(id = across_full_id, disabled = TRUE))
          updateSelectInput(session, paste0("ov_pw_", sid),
                            selected = "Suspected bias")
        } else if (grp == "A") {
          session$sendCustomMessage("robmen_toggle_across",
                                    list(id = across_full_id, disabled = FALSE))
          # Auto-update pairwise overall when both fields are filled
          if (!is.null(w_val) && nzchar(w_val) && !is.null(a_val) && nzchar(a_val)) {
            updateSelectInput(session, paste0("ov_pw_", sid),
                              selected = compute_overall_pw(w_val, a_val))
          }
        }
      }
    })

    # ------------------------------------------------------------------
    # Set-all observers — ROB-MEN Table
    # ------------------------------------------------------------------
    observeEvent(input$set_all_contrib_no, {
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      if (is.null(ne)) return()
      for (i in seq_len(nrow(ne)))
        updateSelectInput(session,
          paste0("contrib_eval_", safe_id(ne$comparison[i])),
          selected = "No substantial contribution from bias")
    })
    observeEvent(input$set_all_sse_no, {
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      if (is.null(ne)) return()
      for (i in seq_len(nrow(ne)))
        updateSelectInput(session,
          paste0("sse_eval_", safe_id(ne$comparison[i])),
          selected = "No evidence of small-study effects")
    })

    # ------------------------------------------------------------------
    # Reactive auto-fill ⑤a: contribution evaluation
    # Fires whenever pct_biased() changes; skips comparisons already rated.
    # ------------------------------------------------------------------
    observe({
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      pb <- tryCatch(pct_biased(),    error = function(e) NULL)
      if (is.null(ne) || is.null(pb) || nrow(ne) == 0) return()
      for (i in seq_len(nrow(ne))) {
        sid    <- safe_id(ne$comparison[i])
        pb_row <- pb %>% filter(comparison == ne$comparison[i])
        if (nrow(pb_row) == 0) next
        p1 <- if (is.na(pb_row$pct_fav_t1[1])) 0 else pb_row$pct_fav_t1[1]
        p2 <- if (is.na(pb_row$pct_fav_t2[1])) 0 else pb_row$pct_fav_t2[1]
        # Chiocchia 2021: |pct_t1 - pct_t2| > 15 pp → favouring one treatment.
        # No guard for "already set" — always reflects the current pct values.
        c_auto <- if (p1 == 0 && p2 == 0) {
          "No substantial contribution from bias"
        } else if (abs(p1 - p2) > 15) {
          "Substantial contribution from bias \u2013 favouring one treatment"
        } else {
          "Substantial contribution from bias \u2013 balanced"
        }
        updateSelectInput(session, paste0("contrib_eval_", sid), selected = c_auto)
      }
    })

    # ------------------------------------------------------------------
    # Reactive auto-fill ⑤b: small-study effects evaluation
    # Uses network_sse_df() sse_auto (CI-overlap logic from netmetaregression).
    # Fires when network_sse_df changes; skips comparisons already rated.
    # ------------------------------------------------------------------
    observe({
      ne  <- tryCatch(nma_estimates(),  error = function(e) NULL)
      nmr <- tryCatch(network_sse_df(), error = function(e) NULL)
      if (is.null(ne) || is.null(nmr) || nrow(ne) == 0 || nrow(nmr) == 0) return()
      for (i in seq_len(nrow(ne))) {
        sid   <- safe_id(ne$comparison[i])
        s_cur <- isolate(input[[paste0("sse_eval_", sid)]])
        if (!is.null(s_cur) && nzchar(s_cur)) next  # already set by user

        nmr_row <- nmr %>% filter(comparison == ne$comparison[i])
        if (nrow(nmr_row) == 0) next
        s_auto <- nmr_row$sse_auto[1]
        if (!nzchar(s_auto)) next
        updateSelectInput(session, paste0("sse_eval_", sid), selected = s_auto)
      }
    })

    # ------------------------------------------------------------------
    # Reactive auto-fill ⑤ final judgement
    # When both ⑤a and ⑤b are filled (by auto or user), compute overall.
    # Skips comparisons where overall has already been set.
    # ------------------------------------------------------------------
    observe({
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      if (is.null(ne) || nrow(ne) == 0) return()
      for (i in seq_len(nrow(ne))) {
        sid    <- safe_id(ne$comparison[i])
        ov_cur <- isolate(input[[paste0("ov_robmen_", sid)]])
        if (!is.null(ov_cur) && nzchar(ov_cur)) next  # already set by user

        c_val <- input[[paste0("contrib_eval_", sid)]]
        s_val <- input[[paste0("sse_eval_", sid)]]
        if (is.null(c_val) || !nzchar(c_val) ||
            is.null(s_val) || !nzchar(s_val)) next
        ov_auto <- compute_overall_robmen(c_val, s_val)
        updateSelectInput(session, paste0("ov_robmen_", sid), selected = ov_auto)
      }
    })

    # ------------------------------------------------------------------
    # Auto-compute overall ROB-MEN ratings
    # ------------------------------------------------------------------
    observeEvent(input$calc_overall_robmen, {
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      if (is.null(ne)) return()
      pb <- tryCatch(pct_biased(), error = function(e) NULL)

      computed_ov <- character(nrow(ne))  # store computed ratings for D2 sync

      for (i in seq_len(nrow(ne))) {
        sid   <- safe_id(ne$comparison[i])

        # Auto-compute "Evaluation of contribution" (Chiocchia 2021, 15 pp threshold).
        # Concerning direction = same as NMA estimate direction.
        #   te > 0  (t2 better) → biased evidence favouring t2 (pct_fav_t2) is concerning
        #   te <= 0 (t1 better) → biased evidence favouring t1 (pct_fav_t1) is concerning
        #
        # Mapping to official CONTRIB_CHOICES (Chiocchia 2021 Table 3 col 3):
        #   |p_concern - p_other| > 15 pp  → "Substantial contribution – favouring NMA result"
        #   both p1 > 15 AND p2 > 15       → "Substantial contribution – balanced"
        #     (substantial bias in both directions: may cancel out)
        #   otherwise                       → "No substantial contribution from bias"
        c_val <- input[[paste0("contrib_eval_", sid)]]
        if (is.null(c_val) || !nzchar(c_val)) {
          pb_row <- if (!is.null(pb)) pb %>% filter(comparison == ne$comparison[i]) else NULL
          if (!is.null(pb_row) && nrow(pb_row) > 0) {
            te_i  <- ne$te[i]
            p1    <- if (is.na(pb_row$pct_fav_t1[1])) 0 else pb_row$pct_fav_t1[1]
            p2    <- if (is.na(pb_row$pct_fav_t2[1])) 0 else pb_row$pct_fav_t2[1]
            if (!is.na(te_i) && te_i > 0) {
              p_concern <- p2; p_other <- p1
            } else {
              p_concern <- p1; p_other <- p2
            }
            c_val <- if (p_concern - p_other > 15) {
              "Substantial contribution from bias \u2013 favouring one treatment"
            } else if (p1 > 15 && p2 > 15) {
              "Substantial contribution from bias \u2013 balanced"
            } else {
              "No substantial contribution from bias"
            }
          } else {
            c_val <- "No substantial contribution from bias"
          }
          updateSelectInput(session, paste0("contrib_eval_", sid), selected = c_val)
        }

        s_val <- input[[paste0("sse_eval_", sid)]]
        if (is.null(s_val) || !nzchar(s_val)) {
          # Auto-suggest small-study effects from NMR when not yet set by user
          nmr_df  <- tryCatch(network_sse_df(), error = function(e) NULL)
          nmr_row <- if (!is.null(nmr_df))
                       nmr_df %>% filter(comparison == ne$comparison[i])
                     else
                       NULL
          s_val <- if (!is.null(nmr_row) && nrow(nmr_row) > 0 &&
                       nzchar(nmr_row$sse_auto[1])) {
            nmr_row$sse_auto[1]
          } else {
            "No evidence of small-study effects"
          }
          updateSelectInput(session, paste0("sse_eval_", sid), selected = s_val)
        }

        ov_val <- compute_overall_robmen(c_val, s_val)
        computed_ov[i] <- ov_val
        updateSelectInput(session, paste0("ov_robmen_", sid), selected = ov_val)
      }

      # NOTE: Do NOT auto-push to Domain 2 here.
      # Pushing triggers cinema_results() to invalidate → robmen_main_ui re-renders
      # → tabsetPanel resets to tab 1.  Use the manual "Update CINeMA Domain 2"
      # button (send_to_cinema) for the sync step.
    })

    # ------------------------------------------------------------------
    # Collect final ROB-MEN ratings (for Module B integration + return)
    # ------------------------------------------------------------------
    robmen_results <- reactive({
      ne <- tryCatch(nma_estimates(), error = function(e) NULL)
      req(!is.null(ne))
      lapply(seq_len(nrow(ne)), function(i) {
        sid <- safe_id(ne$comparison[i])
        ov  <- input[[paste0("ov_robmen_", sid)]]
        data.frame(
          comparison    = ne$comparison[i],
          robmen_rating = if (!is.null(ov) && nzchar(ov)) ov else "Not assessed",
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
    })

    # Manual "sync now" button — sync ratings then navigate to CINeMA Domain 2
    observeEvent(input$send_to_cinema, {
      rb <- tryCatch(robmen_results(), error = function(e) NULL)
      req(!is.null(rb))
      cinema_module$set_robmen(rb)
      n_rated <- sum(rb$robmen_rating != "Not assessed")
      n_total <- nrow(rb)
      showNotification(
        paste0("ROB-MEN ratings synced to CINeMA Domain 2. (",
               n_rated, "/", n_total, " comparisons rated)"),
        type = "message", duration = 4
      )
      if (!is.null(go_to_cinema)) go_to_cinema()
      # Switch to Domain 2 tab within CINeMA
      tryCatch(cinema_module$go_to_domain2(), error = function(e) NULL)
    })

    # ==================================================================
    # MAIN UI SKELETON (shown after button click)
    # ==================================================================
    output$robmen_main_ui <- renderUI({
      req(!is.null(egger_df()))

      tagList(
        tabsetPanel(id = ns("robmen_tabs"),

          # ---- Tab 1: Pairwise Assessment (①②③) ----------------------
          tabPanel(tagList(icon("table"), " ①②③ Pairwise Assessment"),
            br(),
            div(style = "overflow-x:auto;",
                uiOutput(ns("pairwise_table_ui")))
          ),

          # ---- Tab 2: ROB-MEN Final Rating (④⑤) ----------------------
          tabPanel(tagList(icon("clipboard-check"), " ④⑤ ROB-MEN Final Rating"),
            br(),
            div(style = "display:flex; justify-content:space-between; align-items:flex-start; gap:12px; margin-bottom:10px;",
              p(style = "font-size:0.88em; color:#555; margin:0;",
                icon("arrow-left"),
                " Complete Tab 1 first. ④ % contribution columns update automatically.",
                " Use the ", icon("question-circle"), " icons in column headers for guidance."),
              div(style = "flex-shrink:0;",
                actionButton(ns("send_to_cinema"),
                             tagList(icon("arrow-right"), " Update CINeMA Domain 2"),
                             class = "btn btn-primary btn-sm",
                             title = "Send ROB-MEN ratings to CINeMA Module B (Domain 2)"),
                uiOutput(ns("send_status"))
              )
            ),
            div(style = "overflow-x:auto;",
                uiOutput(ns("robmen_table_ui")))
          )

        )
      )
    })

    # ==================================================================
    # Pairwise Comparisons Table
    # ==================================================================
    output$pairwise_table_ui <- renderUI({
      eg    <- egger_df()
      dir_c <- direct_comps_df()
      grp_b <- tryCatch(group_b_comps_df(), error = function(e) NULL)
      grp_c <- tryCatch(group_c_comps_df(), error = function(e) NULL)
      if (is.null(grp_b)) grp_b <- data.frame(comp_key=character(), t1=character(),
                                               t2=character(), n_direct=integer(),
                                               stringsAsFactors=FALSE)
      if (is.null(grp_c)) grp_c <- data.frame(comp_key=character(), t1=character(),
                                               t2=character(), n_direct=integer(),
                                               stringsAsFactors=FALSE)

      th_style <- "padding:8px 10px; text-align:left; white-space:nowrap;"

      header_row <- tags$tr(style = "background:#6c3483; color:white; font-weight:bold;",
        tags$th(style = th_style, "Pairwise comparison"),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div("Reporting this outcome"),
          tags$small(style = "font-weight:normal; opacity:0.85;",
                     HTML("k (n)"))),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div("Total identified in the SR"),
          tags$small(style = "font-weight:normal; opacity:0.85;",
                     HTML("k (n)"))),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("① Within-study bias"),
            actionButton(ns("info_within"), label = icon("question-circle"),
              class = "btn btn-xs btn-link", style = "padding:0; color:rgba(255,255,255,0.8); font-size:1em;",
              title = "Assessment guide")),
          div(style = "margin-top:4px;",
            actionButton(ns("set_all_within_no"), "set all \u2192 No bias",
              class = "btn btn-xs btn-warning", style = "font-size:0.75em;"))),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("② Across-study bias"),
            actionButton(ns("info_across"), label = icon("question-circle"),
              class = "btn btn-xs btn-link", style = "padding:0; color:rgba(255,255,255,0.8); font-size:1em;",
              title = "Assessment guide")),
          div(style = "margin-top:4px;",
            actionButton(ns("set_all_across_no"), "set all \u2192 No bias",
              class = "btn btn-xs btn-warning", style = "font-size:0.75em;"))),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("③ Overall judgement")),
          div(style = "margin-top:4px;",
            actionButton(ns("calc_overall_pw"), tagList(icon("calculator"), " auto"),
              class = "btn btn-xs btn-info", style = "font-size:0.75em;",
              title = paste0(
                "Algorithm (Chiocchia 2021):\n",
                "\u2460 Suspected bias \u2192 Suspected bias (short-circuit)\n",
                "\u2461 Across = Suspected bias \u2192 Suspected bias\n",
                "\u2462 Both = No bias detected \u2192 No bias detected\n",
                "Otherwise \u2192 Some concerns"
              ))),
          tags$small(style = "display:block; font-weight:normal; opacity:0.85; white-space:normal; font-size:0.8em; margin-top:2px;",
            HTML("\u2460 Suspected \u2192 Suspected<br>\u2461 Across Suspected \u2192 Suspected<br>Both No bias \u2192 No bias<br>else \u2192 Some concerns"))),
        tags$th(style = th_style,
          div(HTML("Direction of bias"),
            style = "white-space:normal;"),
          tags$small(style = "display:block; font-weight:normal; opacity:0.85; white-space:normal; font-size:0.8em;",
            HTML("Which treatment does<br>bias favour? (used for ④)")))
      )

      grp_header <- function(label, subtitle = NULL, ncols = 7) {
        tags$tr(style = "background:#6c3483; color:white;",
          tags$td(colspan = ncols, style = "padding:6px 10px; font-weight:bold;",
            label,
            if (!is.null(subtitle))
              tags$span(style = "font-weight:normal; font-size:0.85em; margin-left:8px;",
                        subtitle)
          )
        )
      }

      # Build Group A rows (direct evidence for this outcome)
      group_a_rows <- lapply(seq_len(nrow(dir_c)), function(i) {
        ck     <- dir_c$comp_key[i]
        eg_row <- eg %>% filter(comp_key == ck)
        ad <- if (nrow(eg_row) > 0 && !is.na(eg_row$across_auto[1]))
                eg_row$across_auto[1] else NA
        make_pw_row(ns, ck, dir_c$t1[i], dir_c$t2[i], dir_c$n_direct[i],
                    across_default = ad,
                    within_default = "",
                    n_total = dir_c$n_total[i])
      })

      # Group C rows with "→ Group B" toggle button
      grp_c_with_toggle <- if (nrow(grp_c) > 0) {
        lapply(seq_len(nrow(grp_c)), function(i) {
          ck  <- grp_c$comp_key[i]
          sid <- safe_id(ck)
          tags$tr(style = "background:#f8f9fa;",
            tags$td(style = "padding:4px 8px; white-space:nowrap;",
              strong(ck), " ",
              actionButton(ns(paste0("grpb_toggle_", sid)),
                           label = "→ Group B",
                           class = "btn btn-xs btn-outline-warning",
                           style = "font-size:0.72em; padding:1px 5px;",
                           title = paste0("Mark '", ck, "' as Group B: ",
                                          "studies exist for this comparison but did not report this outcome"))),
            tags$td(style = "padding:4px 8px; text-align:center;",
              tags$span(style = paste0("background:#e9ecef; color:#6c757d; padding:3px 8px;",
                                       " border:1px solid #ced4da; border-radius:4px; font-size:0.85em;"),
                        "0 (—)")),
            # Total identified in SR: editable
            tags$td(style = "padding:4px 8px;",
              div(style = "display:flex; align-items:center; gap:3px; white-space:nowrap;",
                numericInput(ns(paste0("n_sr_k_", sid)), label = NULL,
                             value = NA, min = 0, step = 1, width = "60px"),
                tags$span("("),
                numericInput(ns(paste0("n_sr_n_", sid)), label = NULL,
                             value = NA, min = 0, step = 1, width = "80px"),
                tags$span(")")
              )
            ),
            # within: not applicable
            tags$td(style = "padding:4px 8px;",
              tags$span(style = paste0("background:#e9ecef; color:#6c757d; padding:4px 8px;",
                                       " border:1px solid #ced4da; border-radius:4px;",
                                       " display:inline-block; font-size:0.85em;"),
                        title = "Group C: no studies — within-study bias does not apply",
                        "Not applicable")),
            # across: qualitative dropdown
            tags$td(style = "padding:4px 8px;",
              div(
                selectInput(ns(paste0("across_", sid)), label = NULL,
                            choices = ACROSS_CHOICES, selected = "", width = "150px"),
                tags$small(style = "color:#666; font-size:0.78em;",
                           "Qualitative only (no Egger's)")
              )
            ),
            tags$td(style = "padding:4px 8px;",
              selectInput(ns(paste0("ov_pw_", sid)), label = NULL,
                          choices = OVERALL_PW_CHOICES, selected = "", width = "150px"))
          )
        })
      } else list()

      # Group B rows with toggle back to C
      grp_b_with_toggle <- if (nrow(grp_b) > 0) {
        lapply(seq_len(nrow(grp_b)), function(i) {
          ck  <- grp_b$comp_key[i]
          sid <- safe_id(ck)
          tags$tr(style = "background:#fffde7;",
            tags$td(style = "padding:4px 8px; white-space:nowrap;",
              strong(ck), " ",
              actionButton(ns(paste0("grpb_toggle_", sid)),
                           label = "← Group C",
                           class = "btn btn-xs btn-outline-secondary",
                           style = "font-size:0.72em; padding:1px 5px;",
                           title = paste0("Move '", ck, "' back to Group C (unobserved)"))),
            tags$td(style = "padding:4px 8px; text-align:center;",
              tags$span(style = paste0("background:#e9ecef; color:#6c757d; padding:3px 8px;",
                                       " border:1px solid #ced4da; border-radius:4px; font-size:0.85em;"),
                        "0 (—)")),
            # Total identified in SR: editable (studies were found in SR)
            tags$td(style = "padding:4px 8px;",
              div(style = "display:flex; align-items:center; gap:3px; white-space:nowrap;",
                numericInput(ns(paste0("n_sr_k_", sid)), label = NULL,
                             value = NA, min = 0, step = 1, width = "60px"),
                tags$span("("),
                numericInput(ns(paste0("n_sr_n_", sid)), label = NULL,
                             value = NA, min = 0, step = 1, width = "80px"),
                tags$span(")")
              )
            ),
            # within: enabled selectInput (Q1 = Yes by definition; assess Q2)
            tags$td(style = "padding:4px 8px;",
              div(style = "display:flex; gap:4px; align-items:flex-start; flex-direction:column;",
                selectInput(ns(paste0("within_", sid)), label = NULL,
                            choices = WITHIN_CHOICES, selected = "",
                            width = "160px"),
                tags$small(style = "color:#856404; font-size:0.78em;",
                  HTML("Q1=Yes (by def). Rate based on whether omission is outcome-selective."))
              )
            ),
            # across: not applicable
            tags$td(style = "padding:4px 8px;",
              tags$span(style = paste0("background:#e9ecef; color:#6c757d; padding:4px 8px;",
                                       " border:1px solid #ced4da; border-radius:4px;",
                                       " display:inline-block; font-size:0.85em;"),
                        title = "Group B: no outcome data — Egger's test not applicable",
                        "Not applicable")),
            tags$td(style = "padding:4px 8px;",
              selectInput(ns(paste0("ov_pw_", sid)), label = NULL,
                          choices = OVERALL_PW_CHOICES, selected = "", width = "150px"))
          )
        })
      } else list()

      tagList(
        div(style = "font-size:0.82em; color:#666; margin-bottom:4px;",
          icon("arrows-alt-h"), " Scroll horizontally to see all columns"),
        div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
          tags$table(
            style = paste0("min-width:650px; border-collapse:collapse;",
                           " border:1px solid #dee2e6; font-size:0.9em;"),
            class = "table table-bordered",
            tags$thead(header_row),
            tags$tbody(
              grp_header("Group A — Observed for this outcome",
                         subtitle = "(direct evidence: Egger's test + ROB-ME Step 2)"),
              group_a_rows,

              grp_header("Group B — Observed for other outcomes",
                         subtitle = "(studies exist but did NOT report this outcome: within-study only)"),
              if (nrow(grp_b) > 0) grp_b_with_toggle
              else tags$tr(tags$td(colspan = 7,
                style = "padding:6px 10px; color:#6c757d; font-style:italic; font-size:0.88em;",
                "No Group B comparisons identified.")),

              grp_header("Group C — Unobserved",
                         subtitle = "(no studies at all: qualitative across-study assessment only; click '\u2192 Group B' to reclassify)"),
              grp_c_with_toggle
            )
          )
        )
      )
    })

    # ==================================================================
    # ROB-MEN Table
    # ==================================================================
    output$robmen_table_ui <- renderUI({
      ne <- nma_estimates()
      pb <- tryCatch(pct_biased(), error = function(e) {
        data.frame(comparison = character(0),
                   pct_fav_t1 = numeric(0),
                   pct_fav_t2 = numeric(0))
      })
      eg <- tryCatch(egger_df(), error = function(e) NULL)

      th_style <- "padding:6px 8px; text-align:left; white-space:normal;"

      header_row <- tags$tr(style = "background:#6c3483; color:white; font-weight:bold;",
        tags$th(style = th_style, "NMA estimate"),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div(HTML("④ % Biased contrib.")),
          tags$small(style = "font-weight:normal; opacity:0.85;", "Favours 1st treat.")),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div(HTML("④ % Biased contrib.")),
          tags$small(style = "font-weight:normal; opacity:0.85;", "Favours 2nd treat.")),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("⑤a Contribution"),
            actionButton(ns("info_contrib"), label = icon("question-circle"),
              class = "btn btn-xs btn-link", style = "padding:0; color:rgba(255,255,255,0.8); font-size:1em;",
              title = "Decision guide")),
          tags$small(style = "display:block; font-weight:normal; opacity:0.85; white-space:normal;",
            HTML("\u226515 pp diff \u2192 Substantial")),
          div(style = "margin-top:4px;",
            actionButton(ns("set_all_contrib_no"), "set all \u2192 No substantial",
              class = "btn btn-xs btn-warning", style = "font-size:0.75em;"))),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div(HTML("NMA effect")),
          tags$small(style = "font-weight:normal; opacity:0.85; white-space:normal;",
            "TE [95% CI]", tags$br(), "unadjusted")),
        tags$th(style = paste0(th_style, "text-align:center;"),
          div(HTML("NMR effect")),
          tags$small(style = "font-weight:normal; opacity:0.85; white-space:normal;",
            "TE [95% CI]", tags$br(), "adjusted")),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("⑤b Small-study effects"),
            actionButton(ns("info_sse"), label = icon("question-circle"),
              class = "btn btn-xs btn-link", style = "padding:0; color:rgba(255,255,255,0.8); font-size:1em;",
              title = "Decision guide")),
          tags$small(style = "display:block; font-weight:normal; opacity:0.85; white-space:normal;",
            HTML("Compare NMA vs NMR")),
          div(style = "margin-top:4px;",
            actionButton(ns("set_all_sse_no"), "set all \u2192 No evidence",
              class = "btn btn-xs btn-warning", style = "font-size:0.75em;"))),
        tags$th(style = th_style,
          div(style = "display:flex; align-items:center; gap:4px;",
            HTML("⑤ ROB-MEN rating"),
            actionButton(ns("info_robmen_alg"), label = icon("question-circle"),
              class = "btn btn-xs btn-link", style = "padding:0; color:rgba(255,255,255,0.8); font-size:1em;",
              title = "Algorithm (Table 5)")),
          div(style = "margin-top:4px;",
            actionButton(ns("calc_overall_robmen"), "calculate overall",
              class = "btn btn-xs btn-warning", style = "font-size:0.75em;")))
      )

      robmen_grp_header <- function(label) {
        tags$tr(style = "background:#6c3483; color:white;",
          tags$td(colspan = 8, style = "padding:6px 10px; font-weight:bold;", label))
      }

      mixed_idx <- which(ne$evidence_type == "mixed")
      indir_idx <- which(ne$evidence_type == "indirect")

      nmr_df <- tryCatch(network_sse_df(), error = function(e) NULL)

      make_row <- function(i) {
        comp <- ne$comparison[i]
        pb_row <- pb %>% filter(comparison == comp)
        p1 <- if (nrow(pb_row) > 0) pb_row$pct_fav_t1[1] else 0
        p2 <- if (nrow(pb_row) > 0) pb_row$pct_fav_t2[1] else 0
        if (is.na(p1)) p1 <- 0
        if (is.na(p2)) p2 <- 0

        # Primary: use network-level NMR (netmetaregression, has CIs)
        nmr_te <- nmr_lo <- nmr_hi <- NA_real_
        if (!is.null(nmr_df) && nrow(nmr_df) > 0) {
          nmr_row <- nmr_df %>% filter(comparison == comp)
          if (nrow(nmr_row) > 0) {
            nmr_te <- nmr_row$nmr_te[1]
            nmr_lo <- nmr_row$nmr_lo[1]
            nmr_hi <- nmr_row$nmr_hi[1]
          }
        }
        # Fallback: pairwise Egger's nmr_te with CI
        if (is.na(nmr_te) && !is.null(eg)) {
          ck1 <- paste(pmin(ne$t1[i], ne$t2[i]), pmax(ne$t1[i], ne$t2[i]), sep = ":")
          eg_row <- eg %>% filter(comp_key == ck1)
          if (nrow(eg_row) > 0 && !is.na(eg_row$nmr_te[1])) {
            nmr_te <- eg_row$nmr_te[1]
            if ("nmr_lo" %in% names(eg_row)) nmr_lo <- eg_row$nmr_lo[1]
            if ("nmr_hi" %in% names(eg_row)) nmr_hi <- eg_row$nmr_hi[1]
          }
        }

        make_robmen_row(ns, comp,
                        ne$te[i], ne$lo[i], ne$hi[i], p1, p2,
                        nmr_te = nmr_te, nmr_lo = nmr_lo, nmr_hi = nmr_hi,
                        bg = if (i %% 2 == 0) "#fafafa" else "white")
      }

      div(style = "overflow-x: auto; -webkit-overflow-scrolling: touch;",
        div(style = "font-size:0.82em; color:#666; margin-bottom:4px;",
          icon("arrows-alt-h"), " Scroll horizontally to see all columns"),
        tags$table(
          style = paste0("min-width:800px; border-collapse:collapse;",
                         " border:1px solid #dee2e6; font-size:0.9em;"),
          class = "table table-bordered",
          tags$thead(header_row),
          tags$tbody(
            if (length(mixed_idx) > 0) tagList(
              robmen_grp_header("mixed / only direct"),
              lapply(mixed_idx, make_row)
            ),
            if (length(indir_idx) > 0) tagList(
              robmen_grp_header("indirect"),
              lapply(indir_idx, make_row)
            )
          )
        )
      )
    })

    # ------------------------------------------------------------------
    # Per-comparison funnel plot modals (rendered on demand)
    # Created dynamically for each direct comparison after egger_df fires
    # ------------------------------------------------------------------
    observeEvent(egger_df(), {
      eg    <- tryCatch(egger_df(),          error = function(e) NULL)
      dc    <- tryCatch(direct_comps_df(),   error = function(e) NULL)
      df_pw <- tryCatch(pairwise_data(),     error = function(e) NULL)
      req(!is.null(eg), !is.null(dc), !is.null(df_pw))

      for (i in seq_len(nrow(dc))) {
        local({
          ck_i  <- dc$comp_key[i]
          sid_i <- safe_id(ck_i)
          t1_i  <- dc$t1[i]
          t2_i  <- dc$t2[i]

          output[[paste0("modal_egger_", sid_i)]] <- renderUI({
            eg_row <- eg %>% filter(comp_key == ck_i)
            if (nrow(eg_row) == 0) return(div(class = "alert alert-warning",
                                              "No Egger's test data available."))
            r <- eg_row[1, ]
            if (r$n_s < 2) return(div(class = "alert alert-warning",
              paste0("Only ", r$n_s, " study. Egger's test requires \u2265 2 studies.")))
            # Chiocchia 2021: Egger's test only shown when ≥ 10 studies
            if (isTRUE(r$n_few)) return(
              div(class = "alert alert-warning", style = "font-size:0.87em; padding:8px 12px;",
                icon("exclamation-triangle"),
                strong(paste0(" Only ", r$n_s, " studies.")),
                HTML(" Egger's test is not shown \u2014 Chiocchia 2021 recommends"),
                strong(" \u2265 10 studies"), ". Use the funnel plot for qualitative assessment.")
            )
            if (is.na(r$egger_p)) return(div(class = "alert alert-warning",
              "Egger's test failed for this comparison."))
            bg <- if (r$egger_p >= 0.10) "#d4edda"
                  else if (r$egger_p >= 0.05) "#fff3cd"
                  else "#f8d7da"
            div(style = paste0("padding:8px 12px; background:", bg,
                               "; border-radius:4px; margin-bottom:8px;"),
              strong(paste0("N = ", r$n_s, " studies.  ")),
              paste0("Egger p = ", round(r$egger_p, 3), ".  "),
              paste0("Bias coefficient (slope) = ", round(r$egger_bias, 3), ".  "),
              "Auto-rating: ", strong(r$across_auto), ".",
              tags$br(),
              if (!is.na(r$egger_eff))
                tags$span(style = "font-size:0.9em; color:#333;",
                  HTML(paste0(
                    "Adjusted estimate (Egger intercept at SE\u202f=\u202f0): ",
                    "<strong>", round(r$egger_eff, 3), "</strong>",
                    " \u2014 compare with pooled est. in funnel plot.",
                    " Red dashed line = Egger regression line."))),
              tags$br(),
              tags$small(style = "color:#555;",
                "p \u2265 0.10 \u2192 No bias | 0.05\u20130.10 \u2192 Some concerns | < 0.05 \u2192 High risk")
            )
          })

          output[[paste0("modal_funnel_", sid_i)]] <- renderPlotly({
            sub <- df_pw %>%
              filter((t1 == t1_i & t2 == t2_i) | (t1 == t2_i & t2 == t1_i)) %>%
              mutate(y_can = ifelse(t1 == t1_i & t2 == t2_i, y, -y))
            validate(need(nrow(sub) >= 2,
                          "Fewer than 2 direct studies — funnel plot not available."))

            w         <- 1 / sub$se^2
            summary_y <- sum(sub$y_can * w) / sum(w)
            se_max    <- max(sub$se) * 1.1

            # Contour lines diverging from null (0): x = ±z * SE
            se_seq <- c(0, se_max)
            ctr <- data.frame(
              se    = se_seq,
              p05p  =  1.96  * se_seq,
              p05n  = -1.96  * se_seq,
              p01p  =  2.576 * se_seq,
              p01n  = -2.576 * se_seq
            )

            # Trim-and-fill: only when ≥ 10 studies (same threshold as Egger's test)
            tf_info <- if (nrow(sub) < 10) {
              list(n_trimmed = -1)
            } else {
              tryCatch({
                m  <- metagen(TE = sub$y_can, seTE = sub$se, studlab = sub$studlab,
                              sm = "MD", common = FALSE, random = TRUE)
                tf <- trimfill(m)
                if (tf$k > m$k) {
                  imp_mask <- !tf$studlab %in% sub$studlab
                  list(
                    n_trimmed  = sum(imp_mask),
                    imp_y      = tf$TE[imp_mask],
                    imp_se     = tf$seTE[imp_mask],
                    pooled_te  = tf$TE.random,
                    pooled_lo  = tf$lower.random,
                    pooled_hi  = tf$upper.random
                  )
                } else {
                  list(n_trimmed = 0)
                }
              }, error = function(e) list(n_trimmed = 0))
            }

            p <- ggplot(sub, aes(x = y_can, y = se,
                                 text = paste0("Study: ", studlab,
                                   "<br>Effect: ", round(y_can, 3),
                                   "<br>SE: ", round(se, 3)))) +
              # Contour lines from null (p=0.05 dashed, p=0.01 dotted)
              geom_line(data = ctr, aes(x = p05p, y = se), inherit.aes = FALSE,
                        colour = "#888888", linetype = "dashed", linewidth = 0.5) +
              geom_line(data = ctr, aes(x = p05n, y = se), inherit.aes = FALSE,
                        colour = "#888888", linetype = "dashed", linewidth = 0.5) +
              geom_line(data = ctr, aes(x = p01p, y = se), inherit.aes = FALSE,
                        colour = "#555555", linetype = "dotted", linewidth = 0.5) +
              geom_line(data = ctr, aes(x = p01n, y = se), inherit.aes = FALSE,
                        colour = "#555555", linetype = "dotted", linewidth = 0.5) +
              # Null and summary lines
              geom_vline(xintercept = 0,         colour = "grey40",   linetype = "dotted",  linewidth = 0.5) +
              geom_vline(xintercept = summary_y, colour = "steelblue", linetype = "solid",  linewidth = 0.8) +
              # Original study points
              geom_point(size = 3, alpha = 0.85, colour = "steelblue") +
              scale_y_reverse(limits = c(se_max, 0)) +
              labs(x = "Effect size", y = "Standard error (SE)",
                   title    = paste("Contour-enhanced funnel plot:", ck_i),
                   subtitle = "Grey dashed = p\u202f<\u202f0.05; Grey dotted = p\u202f<\u202f0.01 (from null). Blue = pooled est. Red dashed = Egger regression line.") +
              theme_minimal(base_size = 11)

            # Add trim-and-fill imputed studies
            if (tf_info$n_trimmed > 0) {
              imp_df <- data.frame(y_can = tf_info$imp_y, se = tf_info$imp_se)
              p <- p +
                geom_point(data = imp_df, aes(x = y_can, y = se),
                           shape = 1, size = 3.5, colour = "#d9534f",
                           stroke = 1.2, inherit.aes = FALSE)
            }

            # Egger regression line: TE = egger_eff + egger_bias * SE
            # At SE = 0 (top of funnel) x = egger_eff (small-study-adjusted estimate).
            # Slope = egger_bias; direction reflects the source of asymmetry.
            eg_row_local <- if (!is.null(eg)) eg %>% filter(comp_key == ck_i) else NULL
            if (!is.null(eg_row_local) && nrow(eg_row_local) > 0 &&
                !is.na(eg_row_local$egger_eff[1]) && !is.na(eg_row_local$egger_bias[1])) {
              eff_v  <- eg_row_local$egger_eff[1]
              bias_v <- eg_row_local$egger_bias[1]
              reg_df <- data.frame(
                x = eff_v, xend = eff_v + bias_v * se_max,
                y = 0,     yend = se_max
              )
              p <- p +
                geom_segment(data = reg_df,
                             aes(x = x, y = y, xend = xend, yend = yend),
                             colour = "#c0392b", linetype = "dashed",
                             linewidth = 0.9, inherit.aes = FALSE)
            }

            ggplotly(p, tooltip = "text")
          })

          # Trim-and-fill summary note (shown below funnel in modal)
          output[[paste0("modal_trimfill_note_", sid_i)]] <- renderUI({
            sub <- df_pw %>%
              filter((t1 == t1_i & t2 == t2_i) | (t1 == t2_i & t2 == t1_i)) %>%
              mutate(y_can = ifelse(t1 == t1_i & t2 == t2_i, y, -y))
            if (nrow(sub) < 2) return(NULL)
            # Trim-and-fill requires ≥ 10 studies
            if (nrow(sub) < 10) {
              return(div(style = "font-size:0.87em; color:#6c757d; margin-top:6px;",
                         icon("info-circle"),
                         " Trim-and-fill not shown \u2014 \u2265\u202f10 studies required",
                         paste0(" (k\u202f=\u202f", nrow(sub), ").")))
            }

            tf_info <- tryCatch({
              m  <- metagen(TE = sub$y_can, seTE = sub$se, studlab = sub$studlab,
                            sm = "MD", common = FALSE, random = TRUE)
              tf <- trimfill(m)
              list(
                n_trimmed = tf$k - m$k,
                pooled_te = tf$TE.random,
                pooled_lo = tf$lower.random,
                pooled_hi = tf$upper.random
              )
            }, error = function(e) list(n_trimmed = -1))

            if (tf_info$n_trimmed < 0) return(NULL)
            if (tf_info$n_trimmed == 0) {
              return(div(class = "alert alert-success",
                         style = "font-size:0.87em; margin-top:6px;",
                         icon("check-circle"),
                         strong(" Trim-and-fill: "), "No studies imputed.",
                         " Funnel plot appears symmetric — no evidence of asymmetry."))
            }
            te_str <- paste0(round(tf_info$pooled_te, 3),
                             " [", round(tf_info$pooled_lo, 3),
                             ", ", round(tf_info$pooled_hi, 3), "]")
            div(class = "alert alert-warning",
                style = "font-size:0.87em; margin-top:6px;",
                icon("exclamation-circle"),
                strong(" Trim-and-fill: "),
                paste0(tf_info$n_trimmed,
                       " stud", if (tf_info$n_trimmed == 1) "y" else "ies",
                       " imputed (open red circles in plot)."),
                " Adjusted estimate: ", strong(te_str), ".",
                " Possible publication bias — consider alongside Egger's test.")
          })
        })
      }
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # ------------------------------------------------------------------
    # RETURN
    # ------------------------------------------------------------------
    return(list(
      robmen_results = robmen_results
    ))
  })
}
