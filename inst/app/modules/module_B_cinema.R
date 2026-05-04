# =============================================================================
# Module B: CINeMA Analysis
# =============================================================================
# REFERENCE:
#   Nikolakopoulou A et al. CINeMA: An approach for assessing confidence in
#   the results of a network meta-analysis. PLoS Med. 2020;17(4):e1003082.
#
# DOMAINS:
#   D1 Within-study bias      (contribution-weighted ROB)
#   D2 Reporting bias         (from ROB-MEN Module C)
#   D3 Indirectness           (contribution-weighted indirectness)
#   D4 Imprecision            (zone-based CI analysis)
#   D5 Heterogeneity          (CI vs prediction interval)
#   D6 Incoherence            (SIDE test + zone overlap)
# =============================================================================

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(netmeta)
library(shinycssloaders)

# CINEMA_COLOURS / CINEMA_TEXT / CONFIDENCE_COLOURS defined in utils.R
ROB_BG <- c(
  "low"           = "#E2EFDA",
  "some concerns" = "#FFF2CC",
  "high"          = "#FFE0E0"
)
INDIR_BG <- c(
  "low"           = "#E2EFDA",
  "some concerns" = "#FFF2CC",
  "high"          = "#FFE0E0"
)

DOMAIN_CHOICES <- c(
  "(auto)" = "", "No concerns", "Some concerns",
  "Major concerns", "Not assessed"
)
CONFIDENCE_CHOICES <- c(
  "(select)" = "", "High", "Moderate", "Low", "Very low"
)

# --------------------------------------------------------------------------
# UI FUNCTIONS
# --------------------------------------------------------------------------
# Each CINeMA domain is a top-level tab in app.R; the six moduleB_d{1..6}_ui
# functions below produce the per-tab content. They share a single
# moduleB_server (one set of reactives) via Shiny module namespacing.
# --------------------------------------------------------------------------

moduleB_d1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    wellPanel(
      fluidRow(
        column(6,
          selectInput(ns("d1_judgement"), "Judgement method",
            choices = c(
              "Average (contribution-weighted mean)"  = "average",
              "Majority (largest contribution share)" = "majority",
              "Highest (most severe contributor)"     = "highest",
              "Sensitivity-based (excl. high-RoB)"    = "sens"
            ),
            selected = "average"
          )
        ),
        column(6,
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sens'", ns("d1_judgement")),
            numericInput(ns("sens_inf_thresh"),
                         "Inflation threshold (relative |TE| change)",
                         value = 0.10, min = 0.0, max = 0.5, step = 0.05)
          )
        )
      )
    ),
    uiOutput(ns("d1_debug_msg")),
    h5("Contribution Chart (ROB by direct comparison)"),
    plotlyOutput(ns("d1_contrib_chart"), height = "320px"),
    br(),
    p(strong("Algorithm (Nikolakopoulou 2020):"),
      "ROB scores encoded as low=1, some concerns=2, high=3.",
      "Three aggregation rules selectable above:",
      strong("Average"), "\u2014 contribution-weighted mean;",
      strong("Majority"), "\u2014 category with largest total weight;",
      strong("Highest"), "\u2014 most severe ROB among non-negligible contributors."),
    p(style = "color:#666; font-size:0.9em; margin-bottom:6px;",
      "Each card shows ", strong("TE_all"), " (all studies) and ",
      strong("TE excl. high-RoB"), ". Sensitivity-based judgement: ",
      strong("Major concerns"), " if the sign flips between the two; ",
      strong("No concerns"), " if CI overlap \u2265 80% of mean CI width ",
      "(high-RoB studies do not change the conclusion); ",
      strong("Some concerns"), " if CI overlap < 80% ", em("and"),
      " (statistical significance changes ",
      em("or"), " inflation > threshold); otherwise ",
      strong("No concerns"), "."),
    h5("Domain 1 Ratings \u2014 Auto-computed + Override"),
    shinycssloaders::withSpinner(uiOutput(ns("d1_override_ui")),
      type = 4, color = "#4472C4")
  )
}

moduleB_d2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    moduleC_ui("module_c"),
    hr(),
    h4("Domain 2 (Reporting bias) \u2014 Final Ratings"),
    p(style = "color:#555; font-size:0.9em;",
      "ROB-MEN ratings above populate this domain automatically.",
      " Use the bulk buttons or per-comparison overrides for manual adjustment."),
    div(style = "display:flex; gap:6px; align-items:center; flex-wrap:wrap; margin-bottom:12px;",
      actionButton(ns("d2_set_all_no"), "Set all: No concerns",
        class = "btn btn-success btn-sm"),
      actionButton(ns("d2_set_all_some"), "Set all: Some concerns",
        class = "btn btn-warning btn-sm"),
      actionButton(ns("d2_set_all_major"), "Set all: Major concerns",
        class = "btn btn-danger btn-sm")
    ),
    shinycssloaders::withSpinner(uiOutput(ns("d2_override_ui")),
      type = 4, color = "#4472C4")
  )
}

moduleB_d3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    wellPanel(
      fluidRow(
        column(6,
          selectInput(ns("d3_agg_rule"), "Aggregation rule",
            choices = c(
              "Average (contribution-weighted mean)"  = "average",
              "Majority (largest contribution share)" = "majority",
              "Highest (most severe contributor)"     = "highest"
            ),
            selected = "average"
          )
        )
      )
    ),
    h5("Contribution Chart (indirectness by direct comparison)"),
    plotlyOutput(ns("d3_contrib_chart"), height = "320px"),
    br(),
    p(strong("Algorithm:"),
      "Same as D1 but using study-level indirectness ratings.",
      "Requires 'indirectness' column in the input data."),
    h5("Domain 3 Ratings \u2014 Auto-computed + Override"),
    shinycssloaders::withSpinner(uiOutput(ns("d3_override_ui")),
      type = 4, color = "#4472C4")
  )
}

moduleB_d4_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    wellPanel(
      fluidRow(
        column(6,
          numericInput(ns("delta"),
            "Clinical threshold \u03b4 (ratio for OR/RR; effect size for SMD/MD)",
            value = 0.2, step = 0.05, min = 0.001)
        )
      ),
      p(style = "color:#666; font-size:0.9em; margin-bottom:0;",
        strong("OR/RR:"), " enter \u03b4 on the ratio scale (e.g., 1.2)",
        " \u2014 boundaries [1/\u03b4, \u03b4] are log-transformed internally. ",
        strong("SMD/MD:"), " enter \u03b4 on the effect-size scale (e.g., 0.2).")
    ),
    p(strong("Algorithm:"), "\u03b4 defines zone boundaries \u00b1\u03b4.",
      "Zone A = CI lies entirely in the beneficial direction.",
      "Zone B = CI lies within the equivalence zone [\u2212\u03b4, +\u03b4].",
      "Zone C = CI extends into the unfavourable direction.",
      strong("No concerns:"), "Zone A or B.",
      strong("Some concerns:"), "CI includes null but does not reach Zone C.",
      strong("Major concerns:"), "CI extends into Zone C."),
    shinycssloaders::withSpinner(uiOutput(ns("d4_cards")),
      type = 4, color = "#4472C4")
  )
}

moduleB_d5_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    p(strong("Algorithm:"),
      "Count how many zone boundaries (\u00b1\u03b4) the CI and PrI each cross.",
      "PrI crossings \u2212 CI crossings = 0 \u2192 No concerns; 1 \u2192 Some concerns; 2 \u2192 Major concerns.",
      "Common-effects model: No concerns (\u03c4\u00b2 = 0)."),
    shinycssloaders::withSpinner(uiOutput(ns("d5_cards")),
      type = 4, color = "#4472C4")
  )
}

moduleB_d6_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nma_error_banner")),
    p(strong("Algorithm (Nikolakopoulou 2020):"),
      strong("Both direct & indirect evidence:"),
      "SIDE p > 0.10 \u2192 No concerns; otherwise count zones shared by",
      "the direct and indirect CIs (3 \u2192 No, 2 \u2192 Some, \u22641 \u2192 Major).",
      strong("Only direct OR only indirect evidence:"),
      "global design-by-treatment interaction test \u2014",
      "p > 0.10 \u2192 No concerns; 0.05 < p \u2264 0.10 \u2192 Some concerns;",
      "p \u2264 0.05 \u2192 Major concerns.",
      strong("Closed loops absent (test cannot be computed):"),
      "Major concerns.",
      em("D6 is fully algorithm-driven. Override only when expert judgement differs.")),
    shinycssloaders::withSpinner(uiOutput(ns("d6_cards")),
      type = 4, color = "#4472C4")
  )
}

# --------------------------------------------------------------------------
# SERVER FUNCTION
# --------------------------------------------------------------------------
moduleB_server <- function(id, processed_data,
                           nma_settings = NULL,
                           run_trigger  = NULL,
                           go_to_robmen = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Settings accessors — single source of truth is Module A's
    # nma_settings reactive. Effect measure / reference treatment /
    # model type are configured on the Configuration tab; δ lives on
    # the D4 tab; D1 and D3 judgement controls live in their own tabs.
    # ------------------------------------------------------------------
    cur_settings <- function() {
      tryCatch(nma_settings(), error = function(e) NULL)
    }
    sm_get <- function() {
      s <- cur_settings()
      em <- if (!is.null(s)) s$effect_measure else NULL
      em %||% "SMD"
    }
    mt_get <- function() {
      s <- cur_settings()
      mt <- if (!is.null(s)) s$model_type else NULL
      mt %||% "random"
    }
    ref_get <- function() {
      s <- cur_settings()
      r <- if (!is.null(s)) s$ref_treatment else NULL
      r
    }
    delta_get <- function() {
      d <- input$delta
      if (is.null(d) || is.na(d)) {
        s <- cur_settings()
        d <- if (!is.null(s)) s$delta else NULL
      }
      d %||% 0.2
    }

    # Sync D-tab judgement inputs from Module A's nma_settings when run
    # fires (legacy keys kept for backwards compatibility).
    if (!is.null(nma_settings)) {
      observeEvent(run_trigger(), {
        s <- cur_settings()
        if (is.null(s)) return()
        if (!is.null(s$delta) && !is.na(s$delta))
          updateNumericInput(session, "delta",          value    = s$delta)
        if (!is.null(s$agg_rule)) {
          updateSelectInput(session, "d3_agg_rule",     selected = s$agg_rule)
          updateSelectInput(session, "d1_judgement",    selected = s$agg_rule)
        }
        if (!is.null(s$d3_agg_rule))
          updateSelectInput(session, "d3_agg_rule",     selected = s$d3_agg_rule)
        if (!is.null(s$d1_judgement))
          updateSelectInput(session, "d1_judgement",    selected = s$d1_judgement)
        if (!is.null(s$d1_method) && identical(s$d1_method, "sens"))
          updateSelectInput(session, "d1_judgement",    selected = "sens")
        if (!is.null(s$sens_inf_thresh) && !is.na(s$sens_inf_thresh))
          updateNumericInput(session, "sens_inf_thresh", value   = s$sens_inf_thresh)
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }

    # ------------------------------------------------------------------
    # ROB-MEN integration: reactiveVal updated by Module C
    # ------------------------------------------------------------------
    robmen_ratings <- reactiveVal(NULL)

    set_robmen <- function(robmen_df) {
      # Build a lookup that handles both label orderings ("A vs B" and "B vs A")
      rating_lookup <- stats::setNames(robmen_df$robmen_rating, robmen_df$comparison)
      rev_labels    <- sapply(robmen_df$comparison, function(x) {
        parts <- strsplit(x, " vs ", fixed = TRUE)[[1]]
        if (length(parts) == 2) paste(parts[2], parts[1], sep = " vs ") else x
      })
      rating_lookup_rev <- stats::setNames(robmen_df$robmen_rating, rev_labels)
      combined_lookup   <- c(rating_lookup, rating_lookup_rev)

      robmen_df$domain2 <- dplyr::recode(
        robmen_df$robmen_rating,
        "Low risk"      = "No concerns",
        "Some concerns" = "Some concerns",
        "High risk"     = "Major concerns",
        .default        = "Not assessed"
      )
      robmen_ratings(robmen_df)
    }

    # Helper used in auto_domains(): look up ROB-MEN rating for a comparison,
    # trying both label orderings.
    lookup_robmen_rating <- function(cmp, rr) {
      if (is.null(rr)) return("Not assessed")
      idx <- which(rr$comparison == cmp)
      if (length(idx) == 0) {
        parts <- strsplit(cmp, " vs ", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
          rev_cmp <- paste(parts[2], parts[1], sep = " vs ")
          idx <- which(rr$comparison == rev_cmp)
        }
      }
      if (length(idx) == 0) return("Not assessed")
      as.character(rr$robmen_rating[idx[1]])
    }

    # ------------------------------------------------------------------
    # NMA computation — fires on local button OR Module A run_trigger
    # ------------------------------------------------------------------
    run_counter <- reactiveVal(0)
    observeEvent(
      if (!is.null(run_trigger)) run_trigger() else NULL,
      { run_counter(run_counter() + 1) },
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

    nma_result <- eventReactive(run_counter(), {
      res <- processed_data()
      req(!is.null(res), !is.null(res$data), nrow(res$data) > 0)
      df  <- res$data

      # All NMA-level settings come from Module A's Configuration tab.
      sm_use <- sm_get()
      mt_use <- mt_get()

      ref_trt <- ref_get()
      if (is.null(ref_trt) || !nzchar(ref_trt)) ref_trt <- df$t1[1]
      req(ref_trt %in% c(df$t1, df$t2))

      withProgress(message = "Running NMA...", value = 0.2, {

        net_err <- NULL
        net <- tryCatch(
          netmeta(
            TE       = df$y,
            seTE     = df$se,
            treat1   = df$t1,
            treat2   = df$t2,
            studlab  = df$studlab,
            reference.group     = ref_trt,
            sm                  = sm_use,
            common              = (mt_use == "common"),
            random              = (mt_use == "random"),
            details.chkmultiarm = FALSE
          ),
          error = function(e) { net_err <<- conditionMessage(e); NULL }
        )
        if (is.null(net)) return(list(error = net_err))

        setProgress(value = 0.5, message = "Computing contribution matrix...")
        contrib_err <- NULL
        contrib <- tryCatch(
          netcontrib(net),
          error   = function(e) { contrib_err <<- conditionMessage(e); NULL },
          warning = function(w) suppressWarnings(netcontrib(net))
        )

        setProgress(value = 0.8, message = "Computing inconsistency...")
        nsplit <- tryCatch(netsplit(net), error = function(e) NULL)

        setProgress(value = 1.0, message = "Done.")
        showNotification(
          tagList(
            icon("check-circle", class = "text-success"),
            strong(" CINeMA analysis complete."),
            tags$br(),
            "\u2192 Review results in each Domain tab."
          ),
          type = "message", duration = 10
        )
        list(net = net, contrib = contrib, contrib_err = contrib_err,
             nsplit = nsplit, df = df)
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ------------------------------------------------------------------
    # All pairwise comparisons (canonical t1 < t2 alphabetical ordering)
    # ------------------------------------------------------------------
    comps_df <- reactive({
      res <- nma_result()
      req(!is.null(res), !is.null(res$net))
      net  <- res$net
      trts <- sort(net$trts)
      validate(need(length(trts) >= 2, "Need at least 2 treatments."))
      pairs <- combn(trts, 2, simplify = FALSE)
      data.frame(
        t1 = sapply(pairs, `[[`, 1),
        t2 = sapply(pairs, `[[`, 2),
        stringsAsFactors = FALSE
      )
    })

    # ------------------------------------------------------------------
    # Auto-computed domains (reactive)
    # ------------------------------------------------------------------
    auto_domains <- reactive({
      res  <- nma_result()
      req(!is.null(res), !is.null(res$net))

      net        <- res$net
      contrib    <- res$contrib
      nsplit     <- res$nsplit
      df         <- res$df
      comps      <- comps_df()
      model_type <- if (mt_get() == "random") "random" else "common"
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      # Domain 1: Within-study bias — single dropdown selects the rule
      d1_choice <- input$d1_judgement %||% "average"
      if (identical(d1_choice, "sens")) {
        inf_t <- if (is.null(input$sens_inf_thresh) || is.na(input$sens_inf_thresh))
                   0.10 else input$sens_inf_thresh
        d1 <- compute_domain1_wsb_sens(comps, contrib, df,
                                       inf_threshold = inf_t,
                                       small_values  = NULL)
      } else {
        d1 <- compute_domain1_wsb(comps, contrib, df, d1_choice)
      }

      # Domain 2: Reporting bias (from ROB-MEN or "Not assessed")
      # lookup_robmen_rating() tries both "A vs B" and "B vs A" orderings.
      rr <- robmen_ratings()
      d2 <- data.frame(
        comparison = comp_labels,
        domain2    = sapply(comp_labels, function(cmp) {
          rating <- lookup_robmen_rating(cmp, rr)
          dplyr::recode(rating,
            "Low risk"      = "No concerns",
            "Some concerns" = "Some concerns",
            "High risk"     = "Major concerns",
            .default        = "Not assessed"
          )
        }),
        stringsAsFactors = FALSE
      )

      # Domain 3: Indirectness (contribution-weighted)
      d3 <- if ("indirectness" %in% names(df)) {
        compute_domain3_indirectness(comps, contrib, df,
                                     input$d3_agg_rule %||% "average")
      } else {
        data.frame(comparison = comp_labels,
                   domain3    = rep("Not assessed", nrow(comps)),
                   stringsAsFactors = FALSE)
      }

      # Domains 4-6 — for OR/RR, delta is entered as a ratio (e.g. 1.2) and must be
      # log-transformed because netmeta stores effects on the log scale internally.
      is_ratio_em <- sm_get() %in% c("OR", "RR")
      delta_in    <- delta_get()
      delta_used  <- if (is_ratio_em) log(delta_in) else delta_in
      d4 <- compute_domain4_imprecision(comps, net, model_type, delta_used)
      d5 <- compute_domain5_heterogeneity(comps, net, model_type, delta_used)
      d6 <- compute_domain6_incoherence(comps, nsplit, net, delta_used)

      # Evidence type: "mixed" if direct evidence exists in netsplit, else "indirect"
      evidence_type <- sapply(seq_len(nrow(comps)), function(i) {
        if (is.null(nsplit)) return("mixed")
        t1i <- comps$t1[i]; t2i <- comps$t2[i]
        lbls <- nsplit$comparison
        has_direct <- any(lbls == paste(t1i, t2i, sep = ":") |
                          lbls == paste(t2i, t1i, sep = ":"))
        if (has_direct) "mixed" else "indirect"
      })

      # Number of direct studies per comparison
      n_studies <- sapply(seq_len(nrow(comps)), function(i) {
        t1i <- comps$t1[i]; t2i <- comps$t2[i]
        sum((df$t1 == t1i & df$t2 == t2i) | (df$t1 == t2i & df$t2 == t1i))
      })

      list(
        d1 = d1, d2 = d2, d3 = d3, d4 = d4, d5 = d5, d6 = d6,
        comps = comps, net = net, contrib = contrib, nsplit = nsplit,
        df = df, model_type = model_type,
        evidence_type = evidence_type, n_studies = n_studies
      )
    })

    # ------------------------------------------------------------------
    # Effective cinema results: auto + manual overrides
    # ------------------------------------------------------------------
    cinema_results <- reactive({
      ad    <- auto_domains()
      req(!is.null(ad))

      comps       <- ad$comps
      n           <- nrow(comps)
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      dn        <- c("within_study_bias", "reporting_bias", "indirectness",
                     "imprecision", "heterogeneity", "incoherence")
      auto_col  <- c("domain1","domain2","domain3","domain4","domain5","domain6")
      ov_prefix <- c("ov_d1","ov_d2","ov_d3","ov_d4","ov_d5","ov_d6")
      auto_dfs  <- list(ad$d1, ad$d2, ad$d3, ad$d4, ad$d5, ad$d6)

      merged <- data.frame(
        comparison    = comp_labels,
        evidence_type = ad$evidence_type,
        n_studies     = ad$n_studies,
        stringsAsFactors = FALSE
      )

      for (di in seq_along(dn)) {
        merged[[dn[di]]] <- sapply(seq_len(n), function(i) {
          ov <- input[[paste0(ov_prefix[di], "_", i)]]
          if (!is.null(ov) && nzchar(ov)) ov else
            auto_dfs[[di]][[auto_col[di]]][i]
        })
      }

      # Suggested confidence (D1-D6, anti-double-counting)
      # Algorithm is selected in Module D; use standard here as a baseline.
      algo <- "standard"
      merged$suggested_confidence <- sapply(seq_len(n), function(i) {
        compute_auto_confidence(as.list(merged[i, ]), algorithm = algo)
      })

      # Final confidence: manual only
      merged$confidence <- sapply(seq_len(n), function(i) {
        conf <- input[[paste0("conf_", i)]]
        if (!is.null(conf) && nzchar(conf)) conf else ""
      })

      # Downgrade reason (overall, from confidence panel)
      merged$downgrade_reason <- sapply(seq_len(n), function(i) {
        r <- input[[paste0("reason_", i)]]
        if (!is.null(r)) r else ""
      })

      # Per-domain override reasons
      dn_reason_prefix <- c("reason_d1", "reason_d2", "reason_d3",
                            "reason_d4", "reason_d5", "reason_d6")
      dn_reason_col    <- paste0(dn, "_reason")
      for (di in seq_along(dn)) {
        rp <- dn_reason_prefix[di]
        merged[[dn_reason_col[di]]] <- sapply(seq_len(n), function(i) {
          r <- input[[paste0(rp, "_", i)]]
          if (!is.null(r) && nzchar(r)) r else ""
        })
      }

      # Network estimates for all pairs
      net <- ad$net
      te_df <- data.frame(
        comparison = comp_labels,
        TE    = sapply(seq_len(n), function(i) tryCatch(
          if (ad$model_type == "random") net$TE.random[comps$t1[i], comps$t2[i]]
          else net$TE.common[comps$t1[i], comps$t2[i]],
          error = function(e) NA_real_)),
        lower = sapply(seq_len(n), function(i) tryCatch(
          if (ad$model_type == "random") net$lower.random[comps$t1[i], comps$t2[i]]
          else net$lower.common[comps$t1[i], comps$t2[i]],
          error = function(e) NA_real_)),
        upper = sapply(seq_len(n), function(i) tryCatch(
          if (ad$model_type == "random") net$upper.random[comps$t1[i], comps$t2[i]]
          else net$upper.common[comps$t1[i], comps$t2[i]],
          error = function(e) NA_real_)),
        stringsAsFactors = FALSE
      )

      list(
        merged = merged, te_df = te_df, net = net,
        contrib = ad$contrib, df = ad$df, model_type = ad$model_type,
        nsplit = ad$nsplit
      )
    })

    # ------------------------------------------------------------------
    # OUTPUT: NMA status / error banner — embedded at the top of every
    # domain tab. Returns NULL when NMA succeeded and no run is required,
    # otherwise renders an info or danger alert.
    # ------------------------------------------------------------------
    output$nma_error_banner <- renderUI({
      res <- nma_result()
      if (is.null(res)) {
        return(div(class = "alert alert-info",
                   icon("info-circle"),
                   " Configure data and settings on the Configuration tab,",
                   " then click Run Analysis to populate this domain."))
      }
      if (!is.null(res$error) && is.null(res$net)) {
        return(div(class = "alert alert-danger",
                   icon("exclamation-triangle"),
                   strong(" NMA Error: "), res$error))
      }
      NULL
    })

    # ------------------------------------------------------------------
    # OUTPUT: Confidence rating selectors
    # ------------------------------------------------------------------
    output$confidence_ui <- renderUI({
      cr <- cinema_results(); req(!is.null(cr))
      comp_labels <- cr$merged$comparison
      ev_types    <- cr$merged$evidence_type

      make_conf_row <- function(i) {
        fluidRow(style = "margin-bottom:4px;",
          column(5, comp_labels[i],
            span(style = "margin-left:6px; font-size:0.8em; color:#888;",
                 paste0("(", ev_types[i], ")"))),
          column(3,
            selectInput(ns(paste0("conf_", i)), label = NULL,
              choices = CONFIDENCE_CHOICES, selected = "", width = "100%")),
          column(4,
            textInput(ns(paste0("reason_", i)), label = NULL,
              placeholder = "Reason for downgrading", width = "100%"))
        )
      }

      mixed_idx <- which(ev_types == "mixed")
      indir_idx <- which(ev_types == "indirect")

      tagList(
        if (length(mixed_idx) > 0) tagList(
          h5(style = "border-bottom:1px solid #ccc; padding-bottom:4px;",
             "Mixed evidence"),
          lapply(mixed_idx, make_conf_row)
        ),
        if (length(indir_idx) > 0) tagList(
          h5(style = "margin-top:16px; border-bottom:1px solid #ccc; padding-bottom:4px;",
             "Indirect evidence only"),
          lapply(indir_idx, make_conf_row)
        )
      )
    })

    # ------------------------------------------------------------------
    # D1 debug message
    # ------------------------------------------------------------------
    output$d1_debug_msg <- renderUI({
      res <- nma_result(); req(!is.null(res))
      cm  <- get_contrib_matrix(res$contrib)

      if (!is.null(res$contrib_err)) {
        return(div(class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong(" netcontrib() error: "), res$contrib_err, br(),
          "D1 and D3 will show 'Not assessed'."))
      }
      if (is.null(res$contrib)) {
        return(div(class = "alert alert-warning",
          icon("exclamation-circle"),
          strong(" Contribution matrix unavailable. "),
          "D1 and D3 cannot be computed automatically."))
      }
      if (is.null(cm)) {
        flds <- paste(names(res$contrib), collapse = ", ")
        return(div(class = "alert alert-warning",
          icon("exclamation-circle"),
          strong(" Unrecognized netcontrib() structure. "),
          "Fields: ", flds))
      }
      div(class = "alert alert-success",
        icon("check-circle"),
        strong(" Contribution matrix loaded. "),
        paste0(nrow(cm), " network comparisons × ", ncol(cm), " direct comparisons."),
        " Row format: ", em(rownames(cm)[1]))
    })

    # ------------------------------------------------------------------
    # D1 contribution chart (per-study)
    # Stacked horizontal bar: for each NMA comparison, study-level
    # contribution split into Low | Some concerns | High based on each
    # study's own ROB (not direct-comparison mean). Hover lists studies.
    # ------------------------------------------------------------------
    output$d1_contrib_chart <- renderPlotly({
      res <- nma_result(); req(!is.null(res), !is.null(res$df))
      cm  <- get_contrib_matrix(res$contrib)
      req(!is.null(cm))
      ad  <- auto_domains(); req(!is.null(ad))

      df    <- res$df
      comps <- ad$comps
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      rob_num <- c("low" = 0, "some concerns" = 1, "high" = 2)
      df$rnk  <- rob_num[as.character(df$rob)]
      df_iv   <- build_study_iv_table(df)

      plot_rows <- lapply(seq_len(nrow(comps)), function(i) {
        sc <- study_contrib_per_target(df_iv, cm, comps$t1[i], comps$t2[i])
        if (is.null(sc)) return(NULL)
        sc <- sc[!is.na(sc$rnk) & sc$study_contrib > 1e-6, , drop = FALSE]
        if (nrow(sc) == 0) return(NULL)
        sc$rob_label <- rob_num_to_label(sc$rnk)
        data.frame(
          nma_comp    = comp_labels[i],
          rob_label   = sc$rob_label,
          contrib_pct = sc$study_contrib * 100,
          studlab     = sc$studlab,
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()

      req(!is.null(plot_rows), nrow(plot_rows) > 0)

      # Multi-arm studies contribute through multiple pairwise rows for the same
      # (target, ROB-category): merge them so each study is one segment.
      plot_rows <- plot_rows %>%
        group_by(nma_comp, studlab, rob_label) %>%
        summarise(contrib_pct = sum(contrib_pct, na.rm = TRUE),
                  .groups = "drop")

      # Order within each bar: by ROB category, then by descending contribution
      # so larger studies anchor each colour band. Each study stays its own
      # segment with a thin white border separating studies.
      all_levels <- c("Low", "Some concerns", "High")
      plot_rows <- plot_rows %>%
        mutate(rob_label = factor(rob_label, levels = rev(all_levels))) %>%
        group_by(nma_comp) %>%
        arrange(rob_label, desc(contrib_pct), .by_group = TRUE) %>%
        ungroup() %>%
        mutate(
          stack_id = paste(nma_comp, studlab, rob_label, sep = "||"),
          stack_id = factor(stack_id, levels = unique(stack_id))
        )
      rob_colours <- c("Low" = "#5cb85c", "Some concerns" = "#f0ad4e", "High" = "#d9534f")

      p <- ggplot(plot_rows,
                  aes(x = nma_comp, y = contrib_pct, fill = rob_label,
                      group = stack_id,
                      text = paste0(nma_comp,
                                    "<br>Study: ", studlab,
                                    "<br>ROB: ", rob_label,
                                    "<br>Contribution: ", round(contrib_pct, 1), "%"))) +
        geom_col(position = "stack", colour = "white", linewidth = 0.4) +
        scale_fill_manual(values = rob_colours, name = "Risk of bias",
                          limits = c("Low", "Some concerns", "High"),
                          drop = FALSE) +
        coord_flip() +
        labs(x = NULL, y = "Contribution (%)",
             title = "D1: Per-study contribution by Risk of Bias") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.15,
                             traceorder = "normal"))
    })

    # ------------------------------------------------------------------
    # D3 contribution chart (per-study)
    # Stacked horizontal bar: same logic as D1 but for indirectness.
    # ------------------------------------------------------------------
    output$d3_contrib_chart <- renderPlotly({
      res <- nma_result(); req(!is.null(res), !is.null(res$df))
      cm  <- get_contrib_matrix(res$contrib)
      req(!is.null(cm))
      ad  <- auto_domains(); req(!is.null(ad))
      df  <- res$df
      req("indirectness" %in% names(df))

      comps <- ad$comps
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      indir_num  <- c("low" = 0, "some concerns" = 1, "high" = 2)
      df$indir_n <- indir_num[as.character(df$indirectness)]
      df_iv      <- build_study_iv_table(df)

      plot_rows <- lapply(seq_len(nrow(comps)), function(i) {
        sc <- study_contrib_per_target(df_iv, cm, comps$t1[i], comps$t2[i])
        if (is.null(sc)) return(NULL)
        sc <- sc[!is.na(sc$indir_n) & sc$study_contrib > 1e-6, , drop = FALSE]
        if (nrow(sc) == 0) return(NULL)
        sc$indir_label <- rob_num_to_label(sc$indir_n)
        data.frame(
          nma_comp    = comp_labels[i],
          indir_label = sc$indir_label,
          contrib_pct = sc$study_contrib * 100,
          studlab     = sc$studlab,
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()

      req(!is.null(plot_rows), nrow(plot_rows) > 0)

      # Multi-arm studies contribute through multiple pairwise rows for the same
      # (target, indirectness-category): merge so each study is one segment.
      plot_rows <- plot_rows %>%
        group_by(nma_comp, studlab, indir_label) %>%
        summarise(contrib_pct = sum(contrib_pct, na.rm = TRUE),
                  .groups = "drop")

      all_levels <- c("Low", "Some concerns", "High")
      plot_rows <- plot_rows %>%
        mutate(indir_label = factor(indir_label, levels = rev(all_levels))) %>%
        group_by(nma_comp) %>%
        arrange(indir_label, desc(contrib_pct), .by_group = TRUE) %>%
        ungroup() %>%
        mutate(
          stack_id = paste(nma_comp, studlab, indir_label, sep = "||"),
          stack_id = factor(stack_id, levels = unique(stack_id))
        )
      indir_colours <- c("Low" = "#5cb85c", "Some concerns" = "#f0ad4e", "High" = "#d9534f")

      p <- ggplot(plot_rows,
                  aes(x = nma_comp, y = contrib_pct, fill = indir_label,
                      group = stack_id,
                      text = paste0(nma_comp,
                                    "<br>Study: ", studlab,
                                    "<br>Indirectness: ", indir_label,
                                    "<br>Contribution: ", round(contrib_pct, 1), "%"))) +
        geom_col(position = "stack", colour = "white", linewidth = 0.4) +
        scale_fill_manual(values = indir_colours, name = "Indirectness",
                          limits = c("Low", "Some concerns", "High"),
                          drop = FALSE) +
        coord_flip() +
        labs(x = NULL, y = "Contribution (%)",
             title = "D3: Per-study contribution by Indirectness") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.15,
                             traceorder = "normal"))
    })

    # ------------------------------------------------------------------
    # D1 outputs
    # ------------------------------------------------------------------
    output$d1_override_ui <- renderUI({
      ad <- auto_domains(); req(!is.null(ad))
      cr <- cinema_results(); req(!is.null(cr))
      comp_labels <- paste(ad$comps$t1, ad$comps$t2, sep = " vs ")

      df <- cr$df
      # processed_data uses y/se (not TE/seTE) for the pairwise effect/SE.
      te_col <- if ("TE"   %in% names(df)) "TE"   else if ("y"  %in% names(df)) "y"  else NA_character_
      se_col <- if ("seTE" %in% names(df)) "seTE" else if ("se" %in% names(df)) "se" else NA_character_
      need_ok <- !is.na(te_col) && !is.na(se_col) &&
                 all(c("t1", "t2", "rob") %in% names(df))
      if (!need_ok) {
        return(render_override_ui(ns, "ov_d1", comp_labels, ad$d1$domain1,
                                  evidence_types = ad$evidence_type,
                                  reason_prefix = "reason_d1"))
      }
      pick_sm <- function(x) {
        if (is.null(x)) return(NA_character_)
        v <- suppressWarnings(as.character(x))
        if (length(v) == 0) return(NA_character_)
        v <- v[1]
        if (is.na(v) || !nzchar(v)) NA_character_ else v
      }
      sm_settings <- tryCatch(nma_settings()$effect_measure,
                              error = function(e) NULL)
      sm_val <- pick_sm(cr$net$sm)
      if (is.na(sm_val)) sm_val <- pick_sm(sm_settings)
      if (is.na(sm_val)) sm_val <- pick_sm(sm_get())
      if (is.na(sm_val)) sm_val <- ""
      inf_t  <- if (is.null(input$sens_inf_thresh) || is.na(input$sens_inf_thresh))
                  0.10 else input$sens_inf_thresh

      sens_extra <- function(i) {
        out <- tryCatch({
          t1 <- ad$comps$t1[i]; t2 <- ad$comps$t2[i]
          sub <- df[(df$t1 == t1 & df$t2 == t2) |
                    (df$t1 == t2 & df$t2 == t1), , drop = FALSE]
          if (nrow(sub) == 0)
            return(p(style = "margin:4px 0; color:#666;",
                     em("No direct evidence — judgement carried over from contributing comparisons.")))

          te_raw     <- sub[[te_col]]
          se_raw     <- sub[[se_col]]
          te_aligned <- ifelse(sub$t1 == t1, te_raw, -te_raw)
          v <- judge_rob_direct_sens_v(
            rob_vec       = as.character(sub$rob),
            te_vec        = te_aligned,
            se_vec        = se_raw,
            inf_threshold = inf_t,
            small_values  = NULL
          )

          n_high <- sum(as.character(sub$rob) == "high", na.rm = TRUE)
          te_all_lo <- if (!is.na(v$te_all) && !is.na(v$se_all))
                         v$te_all - 1.96 * v$se_all else NA_real_
          te_all_hi <- if (!is.na(v$te_all) && !is.na(v$se_all))
                         v$te_all + 1.96 * v$se_all else NA_real_
          te_low_lo <- if (!is.na(v$te_low) && !is.na(v$se_low))
                         v$te_low - 1.96 * v$se_low else NA_real_
          te_low_hi <- if (!is.na(v$te_low) && !is.na(v$se_low))
                         v$te_low + 1.96 * v$se_low else NA_real_

          te_all_str <- format_te_ci(v$te_all, te_all_lo, te_all_hi, sm_val)
          te_low_str <- format_te_ci(v$te_low, te_low_lo, te_low_hi, sm_val)

          flag_inflation <- !is.na(v$inflation) && v$inflation >= inf_t
          flag_flip      <- isTRUE(v$sign_flip)

          infl_text <- if (flag_flip) {
            tags$span(
              title = "Sign reversal: pooled TE flips when high-RoB studies are excluded",
              style = "background:#f8d7da; color:#721c24; padding:1px 6px;
                       border-radius:3px; font-weight:bold;",
              "sign flip ⚠")
          } else if (is.na(v$inflation)) {
            tags$span(style = "color:#888;", "—")
          } else {
            pct <- sprintf("%+.1f%%", v$inflation * 100)
            if (flag_inflation)
              tags$span(
                title = paste0("Inflation ≥ ", round(inf_t * 100), "% threshold"),
                style = "background:#fff3cd; color:#856404; padding:1px 6px;
                         border-radius:3px; font-weight:bold;",
                pct, " ⚠")
            else
              tags$span(style = "color:#198754;", pct)
          }

          # Conclusion-change indicators
          flip_badge <- if (isTRUE(v$sign_flip))
            tags$span(style = "background:#f8d7da; color:#721c24;
                      padding:1px 6px; border-radius:3px; font-weight:bold;
                      margin-left:6px;", "sign flip")
          else NULL
          sig_badge <- if (isTRUE(v$sig_changed))
            tags$span(style = "background:#fff3cd; color:#856404;
                      padding:1px 6px; border-radius:3px; font-weight:bold;
                      margin-left:6px;", "significance change")
          else NULL

          ovlp_text <- if (!is.na(v$overlap_ratio))
            paste0(sprintf("%.0f%%", v$overlap_ratio * 100), " of mean CI width")
          else "—"
          ovlp_color <- if (!is.na(v$overlap_ratio) && v$overlap_ratio >= 0.8)
            "#198754" else "#856404"

          tagList(
            p(style = "margin:2px 0;",
              "TE_all (k = ", nrow(sub), "): ", strong(te_all_str)),
            p(style = "margin:2px 0;",
              "TE excl. high-RoB (k = ",
              max(nrow(sub) - n_high, 0L), "): ",
              if (is.na(v$te_low)) tags$span(style = "color:#888;", "—")
              else strong(te_low_str)),
            p(style = "margin:2px 0;",
              "Inflation: ", infl_text,
              tags$span(style = "color:#666; margin-left:10px;",
                        "CI overlap: ",
                        tags$span(style = paste0("color:", ovlp_color, ";"),
                                  ovlp_text)),
              flip_badge, sig_badge)
          )
        }, error = function(e)
          p(style = "margin:4px 0; color:#888; font-size:0.85em;",
            em(paste("Sensitivity TE not computed:", conditionMessage(e)))))
        out
      }

      render_domain_cards(
        n              = nrow(ad$comps),
        comps          = ad$comps,
        evidence_type  = ad$evidence_type,
        auto_vals      = ad$d1$domain1,
        ns_func        = ns,
        ov_prefix      = "ov_d1",
        card_extra     = sens_extra,
        reason_prefix  = "reason_d1",
        selected_vals  = ad$d1$domain1
      )
    })

    # ------------------------------------------------------------------
    # D2 outputs + set-all observers
    # ------------------------------------------------------------------
    output$d2_override_ui <- renderUI({
      ad <- auto_domains(); req(!is.null(ad))
      comp_labels <- paste(ad$comps$t1, ad$comps$t2, sep = " vs ")
      render_override_ui(ns, "ov_d2", comp_labels, ad$d2$domain2,
                         evidence_types = ad$evidence_type,
                         na_label = "indirect: N/A",
                         reason_prefix = "reason_d2")
    })

    d2_set_all <- function(val) {
      ad <- tryCatch(auto_domains(), error = function(e) NULL)
      if (is.null(ad)) return()
      for (i in seq_len(nrow(ad$comps)))
        updateSelectInput(session, paste0("ov_d2_", i), selected = val)
    }
    observeEvent(input$d2_set_all_no,    d2_set_all("No concerns"))
    observeEvent(input$d2_set_all_some,  d2_set_all("Some concerns"))
    observeEvent(input$d2_set_all_major, d2_set_all("Major concerns"))

    # ------------------------------------------------------------------
    # D3 outputs
    # ------------------------------------------------------------------
    output$d3_override_ui <- renderUI({
      ad <- auto_domains(); req(!is.null(ad))
      comp_labels <- paste(ad$comps$t1, ad$comps$t2, sep = " vs ")
      render_override_ui(ns, "ov_d3", comp_labels, ad$d3$domain3,
                         evidence_types = ad$evidence_type,
                         reason_prefix = "reason_d3")
    })

    # ------------------------------------------------------------------
    # D4 per-comparison cards (Imprecision)
    # ------------------------------------------------------------------
    output$d4_cards <- renderUI({
      cr <- cinema_results(); req(!is.null(cr))
      ad <- auto_domains();   req(!is.null(ad))
      n  <- nrow(ad$comps)
      te_df <- cr$te_df
      em        <- sm_get()
      is_ratio  <- em %in% c("OR", "RR")
      delta_in  <- delta_get()
      delta <- if (is_ratio) log(delta_in) else delta_in
      fmt_est   <- function(x) if (is_ratio) round(exp(x), 3) else round(x, 3)
      est_label <- if (is_ratio) em else "NMA estimate"
      ci_unit   <- if (is_ratio) paste0(" (", em, " scale)") else ""

      render_domain_cards(
        n, ad$comps, cr$merged$evidence_type, ad$d4$domain4, ns, "ov_d4",
        card_extra = function(i) {
          te <- te_df$TE[i]; lo <- te_df$lower[i]; hi <- te_df$upper[i]
          if (any(is.na(c(te, lo, hi)))) return(NULL)
          tagList(
            p(style = "margin:2px 0;",
              est_label, ": ", strong(fmt_est(te)),
              "  95% CI", ci_unit, ": [", fmt_est(lo), ", ", fmt_est(hi), "]"),
            p(style = "margin:2px 0; font-style:italic; font-size:0.9em;",
              imprecision_zone_text(te, lo, hi, delta, is_ratio = is_ratio))
          )
        },
        reason_prefix = "reason_d4"
      )
    })

    # ------------------------------------------------------------------
    # D5 per-comparison cards (Heterogeneity)
    # ------------------------------------------------------------------
    output$d5_cards <- renderUI({
      cr  <- cinema_results(); req(!is.null(cr))
      ad  <- auto_domains();   req(!is.null(ad))
      n   <- nrow(ad$comps)
      net    <- ad$net
      has_pri <- !is.null(net$lower.predict) && is.matrix(net$lower.predict)
      em_d5      <- sm_get()
      is_ratio_5 <- em_d5 %in% c("OR", "RR")
      delta_in_5 <- delta_get()
      delta  <- if (is_ratio_5) log(delta_in_5) else delta_in_5
      fv5        <- function(x) if (is_ratio_5 && !is.na(x)) round(exp(x), 3) else round(x, 3)
      ci_unit_5  <- if (is_ratio_5) paste0(" (", em_d5, ")") else ""

      render_domain_cards(
        n, ad$comps, cr$merged$evidence_type, ad$d5$domain5, ns, "ov_d5",
        card_extra = function(i) {
          t1i <- ad$comps$t1[i]; t2i <- ad$comps$t2[i]
          lo_ci <- tryCatch(net$lower.random[t1i, t2i], error = function(e) NA_real_)
          hi_ci <- tryCatch(net$upper.random[t1i, t2i], error = function(e) NA_real_)
          lo_pri <- if (has_pri) tryCatch(net$lower.predict[t1i, t2i], error = function(e) NA_real_) else NA_real_
          hi_pri <- if (has_pri) tryCatch(net$upper.predict[t1i, t2i], error = function(e) NA_real_) else NA_real_
          tagList(
            if (!is.na(lo_ci)) p(style = "margin:2px 0;",
              "95% CI", ci_unit_5, ": [", fv5(lo_ci), ", ", fv5(hi_ci), "]"),
            if (!is.na(lo_pri)) p(style = "margin:2px 0;",
              "95% PrI", ci_unit_5, ": [", fv5(lo_pri), ", ", fv5(hi_pri), "]"),
            p(style = "margin:2px 0; font-style:italic; font-size:0.9em;",
              heterogeneity_zone_text(lo_ci, hi_ci, lo_pri, hi_pri, delta))
          )
        },
        reason_prefix = "reason_d5"
      )
    })

    # ------------------------------------------------------------------
    # D6 per-comparison cards (Incoherence — fully auto-evaluated)
    # ------------------------------------------------------------------
    output$d6_cards <- renderUI({
      cr     <- cinema_results(); req(!is.null(cr))
      ad     <- auto_domains();   req(!is.null(ad))
      n      <- nrow(ad$comps)
      nsplit <- ad$nsplit
      em_d6      <- sm_get()
      is_ratio_6 <- em_d6 %in% c("OR", "RR")
      fv6        <- function(x) if (is_ratio_6 && !is.na(x)) round(exp(x), 3) else round(x, 3)
      ci_unit_6  <- if (is_ratio_6) paste0(" (", em_d6, ")") else ""

      render_domain_cards(
        n, ad$comps, cr$merged$evidence_type, ad$d6$domain6, ns, "ov_d6",
        reason_prefix  = "reason_d6",
        selected_vals  = ad$d6$domain6,
        card_extra = function(i) {
          # Auto-computed D6 rating badge
          auto_rating  <- ad$d6$domain6[i]
          badge_bg     <- CINEMA_COLOURS[auto_rating]
          badge_col    <- CINEMA_TEXT[auto_rating]
          if (is.na(badge_bg))  badge_bg  <- "#BFBFBF"
          if (is.na(badge_col)) badge_col <- "white"

          rating_badge_ui <- div(style = "margin-bottom:6px;",
            "Auto-computed: ",
            tags$span(
              style = paste0("background:", badge_bg, "; color:", badge_col,
                             "; padding:2px 10px; border-radius:3px;",
                             " font-size:0.88em; font-weight:bold;"),
              auto_rating
            )
          )

          path_str  <- ad$d6$d6_path[i] %||% ""
          path_p    <- ad$d6$d6_p[i]
          path_text <- if (nzchar(path_str)) {
            p_part <- if (!is.na(path_p))
              paste0(" (p = ", format(round(path_p, 3), nsmall = 3), ")") else ""
            p(style = "margin:2px 0; color:#555;",
              em("Decision route: "), path_str, p_part)
          } else NULL

          if (is.null(nsplit)) {
            return(tagList(rating_badge_ui, path_text,
                           p(em("Node-splitting not available."))))
          }
          t1i  <- ad$comps$t1[i]; t2i <- ad$comps$t2[i]
          lbls <- nsplit$comparison
          idx  <- which(lbls == paste(t1i, t2i, sep = ":") |
                        lbls == paste(t2i, t1i, sep = ":"))

          if (cr$merged$evidence_type[i] == "indirect") {
            return(tagList(rating_badge_ui, path_text,
                           p(em("No direct evidence — global test used for D6."))))
          }
          if (length(idx) == 0) {
            return(tagList(rating_badge_ui, path_text,
                           p(em("No node-splitting result for this pair."))))
          }

          sv <- function(x) { v <- tryCatch(x, error = function(e) NA_real_)
                               if (length(v) == 0) NA_real_ else v }
          d_lo  <- sv(nsplit$direct.random$lower[idx[1]])
          d_hi  <- sv(nsplit$direct.random$upper[idx[1]])
          i_lo  <- sv(nsplit$indirect.random$lower[idx[1]])
          i_hi  <- sv(nsplit$indirect.random$upper[idx[1]])
          # netmeta v3.x: SIDE p-values are in compare.random$p
          p_val <- sv(nsplit$compare.random$p[idx[1]])
          nma_lo <- sv(cr$te_df$lower[i])
          nma_hi <- sv(cr$te_df$upper[i])

          tagList(
            rating_badge_ui,
            path_text,
            p(style = "margin:2px 0;",
              "NMA 95% CI", ci_unit_6, ": [", fv6(nma_lo), ", ", fv6(nma_hi), "]"),
            if (!is.na(d_lo)) p(style = "margin:2px 0;",
              "Direct 95% CI", ci_unit_6, ": [", fv6(d_lo), ", ", fv6(d_hi), "]"),
            if (!is.na(i_lo)) p(style = "margin:2px 0;",
              "Indirect 95% CI", ci_unit_6, ": [", fv6(i_lo), ", ", fv6(i_hi), "]"),
            if (!is.na(p_val)) p(style = "margin:2px 0;",
              "SIDE p-value: ", strong(round(p_val, 3)))
          )
        }
      )
    })

    # ------------------------------------------------------------------
    # D6: auto-computed values are set as initial selected in render_domain_cards
    # via selected_vals = ad$d6$domain6 (no observeEvent needed)

    # ------------------------------------------------------------------
    # RETURN
    # ------------------------------------------------------------------
    # The CINeMA domains are now top-level navbar tabs (no inner tabset).
    # These helpers are kept for API compatibility but are no-ops; cross-tab
    # navigation is handled via the parent session's updateNavbarPage() —
    # see app.R's go_to_cinema callback wiring.
    go_to_domain1 <- function() invisible(NULL)
    go_to_domain2 <- function() invisible(NULL)
    go_to_domain3 <- function() invisible(NULL)

    return(list(
      cinema_results = cinema_results,
      set_robmen     = set_robmen,
      go_to_domain1  = go_to_domain1,
      go_to_domain2  = go_to_domain2,
      go_to_domain3  = go_to_domain3
    ))
  })
}

# =============================================================================
# HELPER FUNCTIONS  (Nikolakopoulou 2020 CINeMA algorithms)
# =============================================================================

# %||% / get_contrib_matrix defined in utils.R

# ----------------------------------------------------------------------------
# build_study_iv_table: tag each pairwise row with its canonical (alphabetical)
# direct comparison key and inverse-variance share within that direct comp.
# Used to decompose comparison-level contribution down to study level.
# ----------------------------------------------------------------------------
build_study_iv_table <- function(df) {
  df %>%
    mutate(
      direct_key = paste(pmin(t1, t2), pmax(t1, t2), sep = ":"),
      iv_w       = 1 / (se ^ 2)
    ) %>%
    group_by(direct_key) %>%
    mutate(iv_share = iv_w / sum(iv_w, na.rm = TRUE)) %>%
    ungroup()
}

# ----------------------------------------------------------------------------
# study_contrib_per_target: for one NMA target comparison, return df rows
# annotated with study_contrib = cm[target, direct_of(study)] * iv_share.
# Returns NULL if the target row is not in the contribution matrix.
# ----------------------------------------------------------------------------
study_contrib_per_target <- function(df_iv, cm, target_t1, target_t2) {
  row_idx <- find_cm_row(cm, target_t1, target_t2)
  if (is.na(row_idx)) return(NULL)
  contr_row <- cm[row_idx, ]
  cn <- names(contr_row)

  # cm column names use ":" with alphabetical ordering. direct_key already
  # canonical; fall back to reverse just in case.
  direct_contrib <- vapply(df_iv$direct_key, function(k) {
    if (k %in% cn) return(as.numeric(contr_row[[k]]))
    parts <- strsplit(k, ":", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      rev_k <- paste(parts[2], parts[1], sep = ":")
      if (rev_k %in% cn) return(as.numeric(contr_row[[rev_k]]))
    }
    0
  }, numeric(1))

  df_iv$direct_contrib <- direct_contrib
  df_iv$study_contrib  <- direct_contrib * df_iv$iv_share
  df_iv
}

# ----------------------------------------------------------------------------
# aggregate_wsb_studies: aggregate study-level rob/indirectness scores (0-2)
# weighted by per-study contribution to a target NMA comparison.
# Returns one of "No concerns" / "Some concerns" / "Major concerns" / "Not assessed".
# ----------------------------------------------------------------------------
aggregate_wsb_studies <- function(scores, weights, rule = "average") {
  ok <- !is.na(scores) & !is.na(weights) & weights > 0
  if (!any(ok) || sum(weights[ok]) < 0.001) return("Not assessed")
  s <- scores[ok]
  w <- weights[ok] / sum(weights[ok])

  num <- switch(rule,
    average  = floor(weighted.mean(s, w) + 0.5) + 1L,
    majority = {
      cat <- pmax(1L, pmin(3L, as.integer(floor(s + 0.5)) + 1L))
      totals <- vapply(1:3, function(c) sum(w[cat == c]), numeric(1))
      max(which(totals == max(totals)))
    },
    highest  = floor(max(s) + 0.5) + 1L,
    floor(weighted.mean(s, w) + 0.5) + 1L
  )
  num <- max(1L, min(3L, as.integer(num)))
  c("No concerns", "Some concerns", "Major concerns")[num]
}

# rob_num_to_label: 0/1/2 → "Low" / "Some concerns" / "High". Used by charts.
rob_num_to_label <- function(r) {
  c("Low", "Some concerns", "High")[pmax(1L, pmin(3L, as.integer(r) + 1L))]
}

# ----------------------------------------------------------------------------
# find_cm_row: locate a comparison in the contribution matrix.
# Tries both t1:t2 and t2:t1 with multiple separator formats.
# ----------------------------------------------------------------------------
find_cm_row <- function(cm, t1, t2) {
  seps <- c(":", " vs ", " - ", " ")
  for (sep in seps) {
    opts <- c(paste(t1, t2, sep = sep), paste(t2, t1, sep = sep))
    idx  <- which(rownames(cm) %in% opts)
    if (length(idx) > 0) return(idx[1])
  }
  for (i in seq_len(nrow(cm))) {
    rn <- rownames(cm)[i]
    if (grepl(t1, rn, fixed = TRUE) && grepl(t2, rn, fixed = TRUE)) return(i)
  }
  NA_integer_
}

# ----------------------------------------------------------------------------
# aggregate_wsb: contribution-weighted ROB aggregation. Only the sensitivity-
# based Domain 1 path uses this (rule="highest"); the contribution-weighted
# default path uses aggregate_wsb_studies() above.
# ----------------------------------------------------------------------------
aggregate_wsb <- function(scores, weights, rule) {
  valid <- !is.na(scores) & weights > 0.001
  if (sum(valid) == 0) return("Not assessed")
  s <- scores[valid]; w <- weights[valid]
  # Round fractional means to nearest integer category (1/2/3) before majority/highest
  s_int <- pmax(1L, pmin(3L, as.integer(floor(s + 0.5))))

  num <- switch(rule,
    average = round(weighted.mean(s, w)),
    majority = {
      # Sum contribution weights per integer category; ties → most severe wins
      totals <- sapply(1:3, function(c) sum(w[s_int == c]))
      max(which(totals == max(totals)))
    },
    highest = max(s_int)
  )
  num <- max(1L, min(3L, as.integer(num)))
  c("No concerns", "Some concerns", "Major concerns")[num]
}


# ----------------------------------------------------------------------------
# Domain 1: Within-study bias (per-study, contribution-weighted)
# ----------------------------------------------------------------------------
# For each NMA target comparison, decompose the comparison-level contribution
# matrix down to study level by IV weight within each direct comparison, then
# aggregate study rob (0=low, 1=some, 2=high) under the chosen rule.
compute_domain1_wsb <- function(comps, contrib, df, rule = "average") {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")
  cm <- get_contrib_matrix(contrib)
  if (is.null(cm)) {
    return(data.frame(comparison = comp_labels,
                      domain1    = rep("Not assessed", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  rob_score   <- c("low" = 0, "some concerns" = 1, "high" = 2)
  df$rob_num  <- rob_score[as.character(df$rob)]
  df_iv       <- build_study_iv_table(df)

  results <- vapply(seq_len(nrow(comps)), function(i) {
    sc <- study_contrib_per_target(df_iv, cm, comps$t1[i], comps$t2[i])
    if (is.null(sc)) return("Not assessed")
    aggregate_wsb_studies(sc$rob_num, sc$study_contrib, rule)
  }, character(1))

  data.frame(comparison = comp_labels, domain1 = results,
             stringsAsFactors = FALSE)
}

# judge_rob_direct_sens() lives in inst/app/modules/_d1_sens_judge.R so it
# can be sourced standalone from tests without loading shiny/DT/plotly.
# app.R sources that file before this one.

# ----------------------------------------------------------------------------
# Domain 1 (Sensitivity-based, network level): apply judge_rob_direct_sens()
# to each direct comparison, then roll up to network comparisons via the
# existing aggregate_wsb() with rule="highest" (the agg_rule UI input is
# intentionally ignored here — direction is already encoded at the direct
# level, so network roll-up is a pure severity-max).
# ----------------------------------------------------------------------------
compute_domain1_wsb_sens <- function(comps, contrib, df,
                                     inf_threshold = 0.10,
                                     small_values  = NULL) {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")
  cm <- get_contrib_matrix(contrib)
  if (is.null(cm)) {
    return(data.frame(comparison = comp_labels,
                      domain1    = rep("Not assessed", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  # processed_data exposes the pairwise effect under "y"/"se" (Module A
  # convention); netmeta-style pairs use "TE"/"seTE". Pick whichever exists.
  te_col <- if ("TE"   %in% names(df)) "TE"   else if ("y"  %in% names(df)) "y"  else NA_character_
  se_col <- if ("seTE" %in% names(df)) "seTE" else if ("se" %in% names(df)) "se" else NA_character_

  num_map  <- c(no = 1L, some_concerns = 2L, serious = 3L)
  judgments <- new.env(hash = TRUE, parent = emptyenv())
  pairs_tab <- unique(rbind(
    data.frame(a = df$t1, b = df$t2, stringsAsFactors = FALSE),
    data.frame(a = df$t2, b = df$t1, stringsAsFactors = FALSE)
  ))
  for (i in seq_len(nrow(pairs_tab))) {
    a <- pairs_tab$a[i]; b <- pairs_tab$b[i]
    sub <- df[(df$t1 == a & df$t2 == b) | (df$t1 == b & df$t2 == a), , drop = FALSE]
    if (nrow(sub) == 0) next
    te_vec <- if (!is.na(te_col)) sub[[te_col]] else rep(NA_real_, nrow(sub))
    se_vec <- if (!is.na(se_col)) sub[[se_col]] else rep(NA_real_, nrow(sub))
    j <- judge_rob_direct_sens(
      rob_vec       = as.character(sub$rob),
      te_vec        = te_vec,
      se_vec        = se_vec,
      inf_threshold = inf_threshold,
      small_values  = small_values
    )
    judgments[[paste(a, b, sep = ":")]] <- j
  }

  results <- sapply(seq_len(nrow(comps)), function(i) {
    row_idx <- find_cm_row(cm, comps$t1[i], comps$t2[i])
    if (is.na(row_idx)) return("Not assessed")
    contr <- cm[row_idx, ]; contr <- contr[contr > 0.001]
    if (length(contr) == 0) return("Not assessed")
    scores <- sapply(names(contr), function(lbl) {
      j <- judgments[[lbl]]
      if (is.null(j)) {
        # Try reverse ordering "B:A"
        parts <- strsplit(lbl, ":", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
          j <- judgments[[paste(parts[2], parts[1], sep = ":")]]
        }
      }
      if (is.null(j)) NA_real_ else as.numeric(num_map[[j]])
    })
    aggregate_wsb(scores, contr, rule = "highest")
  })

  data.frame(comparison = comp_labels, domain1 = results,
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# Domain 3: Indirectness (per-study, contribution-weighted)
# ----------------------------------------------------------------------------
compute_domain3_indirectness <- function(comps, contrib, df, rule = "average") {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")
  cm <- get_contrib_matrix(contrib)
  if (is.null(cm)) {
    return(data.frame(comparison = comp_labels,
                      domain3    = rep("Not assessed", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  indir_score   <- c("low" = 0, "some concerns" = 1, "high" = 2)
  df$indir_num  <- indir_score[as.character(df$indirectness)]
  df_iv         <- build_study_iv_table(df)

  results <- vapply(seq_len(nrow(comps)), function(i) {
    sc <- study_contrib_per_target(df_iv, cm, comps$t1[i], comps$t2[i])
    if (is.null(sc)) return("Not assessed")
    aggregate_wsb_studies(sc$indir_num, sc$study_contrib, rule)
  }, character(1))

  data.frame(comparison = comp_labels, domain3 = results,
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# count_zone_crossings: count how many zone boundaries (±delta) a CI or PrI crosses.
# ----------------------------------------------------------------------------
count_zone_crossings <- function(lo, hi, delta) {
  if (any(is.na(c(lo, hi, delta))) || delta <= 0) return(0L)
  d <- abs(delta)
  sum(c(-d, d) > lo & c(-d, d) < hi)
}

# ----------------------------------------------------------------------------
# count_common_zones: count zones shared by the direct CI and indirect CI.
# ----------------------------------------------------------------------------
count_common_zones <- function(d_lo, d_hi, i_lo, i_hi, delta) {
  if (any(is.na(c(d_lo, d_hi, i_lo, i_hi)))) return(NA_integer_)
  d  <- abs(delta)
  ov <- function(lo, hi, z1, z2) lo < z2 && hi > z1
  zones <- list(c(-Inf, -d), c(-d, d), c(d, Inf))
  sum(sapply(zones, function(z)
    ov(d_lo, d_hi, z[1], z[2]) && ov(i_lo, i_hi, z[1], z[2])))
}

# ----------------------------------------------------------------------------
# Domain 4: Imprecision — zone-based (Nikolakopoulou 2020)
#   No concerns    — Zone A (CI entirely in beneficial direction) or Zone B (equivalence zone)
#   Some concerns  — CI includes null but does not reach Zone C
#   Major concerns — CI extends into Zone C (unfavourable direction)
# ----------------------------------------------------------------------------
compute_domain4_imprecision <- function(comps, net, model_type, delta) {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")
  te_mat    <- if (model_type == "random") net$TE.random    else net$TE.common
  lower_mat <- if (model_type == "random") net$lower.random else net$lower.common
  upper_mat <- if (model_type == "random") net$upper.random else net$upper.common
  d <- abs(delta)

  results <- sapply(seq_len(nrow(comps)), function(i) {
    te <- tryCatch(te_mat[comps$t1[i], comps$t2[i]],    error = function(e) NA_real_)
    lo <- tryCatch(lower_mat[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    hi <- tryCatch(upper_mat[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    if (any(is.na(c(te, lo, hi)))) return("Not assessed")

    if (te < 0) {
      in_fav   <- hi < -d
      in_unfav <- hi > d
    } else {
      in_fav   <- lo > d
      in_unfav <- lo < -d
    }
    in_equiv <- lo >= -d && hi <= d

    if (in_fav || in_equiv) "No concerns"
    else if (!in_unfav)     "Some concerns"
    else                    "Major concerns"
  })

  data.frame(comparison = comp_labels, domain4 = unname(results),
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# Domain 5: Heterogeneity — zone crossing (Nikolakopoulou 2020)
#   PrI crossings − CI crossings = 0 → No concerns
#                               = 1 → Some concerns
#                               = 2 → Major concerns
#   Common-effects model: No concerns (τ² = 0)
# ----------------------------------------------------------------------------
compute_domain5_heterogeneity <- function(comps, net, model_type, delta) {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

  if (model_type == "common") {
    return(data.frame(comparison = comp_labels,
                      domain5    = rep("No concerns", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  has_predict <- !is.null(net$lower.predict) && is.matrix(net$lower.predict)
  rating_map  <- c("No concerns", "Some concerns", "Major concerns")

  results <- sapply(seq_len(nrow(comps)), function(i) {
    lo_ci <- tryCatch(net$lower.random[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    hi_ci <- tryCatch(net$upper.random[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    if (any(is.na(c(lo_ci, hi_ci)))) return("Not assessed")

    ci_cross <- count_zone_crossings(lo_ci, hi_ci, delta)
    if (!has_predict) return(rating_map[min(ci_cross + 1L, 3L)])

    lo_pri <- tryCatch(net$lower.predict[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    hi_pri <- tryCatch(net$upper.predict[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    if (any(is.na(c(lo_pri, hi_pri)))) return(rating_map[min(ci_cross + 1L, 3L)])

    pri_cross <- count_zone_crossings(lo_pri, hi_pri, delta)
    diff     <- max(0L, pri_cross - ci_cross)
    rating_map[min(diff + 1L, 3L)]
  })

  data.frame(comparison = comp_labels, domain5 = unname(results),
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# Domain 6: Incoherence — two-step (Nikolakopoulou 2020)
#   Step 1: SIDE p > 0.10 → No concerns
#   Step 2: p ≤ 0.10 → count zones shared by direct and indirect CIs
#           3 → No concerns; 2 → Some concerns; ≤1 → Major concerns
#   No direct evidence: global design-by-treatment test (decomp.design)
# ----------------------------------------------------------------------------
compute_domain6_incoherence <- function(comps, nsplit, net, delta) {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

  # ----------------------------------------------------------------
  # Global rating via design-by-treatment interaction test (decomp.design).
  # Closed-loop absent or test cannot be computed → Major concerns
  # (CINeMA spec; cannot rule out incoherence without a test).
  # Returns list(rating, p, path) so the per-comparison loop can surface
  # the underlying judgement route in the UI.
  # ----------------------------------------------------------------
  global_info <- tryCatch({
    decomp <- decomp.design(net)
    q_obj  <- decomp$Q.inc.random
    if (is.null(q_obj)) {
      list(rating = "Major concerns", p = NA_real_,
           path = "closed loops absent")
    } else {
      extract_p <- function(obj) {
        for (nm in c("p", "pval", "p.value", "Pval")) {
          v <- suppressWarnings(as.numeric(obj[[nm]]))
          if (length(v) > 0 && !is.na(v[1])) return(v[1])
        }
        NA_real_
      }
      p <- extract_p(q_obj)
      r <- if (is.na(p))           "Major concerns"
           else if (p > 0.10)      "No concerns"
           else if (p > 0.05)      "Some concerns"
           else                    "Major concerns"
      list(rating = r, p = p, path = "global design-by-treatment test")
    }
  }, error = function(e) list(rating = "Major concerns", p = NA_real_,
                              path = "closed loops absent"))
  global_rating <- global_info$rating

  safe_val <- function(x) {
    v <- tryCatch(x, error = function(e) NA_real_)
    if (length(v) == 0) NA_real_ else v
  }

  global_row <- function() list(rating = global_info$rating,
                                p = global_info$p, path = global_info$path)

  results_list <- lapply(seq_len(nrow(comps)), function(i) {
    if (is.null(nsplit)) return(global_row())

    t1   <- comps$t1[i]; t2 <- comps$t2[i]
    lbls <- nsplit$comparison
    idx  <- which(lbls == paste(t1, t2, sep = ":") |
                  lbls == paste(t2, t1, sep = ":"))
    if (length(idx) == 0) return(global_row())

    d_te <- safe_val(nsplit$direct.random$TE[idx[1]])
    i_te <- safe_val(nsplit$indirect.random$TE[idx[1]])
    has_direct   <- !is.na(d_te)
    has_indirect <- !is.na(i_te)

    if (!(has_direct && has_indirect)) return(global_row())

    p_side <- tryCatch({
      v <- nsplit$compare.random$p[idx[1]]
      if (length(v) == 0) NA_real_ else as.numeric(v)
    }, error = function(e) NA_real_)
    if (is.na(p_side))
      return(global_row())
    if (p_side > 0.10)
      return(list(rating = "No concerns", p = p_side, path = "SIDE test"))

    d_lo <- safe_val(nsplit$direct.random$lower[idx[1]])
    d_hi <- safe_val(nsplit$direct.random$upper[idx[1]])
    i_lo <- safe_val(nsplit$indirect.random$lower[idx[1]])
    i_hi <- safe_val(nsplit$indirect.random$upper[idx[1]])

    n_common <- count_common_zones(d_lo, d_hi, i_lo, i_hi, delta)

    rating <- if (is.na(n_common))    "Some concerns"
              else if (n_common >= 3) "No concerns"
              else if (n_common == 2) "Some concerns"
              else                    "Major concerns"
    list(rating = rating, p = p_side, path = "SIDE test")
  })

  data.frame(
    comparison = comp_labels,
    domain6    = vapply(results_list, `[[`, character(1), "rating"),
    d6_path    = vapply(results_list, `[[`, character(1), "path"),
    d6_p       = vapply(results_list,
                        function(x) as.numeric(x$p %||% NA_real_),
                        numeric(1)),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# compute_auto_confidence: D1-D6 → suggested confidence level.
#
# algorithm = "standard" (Nikolakopoulou 2020):
#   Some concerns = -1, Major concerns = -2.
#   Anti-double-counting: D4 (imprecision), D5 (heterogeneity), and D6 (incoherence)
#   are grouped together — only the single most-severe downgrade among D4/D5/D6 counts.
#   D1, D2, D3 each contribute independently.
#
# algorithm = "fractional":
#   Some concerns = -1/3, Major concerns = -1. Sum all 6 domains.
#   Round to nearest integer (round-half-away-from-zero). No anti-double-counting.
# ----------------------------------------------------------------------------
compute_auto_confidence <- function(domains, algorithm = "standard") {

  if (algorithm == "fractional") {
    dg_frac_map <- c("No concerns" = 0, "Some concerns" = -1/3,
                     "Major concerns" = -1, "Not assessed" = 0)
    safe_frac <- function(x) {
      v <- dg_frac_map[as.character(x)]
      if (is.na(v)) 0 else v
    }
    total_dg <- safe_frac(domains$within_study_bias) +
                safe_frac(domains$reporting_bias)     +
                safe_frac(domains$indirectness)       +
                safe_frac(domains$imprecision)        +
                safe_frac(domains$heterogeneity)      +
                safe_frac(domains$incoherence)
    # Round half away from zero
    total_dg_int <- as.integer(floor(total_dg + 0.5))
    score <- max(1L, min(4L, 4L + total_dg_int))
    return(c("Very low", "Low", "Moderate", "High")[score])
  }

  # Algorithm ① standard
  dg_map <- c("No concerns" = 0L, "Some concerns" = -1L,
               "Major concerns" = -2L, "Not assessed" = 0L)

  safe_dg <- function(x) {
    v <- dg_map[as.character(x)]
    if (is.na(v)) 0L else as.integer(v)
  }

  dg_d1 <- safe_dg(domains$within_study_bias)
  dg_d2 <- safe_dg(domains$reporting_bias)
  dg_d3 <- safe_dg(domains$indirectness)
  dg_d4 <- safe_dg(domains$imprecision)
  dg_d5 <- safe_dg(domains$heterogeneity)
  dg_d6 <- safe_dg(domains$incoherence)

  # Anti-double-count D4/D5/D6 as one group: only the most severe counts
  dg_d456 <- min(dg_d4, dg_d5, dg_d6)

  total_dg <- dg_d1 + dg_d2 + dg_d3 + dg_d456
  score    <- max(1L, min(4L, 4L + total_dg))
  c("Very low", "Low", "Moderate", "High")[score]
}

# ----------------------------------------------------------------------------
# render_domain_cards: card-style UI for D4/D5/D6 per-comparison.
# ----------------------------------------------------------------------------
render_domain_cards <- function(n, comps, evidence_type, auto_vals,
                                ns_func, ov_prefix, card_extra = NULL,
                                reason_prefix = NULL, selected_vals = NULL) {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

  card_color   <- function(r) switch(r, "No concerns" = "#e8f5e9",
    "Some concerns" = "#fff8e1", "Major concerns" = "#ffebee", "#f8f9fa")
  border_color <- function(r) switch(r, "No concerns" = "#c8e6c9",
    "Some concerns" = "#ffe082", "Major concerns" = "#ffcdd2", "#e0e0e0")

  tagList(lapply(seq_len(n), function(i) {
    rating <- auto_vals[i]
    init_sel <- if (!is.null(selected_vals) && length(selected_vals) >= i &&
                    nzchar(selected_vals[i])) selected_vals[i] else ""
    div(
      style = paste0("background:", card_color(rating), ";",
                     " border:1px solid ", border_color(rating), ";",
                     " border-radius:4px; padding:8px 10px; margin-bottom:6px;"),
      fluidRow(
        column(5,
          strong(comp_labels[i]),
          span(style = "margin-left:6px; font-size:0.82em; color:#666;",
               paste0("(", evidence_type[i], ")"))
        ),
        column(4,
          selectInput(
            ns_func(paste0(ov_prefix, "_", i)), label = NULL,
            choices  = DOMAIN_CHOICES, selected = init_sel, width = "100%"
          )
        ),
        if (!is.null(reason_prefix)) {
          column(3,
            textInput(
              ns_func(paste0(reason_prefix, "_", i)), label = NULL,
              placeholder = "Override reason",
              width = "100%"
            )
          )
        }
      ),
      if (!is.null(card_extra)) card_extra(i)
    )
  }))
}

# ----------------------------------------------------------------------------
# render_override_ui: card-style override for D1/D2/D3.
# Shows auto-value badge + override dropdown + optional reason field per row.
# ----------------------------------------------------------------------------
render_override_ui <- function(ns_func, prefix, comp_labels, auto_vals,
                               evidence_types = NULL, na_label = NULL,
                               reason_prefix = NULL) {
  tagList(
    p(em("Auto-computed ratings shown below.",
         "Use the dropdown to override; leave '(auto)' to use computed value.")),
    lapply(seq_along(comp_labels), function(i) {
      rating <- as.character(auto_vals[i])
      bg <- switch(rating,
        "No concerns"    = "#e8f5e9",
        "Some concerns"  = "#fff8e1",
        "Major concerns" = "#ffebee",
        "#f5f5f5"
      )
      border <- switch(rating,
        "No concerns"    = "#c8e6c9",
        "Some concerns"  = "#ffe082",
        "Major concerns" = "#ffcdd2",
        "#e0e0e0"
      )
      badge_bg    <- CINEMA_COLOURS[rating]
      badge_color <- CINEMA_TEXT[rating]
      if (is.na(badge_bg))    badge_bg    <- "#BFBFBF"
      if (is.na(badge_color)) badge_color <- "white"

      ev  <- if (!is.null(evidence_types)) evidence_types[i] else "mixed"
      note <- if (!is.null(na_label) && ev == "indirect")
                span(style = "font-size:0.8em; color:#888; margin-left:6px;", na_label)
              else NULL

      div(
        style = paste0("background:", bg, "; border:1px solid ", border,
                       "; border-radius:4px; padding:6px 10px; margin-bottom:5px;"),
        fluidRow(
          column(4,
            strong(comp_labels[i]),
            span(style = "margin-left:6px; font-size:0.8em; color:#666;",
                 paste0("(", ev, ")")),
            note
          ),
          column(2,
            span(
              style = paste0("background:", badge_bg,
                "; color:", badge_color,
                "; padding:2px 8px; border-radius:3px; font-size:0.85em;",
                "; display:inline-block; margin-top:4px;"),
              if (nzchar(rating)) rating else "\u2014"
            )
          ),
          column(3,
            selectInput(
              ns_func(paste0(prefix, "_", i)), label = NULL,
              choices = DOMAIN_CHOICES, selected = "", width = "100%"
            )
          ),
          if (!is.null(reason_prefix)) {
            column(3,
              textInput(
                ns_func(paste0(reason_prefix, "_", i)), label = NULL,
                placeholder = "Override reason", width = "100%"
              )
            )
          }
        )
      )
    })
  )
}

# ----------------------------------------------------------------------------
# imprecision_zone_text: D4 zone description for the CI (δ notation)
# ----------------------------------------------------------------------------
imprecision_zone_text <- function(te, lo, hi, delta, is_ratio = FALSE) {
  if (any(is.na(c(te, lo, hi, delta)))) return("Zone analysis unavailable.")
  d       <- abs(delta)
  d_show  <- if (is_ratio) round(exp(d), 2) else round(d, 2)
  d_inv   <- if (is_ratio) round(1 / exp(d), 2) else round(-d, 2)

  if (te >= 0) { in_fav <- lo > d;  in_unfav <- lo < -d }
  else         { in_fav <- hi < -d; in_unfav <- hi > d  }
  in_equiv <- lo >= -d && hi <= d

  zone_range <- if (is_ratio) paste0("[", d_inv, ", ", d_show, "]")
                else paste0("[\u2212", d_show, ", +", d_show, "]")

  if (in_fav)         paste0("CI lies entirely in the beneficial zone (\u03b4 = ", d_show, ").")
  else if (in_equiv)  paste0("CI lies within the equivalence zone ", zone_range, ".")
  else if (!in_unfav) "CI includes null but does not reach the unfavourable zone."
  else                paste0("CI extends into the unfavourable zone (\u03b4 = ", d_show, ", harmful direction).")
}

# ----------------------------------------------------------------------------
# heterogeneity_zone_text: D5 zone description for CI and PrI (δ notation)
# ----------------------------------------------------------------------------
heterogeneity_zone_text <- function(lo_ci, hi_ci, lo_pri, hi_pri, delta) {
  if (any(is.na(c(lo_ci, hi_ci)))) return("CI unavailable.")
  d <- abs(delta)
  ci_cross <- count_zone_crossings(lo_ci, hi_ci, d)

  if (is.na(lo_pri)) {
    return(paste0("95% CI: [", round(lo_ci, 3), ", ", round(hi_ci, 3),
                  "]. Prediction interval (PrI) not available."))
  }

  pri_cross <- count_zone_crossings(lo_pri, hi_pri, d)

  if (pri_cross <= ci_cross)
    "CI and PrI cross the same number of zones \u2014 limited clinical impact of heterogeneity."
  else if (pri_cross - ci_cross == 1)
    "PrI crosses 1 more zone than CI \u2014 future trials may reach a different conclusion."
  else
    "PrI crosses 2 more zones than CI \u2014 heterogeneity allows both beneficial and harmful conclusions."
}
