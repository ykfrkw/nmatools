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
# UI FUNCTION
# --------------------------------------------------------------------------
moduleB_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Step 2: CINeMA Analysis"),
    p("Frequentist NMA via ", strong("netmeta"),
      " with 6-domain CINeMA confidence assessment.",
      "All pairwise comparisons are shown and can be independently rated."),

    wellPanel(
      h4("NMA Settings"),
      fluidRow(
        column(4,
          selectInput(ns("effect_measure"), "Effect measure",
            choices = c(
              "Standardised mean difference (SMD)" = "SMD",
              "Mean difference (MD)"               = "MD",
              "Odds ratio (OR)"                    = "OR",
              "Risk ratio (RR)"                    = "RR"
            ),
            selected = "SMD"
          )
        ),
        column(4, uiOutput(ns("ref_treatment_ui"))),
        column(4,
          selectInput(ns("model_type"), "Effects model",
            choices = c("Random effects" = "random",
                        "Common effects" = "common"),
            selected = "random"
          )
        )
      ),
      h4("Imprecision / Heterogeneity / Incoherence Settings"),
      p("Clinical threshold \u03b4: defines zone boundaries.",
        "Zone A = CI lies entirely in the beneficial direction (beyond \u03b4).",
        "Zone B = CI lies within the equivalence zone [\u22121/\u03b4, +\u03b4] (or [\u2212\u03b4, +\u03b4] for SMD/MD).",
        "Zone C = CI extends into the unfavourable direction.",
        strong("OR/RR: enter \u03b4 on the ratio scale (e.g., 1.2). Boundaries [1/\u03b4, \u03b4] are log-transformed internally."),
        "SMD/MD: enter \u03b4 on the effect-size scale (e.g., 0.2)."),
      fluidRow(
        column(4,
          numericInput(ns("delta"), "Clinical threshold \u03b4 (ratio for OR/RR; effect size for SMD/MD)",
                       value = 0.2, step = 0.05, min = 0.001)
        ),
        column(4,
          selectInput(ns("agg_rule"), "D1 / D3 aggregation rule",
            choices = c(
              "Average (contribution-weighted mean)" = "average",
              "Majority (largest contribution share)" = "majority",
              "Highest (most severe contributor)"     = "highest"
            ),
            selected = "average"
          )
        )
      ),
    ),

    hr(),
    shinycssloaders::withSpinner(
      uiOutput(ns("results_ui")),
      type    = 4,
      color   = "#4472C4",
      caption = "Running NMA\u2026 (this may take a few seconds to a minute)"
    )
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
    # Sync Module B inputs from Module A's nma_settings when run fires
    # ------------------------------------------------------------------
    if (!is.null(nma_settings)) {
      observeEvent(run_trigger(), {
        s <- tryCatch(nma_settings(), error = function(e) NULL)
        if (is.null(s)) return()
        if (!is.null(s$effect_measure))
          updateSelectInput(session, "effect_measure", selected = s$effect_measure)
        if (!is.null(s$model_type))
          updateSelectInput(session, "model_type",     selected = s$model_type)
        if (!is.null(s$delta) && !is.na(s$delta))
          updateNumericInput(session, "delta",          value    = s$delta)
        if (!is.null(s$agg_rule))
          updateSelectInput(session, "agg_rule",        selected = s$agg_rule)
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
    # Navigate to ROB-MEN tab
    # ------------------------------------------------------------------
    observeEvent(input$go_to_robmen, {
      if (!is.null(go_to_robmen)) go_to_robmen()
    })

    # ------------------------------------------------------------------
    # Dynamic reference treatment UI
    # ------------------------------------------------------------------
    output$ref_treatment_ui <- renderUI({
      res <- processed_data()
      req(!is.null(res), !is.null(res$data))
      df       <- res$data
      all_trts <- sort(unique(c(df$t1, df$t2)))
      # Use Module A's setting if available, else fallback to "placebo" or first
      a_ref <- tryCatch(nma_settings()$ref_treatment, error = function(e) NULL)
      default <- if (!is.null(a_ref) && a_ref %in% all_trts) a_ref
                 else if ("placebo" %in% all_trts) "placebo"
                 else all_trts[1]
      selectInput(ns("ref_treatment"), "Reference treatment (for NMA anchor)",
                  choices = all_trts, selected = default)
    })

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
      ref_trt <- input$ref_treatment
      if (is.null(ref_trt) || !nzchar(ref_trt)) {
        s <- tryCatch(nma_settings(), error = function(e) NULL)
        ref_trt <- if (!is.null(s)) (s$ref_treatment %||% df$t1[1]) else df$t1[1]
      }
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
            sm                  = input$effect_measure,
            common              = (input$model_type == "common"),
            random              = (input$model_type == "random"),
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
      model_type <- if (input$model_type == "random") "random" else "common"
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      # Domain 1: Within-study bias (contribution-weighted ROB)
      rob_score  <- c("low" = 0, "some concerns" = 1, "high" = 2)
      df$rob_num <- rob_score[as.character(df$rob)]
      direct_rob <- bind_rows(
        df %>% mutate(comp_label = paste(t1, t2, sep = ":")),
        df %>% mutate(comp_label = paste(t2, t1, sep = ":"))
      ) %>%
        group_by(comp_label) %>%
        summarise(mean_rob = mean(rob_num, na.rm = TRUE), .groups = "drop")
      d1 <- compute_domain1_wsb(comps, contrib, direct_rob, input$agg_rule)

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
        compute_domain3_indirectness(comps, contrib, df, input$agg_rule)
      } else {
        data.frame(comparison = comp_labels,
                   domain3    = rep("Not assessed", nrow(comps)),
                   stringsAsFactors = FALSE)
      }

      # Domains 4-6 — for OR/RR, delta is entered as a ratio (e.g. 1.2) and must be
      # log-transformed because netmeta stores effects on the log scale internally.
      is_ratio_em <- input$effect_measure %in% c("OR", "RR")
      delta_used  <- if (is_ratio_em) log(input$delta) else input$delta
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
    # OUTPUT: main results UI — tabs ordered D1→D6→Estimates→Summary
    # ------------------------------------------------------------------
    output$results_ui <- renderUI({
      res <- nma_result()
      if (is.null(res)) return(NULL)
      if (!is.null(res$error) && is.null(res$net)) {
        return(div(class = "alert alert-danger",
                   icon("exclamation-triangle"),
                   strong(" NMA Error: "), res$error))
      }

      tagList(
        # Prevent page scroll-to-top when switching CINeMA domain tabs
        tags$script(HTML(sprintf("
          $(document).on('click', '#%s .nav-tabs a', function(e) {
            e.preventDefault();
            var scrollY = window.scrollY;
            $(this).tab('show');
            window.scrollTo(0, scrollY);
          });
        ", ns("cinema_tabs")))),

        tabsetPanel(id = ns("cinema_tabs"),

          # ---- D1: Within-study bias ---------------------------------
          tabPanel(value = "d1_within_study_bias", "D1: Within-study Bias",
            br(),
            uiOutput(ns("d1_debug_msg")),
            h5("Contribution Chart (ROB by direct comparison)"),
            plotlyOutput(ns("d1_contrib_chart"), height = "320px"),
            br(),
            p(strong("Algorithm (Nikolakopoulou 2020):"),
              "ROB scores encoded as low=1, some concerns=2, high=3.",
              "Three aggregation rules selectable above:",
              strong("Average"), "— contribution-weighted mean;",
              strong("Majority"), "— category with largest total weight;",
              strong("Highest"), "— most severe ROB among non-negligible contributors."),
            h5("Domain 1 Ratings — Auto-computed + Override"),
            uiOutput(ns("d1_override_ui"))
          ),

          # ---- D2: Reporting bias ------------------------------------
          tabPanel(
            value = "d2_reporting_bias",
            title = tagList(
              "D2: Reporting Bias",
              tags$small(
                style = "color:#888; font-size:0.8em; margin-left:4px;",
                "\u2190 Module C"
              )
            ),
            br(),
            div(class = "alert alert-info",
                icon("info-circle"),
                " Run Module C (ROB-MEN) — ROB-MEN ratings populate this domain automatically.",
                " Or use the bulk buttons / per-comparison overrides below."),
            div(style = "display:flex; gap:6px; align-items:center; flex-wrap:wrap; margin-bottom:12px;",
              actionButton(ns("go_to_robmen"),
                "→ Go to Module C (ROB-MEN)",
                class = "btn btn-info btn-sm",
                icon  = icon("arrow-right")),
              actionButton(ns("d2_set_all_no"), "Set all: No concerns",
                class = "btn btn-success btn-sm"),
              actionButton(ns("d2_set_all_some"), "Set all: Some concerns",
                class = "btn btn-warning btn-sm"),
              actionButton(ns("d2_set_all_major"), "Set all: Major concerns",
                class = "btn btn-danger btn-sm")
            ),
            h5("Domain 2 Ratings — Auto-computed (from ROB-MEN) + Override"),
            uiOutput(ns("d2_override_ui"))
          ),

          # ---- D3: Indirectness --------------------------------------
          tabPanel(value = "d3_indirectness", "D3: Indirectness",
            br(),
            h5("Contribution Chart (indirectness by direct comparison)"),
            plotlyOutput(ns("d3_contrib_chart"), height = "320px"),
            br(),
            p(strong("Algorithm:"),
              "Same as D1 but using study-level indirectness ratings.",
              "Requires 'indirectness' column in the input data."),
            h5("Domain 3 Ratings — Auto-computed + Override"),
            uiOutput(ns("d3_override_ui"))
          ),

          # ---- D4: Imprecision ---------------------------------------
          tabPanel("D4: Imprecision",
            br(),
            p(strong("Clinical threshold (\u03b4):"), "defines zone boundaries \u00b1\u03b4.",
              "Zone A = CI lies entirely in the beneficial direction.",
              "Zone B = CI lies within the equivalence zone [\u2212\u03b4, +\u03b4].",
              "Zone C = CI extends into the unfavourable direction.",
              strong("No concerns:"), "Zone A or B.",
              strong("Some concerns:"), "CI includes null but does not reach Zone C.",
              strong("Major concerns:"), "CI extends into Zone C."),
            uiOutput(ns("d4_cards"))
          ),

          # ---- D5: Heterogeneity -------------------------------------
          tabPanel("D5: Heterogeneity",
            br(),
            p(strong("Algorithm:"),
              "Count how many zone boundaries (\u00b1\u03b4) the CI and PI each cross.",
              "PI crossings \u2212 CI crossings = 0 \u2192 No concerns; 1 \u2192 Some concerns; 2 \u2192 Major concerns.",
              "Common-effects model: No concerns (\u03c4\u00b2 = 0)."),
            uiOutput(ns("d5_cards"))
          ),

          # ---- D6: Incoherence ---------------------------------------
          tabPanel("D6: Incoherence",
            br(),
            p(strong("Algorithm (Nikolakopoulou 2020 two-step):"),
              strong("Step 1:"), "SIDE p > 0.10 \u2192 No concerns.",
              strong("Step 2"), "(p \u2264 0.10): count zones shared by the direct and indirect CIs.",
              "3 \u2192 No concerns; 2 \u2192 Some concerns; \u22641 \u2192 Major concerns.",
              "No direct evidence: global design-by-treatment test (decomp.design) used.",
              em("D6 is fully algorithm-driven. Override only when expert judgement differs.")),
            uiOutput(ns("d6_cards"))
          )
        )
      )
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
    # D1 contribution chart
    # Stacked horizontal bar: for each NMA comparison, the total %
    # contribution split into Low (green) | Some concerns (yellow) | High (red).
    # ------------------------------------------------------------------
    output$d1_contrib_chart <- renderPlotly({
      res <- nma_result(); req(!is.null(res), !is.null(res$df))
      cm  <- get_contrib_matrix(res$contrib)
      req(!is.null(cm))
      ad  <- auto_domains(); req(!is.null(ad))

      df    <- res$df
      comps <- ad$comps
      comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")

      # ROB per study (numeric 0/1/2)
      rob_num <- c("low" = 0, "some concerns" = 1, "high" = 2)
      df$rnk  <- rob_num[as.character(df$rob)]

      # For each study, map to canonical comp key (both directions)
      study_rob <- bind_rows(
        df %>% mutate(ck = paste(t1, t2, sep = ":")),
        df %>% mutate(ck = paste(t2, t1, sep = ":"))
      ) %>% select(ck, studlab, rnk) %>% distinct()

      # For each NMA comparison, sum contributions split by ROB category
      plot_rows <- lapply(seq_len(nrow(comps)), function(i) {
        row_idx <- find_cm_row(cm, comps$t1[i], comps$t2[i])
        if (is.na(row_idx)) return(NULL)
        contr <- cm[row_idx, ]; contr <- contr[contr > 0.001]
        if (length(contr) == 0) return(NULL)

        # For each contributing direct comparison, get mean ROB and contribution
        sub <- lapply(names(contr), function(col_nm) {
          s <- study_rob %>% filter(ck == col_nm)
          mean_rob <- if (nrow(s) > 0) mean(s$rnk, na.rm = TRUE) else NA_real_
          rob_lbl <- if (is.na(mean_rob)) "Unknown"
                     else if (mean_rob < 0.5) "Low"
                     else if (mean_rob < 1.5) "Some concerns"
                     else "High"
          data.frame(rob_label = rob_lbl,
                     contrib_pct = contr[col_nm] * 100,
                     stringsAsFactors = FALSE)
        }) %>% bind_rows()

        # Aggregate by ROB category
        sub %>%
          group_by(rob_label) %>%
          summarise(contrib_pct = sum(contrib_pct, na.rm = TRUE), .groups = "drop") %>%
          mutate(nma_comp = comp_labels[i])
      }) %>% bind_rows()

      req(!is.null(plot_rows), nrow(plot_rows) > 0)

      # Ensure all three levels present and ordered Low → Some concerns → High (left→right)
      # Reverse levels: ggplotly inverts stacking order, so reversing here
      # makes the final display show Low → Some concerns → High (left to right)
      all_levels <- c("Low", "Some concerns", "High")
      plot_rows$rob_label <- factor(plot_rows$rob_label,
                                    levels = rev(all_levels))
      rob_colours <- c("Low" = "#5cb85c", "Some concerns" = "#f0ad4e", "High" = "#d9534f")

      p <- ggplot(plot_rows,
                  aes(x = nma_comp, y = contrib_pct, fill = rob_label,
                      text = paste0(nma_comp,
                                    "<br>ROB: ", rob_label,
                                    "<br>Contribution: ", round(contrib_pct, 1), "%"))) +
        geom_col(position = "stack") +
        scale_fill_manual(values = rob_colours, name = "Risk of bias",
                          limits = c("Low", "Some concerns", "High"),
                          drop = FALSE) +
        coord_flip() +
        labs(x = NULL, y = "Contribution (%)",
             title = "D1: Contribution by Risk of Bias category") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.15,
                             traceorder = "normal"))
    })

    # ------------------------------------------------------------------
    # D3 contribution chart
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

      study_indir <- bind_rows(
        df %>% mutate(ck = paste(t1, t2, sep = ":")),
        df %>% mutate(ck = paste(t2, t1, sep = ":"))
      ) %>% select(ck, studlab, indir_n) %>% distinct()

      plot_rows <- lapply(seq_len(nrow(comps)), function(i) {
        row_idx <- find_cm_row(cm, comps$t1[i], comps$t2[i])
        if (is.na(row_idx)) return(NULL)
        contr <- cm[row_idx, ]; contr <- contr[contr > 0.001]
        if (length(contr) == 0) return(NULL)

        sub <- lapply(names(contr), function(col_nm) {
          s <- study_indir %>% filter(ck == col_nm)
          mean_in <- if (nrow(s) > 0) mean(s$indir_n, na.rm = TRUE) else NA_real_
          in_lbl <- if (is.na(mean_in)) "Unknown"
                    else if (mean_in < 0.5) "Low"
                    else if (mean_in < 1.5) "Some concerns"
                    else "High"
          data.frame(indir_label = in_lbl,
                     contrib_pct = contr[col_nm] * 100,
                     stringsAsFactors = FALSE)
        }) %>% bind_rows()

        sub %>%
          group_by(indir_label) %>%
          summarise(contrib_pct = sum(contrib_pct, na.rm = TRUE), .groups = "drop") %>%
          mutate(nma_comp = comp_labels[i])
      }) %>% bind_rows()

      req(!is.null(plot_rows), nrow(plot_rows) > 0)

      all_levels <- c("Low", "Some concerns", "High")
      plot_rows$indir_label <- factor(plot_rows$indir_label, levels = rev(all_levels))
      indir_colours <- c("Low" = "#5cb85c", "Some concerns" = "#f0ad4e", "High" = "#d9534f")

      p <- ggplot(plot_rows,
                  aes(x = nma_comp, y = contrib_pct, fill = indir_label,
                      text = paste0(nma_comp,
                                    "<br>Indirectness: ", indir_label,
                                    "<br>Contribution: ", round(contrib_pct, 1), "%"))) +
        geom_col(position = "stack") +
        scale_fill_manual(values = indir_colours, name = "Indirectness",
                          limits = c("Low", "Some concerns", "High"),
                          drop = FALSE) +
        coord_flip() +
        labs(x = NULL, y = "Contribution (%)",
             title = "D3: Contribution by Indirectness category") +
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
      comp_labels <- paste(ad$comps$t1, ad$comps$t2, sep = " vs ")
      render_override_ui(ns, "ov_d1", comp_labels, ad$d1$domain1,
                         evidence_types = ad$evidence_type,
                         reason_prefix = "reason_d1")
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
      em        <- input$effect_measure %||% "SMD"
      is_ratio  <- em %in% c("OR", "RR")
      delta <- if (is_ratio) log(input$delta) else input$delta
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
      has_pi <- !is.null(net$lower.predict) && is.matrix(net$lower.predict)
      em_d5      <- input$effect_measure %||% "SMD"
      is_ratio_5 <- em_d5 %in% c("OR", "RR")
      delta  <- if (is_ratio_5) log(input$delta) else input$delta
      fv5        <- function(x) if (is_ratio_5 && !is.na(x)) round(exp(x), 3) else round(x, 3)
      ci_unit_5  <- if (is_ratio_5) paste0(" (", em_d5, ")") else ""

      render_domain_cards(
        n, ad$comps, cr$merged$evidence_type, ad$d5$domain5, ns, "ov_d5",
        card_extra = function(i) {
          t1i <- ad$comps$t1[i]; t2i <- ad$comps$t2[i]
          lo_ci <- tryCatch(net$lower.random[t1i, t2i], error = function(e) NA_real_)
          hi_ci <- tryCatch(net$upper.random[t1i, t2i], error = function(e) NA_real_)
          lo_pi <- if (has_pi) tryCatch(net$lower.predict[t1i, t2i], error = function(e) NA_real_) else NA_real_
          hi_pi <- if (has_pi) tryCatch(net$upper.predict[t1i, t2i], error = function(e) NA_real_) else NA_real_
          tagList(
            if (!is.na(lo_ci)) p(style = "margin:2px 0;",
              "95% CI", ci_unit_5, ": [", fv5(lo_ci), ", ", fv5(hi_ci), "]"),
            if (!is.na(lo_pi)) p(style = "margin:2px 0;",
              "95% PI", ci_unit_5, ": [", fv5(lo_pi), ", ", fv5(hi_pi), "]"),
            p(style = "margin:2px 0; font-style:italic; font-size:0.9em;",
              heterogeneity_zone_text(lo_ci, hi_ci, lo_pi, hi_pi, delta))
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
      em_d6      <- input$effect_measure %||% "SMD"
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

          if (is.null(nsplit)) {
            return(tagList(rating_badge_ui, p(em("Node-splitting not available."))))
          }
          t1i  <- ad$comps$t1[i]; t2i <- ad$comps$t2[i]
          lbls <- nsplit$comparison
          idx  <- which(lbls == paste(t1i, t2i, sep = ":") |
                        lbls == paste(t2i, t1i, sep = ":"))

          if (cr$merged$evidence_type[i] == "indirect") {
            return(tagList(rating_badge_ui,
                           p(em("No direct evidence — global test used for D6."))))
          }
          if (length(idx) == 0) {
            return(tagList(rating_badge_ui,
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
    go_to_domain1 <- function() {
      updateTabsetPanel(session, "cinema_tabs", selected = "d1_within_study_bias")
    }
    go_to_domain2 <- function() {
      updateTabsetPanel(session, "cinema_tabs", selected = "d2_reporting_bias")
    }
    go_to_domain3 <- function() {
      updateTabsetPanel(session, "cinema_tabs", selected = "d3_indirectness")
    }

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
# find_direct_score: bidirectional lookup in a direct_df(comp_label, score).
# ----------------------------------------------------------------------------
find_direct_score <- function(col_label, direct_df) {
  m <- direct_df$score[direct_df$comp_label == col_label]
  if (length(m) > 0 && !is.na(m[1])) return(m[1])
  for (sep in c(":", " vs ", " - ")) {
    parts <- strsplit(col_label, sep, fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      rev_lbl <- paste(parts[2], parts[1], sep = ":")
      m <- direct_df$score[direct_df$comp_label == rev_lbl]
      if (length(m) > 0 && !is.na(m[1])) return(m[1])
    }
  }
  NA_real_
}

# ----------------------------------------------------------------------------
# aggregate_wsb: contribution-weighted ROB/indirectness aggregation.
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
# Domain 1: Within-study bias
# ----------------------------------------------------------------------------
compute_domain1_wsb <- function(comps, contrib, direct_rob, rule = "average") {
  comp_labels <- paste(comps$t1, comps$t2, sep = " vs ")
  cm <- get_contrib_matrix(contrib)
  if (is.null(cm)) {
    return(data.frame(comparison = comp_labels,
                      domain1    = rep("Not assessed", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  dr <- data.frame(comp_label = direct_rob$comp_label,
                   score      = direct_rob$mean_rob,
                   stringsAsFactors = FALSE)

  results <- sapply(seq_len(nrow(comps)), function(i) {
    row_idx <- find_cm_row(cm, comps$t1[i], comps$t2[i])
    if (is.na(row_idx)) return("Not assessed")
    contr <- cm[row_idx, ]; contr <- contr[contr > 0.001]
    if (length(contr) == 0) return("Not assessed")
    scores <- sapply(names(contr), function(lbl) {
      s <- find_direct_score(lbl, dr)
      # floor(s + 0.5) rounds 0.5 upward (avoid R banker's rounding)
      if (is.na(s)) NA_real_ else floor(s + 0.5) + 1
    })
    aggregate_wsb(scores, contr, rule)
  })

  data.frame(comparison = comp_labels, domain1 = results,
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# Domain 3: Indirectness
# ----------------------------------------------------------------------------
compute_domain3_indirectness <- function(comps, contrib, df, rule = "average") {
  comp_labels  <- paste(comps$t1, comps$t2, sep = " vs ")
  indir_score  <- c("low" = 0, "some concerns" = 1, "high" = 2)
  df$indir_num <- indir_score[as.character(df$indirectness)]

  direct_indir <- bind_rows(
    df %>% mutate(comp_label = paste(t1, t2, sep = ":")),
    df %>% mutate(comp_label = paste(t2, t1, sep = ":"))
  ) %>%
    group_by(comp_label) %>%
    summarise(mean_indir = mean(indir_num, na.rm = TRUE), .groups = "drop")

  di <- data.frame(comp_label = direct_indir$comp_label,
                   score      = direct_indir$mean_indir,
                   stringsAsFactors = FALSE)

  cm <- get_contrib_matrix(contrib)
  if (is.null(cm)) {
    return(data.frame(comparison = comp_labels,
                      domain3    = rep("Not assessed", nrow(comps)),
                      stringsAsFactors = FALSE))
  }

  results <- sapply(seq_len(nrow(comps)), function(i) {
    row_idx <- find_cm_row(cm, comps$t1[i], comps$t2[i])
    if (is.na(row_idx)) return("Not assessed")
    contr <- cm[row_idx, ]; contr <- contr[contr > 0.001]
    if (length(contr) == 0) return("Not assessed")
    scores <- sapply(names(contr), function(lbl) {
      s <- find_direct_score(lbl, di)
      if (is.na(s)) NA_real_ else floor(s + 0.5) + 1
    })
    aggregate_wsb(scores, contr, rule)
  })

  data.frame(comparison = comp_labels, domain3 = results,
             stringsAsFactors = FALSE)
}

# ----------------------------------------------------------------------------
# count_zone_crossings: count how many zone boundaries (±delta) a CI or PI crosses.
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
#   PI crossings − CI crossings = 0 → No concerns
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

    lo_pi <- tryCatch(net$lower.predict[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    hi_pi <- tryCatch(net$upper.predict[comps$t1[i], comps$t2[i]], error = function(e) NA_real_)
    if (any(is.na(c(lo_pi, hi_pi)))) return(rating_map[min(ci_cross + 1L, 3L)])

    pi_cross <- count_zone_crossings(lo_pi, hi_pi, delta)
    diff     <- max(0L, pi_cross - ci_cross)
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

  global_rating <- tryCatch({
    decomp <- decomp.design(net)
    # Q.inc.random is a data.frame; use [[ ]] to extract scalar, not [ ] which returns a df
    extract_p <- function(obj) {
      for (nm in c("p", "pval", "p.value", "Pval")) {
        v <- suppressWarnings(as.numeric(obj[[nm]]))
        if (length(v) > 0 && !is.na(v[1])) return(v[1])
      }
      NA_real_
    }
    p <- extract_p(decomp$Q.inc.random)
    if (is.na(p)) "Not assessed"
    else if (p > 0.10) "No concerns"
    else if (p > 0.05) "Some concerns"
    else               "Major concerns"
  }, error = function(e) "Not assessed")

  results <- sapply(seq_len(nrow(comps)), function(i) {
    if (is.null(nsplit)) return(global_rating)

    t1   <- comps$t1[i]; t2 <- comps$t2[i]
    lbls <- nsplit$comparison
    idx  <- which(lbls == paste(t1, t2, sep = ":") |
                  lbls == paste(t2, t1, sep = ":"))

    if (length(idx) == 0) return(global_rating)

    # netmeta v3.x: SIDE p-values are in compare.random$p, not p.value.random
    p_side <- tryCatch({
      v <- nsplit$compare.random$p[idx[1]]
      if (length(v) == 0) NA_real_ else as.numeric(v)
    }, error = function(e) NA_real_)
    if (is.na(p_side)) return(global_rating)
    if (p_side > 0.10) return("No concerns")

    safe_val <- function(x) {
      v <- tryCatch(x, error = function(e) NA_real_)
      if (length(v) == 0) NA_real_ else v
    }
    d_lo <- safe_val(nsplit$direct.random$lower[idx[1]])
    d_hi <- safe_val(nsplit$direct.random$upper[idx[1]])
    i_lo <- safe_val(nsplit$indirect.random$lower[idx[1]])
    i_hi <- safe_val(nsplit$indirect.random$upper[idx[1]])

    n_common <- count_common_zones(d_lo, d_hi, i_lo, i_hi, delta)

    if (is.na(n_common))    return("Some concerns")
    if (n_common >= 3)      "No concerns"
    else if (n_common == 2) "Some concerns"
    else                    "Major concerns"
  })

  data.frame(comparison = comp_labels, domain6 = unname(results),
             stringsAsFactors = FALSE)
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
# heterogeneity_zone_text: D5 zone description for CI and PI (δ notation)
# ----------------------------------------------------------------------------
heterogeneity_zone_text <- function(lo_ci, hi_ci, lo_pi, hi_pi, delta) {
  if (any(is.na(c(lo_ci, hi_ci)))) return("CI unavailable.")
  d <- abs(delta)
  ci_cross <- count_zone_crossings(lo_ci, hi_ci, d)

  if (is.na(lo_pi)) {
    return(paste0("95% CI: [", round(lo_ci, 3), ", ", round(hi_ci, 3),
                  "]. Prediction interval (PI) not available."))
  }

  pi_cross <- count_zone_crossings(lo_pi, hi_pi, d)

  if (pi_cross <= ci_cross)
    "CI and PI cross the same number of zones \u2014 limited clinical impact of heterogeneity."
  else if (pi_cross - ci_cross == 1)
    "PI crosses 1 more zone than CI \u2014 future trials may reach a different conclusion."
  else
    "PI crosses 2 more zones than CI \u2014 heterogeneity allows both beneficial and harmful conclusions."
}
