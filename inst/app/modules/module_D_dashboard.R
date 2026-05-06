# =============================================================================
# Module D: Dashboard & Export
# =============================================================================
# PURPOSE:
#   Display a colour-coded CINeMA + ROB-MEN summary table.
#   Provide CSV, Excel, and forest-plot PNG downloads.
#
# INPUTS:
#   cinema_module  — returned by moduleB_server()
#   robmen_module  — returned by moduleC_server()
#
# NOTES:
#   "Confidence" column = manually set value if provided, else suggested_confidence.
#   Override reasons from D1-D6 are included in CSV/Excel exports.
#   Excel export requires openxlsx:  install.packages("openxlsx")
# =============================================================================

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

.HAS_OPENXLSX <- requireNamespace("openxlsx", quietly = TRUE)

# --------------------------------------------------------------------------
# UI FUNCTION
# --------------------------------------------------------------------------
moduleD_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Report"),

    uiOutput(ns("status_msg")),

    # Palette selector
    tags$div(
      style = "display:flex; align-items:flex-start; gap:10px; margin:10px 0 4px;",
      tags$span(
        style = "font-weight:600; white-space:nowrap; padding-top:6px;",
        "Color palette:"
      ),
      tags$div(
        style = "margin-bottom:0;",
        radioButtons(
          ns("palette"), label = NULL,
          choices  = c("Pastel" = "pastel",
                       "Classic" = "classic"),
          selected = "pastel", inline = TRUE
        )
      )
    ),
    # Confidence downgrade algorithm selector
    tags$div(
      style = "display:flex; align-items:flex-start; gap:10px; margin:4px 0 10px;",
      tags$span(
        style = "font-weight:600; white-space:nowrap; padding-top:6px;",
        "Downgrade algorithm:"
      ),
      tags$div(
        style = "margin-bottom:0;",
        selectInput(
          ns("algo"), label = NULL,
          choices = c(
            "\u2460 Standard: Some=-1, Major=-2; D4/D5/D6 anti-double-counted as one group" = "standard",
            "\u2461 Fractional: Some=-1/3, Major=-1, sum all domains, round" = "fractional"
          ),
          selected = "standard",
          width = "480px"
        )
      )
    ),
    br(),

    h4("Summary Table"),
    p(em("Click the ", strong("Confidence"), " column to set the confidence level",
         " (High / Moderate / Low / Very low).")),
    tags$small(style = "color:#666; display:block; margin-bottom:6px;",
      "Rows are shown in the analysis's default order ",
      "(mixed evidence first, then indirect-only). ",
      "Use the Excel download for offline reordering."),
    DTOutput(ns("combined_dt")),
    br(),

    hr(),
    h4("Network Graph"),
    tags$details(
      tags$summary(style = "cursor:pointer; color:#444; font-size:0.9em;
                            margin-bottom:6px;",
                   "Display options"),
      wellPanel(
        fluidRow(
          column(4, selectInput(ns("netgraph_node_size"), "Node sizing",
            choices = c("By total sample size" = "n",
                        "By number of studies" = "k",
                        "Equal"                = "equal"),
            selected = "n")),
          column(4, selectInput(ns("netgraph_edge_width"), "Edge thickness",
            choices = c("By number of trials" = "number.of.studies",
                        "Inverse variance"    = "se.fixed",
                        "Equal"               = "equal"),
            selected = "number.of.studies")),
          column(4, selectInput(ns("netgraph_seq"), "Treatment order (around circle)",
            choices = c("Optimal (minimise crossings)" = "optimal",
                        "Alphabetic"                   = "alphabetic"),
            selected = "optimal"))
        ),
        fluidRow(
          column(4, selectInput(ns("netgraph_edge_color"), "Edge colour",
            choices = c("CINeMA confidence"           = "cinema",
                        "Within-study bias (Domain 1)" = "wsb",
                        "Monochrome"                  = "mono"),
            selected = "cinema")),
          column(4, sliderInput(ns("netgraph_rotate"),
                                "Rotation (°)", -180, 180, 0, step = 5)),
          column(4, sliderInput(ns("netgraph_edge_label_pos"),
                                "Edge-label position (0 = node A → 1 = node B)",
                                min = 0.1, max = 0.9, value = 0.45,
                                step = 0.05))
        ),
        fluidRow(
          column(4, checkboxInput(ns("netgraph_show_labels"),
                                  "Show treatment labels", TRUE)),
          column(4, checkboxInput(ns("netgraph_edge_label"),
                                  "Show n-studies on edges", TRUE)),
          column(4, sliderInput(ns("netgraph_height"),
                                "Plot height (px)", 300, 900, 400, step = 50))
        )
      )
    ),
    uiOutput(ns("netgraph_plot_ui")),
    div(style = "margin-top:8px;",
      downloadButton(ns("dl_netgraph"), "Download Network PNG",
                     class = "btn btn-outline-secondary btn-sm",
                     icon  = icon("image"))),
    br(),

    hr(),
    h4("Forest Plot"),
    p(em("Canonical netmeta::forest() output. Use Display Options to",
         " change reference, sort order, x-axis, or appearance.")),
    tags$details(
      tags$summary(style = "cursor:pointer; color:#444; font-size:0.9em;
                            margin-bottom:6px;",
                   "Display options"),
      wellPanel(
        fluidRow(
          column(4, uiOutput(ns("forest_ref_picker"))),
          column(4, selectInput(ns("forest_sort"), "Sort order",
            choices = c(
              "Default (P-score)"                              = "pscore",
              "CINeMA category (High+Moderate first), then P-score"
                                                               = "cinema_pscore",
              "By point estimate"                              = "estimate",
              "Alphabetic"                                     = "alpha"),
            selected = "pscore")),
          column(4, sliderInput(ns("forest_height"),
                                "Plot height (px)", 300, 1500, 600, step = 50))
        ),
        fluidRow(
          column(4, numericInput(ns("forest_xlim_lo"), "x min (blank = auto)",
                                 value = NA)),
          column(4, numericInput(ns("forest_xlim_hi"), "x max (blank = auto)",
                                 value = NA)),
          column(4, checkboxInput(ns("forest_log_scale"),
                                  "Log x-axis (OR/RR)", TRUE))
        ),
        fluidRow(
          column(4, checkboxInput(ns("forest_show_k"),
                                  "Show k (number of studies) column", TRUE)),
          column(4, checkboxInput(ns("forest_show_w"),
                                  "Show weight column", FALSE)),
          column(4, checkboxInput(ns("forest_print_hetstat"),
                                  "Show heterogeneity row (tau², I²)", FALSE))
        ),
        fluidRow(
          column(4, checkboxInput(ns("forest_smaller_text"),
                                  "Smaller text (dense plots)", FALSE)),
          column(8, textInput(ns("forest_smlab"),
                              "Header text (blank = auto)", value = ""))
        )
      )
    ),
    uiOutput(ns("forest_plot_ui")),
    br(),

    hr(),
    h4("League Table"),
    p(em(
      "Lower-left triangle: NMA estimate [95% CI] for column vs row treatment.",
      " Cell color = CINeMA confidence."
    )),
    uiOutput(ns("league_table_ui")),
    br(),

    wellPanel(
      h4("Export"),

      # --- Primary: {netmetaviz}-compatible CSV ---------------------------
      div(style = "border:1px solid #cce5ff; background:#f0f7ff;
                   border-radius:0.5rem; padding:14px 16px; margin-bottom:14px;",
        fluidRow(
          column(8,
            h5(style = "margin-top:0;",
               icon("file-csv"), " {netmetaviz} CSV ",
               tags$span(style = "background:#0d6efd; color:white;
                          padding:2px 8px; border-radius:3px; font-size:0.7em;
                          margin-left:6px; vertical-align:middle;",
                         "RECOMMENDED")),
            p(style = "margin-bottom:6px;",
              "The reproducible artefact for ",
              tags$a("{netmetaviz}", target = "_blank",
                     href = "https://github.com/CINeMA-team/netmetaviz"),
              "-based visualisation pipelines. Schema matches the CINeMA",
              " web tool's exported CSV (one row per comparison, all six",
              " domain ratings, confidence, downgrade reasons).")
          ),
          column(4,
            downloadButton(ns("dl_nmv_csv"), "Download CSV",
                           class = "btn btn-primary btn-lg btn-block",
                           icon  = icon("file-csv")),
            tags$div(style = "margin-top:8px;",
              actionButton(ns("save_to_env"), "Save to R environment",
                           class = "btn btn-outline-secondary btn-sm btn-block",
                           icon  = icon("r-project")))
          )
        )
      ),

      # --- Bundle export (ZIP) — Phase A -----------------------------------
      div(style = "border:1px solid #d4d4d8; background:#fafafa;
                   border-radius:0.5rem; padding:14px 16px; margin-bottom:14px;",
        h5(style = "margin-top:0;",
           icon("file-archive"), " Bundle export (ZIP)"),
        p(style = "font-size:0.9em; color:#555; margin-bottom:8px;",
          "Bundle the analysis artefacts you tick below into a single",
          " ZIP. Local/global tests and the pairwise meta-analysis",
          " appendix are coming in a follow-up phase."),
        fluidRow(
          column(8,
            checkboxGroupInput(ns("bundle_items"),
              label = NULL,
              choices = c(
                "R script (reproducibility template)"          = "r_script",
                "netmeta object (.rds)"                        = "netmeta_rds",
                "{netmetaviz}-format CSV"                      = "cinema_csv",
                "Network graph (PNG)"                          = "netgraph_png",
                "Forest plot (PNG)"                            = "forest_png",
                "CINeMA Summary table — landscape Word"        = "summary_docx",
                "CINeMA Summary table — Excel"                 = "summary_xlsx",
                "League table — landscape Word"                = "league_docx",
                "League table — Excel"                         = "league_xlsx",
                "ROB-MEN evaluation — landscape Word"          = "robmen_docx",
                "ROB-MEN evaluation — Excel"                   = "robmen_xlsx"),
              selected = c("r_script", "netmeta_rds", "cinema_csv",
                           "netgraph_png", "forest_png",
                           "summary_docx", "league_docx", "robmen_docx"))
          ),
          column(4,
            downloadButton(ns("dl_bundle"), "Download Bundle (ZIP)",
                           class = "btn btn-primary btn-block",
                           icon  = icon("file-archive")),
            tags$small(style = "color:#888; display:block; margin-top:6px;",
              "Tick at least one item.")
          )
        )
      ),

      # --- Secondary: raw / convenience exports ---------------------------
      h5(style = "color:#666; font-size:0.95em; margin-top:14px;",
         "Other formats"),
      fluidRow(
        column(4,
          h6(icon("file-csv"), " CSV"),
          p(style = "font-size:0.85em; color:#666;",
            "Raw table — all domains, override reasons, and network estimates."),
          downloadButton(ns("dl_csv"), "Download CSV",
                         class = "btn btn-outline-secondary btn-sm btn-block",
                         icon  = icon("file-csv"))
        ),
        column(4,
          h6(icon("file-excel"), " Excel (.xlsx)"),
          p(style = "font-size:0.85em; color:#666;",
            "Colour-coded workbook (openxlsx)."),
          if (.HAS_OPENXLSX) {
            downloadButton(ns("dl_xlsx"), "Download Excel",
                           class = "btn btn-outline-success btn-sm btn-block",
                           icon  = icon("file-excel"))
          } else {
            div(class = "alert alert-warning",
                style = "font-size:0.85em; padding:6px 10px;",
                icon("exclamation-circle"),
                strong(" openxlsx not installed."), br(),
                code('install.packages("openxlsx")'))
          }
        ),
        column(4,
          h6(icon("image"), " Forest Plot (PNG)"),
          p(style = "font-size:0.85em; color:#666;",
            "NMA estimates with CINeMA confidence colours."),
          downloadButton(ns("dl_forest"), "Download PNG",
                         class = "btn btn-outline-primary btn-sm btn-block",
                         icon  = icon("image"))
        )
      )
    ),

    hr(),
    h4("Recommended Citations"),
    tags$ol(
      tags$li(
        "Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: An approach for assessing",
        " confidence in the results of a network meta-analysis.",
        em(" PLoS Med."), " 2020;17(4):e1003082.",
        tags$a("doi:10.1371/journal.pmed.1003082",
               href = "https://doi.org/10.1371/journal.pmed.1003082",
               target = "_blank")
      ),
      tags$li(
        "Chiocchia V, Nikolakopoulou A, Higgins JPT, et al. ROB-MEN: a tool to assess risk of bias",
        " due to missing evidence in network meta-analysis.",
        em(" BMC Med."), " 2021;19(1):304.",
        tags$a("doi:10.1186/s12916-021-02166-3",
               href = "https://doi.org/10.1186/s12916-021-02166-3",
               target = "_blank")
      )
    )
  )
}

# --------------------------------------------------------------------------
# SERVER FUNCTION
# --------------------------------------------------------------------------
moduleD_server <- function(id, cinema_module, robmen_module,
                           nma_settings_r = NULL, go_to_cinema = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Helper: compute total N per treatment from pairwise data
    # ------------------------------------------------------------------
    compute_trt_n <- function(df_raw) {
      if (is.null(df_raw)) return(NULL)
      has_n1n2 <- all(c("n1", "n2") %in% names(df_raw))
      has_n    <- "n" %in% names(df_raw) && any(!is.na(df_raw$n))
      if (!has_n1n2 && !has_n) return(NULL)

      if (has_n1n2) {
        trt_n <- dplyr::bind_rows(
          data.frame(trt = df_raw$t1, n = df_raw$n1, stringsAsFactors = FALSE),
          data.frame(trt = df_raw$t2, n = df_raw$n2, stringsAsFactors = FALSE)
        )
      } else {
        trt_n <- data.frame(
          trt = c(df_raw$t1, df_raw$t2),
          n   = c(df_raw$n / 2, df_raw$n / 2),
          stringsAsFactors = FALSE
        )
      }
      trt_n %>%
        filter(!is.na(n)) %>%
        group_by(trt) %>%
        summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        { stats::setNames(.$total_n, .$trt) }
    }

    # ------------------------------------------------------------------
    # Helper: build netmetaviz-compatible data frame
    # ------------------------------------------------------------------
    nmv_cinema_df <- reactive({
      merged <- cinema_merged()
      req(!is.null(merged))

      data.frame(
        Comparison                    = gsub(" vs ", ":", merged$comparison),
        `Number of studies`           = merged$n_studies,
        `Within-study bias`           = merged$within_study_bias,
        `Reporting bias`              = merged$reporting_bias,
        Indirectness                  = merged$indirectness,
        Imprecision                   = merged$imprecision,
        Heterogeneity                 = merged$heterogeneity,
        Incoherence                   = merged$incoherence,
        `Confidence rating`           = ifelse(nzchar(merged$confidence),
                                               merged$confidence,
                                               merged$suggested_confidence),
        `Reason(s) for downgrading`   = ifelse(
          "downgrade_reason" %in% names(merged),
          merged$downgrade_reason, ""
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })

    # ------------------------------------------------------------------
    # Active colour palette (switches between classic and pastel)
    # ------------------------------------------------------------------
    current_palette <- reactive({
      pal_name <- input$palette %||% "pastel"
      .NMV_PALETTES[[pal_name]] %||% .NMV_PALETTES[["pastel"]]
    })

    # ------------------------------------------------------------------
    # Convenience reactives
    # ------------------------------------------------------------------
    cinema_data <- reactive({
      cr <- cinema_module$cinema_results()
      req(!is.null(cr))
      cr
    })

    robmen_data <- reactive({
      tryCatch(robmen_module$robmen_results(), error = function(e) NULL)
    })

    # merged with suggested_confidence re-computed using the algo from this tab
    cinema_merged <- reactive({
      cr   <- cinema_data()
      m    <- cr$merged
      algo <- input$algo %||% "standard"
      n    <- nrow(m)
      m$suggested_confidence <- sapply(seq_len(n), function(i) {
        compute_auto_confidence(as.list(m[i, ]), algorithm = algo)
      })
      m
    })

    # Navigate to Module B when user clicks the link in the warning
    observeEvent(input$go_to_cinema_link, {
      if (!is.null(go_to_cinema)) go_to_cinema()
    })

    # ------------------------------------------------------------------
    # Summary table: D1-D6 + single Confidence + ROB-MEN
    # Confidence = manually set value if present, else suggested_confidence
    # ------------------------------------------------------------------
    summary_df <- reactive({
      merged <- cinema_merged()
      rb     <- robmen_data()

      out <- merged %>%
        transmute(
          Comparison              = comparison,
          `D1: Within-study bias` = within_study_bias,
          `D2: Reporting bias`    = reporting_bias,
          `D3: Indirectness`      = indirectness,
          `D4: Imprecision`       = imprecision,
          `D5: Heterogeneity`     = heterogeneity,
          `D6: Incoherence`       = incoherence,
          Confidence              = ifelse(nzchar(confidence),
                                           confidence, suggested_confidence)
        )

      out
    })

    # ------------------------------------------------------------------
    # summary_export_pack: Summary table + per-cell colour matrices for
    # Word/Excel export. Body cells coloured by the CINeMA domain rating
    # palette; the Confidence column uses the confidence palette so it
    # visually matches the on-screen Summary Table.
    # ------------------------------------------------------------------
    summary_export_pack <- reactive({
      df <- summary_df()
      n  <- nrow(df)
      bg <- matrix(NA_character_, nrow = n, ncol = ncol(df))
      tx <- matrix(NA_character_, nrow = n, ncol = ncol(df))

      domain_cols <- which(startsWith(names(df), "D"))   # D1: ... D6: ...
      conf_col    <- which(names(df) == "Confidence")

      for (j in domain_cols) {
        for (i in seq_len(n)) {
          v <- df[i, j]
          if (!is.null(v) && !is.na(v) && nzchar(v) && v %in% names(.CINEMA_COL)) {
            bg[i, j] <- unname(.CINEMA_COL[v])
            tx[i, j] <- unname(.CINEMA_TXT[v])
          }
        }
      }
      pal <- current_palette()
      conf_bg_pal  <- pal$conf
      conf_txt_pal <- pal$conf_txt
      if (length(conf_col) == 1) {
        for (i in seq_len(n)) {
          v <- df[i, conf_col]
          if (!is.null(v) && !is.na(v) && nzchar(v) && v %in% names(conf_bg_pal)) {
            bg[i, conf_col] <- unname(conf_bg_pal[v])
            tx[i, conf_col] <- unname(conf_txt_pal[v])
          }
        }
      }
      list(df = df, cell_colors = bg, cell_text_colors = tx)
    })

    # ------------------------------------------------------------------
    # league_export_pack: lower-triangle league table + colour matrices.
    # ------------------------------------------------------------------
    league_export_pack <- reactive({
      cr     <- cinema_data()
      merged <- cinema_merged()
      sm <- if (!is.null(nma_settings_r))
              nma_settings_r()$effect_measure %||%
                tryCatch(cr$net$sm, error = function(e) "")
            else tryCatch(cr$net$sm, error = function(e) "")
      pal <- current_palette()
      build_league_table_df(cr$net, merged, sm = sm,
                            conf_bg  = pal$conf,
                            conf_txt = pal$conf_txt)
    })

    # ------------------------------------------------------------------
    # robmen_export_pack: ROB-MEN table extracted from moduleC's results.
    # Returns NULL when ROB-MEN has not produced anything yet.
    # ------------------------------------------------------------------
    robmen_export_pack <- reactive({
      rb <- robmen_data()
      df <- build_robmen_export_df(rb)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      list(df = df, cell_colors = NULL, cell_text_colors = NULL)
    })

    # ------------------------------------------------------------------
    # Export data frame: includes per-domain reasons + NE + ROB-MEN
    # ------------------------------------------------------------------
    export_df <- reactive({
      cr     <- cinema_data()
      merged <- cinema_merged()
      te_df  <- cr$te_df
      rb     <- robmen_data()

      domain_labels <- c("D1: Within-study bias", "D2: Reporting bias",
                         "D3: Indirectness", "D4: Imprecision",
                         "D5: Heterogeneity", "D6: Incoherence")
      domain_cols   <- c("within_study_bias", "reporting_bias", "indirectness",
                         "imprecision", "heterogeneity", "incoherence")
      reason_cols   <- paste0(domain_cols, "_reason")

      out <- data.frame(Comparison = merged$comparison, stringsAsFactors = FALSE)

      for (di in seq_along(domain_labels)) {
        out[[domain_labels[di]]] <- merged[[domain_cols[di]]]
        rc   <- reason_cols[di]
        rvals <- if (rc %in% names(merged)) merged[[rc]] else rep("", nrow(merged))
        if (any(nzchar(rvals))) {
          out[[paste0(domain_labels[di], " (reason)")]] <- rvals
        }
      }

      out[["Suggested confidence"]] <- merged$suggested_confidence
      out[["Confidence"]] <- ifelse(nzchar(merged$confidence),
                                    merged$confidence, merged$suggested_confidence)
      if ("downgrade_reason" %in% names(merged)) {
        out[["Confidence reason"]] <- merged$downgrade_reason
      }

      # Network estimates
      te_out <- te_df %>%
        transmute(
          Comparison         = comparison,
          `Effect size (TE)` = round(TE, 4),
          `Lower 95% CI`     = round(lower, 4),
          `Upper 95% CI`     = round(upper, 4)
        )
      out <- left_join(out, te_out, by = "Comparison")

      if (!is.null(rb)) {
        rb_sel <- rb %>% select(Comparison = comparison, `ROB-MEN` = robmen_rating)
        out <- left_join(out, rb_sel, by = "Comparison")
      }
      out
    })

    # ------------------------------------------------------------------
    # OUTPUT: status message
    # ------------------------------------------------------------------
    output$status_msg <- renderUI({
      tryCatch({
        cr <- cinema_module$cinema_results()
        if (is.null(cr)) {
          return(div(
            class = "alert alert-warning",
            icon("exclamation-circle"),
            " Module B (CINeMA) has not been run yet.",
            actionLink(
              ns("go_to_cinema_link"),
              "\u2192 Go to Module B",
              style = "font-weight:bold; margin-left:8px;"
            )
          ))
        }
        NULL  # ready — no banner needed
      }, error = function(e) {
        div(
          class = "alert alert-warning",
          icon("exclamation-circle"),
          " Module B (CINeMA) has not been run yet.",
          actionLink(
            ns("go_to_cinema_link"),
            "\u2192 Go to Module B",
            style = "font-weight:bold; margin-left:8px;"
          )
        )
      })
    })

    # Confidence overrides set directly in the DT table
    conf_overrides <- reactiveValues(vals = list())

    # ------------------------------------------------------------------
    # OUTPUT: combined DT
    # ------------------------------------------------------------------
    output$combined_dt <- renderDT({
      df <- summary_df()

      # Apply conf_overrides to the Confidence column
      ov <- conf_overrides$vals
      if (length(ov) > 0) {
        for (nm in names(ov)) {
          idx <- which(df$Comparison == nm)
          if (length(idx) > 0) df$Confidence[idx] <- ov[[nm]]
        }
      }

      pal           <- current_palette()
      domain_cols   <- c("D1: Within-study bias", "D2: Reporting bias",
                         "D3: Indirectness", "D4: Imprecision",
                         "D5: Heterogeneity", "D6: Incoherence")

      conf_col_idx  <- which(names(df) == "Confidence") - 1L  # 0-indexed for DT
      conf_input_id <- ns("conf_change")

      # Inject palette colours into JS via sprintf so the DT reacts to palette changes
      cb_js <- sprintf("
        var confBg  = {'High':'%s','Moderate':'%s','Low':'%s','Very low':'%s'};
        var confTxt = {'High':'%s','Moderate':'%s','Low':'%s','Very low':'%s'};
        table.on('change', 'select.conf-select', function() {
          var row = parseInt($(this).data('row')) + 1;
          var val = $(this).val();
          var bg  = confBg[val]  || '%s';
          var txt = confTxt[val] || '%s';
          $(this).css({'background-color': bg, 'color': txt});
          Shiny.setInputValue('%s', {row: row, value: val}, {priority: 'event'});
        });
      ",
        pal$conf[["High"]], pal$conf[["Moderate"]],
        pal$conf[["Low"]],  pal$conf[["Very low"]],
        pal$conf_txt[["High"]], pal$conf_txt[["Moderate"]],
        pal$conf_txt[["Low"]],  pal$conf_txt[["Very low"]],
        pal$conf[["Not set"]], pal$conf_txt[["Not set"]],
        conf_input_id
      )

      render_js <- sprintf("function(data, type, row, meta) {
        if (type !== 'display') return data;
        var confBg  = {'High':'%s','Moderate':'%s','Low':'%s','Very low':'%s'};
        var confTxt = {'High':'%s','Moderate':'%s','Low':'%s','Very low':'%s'};
        var bg  = confBg[data]  || '%s';
        var txt = confTxt[data] || '%s';
        var opts = ['High', 'Moderate', 'Low', 'Very low'];
        var sel = '<select class=\"conf-select\" data-row=\"' + meta.row +
          '\" style=\"background-color:' + bg +
          '; color:' + txt +
          '; font-size:0.9em; padding:2px 6px; border:none;' +
          ' border-radius:3px; font-weight:bold; cursor:pointer; width:100%%;\">';
        opts.forEach(function(o) {
          sel += '<option value=\"' + o + '\"' +
            (data === o ? ' selected' : '') + '>' + o + '</option>';
        });
        sel += '</select>';
        return sel;
      }",
        pal$conf[["High"]], pal$conf[["Moderate"]],
        pal$conf[["Low"]],  pal$conf[["Very low"]],
        pal$conf_txt[["High"]], pal$conf_txt[["Moderate"]],
        pal$conf_txt[["Low"]],  pal$conf_txt[["Very low"]],
        pal$conf[["Not set"]], pal$conf_txt[["Not set"]]
      )

      dt <- datatable(df, rownames = FALSE,
                      callback = JS(cb_js),
                      options  = list(
                        pageLength = 20, scrollX = TRUE, dom = "t",
                        ordering   = FALSE,  # spec-10: rows fixed at netmeta
                                             # default order; column-header
                                             # sort is disabled.
                        columnDefs = list(list(
                          targets = conf_col_idx,
                          render  = JS(render_js)
                        ))
                      ),
                      class = "cell-border stripe")

      for (col in domain_cols) {
        if (!col %in% names(df)) next
        dt <- dt %>%
          formatStyle(col,
            backgroundColor = styleEqual(names(pal$cinema), unname(pal$cinema)),
            color           = styleEqual(names(pal$cinema_txt), unname(pal$cinema_txt))
          )
      }

      # NOTE: Confidence column colors are applied inline in the JS render function above.
      # No formatStyle() call here — that would override the inline styles.

      dt
    })

    observeEvent(input$conf_change, {
      info <- input$conf_change
      if (is.null(info)) return()
      df   <- summary_df()
      row  <- info$row
      val  <- info$value
      if (is.null(row) || row < 1 || row > nrow(df)) return()
      comp <- df$Comparison[row]
      valid_vals <- c("High", "Moderate", "Low", "Very low", "")
      if (val %in% valid_vals) {
        conf_overrides$vals[[comp]] <- val
      }
    })

    # ------------------------------------------------------------------
    # DOWNLOAD: CSV
    # ------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("nma_evaluation_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- tryCatch(export_df(), error = function(e) NULL)
        req(!is.null(df))
        write.csv(df, file, row.names = FALSE, na = "")
      }
    )

    # ------------------------------------------------------------------
    # DOWNLOAD: Excel (openxlsx)
    # ------------------------------------------------------------------
    output$dl_xlsx <- downloadHandler(
      filename = function() {
        paste0("nma_evaluation_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(.HAS_OPENXLSX)
        cr <- tryCatch(cinema_data(), error = function(e) NULL)
        req(!is.null(cr))

        wb <- openxlsx::createWorkbook()

        # ---------- Sheet 1: Summary ----------
        sheet1 <- summary_df()
        openxlsx::addWorksheet(wb, "CINeMA + ROB-MEN")
        openxlsx::writeData(wb, "CINeMA + ROB-MEN", sheet1)

        colour_map <- c(
          "No concerns"    = "70AD47",
          "Some concerns"  = "FFC000",
          "Major concerns" = "C00000",
          "Not assessed"   = "BFBFBF"
        )
        conf_map <- c(
          "High"     = "4472C4",
          "Moderate" = "5B9BD5",
          "Low"      = "ED7D31",
          "Very low" = "C00000"
        )
        robmen_map <- c(
          "Low risk"      = "4CAF50",
          "Some concerns" = "FF9800",
          "High risk"     = "F44336",
          "Not assessed"  = "9E9E9E"
        )

        domain_col_names <- c("D1: Within-study bias", "D2: Reporting bias",
                              "D3: Indirectness", "D4: Imprecision",
                              "D5: Heterogeneity", "D6: Incoherence")

        for (col_name in domain_col_names) {
          col_num <- match(col_name, names(sheet1))
          if (is.na(col_num)) next
          vals <- sheet1[[col_name]]
          for (row_i in seq_along(vals)) {
            bg <- colour_map[as.character(vals[row_i])]
            if (!is.na(bg)) {
              openxlsx::addStyle(wb, "CINeMA + ROB-MEN",
                style = openxlsx::createStyle(
                  fgFill     = paste0("#", bg),
                  fontColour = if (as.character(vals[row_i]) == "Some concerns")
                                 "#000000" else "#FFFFFF"
                ),
                rows = row_i + 1, cols = col_num, gridExpand = FALSE)
            }
          }
        }

        conf_col <- match("Confidence", names(sheet1))
        if (!is.na(conf_col)) {
          vals <- sheet1[["Confidence"]]
          for (row_i in seq_along(vals)) {
            bg <- conf_map[as.character(vals[row_i])]
            if (!is.na(bg)) {
              openxlsx::addStyle(wb, "CINeMA + ROB-MEN",
                style = openxlsx::createStyle(
                  fgFill = paste0("#", bg), fontColour = "#FFFFFF",
                  textDecoration = "bold"
                ),
                rows = row_i + 1, cols = conf_col, gridExpand = FALSE)
            }
          }
        }

        if ("ROB-MEN" %in% names(sheet1)) {
          rob_col <- match("ROB-MEN", names(sheet1))
          vals    <- sheet1[["ROB-MEN"]]
          for (row_i in seq_along(vals)) {
            bg <- robmen_map[as.character(vals[row_i])]
            if (!is.na(bg)) {
              openxlsx::addStyle(wb, "CINeMA + ROB-MEN",
                style = openxlsx::createStyle(
                  fgFill = paste0("#", bg), fontColour = "#FFFFFF",
                  textDecoration = "bold"
                ),
                rows = row_i + 1, cols = rob_col, gridExpand = FALSE)
            }
          }
        }

        openxlsx::addStyle(wb, "CINeMA + ROB-MEN",
          style      = openxlsx::createStyle(textDecoration = "bold"),
          rows       = 1,
          cols       = seq_len(ncol(sheet1)),
          gridExpand = FALSE)
        openxlsx::setColWidths(wb, "CINeMA + ROB-MEN", cols = 1,                   widths = 25)
        openxlsx::setColWidths(wb, "CINeMA + ROB-MEN", cols = 2:ncol(sheet1), widths = 18)

        # ---------- Sheet 2: Network estimates ----------
        te_sheet <- cr$te_df %>%
          transmute(
            Comparison         = comparison,
            `Effect size (TE)` = round(TE, 4),
            `Lower 95% CI`     = round(lower, 4),
            `Upper 95% CI`     = round(upper, 4),
            `95% CI (text)`    = paste0("[", round(lower, 3),
                                        ", ", round(upper, 3), "]")
          )
        openxlsx::addWorksheet(wb, "Network estimates")
        openxlsx::writeData(wb, "Network estimates", te_sheet)
        openxlsx::addStyle(wb, "Network estimates",
          style      = openxlsx::createStyle(textDecoration = "bold"),
          rows       = 1,
          cols       = seq_len(ncol(te_sheet)),
          gridExpand = FALSE)
        openxlsx::setColWidths(wb, "Network estimates",
                               cols   = seq_len(ncol(te_sheet)),
                               widths = 20)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # ------------------------------------------------------------------
    # OUTPUT: Network graph (base R plot via netmeta::netgraph)
    # ------------------------------------------------------------------
    # Dynamic-height wrapper so the slider in the Display options accordion
    # actually resizes the rendered plot.
    output$netgraph_plot_ui <- renderUI({
      h <- input$netgraph_height %||% 400
      plotOutput(ns("netgraph_plot"), height = paste0(h, "px"))
    })

    # ------------------------------------------------------------------
    # netgraph_args_r: shared builder (reactive) used by both the inline
    # renderPlot and the dl_netgraph download handler. Returns a flat list
    # ready for do.call(netgraph, ...) plus the chosen plot title.
    #
    # All five user requests for this turn live here:
    #   * seq mapping fix — the dropdown's "alphabetic" value used to be
    #     passed verbatim to netgraph(), which only accepts "optimal" or
    #     a treatment-name vector. Anything else triggered an error and
    #     the previous tryCatch fell back silently to a monochrome plot.
    #     Now we translate "alphabetic" -> sort(net$trts).
    #   * edge_color — separate dropdown (cinema / wsb / mono) controls
    #     how the positional `col` vector is built; "mono" uses a single
    #     dark grey so the user can pick that explicitly rather than
    #     getting it as a silent error fallback.
    #   * rotate — wired to netgraph()'s rotate arg (degrees).
    #   * pos.number.of.studies — wired to the slider so the user can
    #     nudge labels off overlaps.
    #   * Both inline AND export use this same builder so the downloaded
    #     PNG always matches what's on screen.
    # ------------------------------------------------------------------
    netgraph_args_r <- reactive({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      req(!is.null(cr), !is.null(cr$net))

      net    <- cr$net
      merged <- cinema_merged()
      n_trts <- length(net$trts)
      df_raw <- tryCatch(cr$df, error = function(e) NULL)

      # ---- Edge colours ------------------------------------------------
      # Build a per-comparison key-->rating lookup that works for either
      # ordering of the pair (the merged table uses one canonical order;
      # net$comparisons might use the other).
      build_lookup <- function(values) {
        out <- list()
        for (i in seq_along(values)) {
          parts <- strsplit(merged$comparison[i], " vs ")[[1]]
          if (length(parts) == 2) {
            out[[paste(parts[1], parts[2], sep = ":")]] <- values[i]
            out[[paste(parts[2], parts[1], sep = ":")]] <- values[i]
          }
        }
        out
      }

      net_comps <- if (!is.null(net$comparisons)) {
        net$comparisons
      } else {
        trts_sorted <- sort(net$trts)
        nt <- length(trts_sorted)
        comps_fb <- character()
        for (ii in seq_len(nt - 1))
          for (jj in seq(ii + 1, nt))
            comps_fb <- c(comps_fb,
                          paste(trts_sorted[ii], trts_sorted[jj], sep = ":"))
        comps_fb
      }

      pal         <- current_palette()
      ec_choice   <- input$netgraph_edge_color %||% "cinema"

      if (identical(ec_choice, "mono")) {
        edge_cols <- rep("#444444", length(net_comps))
        ec_label  <- "monochrome"
      } else if (identical(ec_choice, "wsb")) {
        # CINeMA Domain 1 (Within-study bias) ratings live on merged$within_study_bias.
        # Map the three ordinal rating strings to the standard CINeMA palette.
        wsb_pal <- c(
          "No concerns"    = unname(.CINEMA_COL["No concerns"]),
          "Some concerns"  = unname(.CINEMA_COL["Some concerns"]),
          "Major concerns" = unname(.CINEMA_COL["Major concerns"]),
          "Not assessed"   = unname(.CINEMA_COL["Not assessed"])
        )
        wsb_vec <- merged$within_study_bias %||% rep("Not assessed", nrow(merged))
        wsb_vec[!nzchar(wsb_vec)] <- "Not assessed"
        wsb_lookup <- build_lookup(wsb_vec)
        edge_cols <- vapply(net_comps, function(k) {
          v <- wsb_lookup[[k]]
          if (is.null(v) || is.na(v)) return(unname(wsb_pal["Not assessed"]))
          res <- wsb_pal[v]
          if (is.na(res)) unname(wsb_pal["Not assessed"]) else unname(res)
        }, character(1))
        ec_label <- "Within-study bias (Domain 1)"
      } else {
        # CINeMA confidence (default).
        conf_col_map <- if ((input$palette %||% "pastel") == "classic") pal$conf
                        else                                            pal$conf_txt
        conf_final <- ifelse(nzchar(merged$confidence),
                             merged$confidence, merged$suggested_confidence)
        conf_final[!nzchar(conf_final)] <- "Not set"
        conf_lookup <- build_lookup(conf_final)
        edge_cols <- vapply(net_comps, function(k) {
          v <- conf_lookup[[k]]
          if (is.null(v) || is.na(v)) return("#BFBFBF")
          res <- conf_col_map[v]
          if (is.na(res)) "#BFBFBF" else unname(res)
        }, character(1))
        ec_label <- "CINeMA confidence"
      }

      # ---- Node size ---------------------------------------------------
      node_mode <- input$netgraph_node_size %||% "n"
      cex_pts <- if (identical(node_mode, "equal")) {
        rep(2.5, n_trts)
      } else if (identical(node_mode, "k")) {
        if (!is.null(df_raw) && all(c("t1", "t2", "studlab") %in% names(df_raw))) {
          k_per <- vapply(net$trts, function(trt) {
            length(unique(df_raw$studlab[df_raw$t1 == trt | df_raw$t2 == trt]))
          }, integer(1))
          r <- range(k_per)
          if (diff(r) > 0) 1.5 + 2.5 * (k_per - r[1]) / diff(r)
          else rep(2.5, n_trts)
        } else rep(2.5, n_trts)
      } else {
        if (!is.null(df_raw) && "n" %in% names(df_raw) && any(!is.na(df_raw$n))) {
          trt_n <- data.frame(
            trt = c(df_raw$t1, df_raw$t2),
            n   = c(df_raw$n / 2, df_raw$n / 2)
          ) %>%
            filter(!is.na(n)) %>%
            group_by(trt) %>%
            summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop")
          raw_sizes <- sapply(net$trts, function(trt) {
            idx <- which(trt_n$trt == trt)
            if (length(idx) > 0) trt_n$total_n[idx] else mean(trt_n$total_n)
          })
          r <- range(raw_sizes)
          if (diff(r) > 0) 1.5 + 2.5 * (raw_sizes - r[1]) / diff(r)
          else rep(2.5, n_trts)
        } else rep(2.5, n_trts)
      }

      # ---- Misc options ------------------------------------------------
      thickness_arg   <- input$netgraph_edge_width %||% "number.of.studies"
      show_labels     <- isTRUE(input$netgraph_show_labels %||% TRUE)
      show_edge_label <- isTRUE(input$netgraph_edge_label  %||% TRUE)
      labels_arg      <- if (show_labels) net$trts else rep("", n_trts)
      rotate_arg      <- as.numeric(input$netgraph_rotate %||% 0)
      pos_lab_arg     <- as.numeric(input$netgraph_edge_label_pos %||% 0.45)

      # Map seq dropdown to a value netgraph actually accepts.
      seq_choice <- input$netgraph_seq %||% "optimal"
      seq_arg <- if (identical(seq_choice, "alphabetic")) sort(net$trts)
                 else                                      "optimal"

      list(
        net   = net,
        title = paste0("Evidence network (edge colour = ", ec_label, ")"),
        args  = list(
          x                     = net,
          seq                   = seq_arg,
          plastic               = FALSE,
          points                = TRUE,
          pch                   = 21,
          cex.points            = cex_pts,
          col.points            = "black",
          bg.points             = "gray",
          thickness             = thickness_arg,
          number.of.studies     = show_edge_label,
          pos.number.of.studies = pos_lab_arg,
          rotate                = rotate_arg,
          multiarm              = FALSE,
          labels                = labels_arg,
          col                   = edge_cols
        )
      )
    })

    # Render the network graph using the shared args builder.
    output$netgraph_plot <- renderPlot({
      built <- netgraph_args_r()
      tryCatch(
        do.call(netgraph, c(built$args, list(main = built$title))),
        error = function(e) {
          # If something below the col-handling errors out, surface it
          # rather than silently going monochrome — the previous fallback
          # masked the seq-value bug for over a year.
          netgraph(built$net, plastic = FALSE,
                   main = paste("Evidence network — fallback (", e$message, ")"))
        }
      )
    })

    # ------------------------------------------------------------------
    # DOWNLOAD: Network graph PNG (uses netgraph_args_r so the file
    # mirrors whatever is currently on screen).
    # ------------------------------------------------------------------
    output$dl_netgraph <- downloadHandler(
      filename = function() {
        paste0("netgraph_", format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        built <- netgraph_args_r()
        h_px  <- input$netgraph_height %||% 600
        png(file, width = 1200, height = max(800, h_px * 2), res = 150)
        on.exit(dev.off())
        tryCatch(
          do.call(netgraph, c(built$args, list(main = built$title))),
          error = function(e) {
            netgraph(built$net, plastic = FALSE,
                     main = paste("Evidence network (", e$message, ")"))
          }
        )
      }
    )

    # ------------------------------------------------------------------
    # OUTPUT: Inline forest plot (plotly, CINeMA confidence colours)
    # ------------------------------------------------------------------
    # Reference-treatment dropdown (driven by the trained network)
    output$forest_ref_picker <- renderUI({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      if (is.null(cr) || is.null(cr$net)) return(NULL)
      trts <- sort(cr$net$trts)
      default <- tryCatch(cr$net$reference.group, error = function(e) trts[1])
      if (is.null(default) || !nzchar(default)) default <- trts[1]
      selectInput(ns("forest_ref"), "Reference treatment",
                  choices = trts, selected = default)
    })

    # ------------------------------------------------------------------
    # forest_confidence_r: per-treatment CINeMA confidence vector keyed
    # by treatment name. Used by the "CINeMA category + P-score" sort
    # mode. Treatments missing from the merged table (or with empty
    # confidence) default to "Not set" so they get the "poor" bucket.
    # ------------------------------------------------------------------
    forest_confidence_r <- reactive({
      cr     <- tryCatch(cinema_data(),    error = function(e) NULL)
      merged <- tryCatch(cinema_merged(),  error = function(e) NULL)
      if (is.null(cr) || is.null(merged)) return(setNames(character(0),
                                                          character(0)))
      ref <- input$forest_ref %||% cr$net$reference.group %||%
             sort(cr$net$trts)[1]
      out <- setNames(rep("Not set", length(cr$net$trts)), cr$net$trts)
      for (i in seq_len(nrow(merged))) {
        parts <- strsplit(merged$comparison[i], " vs ")[[1]]
        if (length(parts) != 2) next
        other <- if (parts[1] == ref) parts[2]
                 else if (parts[2] == ref) parts[1]
                 else next
        cv <- if (nzchar(merged$confidence[i])) merged$confidence[i]
              else                              merged$suggested_confidence[i]
        if (!nzchar(cv)) cv <- "Not set"
        out[other] <- cv
      }
      out[ref] <- "High"   # reference is itself; pin to "good" bucket
      out
    })

    # ------------------------------------------------------------------
    # forest_opts_r: assemble the build_netmeta_forest() options list
    # from Display Options inputs. Single source of truth for both the
    # inline render and the dl_forest download handler.
    # ------------------------------------------------------------------
    forest_opts_r <- reactive({
      cr  <- tryCatch(cinema_data(), error = function(e) NULL)
      req(!is.null(cr), !is.null(cr$net))
      net <- cr$net

      ref <- input$forest_ref %||% net$reference.group %||% sort(net$trts)[1]
      sm  <- if (!is.null(nma_settings_r))
               nma_settings_r()$effect_measure %||%
                 tryCatch(net$sm, error = function(e) "")
             else tryCatch(net$sm, error = function(e) "")
      is_ratio  <- !is.null(sm) && sm %in% c("OR", "RR", "HR")
      small_val <- if (!is.null(nma_settings_r))
                     (nma_settings_r()$small_value_desirable %||% "desirable")
                   else "desirable"

      xlim_lo <- input$forest_xlim_lo
      xlim_hi <- input$forest_xlim_hi
      xlim    <- if (!is.null(xlim_lo) && !is.null(xlim_hi) &&
                     !is.na(xlim_lo)   && !is.na(xlim_hi))
                   c(xlim_lo, xlim_hi) else NULL

      smlab_user <- trimws(input$forest_smlab %||% "")
      smlab <- if (nzchar(smlab_user)) smlab_user else NULL

      list(
        reference     = ref,
        sortvar       = input$forest_sort %||% "pscore",
        confidence    = forest_confidence_r(),
        small_values  = small_val,
        xlim          = xlim,
        log_scale     = isTRUE(input$forest_log_scale %||% TRUE) && is_ratio,
        show_k        = isTRUE(input$forest_show_k        %||% TRUE),
        show_weight   = isTRUE(input$forest_show_w        %||% FALSE),
        print_hetstat = isTRUE(input$forest_print_hetstat %||% FALSE),
        smaller_text  = isTRUE(input$forest_smaller_text  %||% FALSE),
        smlab         = smlab
      )
    })

    # ------------------------------------------------------------------
    # OUTPUT: Inline forest plot (netmeta::forest, rendered as PNG)
    # ------------------------------------------------------------------
    output$forest_plot_ui <- renderUI({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      if (is.null(cr)) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-circle"),
                   " Run Module B (CINeMA) first."))
      }
      h <- input$forest_height %||% 600
      imageOutput(ns("forest_plot_image"),
                  height = paste0(h, "px"),
                  width  = "100%")
    })

    output$forest_plot_image <- renderImage({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      req(!is.null(cr), !is.null(cr$net))

      h_px <- input$forest_height %||% 600
      tmp  <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = 1100, height = max(400, h_px), res = 110)
      tryCatch(
        build_netmeta_forest(cr$net, forest_opts_r()),
        error = function(e) {
          # Fallback: minimal default forest so the user still sees
          # something if a Display Options combination upsets
          # forest.netmeta.
          plot.new()
          title(main = paste("Forest plot unavailable:",
                              conditionMessage(e)))
        },
        finally = grDevices::dev.off()
      )

      list(src         = tmp,
           contentType = "image/png",
           width       = "100%",
           alt         = "NMA forest plot")
    }, deleteFile = TRUE)

    # ------------------------------------------------------------------
    # OUTPUT: League table with CINeMA confidence cell colours
    # ------------------------------------------------------------------
    output$league_table_ui <- renderUI({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      if (is.null(cr)) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-circle"),
                   " Run Module B (CINeMA) first."))
      }

      net    <- cr$net
      merged <- cinema_merged()
      te_df  <- cr$te_df

      sm <- if (!is.null(nma_settings_r))
              nma_settings_r()$effect_measure %||%
                tryCatch(net$sm, error = function(e) "")
            else
              tryCatch(net$sm, error = function(e) "")
      is_ratio <- !is.null(sm) && sm %in% c("OR", "RR")

      trts <- sort(net$trts)
      k    <- length(trts)

      conf_final <- ifelse(nzchar(merged$confidence),
                           merged$confidence, merged$suggested_confidence)
      conf_final[!nzchar(conf_final)] <- "Not set"

      conf_lookup <- setNames(conf_final,         merged$comparison)
      te_lookup   <- setNames(te_df$TE,           te_df$comparison)
      lo_lookup   <- setNames(te_df$lower,        te_df$comparison)
      hi_lookup   <- setNames(te_df$upper,        te_df$comparison)

      pal      <- current_palette()
      conf_bg  <- pal$conf
      conf_txt <- pal$conf_txt

      make_cell <- function(row_trt, col_trt) {
        sorted <- sort(c(row_trt, col_trt))
        key    <- paste(sorted[1], sorted[2], sep = " vs ")

        te_raw <- te_lookup[key]
        lo_raw <- lo_lookup[key]
        hi_raw <- hi_lookup[key]
        conf   <- conf_lookup[key]

        if (is.na(te_raw)) {
          return(tags$td(
            style = "background:#f0f0f0; text-align:center; padding:5px 8px;",
            "—"
          ))
        }

        # te_raw = TE.random[sorted[1], sorted[2]] = effect of sorted[1] vs sorted[2]
        # Cell (row, col) must show effect of col_trt vs row_trt.
        # When row==sorted[1]: col==sorted[2], want TE(sorted[2] vs sorted[1]) = -te_raw
        # When row==sorted[2]: col==sorted[1], want TE(sorted[1] vs sorted[2]) = +te_raw
        if (row_trt == sorted[1]) {
          te_show <- -te_raw; lo_show <- -hi_raw; hi_show <- -lo_raw
        } else {
          te_show <- te_raw;  lo_show <- lo_raw;  hi_show <- hi_raw
        }

        bg  <- if (!is.na(conf) && conf %in% names(conf_bg))
                 conf_bg[conf] else "#BFBFBF"
        txt <- if (!is.na(conf) && conf %in% names(conf_txt))
                 conf_txt[conf] else "white"

        # Convert log scale → ratio scale for OR/RR
        if (is_ratio) {
          te_show <- exp(te_show)
          lo_show <- exp(lo_show)
          hi_show <- exp(hi_show)
        }

        tags$td(
          style = paste0("background:", bg, "; color:", txt, ";",
                         " text-align:center; padding:5px 8px;",
                         " min-width:90px; font-size:0.85em;"),
          div(strong(sprintf("%.2f", te_show))),
          div(style = "font-size:0.9em;",
              paste0("[", sprintf("%.2f", lo_show), ", ", sprintf("%.2f", hi_show), "]"))
        )
      }

      header_row <- tags$tr(
        tags$th(style = "padding:5px 8px;", ""),
        lapply(trts, function(t)
          tags$th(style = "text-align:center; padding:5px 8px; white-space:nowrap;", t)
        )
      )

      body_rows <- lapply(trts, function(row_trt) {
        row_idx <- which(trts == row_trt)
        tags$tr(
          tags$td(
            style = "font-weight:bold; padding:5px 8px; white-space:nowrap;",
            row_trt
          ),
          lapply(trts, function(col_trt) {
            col_idx <- which(trts == col_trt)
            if (row_trt == col_trt) {
              tags$td(
                style = paste0("background:#e8e8e8; font-weight:bold;",
                               " text-align:center; padding:5px 8px;"),
                row_trt
              )
            } else if (row_idx > col_idx) {
              # Lower left triangle: show NMA estimate
              make_cell(row_trt, col_trt)
            } else {
              # Upper right triangle: empty
              tags$td(
                style = "background:#f8f8f8; padding:5px 8px;",
                " "
              )
            }
          })
        )
      })

      legend_badges <- lapply(names(conf_bg), function(lv) {
        span(
          style = paste0("background:", conf_bg[lv], "; color:", conf_txt[lv], ";",
                         " padding:2px 8px; border-radius:3px;",
                         " margin-right:4px; font-size:0.85em;"),
          lv
        )
      })

      tagList(
        p(strong("Confidence: "), legend_badges),
        div(
          style = "overflow-x:auto; margin-top:8px;",
          tags$table(
            style = "border-collapse:collapse;",
            tags$thead(header_row),
            tags$tbody(body_rows)
          )
        )
      )
    })

    # ------------------------------------------------------------------
    # DOWNLOAD: Forest plot PNG (matches the inline view; same builder)
    # ------------------------------------------------------------------
    output$dl_forest <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        cr <- tryCatch(cinema_data(), error = function(e) NULL)
        req(!is.null(cr), !is.null(cr$net))
        h_px <- input$forest_height %||% 600
        # 2x the inline DPI for a print-ready PNG; height scales with the
        # slider so a tall network produces a tall PNG.
        grDevices::png(file,
                       width  = 2200,
                       height = max(800, h_px * 2),
                       res    = 220)
        on.exit(grDevices::dev.off())
        build_netmeta_forest(cr$net, forest_opts_r())
      }
    )

    # ------------------------------------------------------------------
    # DOWNLOAD: netmetaviz-compatible CSV
    # ------------------------------------------------------------------
    output$dl_nmv_csv <- downloadHandler(
      filename = function() {
        paste0("cinema_netmetaviz_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- tryCatch(nmv_cinema_df(), error = function(e) NULL)
        req(!is.null(df))
        write.csv(df, file, row.names = FALSE, na = "")
      }
    )

    # ------------------------------------------------------------------
    # DOWNLOAD: Bundle ZIP (spec Phase A)
    # ------------------------------------------------------------------
    # Builds a ZIP of every artefact the user ticked in the Bundle export
    # checkbox group. Reuses the same builders as the per-format download
    # handlers (build_netmeta_forest, netgraph_args_r, nmv_cinema_df) so the
    # bundle is byte-identical to what the individual buttons would
    # produce. Word/Excel/test/pairwise items are recognised in the choices
    # list but NOT yet implemented here — they are wired in Phase B-D.
    # ------------------------------------------------------------------
    build_r_script <- function(net, ref_trt, sm) {
      # Reproducibility template that loads the bundled netmeta_object.rds
      # and re-runs the headline outputs. Users will customise; the goal is
      # to give them a working starting point that matches what's in the
      # ZIP.
      paste(c(
        "# =====================================================================",
        "# nmatools — bundle reproducibility script",
        sprintf("# Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "# =====================================================================",
        "# This script reproduces the headline outputs for the network",
        "# meta-analysis bundled in this ZIP. Run from the unzipped folder.",
        "",
        "library(netmeta)",
        "library(meta)",
        "",
        "# ---- Load the network object ----------------------------------------",
        "net <- readRDS(\"netmeta_object.rds\")",
        "summary(net)",
        "",
        sprintf("# Effect measure: %s", sm %||% "(unset)"),
        sprintf("# Reference treatment: %s", ref_trt %||% "(auto)"),
        "",
        "# ---- Forest plot (matches forest_plot.png in this bundle) -----------",
        "# Re-render with whatever Display Options you prefer.",
        sprintf("forest(net, reference.group = \"%s\")",
                ref_trt %||% (net$reference.group %||% sort(net$trts)[1])),
        "",
        "# ---- Network graph (matches netgraph.png in this bundle) ------------",
        "netgraph(net, plastic = FALSE, number.of.studies = TRUE)",
        "",
        "# ---- League table ---------------------------------------------------",
        "league <- netleague(net)",
        "print(league)",
        "",
        "# ---- Decomposition / global test of inconsistency -------------------",
        "decomp.design(net)",
        "",
        "# ---- Local test of inconsistency (per design) -----------------------",
        "netsplit(net)",
        "",
        "# ---- {netmetaviz}-format CSV ----------------------------------------",
        "# cinema_netmetaviz_<date>.csv in this bundle is suitable for direct",
        "# use with the {netmetaviz} package or the CINeMA web tool.",
        "# Read it with:",
        "# dat <- read.csv(\"cinema_netmetaviz_<date>.csv\",",
        "#                 check.names = FALSE)",
        ""), collapse = "\n")
    }

    output$dl_bundle <- downloadHandler(
      filename = function() {
        paste0("nmatools_bundle_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        items <- input$bundle_items
        if (is.null(items) || length(items) == 0) {
          # Avoid an empty ZIP: at least one human-readable file the user
          # can see when they unzip.
          tmp <- tempfile()
          dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
          writeLines(c("No items were selected.",
                       "Tick at least one box in the Bundle export panel."),
                     file.path(tmp, "README.txt"))
          old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)
          utils::zip(zipfile = file, files = "README.txt")
          return()
        }

        cr <- tryCatch(cinema_data(), error = function(e) NULL)
        req(!is.null(cr), !is.null(cr$net))

        # Stage every artefact in a temp directory; final step is a single
        # zip() call relative to that directory so the archive's top-level
        # entries are clean (no nested temp paths).
        stage <- tempfile("nmatools_bundle_")
        dir.create(stage, recursive = TRUE)
        on.exit(unlink(stage, recursive = TRUE), add = TRUE)

        date_tag <- format(Sys.Date(), "%Y%m%d")
        ref_trt  <- input$forest_ref %||%
                    tryCatch(cr$net$reference.group, error = function(e) NULL) %||%
                    sort(cr$net$trts)[1]
        sm_val   <- if (!is.null(nma_settings_r))
                      nma_settings_r()$effect_measure %||%
                        tryCatch(cr$net$sm, error = function(e) "")
                    else tryCatch(cr$net$sm, error = function(e) "")

        files_in_zip <- character(0)

        # 1. R script
        if ("r_script" %in% items) {
          fn <- "reproducibility.R"
          writeLines(build_r_script(cr$net, ref_trt, sm_val),
                     file.path(stage, fn))
          files_in_zip <- c(files_in_zip, fn)
        }

        # 2. netmeta object (RDS)
        if ("netmeta_rds" %in% items) {
          fn <- "netmeta_object.rds"
          saveRDS(cr$net, file = file.path(stage, fn))
          files_in_zip <- c(files_in_zip, fn)
        }

        # 3. {netmetaviz} CSV
        if ("cinema_csv" %in% items) {
          df <- tryCatch(nmv_cinema_df(), error = function(e) NULL)
          if (!is.null(df)) {
            fn <- paste0("cinema_netmetaviz_", date_tag, ".csv")
            write.csv(df, file = file.path(stage, fn),
                      row.names = FALSE, na = "")
            files_in_zip <- c(files_in_zip, fn)
          }
        }

        # 4. Network graph PNG
        if ("netgraph_png" %in% items) {
          built <- tryCatch(netgraph_args_r(), error = function(e) NULL)
          if (!is.null(built)) {
            fn <- paste0("netgraph_", date_tag, ".png")
            grDevices::png(file.path(stage, fn),
                           width = 1200, height = 900, res = 150)
            tryCatch(
              do.call(netgraph,
                      c(built$args, list(main = built$title))),
              error = function(e) {
                netgraph(built$net, plastic = FALSE,
                         main = paste("Evidence network (", e$message, ")"))
              },
              finally = grDevices::dev.off())
            files_in_zip <- c(files_in_zip, fn)
          }
        }

        # 5. Forest plot PNG
        if ("forest_png" %in% items) {
          opts <- tryCatch(forest_opts_r(), error = function(e) NULL)
          if (!is.null(opts)) {
            fn   <- paste0("forest_plot_", date_tag, ".png")
            h_px <- input$forest_height %||% 600
            grDevices::png(file.path(stage, fn),
                           width  = 2200,
                           height = max(800, h_px * 2),
                           res    = 220)
            tryCatch(build_netmeta_forest(cr$net, opts),
                     error = function(e) {
                       plot.new()
                       title(main = paste("Forest plot unavailable:",
                                          conditionMessage(e)))
                     },
                     finally = grDevices::dev.off())
            files_in_zip <- c(files_in_zip, fn)
          }
        }

        # ---- Phase B: tables to Word + Excel -------------------------
        # Each item is wrapped in tryCatch so a single missing table
        # doesn't abort the whole ZIP — the user gets whatever else
        # they asked for.

        # 6. Summary Table — Word (landscape)
        if ("summary_docx" %in% items) {
          fn <- paste0("cinema_summary_", date_tag, ".docx")
          tryCatch({
            pack <- summary_export_pack()
            write_landscape_table_docx(
              pack$df, file.path(stage, fn),
              title    = "CINeMA Summary Table",
              subtitle = paste("Generated:", format(Sys.time(),
                                                    "%Y-%m-%d %H:%M")),
              cell_colors      = pack$cell_colors,
              cell_text_colors = pack$cell_text_colors)
            files_in_zip <<- c(files_in_zip, fn)
          }, error = function(e) {
            message("summary_docx failed: ", conditionMessage(e))
          })
        }

        # 7. Summary Table — Excel
        if ("summary_xlsx" %in% items) {
          fn <- paste0("cinema_summary_", date_tag, ".xlsx")
          tryCatch({
            pack <- summary_export_pack()
            write_table_xlsx(
              pack$df, file.path(stage, fn), sheet = "Summary",
              cell_colors      = pack$cell_colors,
              cell_text_colors = pack$cell_text_colors)
            files_in_zip <<- c(files_in_zip, fn)
          }, error = function(e) {
            message("summary_xlsx failed: ", conditionMessage(e))
          })
        }

        # 8. League Table — Word (landscape)
        if ("league_docx" %in% items) {
          fn <- paste0("league_table_", date_tag, ".docx")
          tryCatch({
            pack <- league_export_pack()
            write_landscape_table_docx(
              pack$df, file.path(stage, fn),
              title    = "League Table",
              subtitle = "Lower triangle: NMA estimate of column vs row.",
              footer   = paste0("Cell colour = CINeMA confidence",
                                " (High / Moderate / Low / Very low)."),
              cell_colors      = pack$cell_colors,
              cell_text_colors = pack$cell_text_colors)
            files_in_zip <<- c(files_in_zip, fn)
          }, error = function(e) {
            message("league_docx failed: ", conditionMessage(e))
          })
        }

        # 9. League Table — Excel
        if ("league_xlsx" %in% items) {
          fn <- paste0("league_table_", date_tag, ".xlsx")
          tryCatch({
            pack <- league_export_pack()
            write_table_xlsx(
              pack$df, file.path(stage, fn), sheet = "League",
              cell_colors      = pack$cell_colors,
              cell_text_colors = pack$cell_text_colors)
            files_in_zip <<- c(files_in_zip, fn)
          }, error = function(e) {
            message("league_xlsx failed: ", conditionMessage(e))
          })
        }

        # 10. ROB-MEN evaluation — Word (landscape)
        if ("robmen_docx" %in% items) {
          fn <- paste0("robmen_table_", date_tag, ".docx")
          tryCatch({
            pack <- robmen_export_pack()
            if (is.null(pack)) {
              message("robmen_docx skipped: no ROB-MEN results yet")
            } else {
              write_landscape_table_docx(
                pack$df, file.path(stage, fn),
                title    = "ROB-MEN Evaluation",
                subtitle = "Risk of bias due to missing evidence (Chiocchia 2021).")
              files_in_zip <<- c(files_in_zip, fn)
            }
          }, error = function(e) {
            message("robmen_docx failed: ", conditionMessage(e))
          })
        }

        # 11. ROB-MEN evaluation — Excel
        if ("robmen_xlsx" %in% items) {
          fn <- paste0("robmen_table_", date_tag, ".xlsx")
          tryCatch({
            pack <- robmen_export_pack()
            if (is.null(pack)) {
              message("robmen_xlsx skipped: no ROB-MEN results yet")
            } else {
              write_table_xlsx(pack$df, file.path(stage, fn),
                               sheet = "ROB-MEN")
              files_in_zip <<- c(files_in_zip, fn)
            }
          }, error = function(e) {
            message("robmen_xlsx failed: ", conditionMessage(e))
          })
        }

        if (length(files_in_zip) == 0) {
          writeLines("No artefacts could be produced from the selected items.",
                     file.path(stage, "README.txt"))
          files_in_zip <- "README.txt"
        }

        # zip() needs to be called from inside the staging directory so the
        # archive entries are stored as top-level filenames instead of
        # absolute paths.
        old_wd <- setwd(stage); on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = files_in_zip,
                   flags   = "-q9X")
      }
    )

    # ------------------------------------------------------------------
    # ACTION: Save CINeMA data frame to R global environment
    # ------------------------------------------------------------------
    observeEvent(input$save_to_env, {
      df <- tryCatch(nmv_cinema_df(), error = function(e) NULL)
      if (is.null(df)) {
        showNotification("No CINeMA results to save.", type = "warning")
        return()
      }
      var_name <- "cinema_results"
      assign(var_name, df, envir = .GlobalEnv)
      showNotification(
        tagList(
          icon("check-circle", class = "text-success"),
          strong(paste0(" Saved as '", var_name, "' in R global environment.")),
          tags$br(),
          "Access it with: ", code(var_name)
        ),
        type = "message", duration = 8
      )
    })

  })
}
