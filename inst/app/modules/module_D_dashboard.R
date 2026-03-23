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
    h3("Step 4: Dashboard & Export"),

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
    DTOutput(ns("combined_dt")),
    br(),

    hr(),
    h4("Network Graph"),
    plotOutput(ns("netgraph_plot"), height = "400px"),
    br(),

    hr(),
    h4("Forest Plot"),
    p(em("Points and error bars are colored by CINeMA confidence level.")),
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
      fluidRow(
        column(3,
          h5(icon("file-csv"), " CSV"),
          p(em("All domains, override reasons, and network estimates.")),
          downloadButton(ns("dl_csv"), "Download CSV",
                         class = "btn btn-outline-secondary btn-block",
                         icon  = icon("file-csv"))
        ),
        column(3,
          h5(icon("file-excel"), " Excel (.xlsx)"),
          p(em("Color-coded workbook (openxlsx).")),
          if (.HAS_OPENXLSX) {
            downloadButton(ns("dl_xlsx"), "Download Excel",
                           class = "btn btn-success btn-block",
                           icon  = icon("file-excel"))
          } else {
            div(class = "alert alert-warning",
                icon("exclamation-circle"),
                strong(" openxlsx not installed."), br(),
                code('install.packages("openxlsx")'))
          }
        ),
        column(3,
          h5(icon("image"), " Forest Plot (PNG)"),
          p(em("NMA estimates with CINeMA confidence colors.")),
          downloadButton(ns("dl_forest"), "Download PNG",
                         class = "btn btn-outline-primary btn-block",
                         icon  = icon("image"))
        ),
        column(3,
          h5(icon("file-csv"), " netmetaviz CSV"),
          p(em("CINeMA results in {netmetaviz} format.")),
          downloadButton(ns("dl_nmv_csv"), "Download CINeMA results CSV",
                         class = "btn btn-outline-info btn-block",
                         icon  = icon("file-csv")),
          tags$div(
            style = "margin-top:8px;",
            actionButton(ns("save_to_env"), "Save to R environment",
                         class = "btn btn-outline-dark btn-sm btn-block",
                         icon  = icon("r-project"))
          )
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
    output$netgraph_plot <- renderPlot({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      req(!is.null(cr), !is.null(cr$net))

      net    <- cr$net
      merged <- cinema_merged()

      # Build per-comparison confidence colours for edge colouring
      pal         <- current_palette()
      conf_col_map <- if ((input$palette %||% "pastel") == "classic") pal$conf else pal$conf_txt
      conf_final  <- ifelse(nzchar(merged$confidence),
                            merged$confidence, merged$suggested_confidence)
      conf_final[!nzchar(conf_final)] <- "Not set"

      # Build per-comparison confidence lookup keyed by "A:B" (both orderings)
      conf_key_lookup <- list()
      for (i in seq_along(conf_final)) {
        parts <- strsplit(merged$comparison[i], " vs ")[[1]]
        if (length(parts) == 2) {
          k1 <- paste(parts[1], parts[2], sep = ":")
          k2 <- paste(parts[2], parts[1], sep = ":")
          conf_key_lookup[[k1]] <- conf_final[i]
          conf_key_lookup[[k2]] <- conf_final[i]
        }
      }

      # Get ordered comparison list from the network object.
      # netmeta::netgraph() expects col as a POSITIONAL vector matching the
      # internal comparison order (net$comparisons, alphabetically sorted pairs).
      net_comps <- if (!is.null(net$comparisons)) {
        net$comparisons
      } else {
        # Fallback: generate all pairs in the same alphabetical order netmeta uses
        trts_sorted <- sort(net$trts)
        nt <- length(trts_sorted)
        comps_fb <- character()
        for (ii in seq_len(nt - 1))
          for (jj in seq(ii + 1, nt))
            comps_fb <- c(comps_fb, paste(trts_sorted[ii], trts_sorted[jj], sep = ":"))
        comps_fb
      }

      # Positional edge color vector (same length & order as net_comps)
      edge_cols <- vapply(net_comps, function(k) {
        cv <- conf_key_lookup[[k]]
        if (is.null(cv) || is.na(cv)) return("#BFBFBF")
        res <- conf_col_map[cv]
        if (is.na(res)) "#BFBFBF" else unname(res)
      }, character(1))

      n_trts <- length(net$trts)

      # Node size: proportional to total participants per treatment
      df_raw <- tryCatch(cr$df, error = function(e) NULL)
      cex_pts <- if (!is.null(df_raw) && "n" %in% names(df_raw) &&
                      any(!is.na(df_raw$n))) {
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
        else rep(2.5, length(raw_sizes))
      } else {
        rep(2.5, n_trts)
      }

      tryCatch(
        netgraph(net,
                 seq              = "optimal",
                 plastic          = FALSE,
                 points           = TRUE,
                 pch              = 21,
                 cex.points       = cex_pts,
                 col.points       = "black",
                 bg.points        = "gray",
                 thickness        = "number.of.studies",
                 number.of.studies = TRUE,
                 pos.number.of.studies = 0.45,
                 multiarm         = FALSE,
                 col              = edge_cols,
                 main             = "Evidence network (edge color = CINeMA confidence)"),
        error = function(e) {
          tryCatch(
            netgraph(net,
                     seq              = "optimal",
                     plastic          = FALSE,
                     points           = TRUE, pch = 21,
                     cex.points       = cex_pts,
                     col.points       = "black",
                     bg.points        = "gray",
                     thickness        = "number.of.studies",
                     number.of.studies = TRUE,
                     pos.number.of.studies = 0.45,
                     multiarm         = FALSE,
                     main             = "Evidence network"),
            error = function(e2) {
              netgraph(net, plastic = FALSE,
                       main = "Evidence network")
            }
          )
        }
      )
    })

    # ------------------------------------------------------------------
    # OUTPUT: Inline forest plot (plotly, CINeMA confidence colours)
    # ------------------------------------------------------------------
    output$forest_plot_ui <- renderUI({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      if (is.null(cr)) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-circle"),
                   " Run Module B (CINeMA) first."))
      }
      n_comps <- nrow(cr$te_df)
      plot_height <- paste0(max(300, 60 + n_comps * 32), "px")
      plotlyOutput(ns("forest_plot_inline"), height = plot_height)
    })

    output$forest_plot_inline <- renderPlotly({
      cr <- tryCatch(cinema_data(), error = function(e) NULL)
      req(!is.null(cr))

      te_df  <- cr$te_df
      merged <- cinema_merged()
      net    <- cr$net
      pal    <- current_palette()
      ref    <- tryCatch(net$reference.group, error = function(e) NULL)
      sm     <- if (!is.null(nma_settings_r)) nma_settings_r()$effect_measure %||%
                  tryCatch(net$sm, error = function(e) "") else
                  tryCatch(net$sm, error = function(e) "")
      is_ratio <- !is.null(sm) && sm %in% c("OR", "RR")

      # small_value_desirable from Module A settings
      small_val <- if (!is.null(nma_settings_r))
                     (nma_settings_r()$small_value_desirable %||% "desirable")
                   else "desirable"

      # Per-treatment total N
      trt_n <- compute_trt_n(cr$df)

      # P-scores for sorting
      p_scores_obj <- tryCatch(netrank(net, small.values = small_val),
                               error = function(e) NULL)
      p_scores <- if (!is.null(p_scores_obj)) {
        p_scores_obj$Pscore.random %||%
          p_scores_obj$Pscore.common %||%
          p_scores_obj$random.w     %||%
          p_scores_obj$common.w
      } else NULL

      if (!is.null(ref) && nzchar(ref)) {
        te_df  <- te_df  %>% filter(grepl(ref, comparison, fixed = TRUE))
        merged <- merged %>% filter(grepl(ref, comparison, fixed = TRUE))
      }

      conf_col_map <- if ((input$palette %||% "pastel") == "classic") pal$conf else pal$conf_txt

      plot_df <- left_join(
        te_df,
        merged %>% select(comparison, confidence, suggested_confidence),
        by = "comparison"
      ) %>%
        mutate(
          conf_final = ifelse(nzchar(confidence), confidence, suggested_confidence),
          conf_final = ifelse(nzchar(conf_final), conf_final, "Not set")
        ) %>%
        rowwise() %>%
        mutate(
          t1_c      = strsplit(comparison, " vs ")[[1]][1],
          t2_c      = strsplit(comparison, " vs ")[[1]][2],
          is_ref_t1 = !is.null(ref) && nzchar(ref) && (t1_c == ref),
          label     = if (is_ref_t1) t2_c else t1_c,
          TE_log    = if (is_ref_t1) -TE     else TE,
          lo_log    = if (is_ref_t1) -upper  else lower,
          hi_log    = if (is_ref_t1) -lower  else upper,
          TE_disp   = if (is_ratio) exp(TE_log) else TE_log,
          lo_disp   = if (is_ratio) exp(lo_log) else lo_log,
          hi_disp   = if (is_ratio) exp(hi_log) else hi_log
        ) %>%
        ungroup()

      # Add per-treatment N to labels
      if (!is.null(trt_n)) {
        plot_df <- plot_df %>%
          mutate(label_n = paste0(label, " (N=", trt_n[label], ")"))
      } else {
        plot_df <- plot_df %>% mutate(label_n = label)
      }

      # Sort: largest TE at top (y_pos = n).
      if (!is.null(p_scores) && length(p_scores) > 0) {
        plot_df <- plot_df %>% mutate(p_score = p_scores[label])
        plot_df <- if (small_val == "undesirable")
                     plot_df %>% arrange(coalesce(p_score, Inf))
                   else
                     plot_df %>% arrange(desc(coalesce(p_score, -Inf)))
      } else {
        plot_df <- plot_df %>% arrange(TE_log)
      }

      plot_df <- plot_df %>%
        mutate(
          y_pos      = seq_len(n()),
          label_n    = factor(label_n, levels = label_n),
          conf_final = factor(conf_final,
                              levels = c("High", "Moderate", "Low", "Very low", "Not set")),
          p_score_lbl = if (!is.null(p_scores))
                          ifelse(!is.na(p_score), sprintf("P=%.2f", p_score), "")
                        else "",
          tooltip    = paste0(
            "<b>", label, "</b>",
            if (!is.null(p_scores) && "p_score" %in% names(.)) {
              ifelse(!is.na(p_score), paste0("  (P-score=", sprintf("%.2f", p_score), ")"), "")
            } else "",
            "<br>",
            if (!is.null(sm) && nzchar(sm)) sm else "Estimate",
            ": ", sprintf("%.3f", TE_disp),
            if (!is.null(ref) && nzchar(ref)) paste0("  (vs. ", ref, ")") else "",
            "<br>95% CI: [", sprintf("%.3f", lo_disp), ", ", sprintf("%.3f", hi_disp), "]<br>",
            "CINeMA confidence: ", conf_final
          )
        )

      null_x  <- if (is_ratio) 1 else 0
      ref_lbl <- if (!is.null(ref) && nzchar(ref)) ref else "reference"
      x_label <- if (!is.null(sm) && nzchar(sm))
                   paste0(sm, "  (vs. ", ref_lbl, ")")
                 else
                   paste0("Effect size  (vs. ", ref_lbl, ")")

      p <- ggplot(plot_df,
                  aes(y = y_pos, colour = conf_final, fill = conf_final,
                      text = tooltip)) +
        geom_vline(xintercept = null_x, linetype = "dashed",
                   colour = "grey40", linewidth = 0.6) +
        geom_errorbarh(aes(xmin = lo_disp, xmax = hi_disp),
                       height = 0.35, linewidth = 0.9) +
        geom_point(aes(x = TE_disp),
                   shape = 22, size = 4.5, stroke = 0.3, colour = "grey20") +
        scale_colour_manual(values = conf_col_map, name = "CINeMA confidence",
                            aesthetics = c("colour", "fill"), drop = FALSE) +
        scale_y_continuous(breaks = plot_df$y_pos, labels = plot_df$label_n,
                           expand = expansion(add = 0.7)) +
        labs(x = x_label, y = NULL,
             title = paste0("NMA Forest Plot",
                            if (!is.null(ref) && nzchar(ref))
                              paste0(" — vs. ", ref) else "",
                            "  [sorted by P-score]")) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position    = "bottom",
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y        = element_text(size = 11),
          plot.title         = element_text(face = "bold")
        )

      if (is_ratio) p <- p + scale_x_log10()

      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.15))
    })

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
    # DOWNLOAD: Forest plot PNG
    # ------------------------------------------------------------------
    output$dl_forest <- downloadHandler(
      filename = function() {
        paste0("forest_plot_", format(Sys.Date(), "%Y%m%d"), ".png")
      },
      content = function(file) {
        cr <- tryCatch(cinema_data(), error = function(e) NULL)
        req(!is.null(cr))

        te_df  <- cr$te_df
        merged <- cinema_merged()
        net    <- cr$net
        pal    <- current_palette()
        ref    <- tryCatch(net$reference.group, error = function(e) NULL)
        sm     <- if (!is.null(nma_settings_r)) nma_settings_r()$effect_measure %||%
                    tryCatch(net$sm, error = function(e) "") else
                    tryCatch(net$sm, error = function(e) "")
        is_ratio  <- !is.null(sm) && sm %in% c("OR", "RR")
        small_val <- if (!is.null(nma_settings_r))
                       (nma_settings_r()$small_value_desirable %||% "desirable")
                     else "desirable"

        # Per-treatment total N
        trt_n <- compute_trt_n(cr$df)

        # P-scores for sorting
        p_scores_obj <- tryCatch(netrank(net, small.values = small_val),
                                 error = function(e) NULL)
        p_scores <- if (!is.null(p_scores_obj)) {
          p_scores_obj$Pscore.random %||%
            p_scores_obj$Pscore.common %||%
            p_scores_obj$random.w     %||%
            p_scores_obj$common.w
        } else NULL

        if (!is.null(ref) && nzchar(ref)) {
          te_df  <- te_df  %>% filter(grepl(ref, comparison, fixed = TRUE))
          merged <- merged %>% filter(grepl(ref, comparison, fixed = TRUE))
        }

        conf_col_map <- if ((input$palette %||% "pastel") == "classic") pal$conf else pal$conf_txt

        plot_df <- left_join(
          te_df,
          merged %>% select(comparison, confidence, suggested_confidence),
          by = "comparison"
        ) %>%
          mutate(
            conf_final = ifelse(nzchar(confidence), confidence, suggested_confidence),
            conf_final = ifelse(nzchar(conf_final), conf_final, "Not set")
          ) %>%
          rowwise() %>%
          mutate(
            t1_c      = strsplit(comparison, " vs ")[[1]][1],
            t2_c      = strsplit(comparison, " vs ")[[1]][2],
            is_ref_t1 = !is.null(ref) && nzchar(ref) && (t1_c == ref),
            label     = if (is_ref_t1) t2_c else t1_c,
            TE_log    = if (is_ref_t1) TE    else -TE,
            lo_log    = if (is_ref_t1) lower else -upper,
            hi_log    = if (is_ref_t1) upper else -lower,
            TE_disp   = if (is_ratio) exp(TE_log) else TE_log,
            lo_disp   = if (is_ratio) exp(lo_log) else lo_log,
            hi_disp   = if (is_ratio) exp(hi_log) else hi_log
          ) %>%
          ungroup()

        # Add per-treatment N to labels
        if (!is.null(trt_n)) {
          plot_df <- plot_df %>%
            mutate(label_n = paste0(label, " (N=", trt_n[label], ")"))
        } else {
          plot_df <- plot_df %>% mutate(label_n = label)
        }

        # Sort: largest TE at top (same logic as inline plot)
        if (!is.null(p_scores) && length(p_scores) > 0) {
          plot_df <- plot_df %>% mutate(p_score = p_scores[label])
          plot_df <- if (small_val == "undesirable")
                       plot_df %>% arrange(coalesce(p_score, Inf))
                     else
                       plot_df %>% arrange(desc(coalesce(p_score, -Inf)))
        } else {
          plot_df <- plot_df %>% arrange(TE_log)
        }

        plot_df <- plot_df %>%
          mutate(
            y_pos      = seq_len(n()),
            label_n    = factor(label_n, levels = label_n),
            conf_final = factor(conf_final,
                                levels = c("High", "Moderate", "Low", "Very low", "Not set")),
            ci_text    = paste0(sprintf("%.2f", TE_disp),
                                " [", sprintf("%.2f", lo_disp),
                                ", ", sprintf("%.2f", hi_disp), "]")
          )

        null_x  <- if (is_ratio) 1 else 0
        ref_lbl <- if (!is.null(ref) && nzchar(ref)) ref else "reference"
        x_label <- if (!is.null(sm) && nzchar(sm))
                     paste0(sm, "  (vs. ", ref_lbl, ")")
                   else
                     paste0("Effect size  (vs. ", ref_lbl, ")")

        x_vals <- c(plot_df$lo_disp, plot_df$hi_disp)
        x_max  <- max(x_vals[is.finite(x_vals)], null_x, na.rm = TRUE)
        x_text <- x_max + diff(range(x_vals[is.finite(x_vals)], null_x)) * 0.08

        p <- ggplot(plot_df, aes(y = y_pos, colour = conf_final, fill = conf_final)) +
          geom_vline(xintercept = null_x, linetype = "dashed",
                     colour = "grey40", linewidth = 0.6) +
          geom_errorbarh(aes(xmin = lo_disp, xmax = hi_disp),
                         height = 0.35, linewidth = 0.9) +
          geom_point(aes(x = TE_disp),
                     shape = 22, size = 5, stroke = 0.3, colour = "grey20") +
          geom_text(aes(x = x_text, label = ci_text),
                    hjust = 0, size = 3.4, colour = "grey25") +
          scale_colour_manual(values = conf_col_map, name = "CINeMA confidence",
                              aesthetics = c("colour", "fill"), drop = FALSE) +
          scale_y_continuous(breaks = plot_df$y_pos, labels = plot_df$label_n,
                             expand = expansion(add = 0.7)) +
          coord_cartesian(clip = "off") +
          labs(
            x       = x_label,
            y       = NULL,
            title   = paste0("NMA Forest Plot — vs. ", ref_lbl,
                             "  [sorted by P-score, small value = ", small_val, "]"),
            caption = paste0("Squares = NMA point estimates; bars = 95% CI;",
                             " color = CINeMA confidence level;",
                             " N = total randomized per treatment")
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position    = "bottom",
            panel.grid.minor   = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y        = element_text(size = 12),
            plot.title         = element_text(face = "bold"),
            plot.caption       = element_text(colour = "grey50", size = 10),
            plot.margin        = margin(10, 160, 10, 10)
          )

        if (is_ratio) p <- p + scale_x_log10()

        fig_height <- max(4, 1.2 + 0.5 * nrow(plot_df))
        ggsave(file, plot = p, width = 12, height = fig_height,
               dpi = 300, device = "png")
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
