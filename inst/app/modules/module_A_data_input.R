# =============================================================================
# Module A: Data Input & Preprocessing
# =============================================================================
#
# FLOW (v2 – auto-detection + column mapping):
#   1. Upload file (CSV/XLSX) or load built-in demo data
#   2. App auto-detects structural format; user confirms format, outcome type,
#      and effect measure
#   3. Column mapping — user maps data columns to required fields
#   4. ROB / indirectness value mapping — only when non-standard values found
#   [Validation banner + NMA settings + Run button appear when data is valid]
#
# SUPPORTED STRUCTURAL FORMATS:
#   long       — one row per treatment arm
#                  continuous: studlab / treat / n / mean / sd
#                  binary:     studlab / treat / n / event
#   comparison — one row per pairwise comparison
#                  generic:    studlab / t1 / t2 / y / se
#   wide       — one row per study; multiple numbered arm columns
#                  continuous: studlab / treat1 / n1 / mean1 / sd1 / treat2 / …
#                  binary:     studlab / treat1 / n1 / event1 / treat2 / …
#
# ROB / INDIRECTNESS COLUMNS (all formats):
#   Values must be "low", "some concerns", or "high" — OR mapped in Step 4.
#   If column is omitted, all rows default to "low".
#
# OUTPUT (unchanged API):
#   processed_data() → list(data, error, warning)  — pairwise-format data frame
#   nma_settings()   → list(effect_measure, ref_treatment, …)
#   run_trigger      → reactive integer (increments on each run)
# =============================================================================

library(shiny)
library(DT)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(netmeta)

ROB_LEVELS   <- c("low", "some concerns", "high")
INDIR_LEVELS <- c("low", "some concerns", "high")

# ============================================================================
# FORMAT / COLUMN DETECTION HELPERS
# ============================================================================

#' Detect structural format of an NMA data frame.
#' @param df Data frame (column names already lower-cased & trimmed).
#' @return "long" | "comparison" | "wide"
detect_data_format <- function(df) {
  cols <- tolower(trimws(names(df)))

  # Comparison format: pre-computed pairwise (t1/t2/y/se or variants)
  has_pairwise <- (all(c("y", "se") %in% cols) &&
    (all(c("t1", "t2") %in% cols) ||
     all(c("treat1", "treat2") %in% cols) ||
     all(c("treatment1", "treatment2") %in% cols))) ||
    (all(c("te", "sete") %in% cols) &&
     any(c("t1", "t2", "treat1", "treat2") %in% cols))
  if (has_pairwise) return("comparison")

  # Wide format: ≥2 numbered arm columns (treat1/treat2, n1/n2, etc.)
  n_treat_num <- sum(grepl("^(treat|treatment|arm)[0-9]+$", cols))
  n_n_num     <- sum(grepl("^n[0-9]+$", cols))
  n_out_num   <- sum(grepl("^(mean|sd|event|events)[0-9]+$", cols))
  if (n_treat_num >= 2 || (n_n_num >= 2 && n_out_num >= 2)) return("wide")

  # Default: long (arm-level)
  "long"
}

#' Auto-detect outcome type from column names.
#' @return "continuous" | "binary"
detect_outcome_type <- function(df) {
  cols <- tolower(trimws(names(df)))
  if (any(grepl("^event", cols))) "binary" else "continuous"
}

#' Find first matching column name (case-insensitive). Returns "(none)" if no match.
find_col <- function(all_cols, candidates) {
  m <- match(tolower(candidates), tolower(all_cols))
  m <- m[!is.na(m)]
  if (length(m) > 0) all_cols[m[1]] else "(none)"
}

#' Suggest column mapping for long format.
suggest_long_cols <- function(cols, outcome_type) {
  base <- list(
    studlab = find_col(cols, c("studlab", "study", "studyid", "study_id", "author", "id")),
    treat   = find_col(cols, c("treat", "treatment", "arm", "intervention", "trt")),
    n       = find_col(cols, c("n", "total", "n_total", "sample_size"))
  )
  out <- if (outcome_type == "continuous") {
    list(
      mean = find_col(cols, c("mean", "m", "mn", "mean_outcome")),
      sd   = find_col(cols, c("sd", "std", "stdev", "std_dev"))
    )
  } else {
    list(event = find_col(cols, c("event", "events", "responders", "r", "count", "n_event")))
  }
  c(base, out, list(
    rob          = find_col(cols, c("rob", "risk_of_bias", "bias", "quality", "overall")),
    indirectness = find_col(cols, c("indirectness", "indir", "applicability", "directness"))
  ))
}

#' Suggest column mapping for comparison (pairwise) format.
suggest_comparison_cols <- function(cols) {
  list(
    studlab      = find_col(cols, c("studlab", "study", "studyid", "study_id", "author", "id")),
    t1           = find_col(cols, c("t1", "treat1", "treatment1", "control", "ref")),
    t2           = find_col(cols, c("t2", "treat2", "treatment2", "active", "exp")),
    y            = find_col(cols, c("y", "te", "eff", "effect", "smd", "or", "rr", "md", "yi", "es")),
    se           = find_col(cols, c("se", "sete", "sei", "se_effect", "standard_error")),
    rob          = find_col(cols, c("rob", "risk_of_bias", "bias", "quality", "overall")),
    indirectness = find_col(cols, c("indirectness", "indir", "applicability", "directness"))
  )
}

#' Detect numbered arm columns for wide format.
#' @return list(arm_nums, arms, studlab, rob, indirectness)
detect_wide_arms <- function(df, outcome_type) {
  cols       <- names(df)
  cols_lower <- tolower(cols)

  treat_idx <- which(grepl("^(treat|treatment|arm)[0-9]+$", cols_lower))
  arm_nums  <- if (length(treat_idx) >= 2) {
    sort(unique(as.integer(str_extract(cols_lower[treat_idx], "[0-9]+$"))))
  } else {
    n_idx <- which(grepl("^n[0-9]+$", cols_lower))
    sort(unique(as.integer(str_extract(cols_lower[n_idx], "[0-9]+$"))))
  }
  if (length(arm_nums) == 0) arm_nums <- 1:2

  find_arm_col <- function(base_pat, i) {
    m <- which(grepl(paste0("^(", base_pat, ")", i, "$"), cols_lower))
    if (length(m) > 0) cols[m[1]] else "(none)"
  }
  arms <- lapply(arm_nums, function(i) {
    base <- list(treat = find_arm_col("treat|treatment|arm", i),
                 n     = find_arm_col("n", i))
    if (outcome_type == "continuous") {
      c(base, list(mean = find_arm_col("mean|m", i), sd = find_arm_col("sd|std|stdev", i)))
    } else {
      c(base, list(event = find_arm_col("event|events|r", i)))
    }
  })

  list(
    arm_nums     = arm_nums,
    arms         = arms,
    studlab      = find_col(cols, c("studlab", "study", "studyid", "study_id", "author", "id")),
    rob          = find_col(cols, c("rob", "risk_of_bias", "bias", "quality", "overall")),
    indirectness = find_col(cols, c("indirectness", "indir", "applicability", "directness"))
  )
}

#' Auto-map a single ROB/indirectness value to standard level.
auto_map_rob_value <- function(v) {
  v_clean <- tolower(trimws(as.character(v)))
  dplyr::case_when(
    v_clean %in% c("low", "l", "1", "low risk", "low risk of bias", "lo")        ~ "low",
    v_clean %in% c("some concerns", "some", "m", "moderate", "medium", "unclear",
                   "2", "moderate risk", "mod", "partial")                         ~ "some concerns",
    v_clean %in% c("high", "h", "3", "high risk", "serious", "critical", "hi")   ~ "high",
    TRUE                                                                            ~ "some concerns"
  )
}

#' Apply a value map to a character vector.
#' @param x Character vector of raw values.
#' @param value_map Named vector: names = raw values (lower-trimmed), values = standard levels.
#' @return Remapped character vector.
remap_values <- function(x, value_map) {
  x_lower <- tolower(trimws(as.character(x)))
  map_keys_lower <- tolower(trimws(names(value_map)))
  result <- x_lower
  for (i in seq_along(map_keys_lower)) {
    result[x_lower == map_keys_lower[i]] <- value_map[[i]]
  }
  result
}

# ============================================================================
# UI FUNCTION
# ============================================================================
moduleA_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Step 1: Upload NMA Data"),

    # --- Upload section (always visible) ---
    wellPanel(
      fileInput(
        ns("file_upload"), "Choose file (CSV or XLSX)",
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Browse...", placeholder = "No file selected"
      ),
      hr(style = "margin: 10px 0;"),
      div(
        h5(style = "margin-bottom: 6px;", icon("database"), " Built-in demo data"),
        actionButton(
          ns("load_demo"),
          label = tagList(icon("play-circle"), " Load SLEEPI demo data"),
          class = "btn btn-outline-info btn-sm"
        ),
        tags$p(
          style = "color:#555; font-size:0.85em; margin-top:6px; margin-bottom:0;",
          "9 studies \u00b7 3 treatments (CBT-I, Combination, Pharmacotherapy)",
          " \u00b7 binary outcome \u00b7 OR scale.", tags$br(),
          tags$em("Source: Furukawa Y et al. Psychiatry Clin Neurosci. 2024;78(11):646\u2013653.")
        )
      )
    ),

    # --- Step 2: Format & Outcome Type (shown after raw data loaded) ---
    uiOutput(ns("step2_ui")),

    # --- Step 3: Column Mapping (file uploads only) ---
    uiOutput(ns("step3_col_map_ui")),

    # --- Step 4: ROB / Indirectness Value Mapping (file uploads only) ---
    uiOutput(ns("step4_rob_map_ui")),

    # --- Validation banner ---
    uiOutput(ns("validation_msg")),

    # --- NMA Settings (shown after data is valid) ---
    uiOutput(ns("nma_settings_ui")),

    # --- Run Analysis button ---
    uiOutput(ns("run_analysis_ui")),

    hr(),
    h4("Pairwise Data Preview"),
    p(
      "The table below shows data after conversion to pairwise format.",
      "This is what Module B (CINeMA) and Module C (ROB-MEN) receive."
    ),
    DTOutput(ns("data_preview")),
    hr(),
    h4("Summary"),
    verbatimTextOutput(ns("data_summary"))
  )
}

# ============================================================================
# SERVER FUNCTION
# ============================================================================
moduleA_server <- function(id, go_to_cinema = NULL, initial_data = NULL) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # Effect measure (reactiveVal so it persists across re-renders)
    # ------------------------------------------------------------------
    init_em     <- if (!is.null(initial_data)) initial_data$effect_measure else "SMD"
    em_selected <- reactiveVal(init_em)

    observeEvent(input$effect_measure, {
      em_selected(input$effect_measure)
    }, ignoreNULL = TRUE)

    # ------------------------------------------------------------------
    # Data source tracking: "injected" | "demo" | "file" | NULL
    # ------------------------------------------------------------------
    data_source <- reactiveVal(if (!is.null(initial_data)) "injected" else NULL)

    observe({
      if (!is.null(initial_data)) {
        updateRadioButtons(session, "data_format", selected = initial_data$format)
      }
    }) |> bindEvent(TRUE, once = TRUE)

    # ------------------------------------------------------------------
    # Raw data (file upload)
    # ------------------------------------------------------------------
    raw_data <- reactive({
      req(input$file_upload)
      path <- input$file_upload$datapath
      ext  <- tools::file_ext(input$file_upload$name)
      tryCatch({
        df <- if (tolower(ext) == "csv") {
          read_csv(path, col_types = cols(), show_col_types = FALSE,
                   locale = locale(decimal_mark = "."))
        } else if (tolower(ext) %in% c("xlsx", "xls")) {
          read_excel(path)
        } else {
          stop("Unsupported file type: ", ext)
        }
        names(df) <- str_to_lower(str_trim(names(df)))
        df
      }, error = function(e) list(error = conditionMessage(e)))
    })

    observeEvent(input$file_upload, {
      if (!is.null(input$file_upload)) data_source("file")
    })

    # ------------------------------------------------------------------
    # Demo data
    # ------------------------------------------------------------------
    demo_result <- eventReactive(input$load_demo, { load_demo_cinema_data() })

    observeEvent(input$load_demo, {
      data_source("demo")
      em_selected("OR")
    })

    # Raw demo df (for column scanning in step3/step4 if needed)
    demo_raw_df <- reactive({
      tryCatch({
        df <- readr::read_csv("df_cinema.csv", col_types = readr::cols(),
                              show_col_types = FALSE)
        names(df) <- str_to_lower(str_trim(names(df)))
        if ("treatment" %in% names(df)) df <- rename(df, treat = treatment)
        if ("overall"   %in% names(df)) df <- rename(df, rob   = overall)
        df
      }, error = function(e) NULL)
    })

    # ------------------------------------------------------------------
    # current_df: raw data frame for scanning (demo or file)
    # ------------------------------------------------------------------
    current_df <- reactive({
      src <- data_source()
      if (identical(src, "demo")) return(demo_raw_df())
      if (identical(src, "file")) {
        raw <- raw_data()
        if (is.list(raw) && !is.null(raw$error)) return(NULL)
        return(raw)
      }
      NULL
    })

    avail_cols <- reactive({
      df <- current_df()
      if (is.null(df)) return(c("(none)"))
      c("(none)", names(df))
    })

    # ------------------------------------------------------------------
    # Auto-detected format
    # ------------------------------------------------------------------
    auto_fmt <- reactive({
      src <- data_source()
      if (identical(src, "demo"))     return("long")
      if (identical(src, "injected")) return(initial_data$format %||% "long")
      df <- current_df()
      if (is.null(df)) return("long")
      detect_data_format(df)
    })

    auto_outcome <- reactive({
      src <- data_source()
      if (identical(src, "demo"))     return("binary")
      if (identical(src, "injected")) return("continuous")
      df <- current_df()
      if (is.null(df)) return("continuous")
      detect_outcome_type(df)
    })

    # ====================================================================
    # Step 2: Format & Outcome Type
    # ====================================================================
    output$step2_ui <- renderUI({
      src <- data_source()
      if (is.null(src) || identical(src, "injected")) return(NULL)

      fmt    <- auto_fmt()
      ot     <- auto_outcome()
      is_cmp <- identical(fmt, "comparison")

      fmt_label <- switch(fmt,
        long       = "Long format (arm-level, one row per arm)",
        comparison = "Comparison format (pairwise, one row per comparison)",
        wide       = "Wide format (one row per study with multiple arm columns)"
      )

      ot_choices <- if (is_cmp) {
        c("Generic / pre-computed (y, SE)" = "generic")
      } else {
        c("Continuous (mean, SD)" = "continuous", "Binary (events, N)" = "binary")
      }
      ot_sel <- if (is_cmp) "generic" else ot

      em_choices <- if (ot_sel == "continuous") {
        c("Standardised mean difference (SMD)" = "SMD", "Mean difference (MD)" = "MD")
      } else if (ot_sel %in% c("binary", "generic")) {
        c("Odds ratio (OR)" = "OR", "Risk ratio (RR)" = "RR")
      } else {
        c("Standardised mean difference (SMD)" = "SMD", "Mean difference (MD)" = "MD",
          "Odds ratio (OR)" = "OR", "Risk ratio (RR)" = "RR")
      }
      em_sel <- isolate({
        cur <- em_selected()
        if (!is.null(cur) && cur %in% em_choices) cur else em_choices[1]
      })

      wellPanel(
        h4("Step 2: Data Format & Outcome Type"),
        div(
          class = "alert alert-info", style = "margin-bottom:10px;",
          icon("search"), " Auto-detected: ", strong(fmt_label), tags$br(),
          tags$small("Correct if needed using the options below.")
        ),
        fluidRow(
          column(4,
            radioButtons(
              ns("data_format"), "Structural format",
              choices = c(
                "Long format (arm-level)"       = "long",
                "Comparison format (pairwise)"  = "comparison",
                "Wide format (one row/study)"   = "wide"
              ),
              selected = fmt
            )
          ),
          column(4,
            radioButtons(ns("outcome_type"), "Outcome type",
                         choices = ot_choices, selected = ot_sel)
          ),
          column(4,
            selectInput(ns("effect_measure"), "Effect measure",
                        choices = em_choices, selected = em_sel)
          )
        )
      )
    })

    # Update outcome_type choices when data_format changes
    observeEvent(input$data_format, {
      if (identical(input$data_format, "comparison")) {
        updateRadioButtons(session, "outcome_type",
          choices  = c("Generic / pre-computed (y, SE)" = "generic"),
          selected = "generic")
      } else {
        cur_ot <- input$outcome_type
        if (is.null(cur_ot) || cur_ot == "generic") cur_ot <- "continuous"
        updateRadioButtons(session, "outcome_type",
          choices  = c("Continuous (mean, SD)" = "continuous",
                       "Binary (events, N)"    = "binary"),
          selected = cur_ot)
      }
    }, ignoreNULL = TRUE)

    # Update effect_measure choices when outcome_type changes
    observeEvent(input$outcome_type, {
      ot <- input$outcome_type
      em_choices <- if (ot == "continuous") {
        c("Standardised mean difference (SMD)" = "SMD", "Mean difference (MD)" = "MD")
      } else if (ot %in% c("binary", "generic")) {
        c("Odds ratio (OR)" = "OR", "Risk ratio (RR)" = "RR")
      } else {
        c("Standardised mean difference (SMD)" = "SMD", "Mean difference (MD)" = "MD",
          "Odds ratio (OR)" = "OR", "Risk ratio (RR)" = "RR")
      }
      cur_em  <- em_selected()
      new_sel <- if (cur_em %in% em_choices) cur_em else em_choices[1]
      updateSelectInput(session, "effect_measure", choices = em_choices, selected = new_sel)
      em_selected(new_sel)
    }, ignoreNULL = TRUE)

    # ====================================================================
    # Step 3: Column Mapping (file uploads only)
    # ====================================================================
    output$step3_col_map_ui <- renderUI({
      src <- data_source()
      if (is.null(src) || identical(src, "injected") || identical(src, "demo")) return(NULL)

      df   <- current_df()
      if (is.null(df)) return(NULL)

      cols     <- avail_cols()    # includes "(none)" at position 1
      raw_cols <- cols[-1]

      fmt <- input$data_format %||% auto_fmt()
      ot  <- input$outcome_type %||% auto_outcome()
      if (identical(fmt, "comparison")) ot <- "generic"

      mk_sel <- function(id, label, default_col) {
        sel <- if (!is.null(default_col) && default_col %in% cols) default_col else "(none)"
        selectInput(ns(id), label, choices = cols, selected = sel)
      }

      wellPanel(
        h4("Step 3: Column Mapping"),
        p(style = "color:#555; font-size:0.9em; margin-bottom:10px;",
          "Map your file's column names to the required fields.",
          " Auto-matched where possible — correct if needed.",
          " Columns marked ", strong("*"), " are required."),

        if (identical(fmt, "long")) {
          sg <- suggest_long_cols(raw_cols, ot)
          tagList(
            fluidRow(
              column(4, mk_sel("col_studlab", "Study ID *",   sg$studlab)),
              column(4, mk_sel("col_treat",   "Treatment *",  sg$treat)),
              column(4, mk_sel("col_n",       "Sample size (N) *", sg$n))
            ),
            if (ot == "continuous") {
              fluidRow(
                column(4, mk_sel("col_mean", "Mean *", sg$mean)),
                column(4, mk_sel("col_sd",   "SD *",   sg$sd))
              )
            } else {
              fluidRow(
                column(4, mk_sel("col_event", "Events *", sg$event))
              )
            },
            fluidRow(
              column(4, mk_sel("col_rob",          "Risk of Bias",  sg$rob)),
              column(4, mk_sel("col_indirectness", "Indirectness",  sg$indirectness))
            )
          )

        } else if (identical(fmt, "comparison")) {
          sg <- suggest_comparison_cols(raw_cols)
          tagList(
            fluidRow(
              column(3, mk_sel("col_studlab", "Study ID *",          sg$studlab)),
              column(3, mk_sel("col_t1",      "Treatment 1 (t1) *",  sg$t1)),
              column(3, mk_sel("col_t2",      "Treatment 2 (t2) *",  sg$t2))
            ),
            fluidRow(
              column(3, mk_sel("col_y",            "Effect size (y) *",      sg$y)),
              column(3, mk_sel("col_se",           "Standard error (SE) *",  sg$se)),
              column(3, mk_sel("col_rob",          "Risk of Bias",           sg$rob)),
              column(3, mk_sel("col_indirectness", "Indirectness",           sg$indirectness))
            )
          )

        } else {  # wide
          da     <- detect_wide_arms(df, ot)
          n_arms <- length(da$arm_nums)
          tagList(
            fluidRow(
              column(4, mk_sel("col_studlab",      "Study ID *",   da$studlab)),
              column(4, mk_sel("col_rob",          "Risk of Bias", da$rob)),
              column(4, mk_sel("col_indirectness", "Indirectness", da$indirectness))
            ),
            hr(style = "margin:10px 0;"),
            h5("Arm Columns"),
            p(style = "color:#555; font-size:0.85em;",
              sprintf(
                "Detected %d arm slot(s) from numbered column patterns.",
                n_arms)),
            lapply(seq_len(n_arms), function(i) {
              arm <- da$arms[[i]]
              div(
                style = "border-left:3px solid #dee2e6; padding-left:10px; margin-bottom:10px;",
                h6(paste("Arm", i)),
                fluidRow(
                  column(3, mk_sel(paste0("col_wide_treat_", i),
                                   paste("Treatment", i, "*"), arm$treat)),
                  column(3, mk_sel(paste0("col_wide_n_", i),
                                   paste("N", i, "*"), arm$n)),
                  if (ot == "continuous") tagList(
                    column(3, mk_sel(paste0("col_wide_mean_", i),
                                     paste("Mean", i, "*"), arm$mean)),
                    column(3, mk_sel(paste0("col_wide_sd_", i),
                                     paste("SD", i, "*"), arm$sd))
                  ) else {
                    column(3, mk_sel(paste0("col_wide_event_", i),
                                     paste("Events", i, "*"), arm$event))
                  }
                )
              )
            })
          )
        }
      )
    })

    # ====================================================================
    # Step 4: ROB / Indirectness Value Mapping (file uploads only)
    # ====================================================================
    output$step4_rob_map_ui <- renderUI({
      src <- data_source()
      if (is.null(src) || identical(src, "injected") || identical(src, "demo")) return(NULL)

      df <- current_df()
      if (is.null(df)) return(NULL)

      rob_col   <- input$col_rob
      indir_col <- input$col_indirectness

      rob_vals <- if (!is.null(rob_col) && rob_col != "(none)" && rob_col %in% names(df)) {
        sort(unique(as.character(df[[rob_col]])))
      } else NULL

      indir_vals <- if (!is.null(indir_col) && indir_col != "(none)" && indir_col %in% names(df)) {
        sort(unique(as.character(df[[indir_col]])))
      } else NULL

      std_vals        <- ROB_LEVELS
      needs_rob_map   <- !is.null(rob_vals)   && !all(tolower(trimws(rob_vals))   %in% std_vals)
      needs_indir_map <- !is.null(indir_vals) && !all(tolower(trimws(indir_vals)) %in% std_vals)

      if (!needs_rob_map && !needs_indir_map) return(NULL)

      map_choices <- c(
        "Low risk"      = "low",
        "Some concerns" = "some concerns",
        "High risk"     = "high",
        "(Exclude row)" = "__EXCLUDE__"
      )

      make_map_rows <- function(vals, prefix) {
        lapply(vals, function(v) {
          fluidRow(
            column(3, tags$p(style = "margin-top:7px; font-weight:bold;", v)),
            column(5,
              selectInput(ns(paste0(prefix, make.names(v))),
                          label = NULL,
                          choices  = map_choices,
                          selected = auto_map_rob_value(v))
            )
          )
        })
      }

      wellPanel(
        h4("Step 4: ROB / Indirectness Value Mapping"),
        p(style = "color:#555; font-size:0.9em;",
          "Non-standard values detected in your data.",
          " Assign each to the corresponding standard risk level,",
          " or choose '(Exclude row)' to drop those rows."),
        if (needs_rob_map) tagList(
          h5("Risk of Bias"),
          make_map_rows(rob_vals, "rob_map_val_")
        ),
        if (needs_indir_map) tagList(
          if (needs_rob_map) hr(),
          h5("Indirectness"),
          make_map_rows(indir_vals, "indir_map_val_")
        )
      )
    })

    # ====================================================================
    # VALUE-MAP HELPERS
    # ====================================================================

    # Build a named vector (raw → standard) from dynamic input$rob_map_val_* inputs.
    get_rob_value_map <- function(vals) {
      if (is.null(vals)) return(NULL)
      setNames(
        vapply(vals, function(v) {
          id  <- paste0("rob_map_val_", make.names(v))
          val <- input[[id]]
          if (is.null(val)) auto_map_rob_value(v) else val
        }, character(1)),
        vals
      )
    }

    get_indir_value_map <- function(vals) {
      if (is.null(vals)) return(NULL)
      setNames(
        vapply(vals, function(v) {
          id  <- paste0("indir_map_val_", make.names(v))
          val <- input[[id]]
          if (is.null(val)) auto_map_rob_value(v) else val
        }, character(1)),
        vals
      )
    }

    # Apply column rename + ROB/indirectness value remapping to a data frame.
    # col_map: named list (new_name = old_col).
    apply_col_mapping <- function(df, col_map) {
      ren <- unlist(col_map[!sapply(col_map, function(x) is.null(x) || x == "(none)")])
      ren <- ren[ren %in% names(df)]       # keep only existing columns
      ren <- ren[names(ren) != unname(ren)] # drop identity renames
      if (length(ren) > 0) df <- df %>% rename(!!!ren)
      df
    }

    apply_rob_indir_map <- function(df, rob_vals, indir_vals) {
      rob_map   <- get_rob_value_map(rob_vals)
      indir_map <- get_indir_value_map(indir_vals)

      if (!is.null(rob_map) && "rob" %in% names(df)) {
        df <- df %>%
          mutate(rob = remap_values(rob, rob_map)) %>%
          filter(rob != "__EXCLUDE__")
      } else if ("rob" %in% names(df)) {
        df <- df %>% mutate(rob = tolower(trimws(as.character(rob))))
      }

      if (!is.null(indir_map) && "indirectness" %in% names(df)) {
        df <- df %>%
          mutate(indirectness = remap_values(indirectness, indir_map)) %>%
          filter(indirectness != "__EXCLUDE__")
      } else if ("indirectness" %in% names(df)) {
        df <- df %>%
          mutate(indirectness = tolower(trimws(as.character(indirectness))))
      }

      if (!"rob"          %in% names(df)) df$rob          <- "low"
      if (!"indirectness" %in% names(df)) df$indirectness <- "low"

      df
    }

    # ====================================================================
    # PROCESSED DATA REACTIVE
    # ====================================================================
    processed_data <- reactive({
      src <- data_source()

      if (identical(src, "injected")) return(initial_data$result)
      if (identical(src, "demo"))     return(demo_result())

      df <- current_df()
      req(!is.null(df))

      fmt <- input$data_format %||% auto_fmt()
      ot  <- input$outcome_type %||% auto_outcome()
      em  <- em_selected() %||% "SMD"
      if (identical(fmt, "comparison")) ot <- "generic"

      # Helper: get ROB/indirectness raw values for mapping
      rob_col   <- input$col_rob
      indir_col <- input$col_indirectness

      rob_vals <- if (!is.null(rob_col) && rob_col != "(none)" && rob_col %in% names(df)) {
        unique(as.character(df[[rob_col]]))
      } else NULL

      indir_vals <- if (!is.null(indir_col) && indir_col != "(none)" && indir_col %in% names(df)) {
        unique(as.character(df[[indir_col]]))
      } else NULL

      if (identical(fmt, "long")) {
        studlab_col <- input$col_studlab %||% "(none)"
        treat_col   <- input$col_treat   %||% "(none)"
        n_col       <- input$col_n       %||% "(none)"

        req_cols <- if (ot == "continuous") {
          c(studlab_col, treat_col, n_col,
            input$col_mean %||% "(none)", input$col_sd %||% "(none)")
        } else {
          c(studlab_col, treat_col, n_col, input$col_event %||% "(none)")
        }
        if (any(req_cols == "(none)")) {
          return(list(data = NULL,
                      error = "Please map all required columns (*) in Step 3.",
                      warning = NULL))
        }

        col_map <- list(studlab = studlab_col, treat = treat_col, n = n_col)
        if (ot == "continuous") {
          col_map$mean <- input$col_mean
          col_map$sd   <- input$col_sd
        } else {
          col_map$event <- input$col_event
        }
        if (!is.null(rob_col)   && rob_col   != "(none)") col_map$rob          <- rob_col
        if (!is.null(indir_col) && indir_col != "(none)") col_map$indirectness <- indir_col

        df2 <- apply_col_mapping(df, col_map)
        df2 <- apply_rob_indir_map(df2, rob_vals, indir_vals)

        if (ot == "continuous") convert_continuous(df2, em) else convert_binary(df2, em)

      } else if (identical(fmt, "comparison")) {
        studlab_col <- input$col_studlab %||% "(none)"
        t1_col      <- input$col_t1      %||% "(none)"
        t2_col      <- input$col_t2      %||% "(none)"
        y_col       <- input$col_y       %||% "(none)"
        se_col      <- input$col_se      %||% "(none)"

        if (any(c(studlab_col, t1_col, t2_col, y_col, se_col) == "(none)")) {
          return(list(data = NULL,
                      error = "Please map all required columns (*) in Step 3.",
                      warning = NULL))
        }

        col_map <- list(studlab = studlab_col, t1 = t1_col, t2 = t2_col,
                        y = y_col, se = se_col)
        if (!is.null(rob_col)   && rob_col   != "(none)") col_map$rob          <- rob_col
        if (!is.null(indir_col) && indir_col != "(none)") col_map$indirectness <- indir_col

        df2 <- apply_col_mapping(df, col_map)
        df2 <- apply_rob_indir_map(df2, rob_vals, indir_vals)
        convert_pairwise(df2)

      } else {  # wide
        studlab_col <- input$col_studlab %||% "(none)"
        if (studlab_col == "(none)") {
          return(list(data = NULL,
                      error = "Please select the Study ID column in Step 3.",
                      warning = NULL))
        }

        da     <- detect_wide_arms(df, ot)
        n_arms <- length(da$arm_nums)

        arm_maps <- lapply(seq_len(n_arms), function(i) {
          base <- list(
            treat = input[[paste0("col_wide_treat_", i)]] %||% "(none)",
            n     = input[[paste0("col_wide_n_", i)]]     %||% "(none)"
          )
          if (ot == "continuous") {
            c(base, list(
              mean = input[[paste0("col_wide_mean_", i)]] %||% "(none)",
              sd   = input[[paste0("col_wide_sd_", i)]]   %||% "(none)"
            ))
          } else {
            c(base, list(event = input[[paste0("col_wide_event_", i)]] %||% "(none)"))
          }
        })

        convert_wide(df, ot, em, studlab_col, arm_maps,
                     rob_col, indir_col, rob_vals, indir_vals,
                     get_rob_value_map(rob_vals), get_indir_value_map(indir_vals))
      }
    })

    # ====================================================================
    # OUTPUT: Validation banner
    # ====================================================================
    output$validation_msg <- renderUI({
      src <- data_source()

      if (identical(src, "injected")) {
        res <- initial_data$result
        if (!is.null(res$error)) {
          return(div(class = "alert alert-danger",
                     icon("exclamation-triangle"), strong(" Error: "), res$error))
        }
        df <- res$data
        return(div(class = "alert alert-success",
                   icon("code"), strong(" Data injected from R session. "),
                   paste(nrow(df), "pairwise rows |",
                         length(unique(c(df$t1, df$t2))), "treatments |",
                         length(unique(df$studlab)), "studies"),
                   tags$br(),
                   tags$small(style = "color:#555;",
                     "Upload a file or click 'Load SLEEPI demo data' to replace.")))
      }

      if (identical(src, "demo")) {
        res <- demo_result()
        if (!is.null(res$error)) {
          return(div(class = "alert alert-danger",
                     icon("exclamation-triangle"), strong(" Error: "), res$error))
        }
        df <- res$data
        return(div(class = "alert alert-success",
                   icon("database"), strong(" W2I demo data loaded. "),
                   paste(nrow(df), "pairwise rows |",
                         length(unique(c(df$t1, df$t2))), "treatments |",
                         length(unique(df$studlab)), "studies"),
                   " \u00b7 Binary outcome \u00b7 OR scale."))
      }

      if (is.null(src)) {
        return(div(class = "alert alert-info",
                   icon("info-circle"),
                   " Upload a CSV/Excel file above, or click",
                   strong(" 'Load demo data'"), " to try the app."))
      }

      # File upload: check if step2 inputs are available
      if (is.null(input$data_format) || is.null(input$outcome_type)) {
        return(div(class = "alert alert-info",
                   icon("info-circle"),
                   " Confirm the format and outcome type in Step 2 above."))
      }
      if (is.null(input$col_studlab)) {
        return(div(class = "alert alert-info",
                   icon("info-circle"),
                   " Map your columns in Step 3 above."))
      }

      res <- tryCatch(processed_data(), error = function(e) {
        list(data = NULL, error = conditionMessage(e), warning = NULL)
      })

      if (!is.null(res$error)) {
        return(div(class = "alert alert-danger",
                   icon("exclamation-triangle"), strong(" Error: "), res$error))
      }
      if (!is.null(res$warning)) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-circle"), strong(" Warning: "), res$warning))
      }

      df <- res$data
      div(class = "alert alert-success",
          icon("check-circle"), strong(" Data loaded successfully. "),
          paste(nrow(df), "pairwise rows |",
                length(unique(c(df$t1, df$t2))), "treatments |",
                length(unique(df$studlab)), "studies"))
    })

    # ====================================================================
    # OUTPUT: Data preview
    # ====================================================================
    output$data_preview <- renderDT({
      res <- processed_data()
      req(!is.null(res$data))
      datatable(res$data,
                options  = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE) %>%
        formatStyle("rob",
          backgroundColor = styleEqual(
            c("low", "some concerns", "high"),
            c("#d4edda", "#fff3cd", "#f8d7da"))) %>%
        formatStyle("indirectness",
          backgroundColor = styleEqual(
            c("low", "some concerns", "high"),
            c("#d4edda", "#fff3cd", "#f8d7da")))
    })

    # ====================================================================
    # OUTPUT: Summary
    # ====================================================================
    output$data_summary <- renderPrint({
      res <- processed_data()
      req(!is.null(res$data))
      df <- res$data
      cat("=== NMA Data Summary (pairwise) ===\n")
      cat("Rows (comparisons)  :", nrow(df), "\n")
      cat("Studies (studlab)   :", length(unique(df$studlab)), "\n")
      cat("Treatments          :", length(unique(c(df$t1, df$t2))), "\n")
      cat("  -", paste(sort(unique(c(df$t1, df$t2))), collapse = ", "), "\n")
      cat("\nRisk of bias:\n")
      print(table(df$rob, useNA = "ifany"))
      cat("\nIndirectness:\n")
      print(table(df$indirectness, useNA = "ifany"))
      cat("\nEffect size (y): [",
          round(min(df$y, na.rm = TRUE), 3), ",",
          round(max(df$y, na.rm = TRUE), 3), "]\n")
    })

    # ====================================================================
    # OUTPUT: NMA Settings (shown once data is valid)
    # ====================================================================
    output$nma_settings_ui <- renderUI({
      res <- tryCatch(processed_data(), error = function(e) NULL)
      if (is.null(res) || !is.null(res$error) || is.null(res$data)) return(NULL)

      df       <- res$data
      all_trts <- sort(unique(c(df$t1, df$t2)))
      ctrl_candidates <- c("WL", "PLB", "placebo", "Placebo", "control", "Control")
      default_match   <- intersect(ctrl_candidates, all_trts)
      default_ref     <- if (length(default_match) > 0) default_match[1] else all_trts[1]

      cur_em    <- em_selected() %||% "SMD"
      delta_def <- if (cur_em %in% c("OR", "RR")) 1.2 else 0.2

      wellPanel(
        h4("NMA Settings"),
        p(style = "color:#555; font-size:0.9em; margin-bottom:10px;",
          "Configure analysis settings. These are applied in Step 2 (CINeMA)",
          " and Step 3 (ROB-MEN)."),
        fluidRow(
          column(4,
            selectInput(ns("ref_treatment"), "Reference treatment",
                        choices = all_trts, selected = default_ref)
          ),
          column(4,
            selectInput(ns("model_type"), "Effects model",
                        choices = c("Random effects" = "random",
                                    "Common effects"  = "common"),
                        selected = "random")
          ),
          column(4,
            numericInput(ns("delta"),
                         "Clinical threshold \u03b4 (effect-size scale)",
                         value = delta_def, step = 0.05, min = 0.01)
          )
        ),
        fluidRow(
          column(5,
            radioButtons(
              ns("small_value_desirable"),
              tagList("Small outcome value is", tags$span(
                style = "font-weight:normal; font-size:0.85em; color:#666;",
                " — used for P-score / rankogram"
              )),
              choices  = c(
                "Desirable  (lower = better, e.g. symptom severity, mortality)" = "desirable",
                "Undesirable  (lower = worse, e.g. remission rate, QoL score)"  = "undesirable"
              ),
              selected = "desirable"
            )
          ),
          column(7,
            div(
              class = "alert alert-light",
              style = "font-size:0.85em; margin-top:28px; padding:8px 12px;",
              icon("info-circle"),
              " This controls P-score direction and the forest plot reference arrow.",
              tags$br(),
              strong("Desirable:"),
              " lower values are better (e.g. insomnia severity, relapse rate, side-effect score).",
              tags$br(),
              strong("Undesirable:"),
              " lower values are worse (e.g. sleep quality score, remission rate, QoL)."
            )
          )
        ),
        hr(),
        h4("ROB-MEN Bayesian Settings"),
        p(style = "color:#555; font-size:0.9em; margin-bottom:6px;",
          "MCMC settings for the Bayesian Egger test (Chiocchia et al. 2021).",
          " Regression slope priors: mean = 0, SD = 100 (precision = 0.0001;",
          " vague/flat prior, fixed to match the ROB-MEN JAGS model files)."),
        fluidRow(
          column(3,
            numericInput(ns("mcmc_burn_in"), "Burn-in",
                         value = 1000, min = 100, step = 100)
          ),
          column(3,
            numericInput(ns("mcmc_iter"), "Iterations",
                         value = 10000, min = 1000, step = 1000)
          ),
          column(3,
            numericInput(ns("mcmc_thin"), "Thinning factor",
                         value = 1, min = 1, step = 1)
          ),
          column(3,
            selectInput(ns("mcmc_interaction"),
                        "Treatment-specific interactions",
                        choices = c(
                          "Unrelated treatment-specific interactions" = "unrelated",
                          "Exchangeable interactions"                 = "exchangeable",
                          "Common interaction"                        = "common"
                        ),
                        selected = "unrelated")
          )
        )
      )
    })

    # Update delta default when effect measure changes
    observeEvent(em_selected(), {
      cur_em    <- em_selected()
      delta_def <- if (cur_em %in% c("OR", "RR")) 1.2 else 0.2
      updateNumericInput(session, "delta", value = delta_def)
    }, ignoreInit = TRUE)

    # NMA settings as a reactive list (consumed by Modules B and C)
    nma_settings <- reactive({
      list(
        effect_measure        = em_selected() %||% "SMD",
        ref_treatment         = input$ref_treatment,
        model_type            = input$model_type,
        delta                 = input$delta %||% 0.2,
        prior_mean            = 0,
        prior_sd              = 100,
        mcmc_burn_in          = input$mcmc_burn_in  %||% 1000L,
        mcmc_iter             = input$mcmc_iter     %||% 10000L,
        mcmc_thin             = input$mcmc_thin     %||% 1L,
        mcmc_interaction      = input$mcmc_interaction %||% "unrelated",
        small_value_desirable = input$small_value_desirable %||% "desirable"
      )
    })

    # ====================================================================
    # OUTPUT: Run Analysis button
    # ====================================================================
    output$run_analysis_ui <- renderUI({
      res <- tryCatch(processed_data(), error = function(e) NULL)
      if (is.null(res) || !is.null(res$error) || is.null(res$data)) return(NULL)
      div(style = "margin: 16px 0 8px;",
        actionButton(ns("run_analysis"),
                     "\u25b6  Run CINeMA + ROB-MEN Analysis",
                     class = "btn btn-primary btn-lg",
                     icon  = icon("play-circle")),
        span(style = "margin-left:12px; color:#555; font-size:0.9em;",
             "Navigates to CINeMA tab. Module B runs NMA + CINeMA; Module C runs ROB-MEN.")
      )
    })

    run_trigger <- reactiveVal(0)
    observeEvent(input$run_analysis, {
      run_trigger(run_trigger() + 1)
      showNotification(
        tagList(
          strong("Analysis started."), br(),
          "\u2713 Step 1: Data validated", br(),
          "\u2192 Step 2: NMA + CINeMA (Module B)", br(),
          "\u2192 Step 3: ROB-MEN (Module C)"
        ),
        type     = "message",
        duration = 8
      )
      if (!is.null(go_to_cinema)) go_to_cinema()
    })

    return(list(
      processed_data = processed_data,
      run_trigger    = run_trigger,
      nma_settings   = nma_settings
    ))
  })
}

# =============================================================================
# CONVERSION HELPERS
# Returns: list(data = <pairwise df>, error = <str|NULL>, warning = <str|NULL>)
# =============================================================================

# ----------------------------------------------------------------------------
# make_ordered_factors: coerce rob and indirectness to ordered factors
# ----------------------------------------------------------------------------
make_ordered_factors <- function(df) {
  df %>%
    mutate(
      rob = factor(str_to_lower(str_trim(as.character(rob))),
                   levels = ROB_LEVELS, ordered = TRUE),
      indirectness = factor(str_to_lower(str_trim(as.character(indirectness))),
                            levels = INDIR_LEVELS, ordered = TRUE)
    )
}

# ----------------------------------------------------------------------------
# convert_continuous: arm-level continuous -> pairwise via pairwise()
# ----------------------------------------------------------------------------
convert_continuous <- function(df, sm) {
  required <- c("studlab", "treat", "n", "mean", "sd", "rob", "indirectness")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(list(data = NULL,
                error = paste("Missing columns:", paste(missing, collapse = ", ")),
                warning = NULL))
  }

  df <- df %>%
    mutate(n = as.integer(n), mean = as.numeric(mean), sd = as.numeric(sd))

  pw <- tryCatch(
    pairwise(treat   = treat,
             n       = n,
             mean    = mean,
             sd      = sd,
             studlab = studlab,
             data    = df,
             sm      = sm),
    error = function(e) list(error = conditionMessage(e))
  )

  if (!is.data.frame(pw)) {
    return(list(data = NULL, error = pw$error, warning = NULL))
  }

  result <- pw %>%
    rename(t1 = treat1, t2 = treat2, y = TE, se = seTE) %>%
    mutate(n = n1 + n2) %>%
    select(studlab, t1, t2, y, se, n)

  rob_map <- df %>%
    group_by(studlab) %>% slice(1) %>% ungroup() %>%
    select(studlab, rob, indirectness)

  result <- left_join(result, rob_map, by = "studlab") %>%
    make_ordered_factors()

  list(data = result, error = NULL, warning = NULL)
}

# ----------------------------------------------------------------------------
# convert_binary: arm-level binary -> pairwise via pairwise()
# ----------------------------------------------------------------------------
convert_binary <- function(df, sm) {
  required <- c("studlab", "treat", "n", "event", "rob", "indirectness")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(list(data = NULL,
                error = paste("Missing columns:", paste(missing, collapse = ", ")),
                warning = NULL))
  }

  df <- df %>%
    mutate(n = as.integer(n), event = as.integer(event))

  pw <- tryCatch(
    pairwise(treat   = treat,
             event   = event,
             n       = n,
             studlab = studlab,
             data    = df,
             sm      = sm),
    error = function(e) list(error = conditionMessage(e))
  )

  if (!is.data.frame(pw)) {
    return(list(data = NULL, error = pw$error, warning = NULL))
  }

  result <- pw %>%
    rename(t1 = treat1, t2 = treat2, y = TE, se = seTE) %>%
    mutate(n = n1 + n2) %>%
    select(studlab, t1, t2, y, se, n)

  rob_map <- df %>%
    group_by(studlab) %>% slice(1) %>% ungroup() %>%
    select(studlab, rob, indirectness)

  result <- left_join(result, rob_map, by = "studlab") %>%
    make_ordered_factors()

  list(data = result, error = NULL, warning = NULL)
}

# ----------------------------------------------------------------------------
# convert_pairwise: pre-computed pairwise (generic format)
# ----------------------------------------------------------------------------
convert_pairwise <- function(df) {
  required <- c("studlab", "t1", "t2", "y", "se", "rob", "indirectness")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) {
    return(list(data = NULL,
                error = paste("Missing columns:", paste(missing, collapse = ", ")),
                warning = NULL))
  }

  df <- df %>%
    mutate(
      studlab = as.character(studlab),
      t1      = as.character(t1),
      t2      = as.character(t2),
      y       = suppressWarnings(as.numeric(y)),
      se      = suppressWarnings(as.numeric(se))
    ) %>%
    filter(!is.na(y), !is.na(se)) %>%
    make_ordered_factors()

  if ("n1" %in% names(df) && "n2" %in% names(df)) {
    df$n <- as.integer(df$n1) + as.integer(df$n2)
  } else if (!"n" %in% names(df)) {
    df$n <- NA_integer_
  }

  warn <- check_multiarm_pairwise(df)
  list(data = df, error = NULL, warning = warn)
}

# ----------------------------------------------------------------------------
# convert_wide: wide format -> pairwise
# ----------------------------------------------------------------------------
convert_wide <- function(df, outcome_type, sm, studlab_col, arm_maps,
                         rob_col, indir_col, rob_vals, indir_vals,
                         rob_value_map, indir_value_map) {
  # Build long-format data frame from wide
  arm_dfs <- lapply(seq_along(arm_maps), function(i) {
    arm <- arm_maps[[i]]
    if (arm$treat == "(none)" || arm$n == "(none)") return(NULL)

    col_sel <- c(studlab = studlab_col, treat = arm$treat, n = arm$n)
    if (outcome_type == "continuous") {
      if (arm$mean == "(none)" || arm$sd == "(none)") return(NULL)
      col_sel <- c(col_sel, mean = arm$mean, sd = arm$sd)
    } else {
      if (arm$event == "(none)") return(NULL)
      col_sel <- c(col_sel, event = arm$event)
    }

    tryCatch({
      df %>%
        select(!!!setNames(col_sel, names(col_sel))) %>%
        filter(!is.na(.data$treat), !is.na(.data$n))
    }, error = function(e) NULL)
  })

  arm_dfs <- arm_dfs[!sapply(arm_dfs, is.null)]
  if (length(arm_dfs) == 0) {
    return(list(data = NULL,
                error = "No valid arm data found in wide format. Check column mapping.",
                warning = NULL))
  }

  long_df <- bind_rows(arm_dfs) %>%
    mutate(n = as.integer(n), treat = as.character(treat))

  # Attach ROB / indirectness from study-level columns
  if (!is.null(rob_col) && rob_col != "(none)" && rob_col %in% names(df)) {
    rob_df  <- df %>% select(studlab = !!studlab_col, rob = !!rob_col)
    long_df <- left_join(long_df, rob_df, by = "studlab")
  } else {
    long_df$rob <- "low"
  }
  if (!is.null(indir_col) && indir_col != "(none)" && indir_col %in% names(df)) {
    indir_df <- df %>% select(studlab = !!studlab_col, indirectness = !!indir_col)
    long_df  <- left_join(long_df, indir_df, by = "studlab")
  } else {
    long_df$indirectness <- "low"
  }

  # Apply value remapping
  if (!is.null(rob_value_map)) {
    long_df <- long_df %>%
      mutate(rob = remap_values(rob, rob_value_map)) %>%
      filter(rob != "__EXCLUDE__")
  } else {
    long_df$rob <- tolower(trimws(as.character(long_df$rob)))
  }
  if (!is.null(indir_value_map)) {
    long_df <- long_df %>%
      mutate(indirectness = remap_values(indirectness, indir_value_map)) %>%
      filter(indirectness != "__EXCLUDE__")
  } else {
    long_df$indirectness <- tolower(trimws(as.character(long_df$indirectness)))
  }

  if (outcome_type == "continuous") {
    long_df <- long_df %>% mutate(mean = as.numeric(mean), sd = as.numeric(sd))
    convert_continuous(long_df, sm)
  } else {
    long_df <- long_df %>% mutate(event = as.integer(event))
    convert_binary(long_df, sm)
  }
}

# ----------------------------------------------------------------------------
# check_multiarm_pairwise: warn if pairwise data may be incomplete
# ----------------------------------------------------------------------------
check_multiarm_pairwise <- function(df) {
  problems <- df %>%
    group_by(studlab) %>%
    summarise(n_rows = n(),
              n_arms = length(unique(c(t1, t2))),
              .groups = "drop") %>%
    mutate(required = n_arms * (n_arms - 1) / 2) %>%
    filter(n_rows != required)

  if (nrow(problems) == 0) return(NULL)
  paste0(
    "Possibly incomplete multi-arm data for: ",
    paste(problems$studlab, collapse = ", "),
    ". A k-arm study needs k*(k-1)/2 rows. netmeta may reject this data."
  )
}

# ----------------------------------------------------------------------------
# load_demo_cinema_data: load built-in SLEEPI demo dataset
# ----------------------------------------------------------------------------
load_demo_cinema_data <- function() {
  tryCatch({
    df <- readr::read_csv("W2I_CINeMA.csv", col_types = readr::cols(),
                          show_col_types = FALSE)
    names(df) <- stringr::str_to_lower(stringr::str_trim(names(df)))

    # W2I_CINeMA.csv column mapping: id→studlab, t→treat, r→event
    if ("id"         %in% names(df)) df <- dplyr::rename(df, studlab = id)
    if ("t"          %in% names(df)) df <- dplyr::rename(df, treat   = t)
    if ("r"          %in% names(df)) df <- dplyr::rename(df, event   = r)
    if ("treatment"  %in% names(df)) df <- dplyr::rename(df, treat   = treatment)
    if ("overall"    %in% names(df)) df <- dplyr::rename(df, rob     = overall)

    df <- df %>%
      dplyr::mutate(
        rob = dplyr::case_when(
          toupper(trimws(as.character(rob))) == "H" ~ "high",
          toupper(trimws(as.character(rob))) == "M" ~ "some concerns",
          toupper(trimws(as.character(rob))) == "L" ~ "low",
          TRUE ~ stringr::str_to_lower(stringr::str_trim(as.character(rob)))
        ),
        indirectness = dplyr::case_when(
          suppressWarnings(as.integer(indirectness)) == 1L ~ "low",
          suppressWarnings(as.integer(indirectness)) == 2L ~ "some concerns",
          suppressWarnings(as.integer(indirectness)) == 3L ~ "high",
          TRUE ~ "low"
        )
      )

    convert_binary(df, "OR")
  }, error = function(e) {
    list(data    = NULL,
         error   = paste("Failed to load demo data:", conditionMessage(e)),
         warning = NULL)
  })
}
