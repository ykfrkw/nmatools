# =============================================================================
# app.R  -  NMA Evaluator (CINeMA + ROB-MEN)
# =============================================================================
# Modules:
#   A  - Data input & preprocessing   (module_A_data_input.R)
#   B  - CINeMA analysis              (module_B_cinema.R)
#   C  - ROB-MEN analysis             (module_C_robmen.R)
#   D  - Dashboard & Export           (module_D_dashboard.R)
# =============================================================================

library(shiny)
library(DT)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(netmeta)
library(meta)
library(shinycssloaders)
library(bslib)

# shadcn-inspired bslib theme — Inter font, zinc palette, soft borders
nma_theme <- bslib::bs_theme(
  version       = 5,
  base_font     = bslib::font_google("Inter"),
  heading_font  = bslib::font_google("Inter"),
  font_scale    = 0.95,
  primary       = "#18181b",
  secondary     = "#71717a",
  success       = "#22c55e",
  info          = "#3b82f6",
  warning       = "#f59e0b",
  danger        = "#ef4444",
  bg            = "#ffffff",
  fg            = "#09090b",
  "border-radius"            = "0.5rem",
  "border-radius-sm"         = "0.375rem",
  "border-radius-lg"         = "0.75rem",
  "card-border-color"        = "#e4e4e7",
  "card-cap-bg"              = "#fafafa",
  "btn-padding-y"            = "0.5rem",
  "btn-padding-x"            = "1rem",
  "input-border-color"       = "#e4e4e7",
  "input-focus-border-color" = "#18181b",
  "navbar-bg"                = "#ffffff",
  "navbar-light-color"       = "#52525b"
)

source("modules/utils.R")            # shared constants and helpers (load first)
source("modules/_d1_sens_judge.R")   # pure RoB sensitivity flowchart (used by Module B)
source("modules/_robmen_bg_plots.R") # pure forest/funnel builder (spec-13 phase 1; used by Module C)
source("modules/module_A_data_input.R")
source("modules/module_B_cinema.R")
source("modules/module_C_robmen.R")
source("modules/module_D_dashboard.R")

ui <- fluidPage(
  theme = nma_theme,
  titlePanel("NMA Evaluator: CINeMA + ROB-MEN"),
  tags$head(tags$style(HTML("
    .alert { padding:10px; border-radius:0.5rem; margin-bottom:10px; }
    .alert-info    { background:#eff6ff; border:1px solid #bfdbfe;
                     color:#1e40af; }
    .alert-success { background:#f0fdf4; border:1px solid #bbf7d0;
                     color:#166534; }
    .alert-warning { background:#fffbeb; border:1px solid #fde68a;
                     color:#92400e; }
    .alert-danger  { background:#fef2f2; border:1px solid #fecaca;
                     color:#991b1b; }
  "))),

  navbarPage(
    id    = "main_navbar",
    title = NULL,
    tabPanel("Configuration",            moduleA_ui("module_a")),
    tabPanel("① Within-study bias", moduleB_d1_ui("module_b")),
    tabPanel("② Reporting bias",    moduleB_d2_ui("module_b")),
    tabPanel("③ Indirectness",      moduleB_d3_ui("module_b")),
    tabPanel("④ Imprecision",       moduleB_d4_ui("module_b")),
    tabPanel("⑤ Heterogeneity",     moduleB_d5_ui("module_b")),
    tabPanel("⑥ Incoherence",       moduleB_d6_ui("module_b")),
    tabPanel("Report",                   moduleD_ui("module_d"))
  )
)

make_server <- function(initial_data = NULL) {
  # When launched via nmatools::cinema(data = ...), retrieve pre-loaded raw data
  if (is.null(initial_data)) {
    raw <- tryCatch(nmatools:::.cinema_env$initial_data, error = function(e) NULL)
    if (!is.null(raw) && !is.null(raw$data)) {
      names(raw$data) <- tolower(trimws(names(raw$data)))
      result <- tryCatch(
        switch(raw$format,
          continuous = convert_continuous(raw$data, raw$effect_measure),
          binary     = convert_binary(raw$data, raw$effect_measure),
          pairwise   = convert_pairwise(raw$data)
        ),
        error = function(e) list(data = NULL, error = conditionMessage(e), warning = NULL)
      )
      if (is.null(result$error)) {
        initial_data <- list(result = result, format = raw$format,
                             effect_measure = raw$effect_measure)
      }
    }
  }
  function(input, output, session) {
    data_a   <- moduleA_server("module_a",
                               initial_data = initial_data,
                               go_to_cinema = function() {
                                 updateNavbarPage(session, "main_navbar",
                                                  selected = "① Within-study bias")
                               })
    cinema_b <- moduleB_server("module_b",
                               processed_data = data_a$processed_data,
                               nma_settings   = data_a$nma_settings,
                               run_trigger    = data_a$run_trigger)

    robmen_c <- moduleC_server("module_c",
                               processed_data = data_a$processed_data,
                               cinema_module  = cinema_b,
                               nma_settings   = data_a$nma_settings,
                               run_trigger    = data_a$run_trigger,
                               go_to_cinema   = function() {
                                 updateNavbarPage(session, "main_navbar",
                                                  selected = "② Reporting bias")
                               })
    moduleD_server("module_d",
                   cinema_module  = cinema_b,
                   robmen_module  = robmen_c,
                   nma_settings_r = data_a$nma_settings,
                   go_to_cinema   = function() {
                     updateNavbarPage(session, "main_navbar",
                                      selected = "① Within-study bias")
                   })
  }
}

# =============================================================================
# launch_nma_evaluator() — R-side data injection
# =============================================================================
# Usage (from RStudio):
#   source("app.R")
#   launch_nma_evaluator(data = my_df, format = "binary", effect_measure = "OR")
#
# Arguments:
#   data           — data.frame. Must match column requirements for 'format'.
#   format         — "continuous" | "binary" | "pairwise"
#   effect_measure — "SMD" | "MD" | "OR" | "RR"
#   launch         — if TRUE (default), starts the app immediately via runApp()
#
# Returns invisibly: the shinyApp object (useful for shinyapps.io deployment).
# =============================================================================
launch_nma_evaluator <- function(
  data,
  format         = c("continuous", "binary", "pairwise"),
  effect_measure = c("SMD", "MD", "OR", "RR"),
  launch         = TRUE
) {
  format         <- match.arg(format)
  effect_measure <- match.arg(effect_measure)

  if (!is.data.frame(data)) stop("`data` must be a data.frame.")

  # Normalise column names to lower-case (same as Module A)
  names(data) <- tolower(trimws(names(data)))

  # Convert to pairwise format using Module A helpers
  result <- switch(format,
    continuous = convert_continuous(data, effect_measure),
    binary     = convert_binary(data, effect_measure),
    pairwise   = convert_pairwise(data)
  )

  if (!is.null(result$error)) {
    stop("Data validation failed: ", result$error)
  }

  initial_data <- list(
    result         = result,
    format         = format,
    effect_measure = effect_measure
  )

  app <- shinyApp(ui = ui, server = make_server(initial_data))
  if (launch) shiny::runApp(app)
  invisible(app)
}

shinyApp(ui = ui, server = make_server())
