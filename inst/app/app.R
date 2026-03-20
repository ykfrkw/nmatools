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

source("modules/utils.R")            # 共通定数・ヘルパー（最初に読み込む）
source("modules/module_A_data_input.R")
source("modules/module_B_cinema.R")
source("modules/module_C_robmen.R")
source("modules/module_D_dashboard.R")

ui <- fluidPage(
  titlePanel("NMA Evaluator: CINeMA + ROB-MEN"),
  tags$head(tags$style(HTML("
    .alert { padding:10px; border-radius:4px; margin-bottom:10px; }
    .alert-info    { background:#d1ecf1; border:1px solid #bee5eb;
                     color:#0c5460; }
    .alert-success { background:#d4edda; border:1px solid #c3e6cb;
                     color:#155724; }
    .alert-warning { background:#fff3cd; border:1px solid #ffeeba;
                     color:#856404; }
    .alert-danger  { background:#f8d7da; border:1px solid #f5c6cb;
                     color:#721c24; }
  "))),

  navbarPage(
    id    = "main_navbar",
    title = NULL,
    tabPanel("A: Data Input", moduleA_ui("module_a")),
    tabPanel("B: CINeMA",     moduleB_ui("module_b")),
    tabPanel("C: ROB-MEN",    moduleC_ui("module_c")),
    tabPanel("D: Dashboard",  moduleD_ui("module_d"))
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
                                                  selected = "B: CINeMA")
                               })
    cinema_b <- moduleB_server("module_b",
                               processed_data = data_a$processed_data,
                               nma_settings   = data_a$nma_settings,
                               run_trigger    = data_a$run_trigger,
                               go_to_robmen   = function() {
                                 updateNavbarPage(session, "main_navbar",
                                                  selected = "C: ROB-MEN")
                               })

    robmen_c <- moduleC_server("module_c",
                               processed_data = data_a$processed_data,
                               cinema_module  = cinema_b,
                               nma_settings   = data_a$nma_settings,
                               run_trigger    = data_a$run_trigger,
                               go_to_cinema   = function() {
                                 updateNavbarPage(session, "main_navbar",
                                                  selected = "B: CINeMA")
                               })
    moduleD_server("module_d",
                   cinema_module  = cinema_b,
                   robmen_module  = robmen_c,
                   nma_settings_r = data_a$nma_settings,
                   go_to_cinema   = function() {
                     updateNavbarPage(session, "main_navbar",
                                      selected = "B: CINeMA")
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
