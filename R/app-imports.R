# app-imports.R -- keep Shiny-app-only Imports visible to R CMD check
#
# The packages referenced below are genuine runtime dependencies, but they are
# used exclusively inside inst/app/ (the cinema() Shiny application).  R CMD
# check only scans R/ for namespace usage, so it reports them as
# "Namespaces in Imports not imported from".  Touching each namespace once in
# this never-called internal helper removes that false-positive NOTE without
# changing any behaviour.
#
# Nothing here is called or exported.  Only the function objects are referenced
# (never invoked), so loading the package has no side effects.

#' @noRd
.app_imports <- function() {
  # inst/app/ UI + tables
  DT::datatable
  bslib::bs_theme
  shinycssloaders::withSpinner
  plotly::plot_ly
  # inst/app/ data import + wrangling
  readr::read_csv
  readxl::read_excel
  stringr::str_detect
  tidyr::pivot_longer
  rlang::sym
  # inst/app/ meta-analytic back-end
  metafor::rma
  invisible(NULL)
}
