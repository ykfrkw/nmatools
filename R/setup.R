#' Create a Recommended NMA Project Structure
#'
#' Scaffolds the recommended directory layout for a network meta-analysis
#' project and copies the analysis template.
#'
#' Created directories:
#' * `data/`    — put arm-level input data files here
#' * `outputs/` — `netmetawrap()` writes all results here
#' * `utils/`   — helper / utility scripts
#'
#' @param path Root directory for the new project. Default: current directory.
#' @param open_template Open the template script in RStudio after creation?
#'   Default: `FALSE`.
#'
#' @return Invisibly, a character vector of created paths.
#' @export
#'
#' @examples
#' \dontrun{
#' create_nma_project("~/my_nma_project")
#' }
create_nma_project <- function(path = ".", open_template = FALSE) {
  dirs <- c("data", "outputs", "utils")
  created <- character(0)

  for (d in dirs) {
    full <- file.path(path, d)
    if (!dir.exists(full)) {
      dir.create(full, recursive = TRUE, showWarnings = FALSE)
      created <- c(created, full)
      message("Created: ", full)
    } else {
      message("Already exists: ", full)
    }
  }

  # Copy template script
  tpl_src <- system.file("templates", "run_analysis_template.R",
                          package = "nmatools", mustWork = FALSE)
  tpl_dst <- file.path(path, "utils", "run_analysis_template.R")

  if (nchar(tpl_src) > 0L && !file.exists(tpl_dst)) {
    file.copy(tpl_src, tpl_dst)
    message("Template copied to: ", tpl_dst)
    created <- c(created, tpl_dst)
  }

  if (open_template && file.exists(tpl_dst)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(tpl_dst)
    }
  }

  invisible(created)
}
