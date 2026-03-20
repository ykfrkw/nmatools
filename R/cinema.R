#' Launch the CINeMA + ROB-MEN NMA Evaluator GUI
#'
#' Opens an interactive Shiny application for assessing confidence in network
#' meta-analysis results using CINeMA (Nikolakopoulou et al. 2020) and
#' ROB-MEN (Chiocchia et al. 2021).
#'
#' @param data Optional data.frame. If provided, it is pre-loaded into the
#'   app, bypassing the file-upload step in Module A.
#'   Supported formats depend on the \code{format} argument.
#' @param format Character. Input data structure:
#'   \describe{
#'     \item{\code{"continuous"}}{Arm-level continuous data (columns:
#'       studlab/treat/n/mean/sd).}
#'     \item{\code{"binary"}}{Arm-level binary data (columns:
#'       studlab/treat/n/event).}
#'     \item{\code{"pairwise"}}{Pre-computed pairwise effects (columns:
#'       studlab/t1/t2/y/se).}
#'   }
#'   Ignored when \code{data = NULL}.
#' @param effect_measure Character. Effect measure: \code{"SMD"}, \code{"MD"},
#'   \code{"OR"}, or \code{"RR"}. Ignored when \code{data = NULL}.
#' @param launch Logical. If \code{TRUE} (default), launches the app
#'   immediately via \code{\link[shiny]{runApp}}. Set to \code{FALSE} to
#'   return the \code{shinyApp} object for programmatic use (e.g.,
#'   \code{shinyapps.io} deployment).
#' @return Invisibly, the \code{shinyApp} object when \code{launch = FALSE};
#'   otherwise \code{NULL}.
#' @references
#' Nikolakopoulou A et al. (2020). CINeMA: An approach for assessing
#' confidence in the results of a network meta-analysis.
#' \emph{PLoS Med} 17(4):e1003082. \doi{10.1371/journal.pmed.1003082}
#'
#' Chiocchia V et al. (2021). ROB-MEN: a tool to assess the risk of bias
#' due to missing evidence in network meta-analysis.
#' \emph{BMC Med} 19:304. \doi{10.1186/s12916-021-02166-3}
#' @export
#' @examples
#' \dontrun{
#' # Launch with no pre-loaded data (upload via the GUI)
#' cinema()
#'
#' # Pre-load binary data from the bundled W2I sample
#' d <- load_w2i()
#' cinema(d, format = "binary", effect_measure = "OR")
#' }
cinema <- function(data           = NULL,
                   format         = c("continuous", "binary", "pairwise"),
                   effect_measure = c("SMD", "MD", "OR", "RR"),
                   launch         = TRUE) {

  app_dir <- system.file("app", package = "nmatools")
  if (!nzchar(app_dir) || !dir.exists(app_dir))
    stop("nmatools Shiny app not found. Try reinstalling the package.",
         call. = FALSE)

  if (!is.null(data)) {
    if (!is.data.frame(data))
      stop("`data` must be a data.frame.", call. = FALSE)
    format         <- match.arg(format)
    effect_measure <- match.arg(effect_measure)
    names(data)    <- tolower(trimws(names(data)))
    .cinema_env$initial_data <- list(
      data           = data,
      format         = format,
      effect_measure = effect_measure
    )
  } else {
    .cinema_env$initial_data <- NULL
  }

  on.exit(.cinema_env$initial_data <- NULL, add = TRUE)

  if (launch) {
    shiny::runApp(app_dir, launch.browser = TRUE)
    return(invisible(NULL))
  }
  invisible(shiny::shinyAppDir(app_dir))
}

#' @keywords internal
.cinema_env <- new.env(parent = emptyenv())
