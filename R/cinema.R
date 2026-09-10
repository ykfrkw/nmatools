#' Launch the CINeMA + ROB-MEN NMA Evaluator GUI
#'
#' Opens an interactive Shiny application for assessing confidence in network
#' meta-analysis results using CINeMA (Nikolakopoulou et al. 2020) and
#' ROB-MEN (Chiocchia et al. 2021).
#'
#' @param data Optional data.frame. If provided, it is pre-loaded into the
#'   app, bypassing the file-upload step in Module A.
#'   Supported formats depend on the \code{format} argument.
#'   Common column aliases are auto-detected and renamed, matching the
#'   in-GUI upload behavior (e.g. \code{id}/\code{study} to \code{studlab},
#'   \code{t}/\code{treatment} to \code{treat}, \code{r}/\code{events} to
#'   \code{event}). Likewise, \code{rob} and \code{indirectness} values such
#'   as \code{"L"}/\code{"M"}/\code{"H"} or \code{1}/\code{2}/\code{3} are
#'   auto-mapped to \code{"low"}/\code{"some concerns"}/\code{"high"}; when
#'   either column is absent, all rows default to \code{"low"}.
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
#' @param robmen Optional named list of ROB-MEN (Domain 2) defaults, applied
#'   to the automation panel of the \emph{Reporting bias} tab so that the
#'   assessment can be scripted instead of clicked. Recognised elements:
#'   \describe{
#'     \item{\code{bias_order}}{Treatments ordered from the one \emph{most}
#'       likely to be favoured by missing evidence to the least (for
#'       example the newest drug first, an established comparator last; for
#'       psychotherapies, whatever order expert judgement suggests). Either
#'       a character vector in that order, or a named numeric vector such
#'       as approval years (\code{c(New = 2019, Old = 1998)}), sorted
#'       decreasingly. Every provisional \emph{"Suspected bias favouring
#'       X"} the app proposes (Component 1 when studies did not report the
#'       outcome; the qualitative Component 2 rule) takes X from this
#'       ordering; treatments left out fall back to a single novel agent
#'       and then to the direction of the observed effect.}
#'     \item{\code{novel_agents}}{Character vector of treatments supported
#'       only by a few early trials (a bias-suggesting condition of the
#'       ROB-MEN paper).}
#'     \item{\code{no_grey_lit}, \code{prior_pub_bias}, \code{registration},
#'       \code{unpub_consistent}}{Logicals for the review-level conditions
#'       (grey literature not searched; previous evidence of publication
#'       bias; tradition of prospective registration; unpublished studies
#'       available and consistent).}
#'     \item{\code{auto_fill}, \code{auto_sync_d2}}{Logicals switching the
#'       auto-fill of the pairwise judgements and the automatic sync into
#'       CINeMA Domain 2 (both default \code{TRUE}).}
#'     \item{\code{contrib_threshold_pp}}{Contribution threshold in
#'       percentage points (default 15).}
#'   }
#'   Everything remains editable in the GUI.
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
#'
#' # Script the ROB-MEN assumptions: Combination is the newest / most
#' # promoted option, CBT-I the established comparator; grey literature
#' # was not searched.
#' cinema(d, format = "binary", effect_measure = "OR",
#'        robmen = list(bias_order  = c("Combination", "Pharmacotherapy", "CBT-I"),
#'                      no_grey_lit = TRUE))
#' }
cinema <- function(data           = NULL,
                   format         = c("continuous", "binary", "pairwise"),
                   effect_measure = c("SMD", "MD", "OR", "RR"),
                   launch         = TRUE,
                   robmen         = NULL) {

  required <- c("shiny", "DT", "plotly", "shinycssloaders",
                "readxl", "readr", "stringr", "tidyr", "rlang")
  missing <- required[!vapply(required, requireNamespace,
                              logical(1L), quietly = TRUE)]
  if (length(missing))
    stop("cinema() needs the following packages: ",
         paste(missing, collapse = ", "),
         ". Install them with install.packages().", call. = FALSE)

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

  if (!is.null(robmen)) {
    if (!is.list(robmen) || is.null(names(robmen)) || any(!nzchar(names(robmen))))
      stop("`robmen` must be a named list, e.g. list(bias_order = c(...)).",
           call. = FALSE)
    known <- c("bias_order", "novel_agents", "no_grey_lit", "prior_pub_bias",
               "registration", "unpub_consistent", "auto_fill", "auto_sync_d2",
               "contrib_threshold_pp")
    unknown <- setdiff(names(robmen), known)
    if (length(unknown))
      warning("cinema(): ignoring unknown `robmen` element(s): ",
              paste(unknown, collapse = ", "), call. = FALSE)
    .cinema_env$robmen <- robmen[intersect(names(robmen), known)]
  } else {
    .cinema_env$robmen <- NULL
  }

  on.exit({ .cinema_env$initial_data <- NULL; .cinema_env$robmen <- NULL },
          add = TRUE)

  if (launch) {
    shiny::runApp(app_dir, launch.browser = TRUE)
    return(invisible(NULL))
  }
  invisible(shiny::shinyAppDir(app_dir))
}

#' @keywords internal
.cinema_env <- new.env(parent = emptyenv())
