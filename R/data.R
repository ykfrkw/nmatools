#' W2I (Waking to Insomnia) Trial Data
#'
#' Arm-level data from the W2I network meta-analysis (Furukawa et al. 2024)
#' comparing CBT-I, Combination (CBT-I + pharmacotherapy), and
#' Pharmacotherapy for insomnia. Contains four binary outcomes.
#'
#' @format A data frame with one row per treatment arm and columns:
#' \describe{
#'   \item{id}{Study identifier (author + year)}
#'   \item{t}{Treatment: `"CBT-I"`, `"Combination"`, or `"Pharmacotherapy"`}
#'   \item{n}{Number of randomized participants}
#'   \item{r}{Remission at long-term follow-up (event count)}
#'   \item{n_dropout}{Dropout at long-term follow-up (event count)}
#'   \item{r_pt}{Remission at post-treatment (event count)}
#'   \item{n_dropout_pt}{Dropout at post-treatment (event count)}
#'   \item{rob}{Risk of bias: `"L"` low, `"M"` some concerns, `"H"` high}
#'   \item{indirectness}{Indirectness score (1 = no concerns)}
#' }
#'
#' @examples
#' head(w2i_trials)
#'
#' # Quick NMA for remission at long-term follow-up
#' \dontrun{
#' netmetawrap(
#'   data            = w2i_trials,
#'   studlab         = "id",
#'   treat           = "t",
#'   outcome         = "remission_lt",
#'   n               = "n",
#'   event           = "r",
#'   sm              = "OR",
#'   reference.group = "Pharmacotherapy",
#'   small.values    = "undesirable",
#'   path            = "./outputs"
#' )
#' }
"w2i_trials"

#' Load W2I Trial Data
#'
#' Convenience function that reads the bundled W2I CSV and returns it as a
#' data frame. Equivalent to accessing the `w2i_trials` lazy-data object but
#' ensures the CSV is read fresh from `inst/extdata/`.
#'
#' The matching CINeMA confidence ratings for the long-term remission outcome
#' are bundled at:
#' \preformatted{system.file("extdata", "w2i_cinema.csv", package = "nmatools")}
#' Pass that path (or `read.csv()` of it) to functions accepting a `cinema`
#' argument such as [color_league()], [color_forest()], or [color_netgraph()].
#'
#' @return A data frame (see `?w2i_trials` for column descriptions).
#' @export
#' @examples
#' d <- load_w2i()
#' head(d)
load_w2i <- function() {
  path <- system.file("extdata", "w2i_trials.csv",
                      package = "nmatools", mustWork = TRUE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Build a netmeta Object from W2I Sample Data
#'
#' Convenience wrapper that loads [load_w2i()] and runs `meta::pairwise()`
#' followed by `netmeta::netmeta()` for a chosen outcome. Useful for
#' demonstrating the visualization functions on a known dataset.
#'
#' Four outcomes are available:
#' \describe{
#'   \item{`"remission_lt"`}{Insomnia remission at long-term follow-up
#'     (OR; higher = better).}
#'   \item{`"dropout_lt"`}{Treatment dropout at long-term follow-up
#'     (OR; lower = better).}
#'   \item{`"remission_pt"`}{Insomnia remission at post-treatment
#'     (OR; higher = better).}
#'   \item{`"dropout_pt"`}{Treatment dropout at post-treatment
#'     (OR; lower = better).}
#' }
#'
#' Bundled CINeMA confidence ratings cover the `"remission_lt"` outcome only.
#' Pass `system.file("extdata", "w2i_cinema.csv", package = "nmatools")` (or a
#' `read.csv()` of it) to the `cinema` argument of [color_league()],
#' [color_forest()], or [color_netgraph()].
#'
#' @param outcome One of `"remission_lt"` (default), `"dropout_lt"`,
#'   `"remission_pt"`, or `"dropout_pt"`.
#' @param reference Reference treatment (default `"Pharmacotherapy"`).
#' @return A `netmeta` object.
#' @export
#'
#' @importFrom meta pairwise
#' @importFrom netmeta netmeta
#'
#' @examples
#' \dontrun{
#' net_lt  <- build_w2i_netmeta("remission_lt")
#' net_dlt <- build_w2i_netmeta("dropout_lt")
#' net_pt  <- build_w2i_netmeta("remission_pt")
#' net_dpt <- build_w2i_netmeta("dropout_pt")
#' }
build_w2i_netmeta <- function(outcome   = c("remission_lt", "dropout_lt",
                                            "remission_pt", "dropout_pt"),
                              reference = "Pharmacotherapy") {
  outcome <- match.arg(outcome)

  d <- load_w2i()

  event_col    <- switch(outcome,
    remission_lt = "r",
    dropout_lt   = "n_dropout",
    remission_pt = "r_pt",
    dropout_pt   = "n_dropout_pt"
  )
  small_values <- switch(outcome,
    remission_lt = "undesirable",   # high OR = more remission = good
    dropout_lt   = "desirable",     # high OR = more dropout   = bad
    remission_pt = "undesirable",
    dropout_pt   = "desirable"
  )

  pw <- meta::pairwise(
    treat   = t,
    event   = d[[event_col]],
    n       = n,
    sm      = "OR",
    data    = d,
    studlab = id
  )

  netmeta::netmeta(
    TE           = pw$TE,
    seTE         = pw$seTE,
    treat1       = pw$treat1,
    treat2       = pw$treat2,
    studlab      = pw$studlab,
    data         = pw,
    ref          = reference,
    sm           = "OR",
    common       = FALSE,
    small.values = small_values
  )
}

#' Path to the Bundled W2I CINeMA Ratings
#'
#' Returns the file path to the W2I CINeMA confidence-rating CSV bundled with
#' the package. Designed for use with [color_league()], [color_forest()],
#' [color_netgraph()], [min_context()], and [part_context()].
#'
#' Ratings cover the long-term remission outcome only.
#'
#' @return Character file path.
#' @export
#' @examples
#' \dontrun{
#' net <- build_w2i_netmeta("remission_lt")
#' color_forest(net, cinema = w2i_cinema_path())
#' }
w2i_cinema_path <- function() {
  system.file("extdata", "w2i_cinema.csv",
              package = "nmatools", mustWork = TRUE)
}
