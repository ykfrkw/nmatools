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
#'   \item{n}{Number of randomised participants}
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
