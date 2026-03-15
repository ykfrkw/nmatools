#' Run Network Meta-Analyses for Multiple Outcomes in Batch
#'
#' Iterates over a list of parameter sets and calls [netmetawrap()] for each
#' outcome. Arguments in `params_list` override `.default_args`, so you only
#' need to specify what differs per outcome.
#'
#' Column name arguments (`studlab`, `treat`, `n`, `event`, `mean_col`,
#' `sd_col`) must be **strings** when used inside `params_list` or
#' `.default_args`, because they are passed via [do.call()].
#' Unquoted names work only with the direct `netmetawrap()` call.
#'
#' @param params_list A named or unnamed list of per-outcome parameter lists.
#'   Every key must be a valid argument name of [netmetawrap()].
#'   At minimum each element needs `outcome`. All other keys are optional if
#'   covered by `.default_args`.
#'
#'   **Column name keys** (strings):
#'   `studlab`, `treat`, `n`, `event` (binary), `mean_col`, `sd_col`
#'   (continuous).
#'
#'   **Effect measure key**: `sm` — `"OR"`, `"RR"`, `"SMD"`, or `"MD"`.
#'
#'   **Other keys**: `reference.group`, `small.values`, `path`,
#'   `netmeta_args`, `forest_args`, `netpairwise_args`, `netsplit_args`,
#'   `a4_rows_per_page`, `trim`, `trim_fuzz`.
#'
#' @param .default_args A named list of default argument values shared across
#'   all outcomes. Values in `params_list` take precedence. Useful for
#'   `data`, `path`, `studlab`, `treat`, `n`, `sm`, `reference.group`, etc.
#'
#' @return Invisibly, a list of fitted objects returned by [netmetawrap()],
#'   one per element of `params_list` (`NULL` when subnetworks are detected).
#'
#' @examples
#' \dontrun{
#' d <- load_w2i()
#'
#' # ── Example 1: multiple binary outcomes ──────────────────────────────────
#' params_list <- list(
#'   list(outcome = "remission_lt",  event = "r",            small.values = "undesirable"),
#'   list(outcome = "dropout_lt",    event = "n_dropout",    small.values = "desirable"),
#'   list(outcome = "remission_pt",  event = "r_pt",         small.values = "undesirable"),
#'   list(outcome = "dropout_pt",    event = "n_dropout_pt", small.values = "desirable")
#' )
#'
#' run_nma_batch(
#'   params_list   = params_list,
#'   .default_args = list(
#'     data            = d,
#'     studlab         = "id",
#'     treat           = "t",
#'     n               = "n",
#'     sm              = "OR",
#'     reference.group = "Pharmacotherapy",
#'     path            = "./outputs"
#'   )
#' )
#'
#' # ── Example 2: binary + continuous mixed ─────────────────────────────────
#' # Specify different column names and sm per outcome in params_list.
#' params_mixed <- list(
#'   list(
#'     outcome      = "remission",
#'     n            = "n",
#'     event        = "r",
#'     sm           = "OR",
#'     small.values = "undesirable"
#'   ),
#'   list(
#'     outcome      = "sleep_efficiency",
#'     n            = "n_cont",
#'     mean_col     = "se_mean",
#'     sd_col       = "se_sd",
#'     sm           = "SMD",
#'     small.values = "desirable"
#'   )
#' )
#'
#' run_nma_batch(
#'   params_list   = params_mixed,
#'   .default_args = list(
#'     data    = my_data,
#'     studlab = "study",
#'     treat   = "treatment",
#'     path    = "./outputs"
#'   )
#' )
#' }
#' @export
run_nma_batch <- function(params_list, .default_args = list()) {
  stopifnot(is.list(params_list))

  results <- purrr::imap(params_list, function(params, idx) {
    # Merge: per-outcome params override shared defaults
    args <- utils::modifyList(.default_args, params)

    label <- if (!is.null(args$outcome)) args$outcome else paste0("outcome_", idx)
    message(
      "\n====== run_nma_batch [", idx, "/", length(params_list), "]: ",
      label, " ======"
    )

    tryCatch(
      do.call(netmetawrap, args),
      error = function(e) {
        message("[ ERROR ] ", label, ": ", conditionMessage(e))
        NULL
      }
    )
  })

  invisible(results)
}
