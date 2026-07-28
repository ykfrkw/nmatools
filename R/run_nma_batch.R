#' Run Network Meta-Analyses for Multiple Outcomes in Batch
#'
#' Iterates over a list of parameter sets and calls [netmetawrap()] for each
#' outcome. Shared arguments can be passed directly through `...` using the
#' very same argument names as [netmetawrap()], so a batch call reads like a
#' single [netmetawrap()] call plus a list of what differs per outcome.
#'
#' Arguments are merged with [utils::modifyList()] in this order (lowest to
#' highest precedence):
#' `.default_args` < `...` < the per-outcome entry in `params_list`.
#'
#' All column name arguments (`studlab`, `treat`, `n`, `event`, `mean_col`,
#' `sd_col`) must be **quoted strings** here, e.g. `studlab = "id"`, because
#' `params_list` / `.default_args` values are forwarded via [do.call()].
#' Unquoted names work only in a direct [netmetawrap()] call.
#'
#' All validation happens before the first analysis starts, so a typo cannot
#' surface after an hour of computation.
#'
#' @param params_list A list of per-outcome parameter lists, each a named list
#'   whose keys are argument names of [netmetawrap()]. At minimum each element
#'   needs `outcome`; everything else may come from `...` or `.default_args`.
#'   A single parameter set (i.e. a named list containing `outcome`) is also
#'   accepted and wrapped automatically.
#'
#'   Valid keys (all arguments of [netmetawrap()]):
#'
#'   * **Data / columns** (quoted strings): `data`, `studlab`, `treat`, `n`,
#'     `event` (binary), `mean_col`, `sd_col` (continuous).
#'   * **Outcome / measure**: `outcome`, `sm` (`"OR"`, `"RR"`, `"SMD"`,
#'     `"MD"`), `reference.group`, `small.values`.
#'   * **Output**: `path`, `a4_rows_per_page`, `trim`, `trim_fuzz`.
#'   * **Forwarded argument lists**: `netmeta_args`, `forest_args`,
#'     `netpairwise_args`, `netsplit_args`.
#'   * **Analysis options**: `rare_events`, `funnel_min_studies`.
#'
#' @param ... Shared [netmetawrap()] arguments applied to every outcome, using
#'   the exact same argument names as [netmetawrap()] (e.g. `data = d`,
#'   `studlab = "id"`, `treat = "t"`, `n = "n"`, `sm = "OR"`). Values given in
#'   an element of `params_list` override these.
#'
#' @param .default_args A named list of shared argument values, kept for
#'   backward compatibility. It has the lowest precedence: `...` and
#'   `params_list` both override it.
#'
#' @return Invisibly, a list of the objects returned by [netmetawrap()], one
#'   per element of `params_list`, named by the resolved `outcome`
#'   (so `res$remission_lt` works). An element is `NULL` when the analysis
#'   errored (the message is printed and the batch continues) or when
#'   subnetworks were detected.
#'
#' @seealso [netmetawrap()] for the single-outcome pipeline and the meaning of
#'   each argument; [plot_transitivity()] for the transitivity companion plot.
#'
#' @examples
#' \dontrun{
#' d <- load_w2i()
#'
#' # Example 1: shared arguments via `...` (recommended)
#' # Column names are quoted strings.
#' params_list <- list(
#'   list(outcome = "remission_lt", event = "r",            small.values = "undesirable"),
#'   list(outcome = "dropout_lt",   event = "n_dropout",    small.values = "desirable"),
#'   list(outcome = "remission_pt", event = "r_pt",         small.values = "undesirable"),
#'   list(outcome = "dropout_pt",   event = "n_dropout_pt", small.values = "desirable")
#' )
#'
#' res <- run_nma_batch(
#'   params_list,
#'   data            = d,
#'   studlab         = "id",
#'   treat           = "t",
#'   n               = "n",
#'   sm              = "OR",
#'   reference.group = "Pharmacotherapy",
#'   path            = "./outputs"
#' )
#' res$remission_lt   # results are named by outcome
#'
#' # Example 2: a single parameter set (no nesting needed)
#' run_nma_batch(
#'   list(outcome = "remission_lt", event = "r"),
#'   data    = d,
#'   studlab = "id",
#'   treat   = "t",
#'   n       = "n",
#'   sm      = "OR"
#' )
#'
#' # Example 3: binary + continuous mixed; per-outcome keys override `...`
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
#'   params_mixed,
#'   data    = my_data,
#'   studlab = "study",
#'   treat   = "treatment",
#'   path    = "./outputs"
#' )
#'
#' # Example 4: the equivalent older style using `.default_args`
#' # (still supported; lowest precedence)
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
#' }
#' @export
run_nma_batch <- function(params_list, ..., .default_args = list()) {
  if (!is.list(params_list)) {
    stop("run_nma_batch(): `params_list` must be a list.", call. = FALSE)
  }
  if (!is.list(.default_args)) {
    stop("run_nma_batch(): `.default_args` must be a named list.", call. = FALSE)
  }

  dots <- list(...)
  if (length(dots) > 0L &&
      (is.null(names(dots)) || !all(nzchar(names(dots))))) {
    stop(
      "run_nma_batch(): all arguments passed via `...` must be named, ",
      "using the same argument names as netmetawrap() ",
      "(e.g. data = d, studlab = \"id\", treat = \"t\").",
      call. = FALSE
    )
  }

  # -- 0. Single parameter set -> wrap ----------------------------------------
  if (.batch_is_single_set(params_list)) {
    params_list <- list(params_list)
  }
  params_list <- unname(params_list)

  # -- 1. Fail-fast validation (before any analysis runs) ----------------------
  valid_args <- .batch_valid_args()

  .batch_check_keys(names(.default_args), valid_args, "`.default_args`")
  .batch_check_keys(names(dots), valid_args, "`...`")
  .batch_check_cols(.default_args, "`.default_args`")
  .batch_check_cols(dots, "`...`")

  shared <- utils::modifyList(.default_args, dots)

  merged_list <- vector("list", length(params_list))
  for (idx in seq_along(params_list)) {
    params <- params_list[[idx]]
    where  <- paste0("params_list[[", idx, "]]")

    if (!is.list(params) ||
        (length(params) > 0L &&
         (is.null(names(params)) || !all(nzchar(names(params)))))) {
      stop(
        "run_nma_batch(): ", where, " must be a named list of ",
        "netmetawrap() arguments (e.g. list(outcome = \"remission_lt\", ",
        "event = \"r\")).",
        call. = FALSE
      )
    }

    .batch_check_keys(names(params), valid_args, where)

    args <- utils::modifyList(shared, params)

    outcome <- args[["outcome"]]
    if (is.null(outcome) || !is.character(outcome) || length(outcome) != 1L ||
        is.na(outcome) || !nzchar(outcome)) {
      stop(
        "run_nma_batch(): ", where, " has no valid `outcome`. ",
        "Every parameter set needs a single non-empty string, ",
        "e.g. outcome = \"remission_lt\".",
        call. = FALSE
      )
    }
    if (!"data" %in% names(args)) {
      stop(
        "run_nma_batch(): no `data` argument for ", where,
        " (outcome \"", outcome, "\"). Supply it once via `...` ",
        "(e.g. run_nma_batch(params_list, data = d, ...)) or per element.",
        call. = FALSE
      )
    }

    .batch_check_cols(args, where)

    merged_list[[idx]] <- args
  }

  # -- 2. Run ------------------------------------------------------------------
  results <- purrr::imap(merged_list, function(args, idx) {
    label <- args$outcome
    message(
      "\n====== run_nma_batch [", idx, "/", length(merged_list), "]: ",
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

  names(results) <- vapply(merged_list, function(a) a$outcome, character(1L))
  invisible(results)
}


# ── internal helpers ─────────────────────────────────────────────────────────

# Column-role arguments of netmetawrap() that go through .nse_col().
.batch_col_keys <- c("studlab", "treat", "n", "event", "mean_col", "sd_col")

# Valid keys = netmetawrap() formals minus internal-only arguments.
# Captured once when the package is loaded (netmetawrap.R collates first), so
# the check does not depend on the binding at call time.
.batch_valid_arg_names <- setdiff(
  names(formals(netmetawrap)),
  c(".subnet_label", "...")
)

.batch_valid_args <- function() .batch_valid_arg_names

# TRUE when params_list is itself a single parameter set rather than a list
# of parameter sets.
.batch_is_single_set <- function(params_list) {
  nms <- names(params_list)
  !is.null(nms) && "outcome" %in% nms && !is.null(params_list[["outcome"]])
}

# Error on keys that are not netmetawrap() arguments.
.batch_check_keys <- function(keys, valid_args, where) {
  if (length(keys) == 0L) return(invisible(NULL))
  bad <- setdiff(keys, valid_args)
  if (length(bad) == 0L) return(invisible(NULL))
  stop(
    "run_nma_batch(): unknown argument(s) in ", where, ": ",
    paste0("`", bad, "`", collapse = ", "), ".\n",
    "  Valid netmetawrap() arguments are: ",
    paste(valid_args, collapse = ", "), ".",
    call. = FALSE
  )
}

# Error on column-role values that are neither a single string nor a symbol.
.batch_check_cols <- function(args, where) {
  for (key in intersect(.batch_col_keys, names(args))) {
    val <- args[[key]]
    if (is.null(val)) next
    ok <- (is.character(val) && length(val) == 1L && !is.na(val)) ||
      is.symbol(val)
    if (ok) next
    stop(
      "run_nma_batch(): `", key, "` must be a quoted column name ",
      "(e.g. ", key, " = \"id\") in ", where,
      ". Unquoted names work only in a direct netmetawrap() call, because ",
      "`params_list` / `.default_args` values are forwarded via do.call(). ",
      "Got: ", class(val)[1L], " of length ", length(val), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}
