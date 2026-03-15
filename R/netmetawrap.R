#' One-stop Network Meta-Analysis Wrapper
#'
#' Runs a complete NMA pipeline and saves all outputs. Column names can be
#' supplied either unquoted (like `meta::pairwise()`) or as strings. Non-column
#' arguments (`sm`, `reference.group`, etc.) are always strings.
#'
#' @param data Data frame in arm-level format (one row per study arm).
#' @param studlab Unquoted or quoted column name for study labels.
#' @param treat Unquoted or quoted column name for treatment.
#' @param outcome Outcome name used for the output sub-directory and file names.
#' @param n Unquoted or quoted column name for sample size.
#' @param event Unquoted or quoted column name for event count (binary only).
#' @param mean_col Unquoted or quoted column name for arm mean (continuous only).
#' @param sd_col Unquoted or quoted column name for arm SD (continuous only).
#' @param sm Effect measure: `"OR"`, `"RR"`, `"SMD"`, or `"MD"`.
#' @param reference.group Reference treatment (string). `NULL` = auto (largest n).
#' @param small.values `"undesirable"` (default) or `"desirable"`.
#' @param path Base output directory. Defaults to `"./outputs"`.
#'   The function creates `{path}/{outcome}/` automatically.
#' @param netmeta_args Named list of extra arguments forwarded to
#'   `netmeta::netmeta()` or `netmeta::netmetabin()`, overriding defaults.
#' @param forest_args Named list of extra arguments forwarded to all
#'   `forest()` calls, overriding defaults.
#' @param netpairwise_args Named list of extra arguments forwarded to
#'   `netmeta::netpairwise()`.
#' @param netsplit_args Named list of extra arguments forwarded to
#'   `netmeta::netsplit()`.
#' @param a4_rows_per_page Estimated max rows per A4 page for large plots.
#'   Default: `45`.
#' @param trim Trim whitespace from output PDFs via `magick`. Default: `TRUE`.
#' @param trim_fuzz Fuzz parameter for `magick::image_trim()`. Default: `30L`.
#' @param .subnet_label Internal — subnetwork label appended to file names.
#'
#' @return Invisibly, the fitted `netmeta`/`netmetabin` object, or `NULL` when
#'   subnetworks are detected.
#'
#' @examples
#' \dontrun{
#' # Unquoted column names (recommended)
#' netmetawrap(
#'   data            = w2i_trials,
#'   studlab         = id,
#'   treat           = t,
#'   outcome         = "remission_lt",
#'   n               = n,
#'   event           = r,
#'   sm              = "OR",
#'   reference.group = "Pharmacotherapy",
#'   small.values    = "undesirable"
#' )
#'
#' # Quoted strings also work
#' netmetawrap(
#'   data     = w2i_trials,
#'   studlab  = "id",
#'   treat    = "t",
#'   outcome  = "remission_lt",
#'   n        = "n",
#'   event    = "r",
#'   sm       = "OR"
#' )
#' }
#' @export
netmetawrap <- function(
    data,
    studlab,
    treat,
    outcome,
    n,
    event            = NULL,
    mean_col         = NULL,
    sd_col           = NULL,
    sm,
    reference.group  = NULL,
    small.values     = "undesirable",
    path             = "./outputs",
    netmeta_args     = list(),
    forest_args      = list(),
    netpairwise_args = list(),
    netsplit_args    = list(),
    a4_rows_per_page   = 45L,
    funnel_min_studies = 10L,
    trim               = TRUE,
    trim_fuzz          = 30L,
    .subnet_label      = NULL
) {

  # ── 0. NSE → string (must run before any evaluation of column args) ─────────
  studlab  <- .nse_col(substitute(studlab))
  treat    <- .nse_col(substitute(treat))
  n        <- .nse_col(substitute(n))
  event    <- .nse_col(substitute(event))
  mean_col <- .nse_col(substitute(mean_col))
  sd_col   <- .nse_col(substitute(sd_col))

  # ── 1. Validate ─────────────────────────────────────────────────────────────
  is_binary     <- !is.null(event) && is.null(mean_col)
  is_continuous <- is.null(event)  && !is.null(mean_col) && !is.null(sd_col)
  if (!is_binary && !is_continuous) {
    stop(
      "Specify either `event` (binary) or both `mean_col` + `sd_col` (continuous)."
    )
  }

  # ── 2. Output directory ──────────────────────────────────────────────────────
  dir_suffix <- if (!is.null(.subnet_label)) paste0("_subnet_", .subnet_label) else ""
  file_label <- paste0(outcome, dir_suffix)
  output_dir <- file.path(path, file_label)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("[ netmetawrap ] Starting: ", file_label)

  # ── 3. Arm-level data preparation ──────────────────────────────────────────
  df <- .prepare_data(data, studlab, treat, n, event, mean_col, sd_col,
                      is_binary)

  # ── 4. Save data CSV ────────────────────────────────────────────────────────
  df_multi <- df |>
    dplyr::group_by(studlab) |>
    dplyr::filter(dplyr::n() >= 2L) |>
    dplyr::ungroup()
  utils::write.csv(
    df_multi,
    file.path(output_dir, paste0("data_", file_label, ".csv")),
    row.names = FALSE
  )

  # ── 5. Pairwise conversion ──────────────────────────────────────────────────
  if (is_binary) {
    df_pw <- meta::pairwise(
      data    = df,
      studlab = studlab,
      treat   = treatment,
      n       = n,
      event   = event,
      sm      = sm,
      common  = FALSE,
      random  = TRUE
    )
  } else {
    df_pw <- meta::pairwise(
      data    = df,
      studlab = studlab,
      treat   = treatment,
      n       = n,
      mean    = mean_val,
      sd      = sd_val,
      sm      = sm,
      common  = FALSE,
      random  = TRUE
    )
  }

  # ── 6. Subnetwork detection ─────────────────────────────────────────────────
  net_conn <- netmeta::netconnection(df_pw)
  if (net_conn$n.subnets > 1L) {
    message(
      "[ netmetawrap ] ", net_conn$n.subnets,
      " subnetworks detected — running each separately."
    )
    subnet_map <- tibble::tibble(
      studlab = net_conn$studlab,
      subnet  = net_conn$subnet
    )

    purrr::walk(unique(subnet_map$subnet), function(s) {
      sub_studies <- subnet_map |>
        dplyr::filter(subnet == s) |>
        dplyr::pull(studlab)
      sub_data <- data |>
        dplyr::filter(.data[[studlab]] %in% sub_studies)

      valid_trts <- unique(sub_data[[treat]])
      ref <- if (!is.null(reference.group) && reference.group %in% valid_trts) {
        reference.group
      } else {
        sub_data |>
          dplyr::group_by(.data[[treat]]) |>
          dplyr::summarise(total_n = sum(.data[[n]], na.rm = TRUE),
                           .groups = "drop") |>
          dplyr::arrange(dplyr::desc(total_n)) |>
          dplyr::slice(1L) |>
          dplyr::pull(.data[[treat]])
      }

      # Use do.call so column-name strings are passed correctly through NSE
      do.call(netmetawrap, list(
        data             = sub_data,
        studlab          = studlab,
        treat            = treat,
        outcome          = outcome,
        n                = n,
        event            = event,
        mean_col         = mean_col,
        sd_col           = sd_col,
        sm               = sm,
        reference.group  = ref,
        small.values     = small.values,
        path             = file.path(path, outcome),
        netmeta_args     = netmeta_args,
        forest_args      = forest_args,
        netpairwise_args   = netpairwise_args,
        netsplit_args      = netsplit_args,
        a4_rows_per_page   = a4_rows_per_page,
        funnel_min_studies = funnel_min_studies,
        trim               = trim,
        trim_fuzz          = trim_fuzz,
        .subnet_label      = s
      ))
    })
    return(invisible(NULL))
  }

  # ── 7. Auto reference group ─────────────────────────────────────────────────
  if (is.null(reference.group)) {
    reference.group <- .auto_reference(df)
    message("[ netmetawrap ] Auto-selected reference group: ", reference.group)
  }

  # ── 8. Run netmeta / netmetabin ─────────────────────────────────────────────
  if (is_binary) {
    default_nm <- list(
      sm              = sm,
      reference.group = reference.group,
      small.values    = small.values,
      sort            = TRUE,
      common          = FALSE,
      random          = TRUE,
      method          = "Inverse",
      incr            = 0.001
    )
    net_meta <- do.call(
      netmeta::netmetabin,
      c(list(df_pw), utils::modifyList(default_nm, netmeta_args))
    )
  } else {
    default_nm <- list(
      reference.group = reference.group,
      small.values    = small.values,
      sort            = TRUE,
      common          = FALSE,
      random          = TRUE
    )
    net_meta <- do.call(
      netmeta::netmeta,
      c(list(df_pw), utils::modifyList(default_nm, netmeta_args))
    )
  }

  # ── 9. Save RDS + text summary ──────────────────────────────────────────────
  saveRDS(net_meta,
          file.path(output_dir, paste0("netmeta_", file_label, ".rds")))
  capture.output(
    print(net_meta),
    file = file.path(output_dir, paste0("netmeta_", file_label, ".txt"))
  )

  # ── 10. Consistency tests ────────────────────────────────────────────────────
  # Global (design-based decomposition)
  capture.output(
    print(netmeta::decomp.design(net_meta)),
    file = file.path(output_dir, paste0("global_test_", file_label, ".txt"))
  )

  # Local (netsplit)
  ns_obj <- do.call(
    netmeta::netsplit,
    c(list(net_meta), utils::modifyList(list(prediction = TRUE), netsplit_args))
  )
  capture.output(
    print(ns_obj),
    file = file.path(output_dir, paste0("local_test_", file_label, ".txt"))
  )

  # ── 11. League table ─────────────────────────────────────────────────────────
  league <- netmeta::netleague(
    net_meta,
    digits    = 2L,
    bracket   = "(",
    separator = " to ",
    seq       = netmeta::netrank(net_meta, small.values = small.values),
    common    = FALSE
  )
  writexl::write_xlsx(
    as.data.frame(league$random),
    col_names = FALSE,
    path = file.path(output_dir, paste0("leaguetable_", file_label, ".xlsx"))
  )

  # ── 12. Shared plot setup ────────────────────────────────────────────────────
  n_trts         <- length(net_meta$trts)
  forest_w       <- .calc_forest_width(net_meta)
  forest_h_ref   <- .calc_forest_height(n_trts)
  xlim_ref       <- .calc_xlim(net_meta, reference.group, sm, wide = FALSE)
  xlim_wide      <- .calc_xlim(net_meta, reference.group, sm, wide = TRUE)

  # Effective leftcols: user override takes priority over default
  default_leftcols   <- c("studlab", "n.trts")
  effective_leftcols <- forest_args$leftcols %||% default_leftcols
  add_rows           <- .calc_add_rows(net_meta, effective_leftcols)

  # Sort direction: Pscore ascending = large value on top (undesirable → lower is better = sort DESC)
  # forest.netmeta sorts so that the treatment at the top has the LARGEST sortvar.
  # Pscore: high = better (for "desirable" small.values, high Pscore = bad → sort ASC)
  sort_var <- if (small.values == "desirable") quote(-Pscore) else quote(Pscore)

  # ── 13. Netgraph ─────────────────────────────────────────────────────────────
  .save_plot(
    file      = file.path(output_dir, paste0("netgraph_", file_label, ".pdf")),
    width     = 10,
    height    = 10,
    trim      = trim,
    trim_fuzz = trim_fuzz,
    expr = {
      netmeta::netgraph(
        net_meta,
        seq                   = "optimal",
        col                   = "black",
        plastic               = FALSE,
        points                = TRUE,
        pch                   = 21L,
        cex.points            = n_trts,
        col.points            = "black",
        bg.points             = "gray",
        thickness             = "number.of.studies",
        multiarm              = FALSE,
        number.of.studies     = TRUE,
        pos.number.of.studies = 0.45
      )
    }
  )

  # ── 14. Forest plot vs reference (PlotForestPlots.R style) ───────────────────
  default_forest <- list(
    smlab                    = paste0(outcome, "\n(Random Effects Model)"),
    leftcols                 = default_leftcols,
    sortvar                  = sort_var,
    xlim                     = xlim_ref,
    add.rows.before.reference = add_rows
  )
  merged_forest <- utils::modifyList(default_forest, forest_args)

  .save_plot(
    file      = file.path(output_dir, paste0("forest_", file_label, ".pdf")),
    width     = forest_w,
    height    = forest_h_ref,
    trim      = trim,
    trim_fuzz = trim_fuzz,
    expr      = do.call(meta::forest, c(list(net_meta), merged_forest))
  )

  # ── 15. Netpairwise forest ───────────────────────────────────────────────────
  np_obj <- do.call(
    netmeta::netpairwise,
    c(list(net_meta),
      utils::modifyList(list(common = FALSE, prediction = TRUE),
                        netpairwise_args))
  )

  default_np_forest <- list(
    smlab      = paste0(outcome, "\n(Random Effects Model)"),
    leftcols   = "studlab",
    xlim       = xlim_wide,
    prediction = TRUE
  )
  np_forest_merged <- utils::modifyList(default_np_forest, forest_args)

  .save_metalist_paged(
    obj         = np_obj,
    forest_args = np_forest_merged,
    file_base   = file.path(output_dir,
                            paste0("forest_netpairwise_", file_label)),
    width       = forest_w + 4,
    a4_rows     = a4_rows_per_page,
    trim        = trim,
    trim_fuzz   = trim_fuzz
  )

  # ── 16. Netsplit forest ──────────────────────────────────────────────────────
  .save_netsplit_paged(
    ns_obj      = ns_obj,
    forest_args = forest_args,
    file_base   = file.path(output_dir,
                            paste0("forest_netsplit_", file_label)),
    width       = forest_w + 2,
    a4_rows     = a4_rows_per_page,
    trim        = trim,
    trim_fuzz   = trim_fuzz
  )

  # ── 17. Contour-enhanced funnel plots (pairs with ≥ funnel_min_studies) ──────
  # Extract pairwise comparison objects from np_obj; guard against non-meta elements.
  np_valid <- Filter(function(m) is.list(m) && !is.null(m[["k"]]), np_obj)
  funnel_pairs <- Filter(function(m) m[["k"]] >= funnel_min_studies, np_valid)

  if (length(funnel_pairs) > 0L) {
    message("[ netmetawrap ] Saving contour-enhanced funnel plots for ",
            length(funnel_pairs), " pair(s) with k \u2265 ", funnel_min_studies, ".")

    pair_labels <- names(funnel_pairs)
    if (is.null(pair_labels))
      pair_labels <- paste0("pair_", seq_along(funnel_pairs))

    for (i in seq_along(funnel_pairs)) {
      m_pw  <- funnel_pairs[[i]]
      lbl   <- if (nzchar(pair_labels[i])) pair_labels[i] else paste0("pair_", i)
      fname <- gsub("[^A-Za-z0-9_-]", "_", lbl)

      .save_plot(
        file      = file.path(output_dir,
                              paste0("funnel_pairwise_", file_label, "_", fname, ".pdf")),
        width     = 7,
        height    = 6,
        trim      = trim,
        trim_fuzz = trim_fuzz,
        expr      = {
          meta::funnel(
            m_pw,
            contour.levels = c(0.90, 0.95, 0.99),
            col.contour    = c("darkgray", "gray", "lightgray"),
            main           = lbl,
            studlab        = TRUE
          )
          graphics::legend(
            "topright",
            fill   = c("darkgray", "gray", "lightgray"),
            legend = c("p < 0.10", "p < 0.05", "p < 0.01"),
            bg     = "white"
          )
        }
      )
    }
  }

  # ── 18. Direct evidence contributions (netcontrib) ───────────────────────────
  tryCatch({
    nc_obj <- netmeta::netcontrib(net_meta, method = "random")
    nc_w   <- max(8, n_trts * 1.5)
    nc_h   <- max(6, n_trts * 1.0)
    .save_plot(
      file      = file.path(output_dir,
                            paste0("contributions_", file_label, ".pdf")),
      width     = nc_w,
      height    = nc_h,
      trim      = trim,
      trim_fuzz = trim_fuzz,
      expr      = {
        graphics::par(mar = c(10, 12, 4, 2))
        plot(nc_obj,
             main = paste0("Direct Evidence Contributions: ", outcome))
      }
    )
  }, error = function(e) {
    message("[ netmetawrap ] netcontrib skipped: ", conditionMessage(e))
  })

  message("[ netmetawrap ] Done: ", file_label)
  invisible(net_meta)
}

# Null-coalescing operator (used internally)
`%||%` <- function(a, b) if (!is.null(a)) a else b
