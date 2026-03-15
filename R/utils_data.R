# utils_data.R — internal data preparation helpers

# ── NSE helper ────────────────────────────────────────────────────────────────

# Convert a substitute()-captured expression to a column name string.
# Accepts:
#   - symbol  → "col"   (user writes: studlab = my_col)
#   - string  → "col"   (user writes: studlab = "my_col")
#   - NULL    → NULL    (optional argument not supplied)
# Usage inside a function: .nse_col(substitute(arg))
.nse_col <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x) && length(x) == 1L) return(x)
  if (is.symbol(x)) return(as.character(x))
  stop(
    "Column name must be an unquoted name (e.g. `studlab = id`) ",
    "or a single string (e.g. `studlab = \"id\"`). Got: ", class(x)[1L]
  )
}


# Combine multiple arms with same study/treatment into one row.
# For binary: sum n and event.
# For continuous: pooled mean and SD (within + between variance).
.combine_arms_binary <- function(sub_data) {
  if (nrow(sub_data) == 1L) return(sub_data)
  data.frame(
    studlab   = sub_data$studlab[1L],
    treatment = sub_data$treatment[1L],
    n         = sum(sub_data$n),
    event     = sum(sub_data$event)
  )
}

.combine_arms_continuous <- function(sub_data) {
  if (nrow(sub_data) == 1L) return(sub_data)
  n_total    <- sum(sub_data$n)
  mean_total <- sum(sub_data$n * sub_data$mean_val) / n_total
  ss_within  <- sum((sub_data$n - 1L) * sub_data$sd_val^2)
  ss_between <- sum(sub_data$n * (sub_data$mean_val - mean_total)^2)
  sd_pooled  <- sqrt((ss_within + ss_between) / (n_total - 1L))
  data.frame(
    studlab   = sub_data$studlab[1L],
    treatment = sub_data$treatment[1L],
    n         = n_total,
    mean_val  = mean_total,
    sd_val    = sd_pooled
  )
}

# Prepare arm-level data frame with standardised column names.
# Returns data with columns: studlab, treatment, n, [event | mean_val + sd_val]
.prepare_data <- function(data, studlab, treat, n, event, mean_col, sd_col,
                          is_binary) {
  if (is_binary) {
    df <- data |>
      dplyr::select(
        studlab   = dplyr::all_of(studlab),
        treatment = dplyr::all_of(treat),
        n         = dplyr::all_of(n),
        event     = dplyr::all_of(event)
      ) |>
      dplyr::filter(!is.na(treatment)) |>
      dplyr::mutate(dplyr::across(c(n, event), \(x) round(as.numeric(x), 0L)))

    df <- df |>
      dplyr::group_split(studlab, treatment) |>
      purrr::map_dfr(.combine_arms_binary) |>
      dplyr::filter(!is.na(n), !is.na(event)) |>
      dplyr::arrange(studlab, treatment)
  } else {
    df <- data |>
      dplyr::select(
        studlab   = dplyr::all_of(studlab),
        treatment = dplyr::all_of(treat),
        n         = dplyr::all_of(n),
        mean_val  = dplyr::all_of(mean_col),
        sd_val    = dplyr::all_of(sd_col)
      ) |>
      dplyr::filter(!is.na(treatment))

    df <- df |>
      dplyr::group_split(studlab, treatment) |>
      purrr::map_dfr(.combine_arms_continuous) |>
      dplyr::mutate(n = round(n, 0L)) |>
      dplyr::arrange(studlab, treatment)
  }
  df
}

# Auto-select reference group: treatment with the largest total n.
.auto_reference <- function(df) {
  df |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_n)) |>
    dplyr::slice(1L) |>
    dplyr::pull(treatment)
}
