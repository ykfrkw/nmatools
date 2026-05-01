#' nmatools: Network Meta-Analysis Tools
#'
#' Wrapper functions for network meta-analysis using the `netmeta` package.
#' The main function `netmetawrap()` provides a one-stop pipeline:
#'
#' * Arm-level data preparation and pairwise conversion
#' * Automatic subnetwork detection and per-subnet analysis
#' * Binary (`netmetabin`) and continuous (`netmeta`) dispatch
#' * All outputs: RDS, TXT summary, global/local consistency tests, data CSV,
#'   league table (XLSX)
#' * Trimmed, A4-ready PDF plots: netgraph, forest, netpairwise, netsplit
#' * Large plots automatically split into multi-page PDFs
#'
#' @keywords internal
"_PACKAGE"

# Silence R CMD check NOTEs about NSE-bound column names used by dplyr,
# meta::pairwise(), and ggplot2 throughout the package.
utils::globalVariables(c(
  # netmetawrap / data prep NSE columns
  "treatment", "studlab", "treat", "n", "n1", "n2", "event",
  "mean_val", "sd_val", "total_n", "id", "subnet", "t",
  ".data", "TE", "seTE", "lower", "upper", "p", "y", "x",
  "fill", "group", "label", "value", "var", "Outcome",
  "abs_effect", "rating", "treatment_label", "trial_count",
  "is_ref", "ref_abs", "fill_color", "signed_pv", "trivial",
  "pos", "level", "estimate",
  # vitruvian() ggplot aes() bindings
  "outcome_idx", "ymin_val", "ymax_val", "fill_col",
  "sector", "arc_lab", "arc_grp", "xend", "yend",
  "eer_x", "eer_y", "val_label", "cer_x", "cer_y",
  "ref_val_label",
  # pval_legend_ggplot() aes()
  "color"
))
