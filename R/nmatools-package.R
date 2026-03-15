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
