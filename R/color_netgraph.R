#' @title Network graph coloured by CINeMA confidence ratings
#'
#' @description Wrapper around \code{netmeta::netgraph()} that colours each
#'   edge (direct comparison) according to its CINeMA confidence rating.
#'   When no CINeMA data are supplied (or a rating is missing), edges fall
#'   back to \code{col_no_cinema}.
#'
#' @param x A \code{netmeta} object.
#' @param cinema Path to a CINeMA report CSV, or a data frame with at least
#'   columns \code{"Comparison"} and \code{"Confidence rating"}.
#'   \code{NULL} (default) colours all edges with \code{col_no_cinema}.
#' @param palette Named list of colour pairs for CINeMA confidence ratings.
#'   Each entry must have \code{$bg} (used as the edge colour).
#'   If \code{NULL} (default), \code{palette_type} selects a built-in palette.
#' @param palette_type One of \code{"pastel"} (default), \code{"classic"}, or
#'   \code{"colorblind"}. Ignored when \code{palette} is supplied.
#'   See \code{\link{cinema_palette}()} for details.
#' @param col_no_cinema Edge colour for comparisons without a CINeMA rating
#'   (default \code{"grey60"}).
#' @param ... Additional arguments passed to \code{\link[netmeta]{netgraph}()}.
#'   The following defaults are applied when a parameter is not explicitly
#'   supplied:
#'   \itemize{
#'     \item \code{plastic = FALSE}
#'     \item \code{points = TRUE}, \code{pch = 21}
#'     \item \code{cex.points}: node size proportional to the total number of
#'       participants in trials including each treatment (computed from
#'       \code{x$data}; falls back to netgraph default when unavailable)
#'     \item \code{col.points = "black"}, \code{bg.points = "gray"}
#'     \item \code{thickness = "number.of.studies"}
#'     \item \code{multiarm = FALSE}
#'     \item \code{number.of.studies = TRUE},
#'       \code{pos.number.of.studies = 0.45}
#'   }
#'   Pass any of these explicitly to override the default.
#' @return The object returned by \code{netmeta::netgraph()} (invisibly).
#' @export
#'
#' @importFrom netmeta netgraph
color_netgraph <- function(x,
                           cinema        = NULL,
                           palette       = NULL,
                           palette_type  = c("pastel", "classic", "colorblind"),
                           col_no_cinema = "grey60",
                           ...) {

  # ---- 1. Palette ----
  if (is.null(palette)) {
    palette_type <- match.arg(palette_type)
    palette <- cinema_palette(palette_type)
  }

  # ---- 2. Parse CINeMA ----
  cinema_df <- if (!is.null(cinema)) parse_cinema(cinema) else NULL

  # ---- 3. Derive edge order: upper triangle of trts, restricted to
  #         pairs with direct evidence.
  #
  #  netmeta::netgraph() draws one edge per unique direct comparison.
  #  Internally it iterates over the upper triangle of the treatment
  #  adjacency matrix in the order of x$trts (i < j).  We replicate
  #  that ordering here to build a colour vector of matching length.
  # ---------------------------------------------------------------
  trts <- x$trts

  # Build presence matrix from raw data (treat1/treat2 columns)
  d <- tryCatch(x$data, error = function(e) NULL)

  has_direct <- function(t1, t2) {
    if (is.null(d)) return(FALSE)
    any((as.character(d$treat1) == t1 & as.character(d$treat2) == t2) |
        (as.character(d$treat1) == t2 & as.character(d$treat2) == t1))
  }

  edge_pairs  <- list()
  for (i in seq_len(length(trts) - 1L)) {
    for (j in seq(i + 1L, length(trts))) {
      if (has_direct(trts[i], trts[j])) {
        edge_pairs[[length(edge_pairs) + 1L]] <- c(trts[i], trts[j])
      }
    }
  }

  # ---- 4. Map each edge to a CINeMA colour ----
  edge_colors <- vapply(edge_pairs, function(pair) {
    if (is.null(cinema_df)) return(col_no_cinema)
    rating <- get_cinema_rating(pair[1L], pair[2L], cinema_df)
    entry  <- cinema_to_color(rating, palette)
    if (is.null(entry)) col_no_cinema else entry$bg
  }, character(1L))

  # ---- 5. Build argument list: defaults overridden by ... ----
  # Compute N per treatment from x$data (n1 + n2, pairwise format).
  # Used as default cex.points so node size reflects participant count.
  cex_default <- NULL
  if (!is.null(d)) {
    t1_col <- grep("^treat1$", colnames(d), value = TRUE)
    t2_col <- grep("^treat2$", colnames(d), value = TRUE)
    n1_col <- grep("^n1$",     colnames(d), value = TRUE)
    n2_col <- grep("^n2$",     colnames(d), value = TRUE)
    if (length(t1_col) && length(n1_col) && length(n2_col)) {
      cex_default <- vapply(trts, function(trt) {
        mask1 <- as.character(d[[t1_col]]) == trt
        mask2 <- as.character(d[[t2_col]]) == trt
        sum(d[[n1_col]][mask1], na.rm = TRUE) +
          sum(d[[n2_col]][mask2], na.rm = TRUE)
      }, numeric(1L))
    }
  }

  defaults <- list(
    plastic               = FALSE,
    points                = TRUE,
    pch                   = 21L,
    cex.points            = cex_default,   # NULL → dropped below
    col.points            = "black",
    bg.points             = "gray",
    thickness             = "number.of.studies",
    multiarm              = FALSE,
    number.of.studies     = TRUE,
    pos.number.of.studies = 0.45
  )
  # Drop defaults that resolved to NULL (e.g., N data unavailable)
  defaults <- defaults[!vapply(defaults, is.null, logical(1L))]
  # User-supplied ... takes priority over defaults
  merged_args <- modifyList(defaults, list(...))

  # ---- 6. Call netgraph ----
  invisible(do.call(netmeta::netgraph, c(list(x = x, col = edge_colors), merged_args)))
}
