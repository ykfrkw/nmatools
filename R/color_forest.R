#' Forest plot with CI squares coloured by CINeMA confidence rating
#'
#' Wrapper around \code{\link[netmeta]{forest.netmeta}()} that colours each
#' treatment's CI square according to its CINeMA confidence rating versus the
#' reference treatment.  All other arguments are forwarded verbatim to
#' \code{netmeta::forest()}.
#'
#' @param x A \code{netmeta} object.
#' @param cinema Path to a CINeMA report CSV file, or a data frame with at
#'   least two columns: \code{"Comparison"} (e.g. \code{"CBT-I:Placebo"}) and
#'   \code{"Confidence rating"} (e.g. \code{"Moderate"}).
#' @param reference.group Reference treatment used both for CINeMA look-up
#'   and as the \code{reference.group} argument of \code{forest()}.
#'   Defaults to \code{x$reference.group}.
#' @param palette Named list of colour pairs (see \code{\link{cinema_palette}()}).
#'   Each entry must have a \code{$bg} element used as the CI square colour.
#'   If \code{NULL} (default), \code{palette_type} selects a built-in palette.
#' @param palette_type One of \code{"pastel"} (default), \code{"classic"}, or
#'   \code{"colorblind"}. Ignored when \code{palette} is supplied.
#' @param col_no_cinema Fallback colour for treatments with no CINeMA rating
#'   (default \code{"grey80"}).
#' @param ... Additional arguments passed verbatim to
#'   \code{\link[netmeta]{forest.netmeta}()}.  User-supplied values always
#'   take priority; e.g. passing \code{col.square} explicitly overrides the
#'   CINeMA-derived colours.
#'
#' @details
#' Colours are applied in the order \code{x$trts} \emph{excluding}
#' \code{reference.group}, which is the default row order used by
#' \code{forest.netmeta()}.  If you also supply \code{sortvar} via \code{...},
#' ensure the resulting row order matches the CINeMA colour vector.
#'
#' The function sets three arguments derived from CINeMA ratings (unless
#' already provided by the user):
#' \describe{
#'   \item{\code{col.square}}{Fill colour of the CI square for each treatment.}
#'   \item{\code{col.square.lines}}{Border colour of the CI square.}
#'   \item{\code{col.study}}{Label colour of the treatment name on the left.}
#' }
#'
#' @return Invisibly returns the value returned by \code{netmeta::forest()}.
#' @export
#'
#' @importFrom meta forest
#'
#' @examples
#' \dontrun{
#' net <- build_w2i_netmeta("remission_lt")
#' w2i <- load_w2i()
#' color_forest(net, cinema = w2i$cinema$remission_lt)
#' }
color_forest <- function(x,
                         cinema,
                         reference.group = x$reference.group,
                         palette         = NULL,
                         palette_type    = c("pastel", "classic", "colorblind"),
                         col_no_cinema   = "grey80",
                         ...) {

  # ---- 1. Resolve palette ----
  if (is.null(palette)) {
    palette_type <- match.arg(palette_type)
    palette <- cinema_palette(palette_type)
  }

  # ---- 2. Parse CINeMA data ----
  cinema_df <- parse_cinema(cinema)

  # ---- 3. Build per-treatment colour vector ----
  # forest.netmeta() plots treatments in x$trts order, excluding the reference.
  ref       <- reference.group
  trts_plot <- x$trts[x$trts != ref]

  col_sq <- vapply(trts_plot, function(trt) {
    rating <- get_cinema_rating(trt, ref, cinema_df)
    entry  <- cinema_to_color(rating, palette)
    if (is.null(entry)) col_no_cinema else entry$bg
  }, character(1L), USE.NAMES = FALSE)

  # ---- 4. Merge with user-supplied arguments (user wins) ----
  dots <- list(...)
  if (!"col.square"       %in% names(dots)) dots$col.square       <- col_sq
  if (!"col.square.lines" %in% names(dots)) dots$col.square.lines <- col_sq
  if (!"col.study"        %in% names(dots)) dots$col.study        <- col_sq

  # ---- 5. Dispatch to forest.netmeta() ----
  call_args <- c(list(x = x, reference.group = ref), dots)
  invisible(do.call(meta::forest, call_args))
}
