# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Overview help topic (?bcnma) for the Bayesian component NMA functions that
# were merged into nmatools from the standalone bcnma package (0.2.0), and
# the re-export of meta::forest(). bcnma defined its own forest() generic;
# nmatools registers the bcnma_bin / bcnma_cont methods on meta::forest
# instead, and re-exports that generic so that forest(fit) keeps working
# after library(bcnma) is replaced by library(nmatools).

#' Bayesian component network meta-analysis (bcnma)
#'
#' Bayesian random-effects component NMA (cNMA) for binary and continuous
#' outcomes using JAGS, optionally with non-binary component values (doses)
#' and arm-level effect modifiers of specific components. Three interaction
#' models are available: additive, SSVS (spike-and-slab priors on every
#' estimable pair) and SSVS restricted to pairs of chosen active components.
#' These functions were developed as the standalone package bcnma
#' (github.com/ykfrkw/bcnma), which is superseded by nmatools; see NEWS.md
#' for the changes made during the methodological review (some results and
#' field names differ from bcnma).
#'
#' Fitting requires the JAGS system library and the suggested packages rjags
#' and MCMCvis. They are not needed for anything else in nmatools.
#'
#' @section Main functions:
#' \code{\link{bcnma_bin}} (binary outcomes), \code{\link{bcnma_cont}}
#' (continuous outcomes) - fit the model
#'
#' @section Output functions:
#' \code{\link{print.bcnma_bin}}, \code{\link{print.bcnma_cont}},
#' \code{\link{forest.bcnma_bin}}, \code{\link{forest.bcnma_cont}},
#' \code{\link{plot_interactions}}, \code{\link{summarise_interactions}},
#' \code{\link{compare_combinations}}, \code{\link{check_additivity}},
#' \code{\link{component_coverage}}, \code{\link{modifier_equivalence}},
#' \code{\link{bcnma_model_code}}
#'
#' @section Index helpers:
#' \code{\link{place}}, \code{\link{which.place}},
#' \code{\link{all.interactions}}
#'
#' @references
#' Furukawa Y, et al. (2024). Components and delivery formats of cognitive
#' behavioral therapy for chronic insomnia in adults: A systematic review and
#' component network meta-analysis. \emph{JAMA Psychiatry}, 81(4), 357-365.
#' \doi{10.1001/jamapsychiatry.2023.5060}
#'
#' Turner RM, et al. (2015). Predictive distributions for between-study
#' heterogeneity. \emph{Statistics in Medicine}, 34, 984-998.
#'
#' @name bcnma
#' @keywords internal
NULL

#' @importFrom meta forest
#' @export
meta::forest
