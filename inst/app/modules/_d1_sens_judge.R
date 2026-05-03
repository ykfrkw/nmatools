# =============================================================================
# Domain 1 (Sensitivity-based) — direct-comparison judgment
# =============================================================================
# Sourced by inst/app/app.R before module_B_cinema.R, and also by
# tests/testthat/test-grade-rob-direct.R via testthat::test_path(). Kept as a
# standalone file (rather than inlined in module_B_cinema.R) so the pure
# function can be tested without loading shiny/DT/plotly etc.
#
# Mirrors pmatools::assess_rob's flowchart at the pairwise level:
#
#   "no"            — high-RoB weight share below dom_threshold, OR excluding
#                     high-RoB does not push effect favourably beyond
#                     inf_threshold
#   "some_concerns" — high-RoB studies inflate |TE| by more than inf_threshold
#                     (also returned conservatively when there is nothing to
#                     compare against, e.g. all studies are high-RoB or
#                     |te_low| ~= 0)
#   "serious"       — excluding high-RoB studies flips the sign of the pooled
#                     effect
#
# rob_vec : "low" / "some concerns" / "high" (case as nmatools normalises it)
# te_vec, se_vec : on the analysis scale (log scale for OR/RR, as netmeta
#                  stores internally)
# small_values = NULL uses |TE| comparison; "desirable" / "undesirable"
# constrains direction.
# =============================================================================
judge_rob_direct_sens <- function(rob_vec, te_vec, se_vec,
                                  dom_threshold = 0.60,
                                  inf_threshold = 0.10,
                                  small_values  = NULL) {
  is_high <- rob_vec == "high"
  if (!any(is_high))     return("no")
  if (all(is_high))      return("some_concerns")

  ok <- !is.na(te_vec) & !is.na(se_vec) & se_vec > 0
  if (!any(ok))          return("no")
  if (!any(ok & !is_high)) return("some_concerns")

  w <- numeric(length(te_vec))
  w[ok] <- 1 / (se_vec[ok]^2)
  prop_high <- sum(w[is_high & ok]) / sum(w[ok])
  if (prop_high < dom_threshold) return("no")

  te_all <- weighted.mean(te_vec[ok],            w[ok])
  te_low <- weighted.mean(te_vec[ok & !is_high], w[ok & !is_high])

  if (abs(te_low) < 1e-9) return("some_concerns")

  if (sign(te_all) != sign(te_low)) return("serious")

  inflation <- (abs(te_all) - abs(te_low)) / abs(te_low)
  direction_ok <- if (is.null(small_values)) {
    abs(te_all) > abs(te_low)
  } else if (identical(small_values, "undesirable")) {
    te_all > te_low
  } else {
    te_all < te_low
  }
  if (isTRUE(direction_ok) && inflation > inf_threshold) return("some_concerns")
  "no"
}
