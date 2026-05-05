# =============================================================================
# Domain 1 (Sensitivity-based) — direct-comparison judgment
# =============================================================================
# Sourced by inst/app/app.R before module_B_cinema.R, and also by
# tests/testthat/test-grade-rob-direct.R via testthat::test_path(). Kept as a
# standalone file (rather than inlined in module_B_cinema.R) so the pure
# function can be tested without loading shiny/DT/plotly etc.
#
# Mirrors pmatools::assess_rob's direction-and-magnitude check at the
# pairwise level (without the dominance gate — every comparison with at
# least one high-RoB study is checked):
#
#   "no"            — excluding high-RoB does not push effect favourably
#                     beyond inf_threshold
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
judge_rob_direct_sens_v <- function(rob_vec, te_vec, se_vec,
                                    inf_threshold = 0.10,
                                    small_values  = NULL) {
  empty <- list(judgement = NA_character_,
                te_all = NA_real_, te_low = NA_real_,
                se_all = NA_real_, se_low = NA_real_,
                inflation = NA_real_, sign_flip = FALSE,
                sig_changed = FALSE, overlap_ratio = NA_real_)

  ok <- !is.na(te_vec) & !is.na(se_vec) & se_vec > 0
  if (!any(ok)) return(modifyList(empty, list(judgement = "no")))

  w <- numeric(length(te_vec))
  w[ok] <- 1 / (se_vec[ok]^2)

  # TE_all is the pooled estimate over ALL studies (high-RoB included).
  te_all <- weighted.mean(te_vec[ok], w[ok])
  se_all <- sqrt(1 / sum(w[ok]))

  is_high <- rob_vec == "high"

  # No high-RoB studies → TE_excl identical to TE_all
  if (!any(is_high)) {
    return(list(judgement = "no",
                te_all = te_all, te_low = te_all,
                se_all = se_all, se_low = se_all,
                inflation = 0, sign_flip = FALSE,
                sig_changed = FALSE, overlap_ratio = 1))
  }

  # All studies high-RoB → cannot compute TE_excl
  if (all(is_high) || !any(ok & !is_high)) {
    return(list(judgement = "some_concerns",
                te_all = te_all, te_low = NA_real_,
                se_all = se_all, se_low = NA_real_,
                inflation = NA_real_, sign_flip = FALSE,
                sig_changed = FALSE, overlap_ratio = NA_real_))
  }

  te_low <- weighted.mean(te_vec[ok & !is_high], w[ok & !is_high])
  se_low <- sqrt(1 / sum(w[ok & !is_high]))

  # ---------------- Overlap-aware judgement ----------------
  all_lo <- te_all - 1.96 * se_all; all_hi <- te_all + 1.96 * se_all
  low_lo <- te_low - 1.96 * se_low; low_hi <- te_low + 1.96 * se_low

  # Sign flip: high-RoB studies reverse the direction of effect
  sign_flip <- !isTRUE(all.equal(te_all, 0)) &&
               !isTRUE(all.equal(te_low, 0)) &&
               sign(te_all) != sign(te_low)

  # Significance change: CI crosses null (0) in one but not the other
  sig_all <- sign(all_lo) == sign(all_hi) && all_lo != 0 && all_hi != 0
  sig_low <- sign(low_lo) == sign(low_hi) && low_lo != 0 && low_hi != 0
  sig_changed <- sig_all != sig_low

  # CI overlap ratio: (overlap length) / (mean CI width).
  # 1 = identical CIs, 0 = no overlap. Used to decide whether inflation
  # actually changes the conclusion or is absorbed by uncertainty.
  ovlp_len   <- max(0, min(all_hi, low_hi) - max(all_lo, low_lo))
  mean_width <- mean(c(all_hi - all_lo, low_hi - low_lo))
  overlap_ratio <- if (mean_width > 0) min(1, ovlp_len / mean_width) else 0

  inflation <- if (abs(te_low) < 1e-9) NA_real_
               else (abs(te_all) - abs(te_low)) / abs(te_low)

  judgement <- if (sign_flip)                                   "serious"
               else if (!is.na(overlap_ratio) &&
                        overlap_ratio >= 0.8)                    "no"
               else if (sig_changed ||
                        (!is.na(inflation) &&
                         inflation > inf_threshold))             "some_concerns"
               else                                              "no"

  list(judgement = judgement,
       te_all = te_all, te_low = te_low,
       se_all = se_all, se_low = se_low,
       inflation = inflation, sign_flip = sign_flip,
       sig_changed = sig_changed, overlap_ratio = overlap_ratio)
}

# Thin wrapper for backwards compatibility — returns only the judgement string.
judge_rob_direct_sens <- function(rob_vec, te_vec, se_vec,
                                  inf_threshold = 0.10,
                                  small_values  = NULL) {
  judge_rob_direct_sens_v(rob_vec, te_vec, se_vec,
                          inf_threshold = inf_threshold,
                          small_values  = small_values)$judgement
}

# =============================================================================
# Domain 1 (Sensitivity-based, split-evidence p-value) — alternative direct
# comparison judgement following the D6 Incoherence framework.
# =============================================================================
# Pools low+some studies and high studies separately (fixed-effect within each
# subset), then runs a two-sample Z-test on the difference of pooled effects.
# Same p-cutoffs as D6:
#   p > p_some                   -> "no"
#   p_serious < p <= p_some      -> "some_concerns"
#   p <= p_serious               -> "serious"
# Sign flip between the two pools always overrides to "serious", matching the
# overlap-aware variant. The high-RoB-only edge case (no comparator pool)
# returns "serious" — this is the user's "-2" downgrade case: we cannot judge
# whether high-RoB studies bias the conclusion when there is nothing to
# compare them against.
#
# Fixed-effect pooling within each subset is intentional: with k=2 studies the
# subset's tau^2 cannot be reliably estimated, so a random-effects pool would
# inflate SE_subset and let p-values drift toward 1.0 and false-negative.
# =============================================================================
judge_rob_direct_split_p_v <- function(rob_vec, te_vec, se_vec,
                                       p_some    = 0.10,
                                       p_serious = 0.05) {
  empty <- list(judgement = NA_character_,
                te_ls = NA_real_, te_h = NA_real_,
                se_ls = NA_real_, se_h = NA_real_,
                k_ls = 0L, k_h = 0L,
                p_value = NA_real_, sign_flip = FALSE)

  ok <- !is.na(te_vec) & !is.na(se_vec) & se_vec > 0
  if (!any(ok)) return(modifyList(empty, list(judgement = "no")))

  ls_idx <- ok & rob_vec %in% c("low", "some concerns")
  h_idx  <- ok & rob_vec == "high"

  k_ls <- sum(ls_idx); k_h <- sum(h_idx)

  # No high-RoB studies -> nothing to test against. Same convention as the
  # overlap-aware variant.
  if (k_h == 0) {
    if (k_ls == 0) return(modifyList(empty, list(judgement = "no")))
    w_ls <- 1 / (se_vec[ls_idx]^2)
    te_ls <- weighted.mean(te_vec[ls_idx], w_ls)
    se_ls <- sqrt(1 / sum(w_ls))
    return(list(judgement = "no",
                te_ls = te_ls, te_h = te_ls,
                se_ls = se_ls, se_h = se_ls,
                k_ls = k_ls, k_h = 0L,
                p_value = 1, sign_flip = FALSE))
  }

  # High-RoB-only: cannot test, return "serious" (-2 downgrade).
  if (k_ls == 0) {
    w_h <- 1 / (se_vec[h_idx]^2)
    te_h <- weighted.mean(te_vec[h_idx], w_h)
    se_h <- sqrt(1 / sum(w_h))
    return(list(judgement = "serious",
                te_ls = NA_real_, te_h = te_h,
                se_ls = NA_real_, se_h = se_h,
                k_ls = 0L, k_h = k_h,
                p_value = NA_real_, sign_flip = FALSE))
  }

  # Both subsets non-empty: pool each side, test difference.
  w_ls <- 1 / (se_vec[ls_idx]^2)
  w_h  <- 1 / (se_vec[h_idx]^2)
  te_ls <- weighted.mean(te_vec[ls_idx], w_ls)
  te_h  <- weighted.mean(te_vec[h_idx],  w_h)
  se_ls <- sqrt(1 / sum(w_ls))
  se_h  <- sqrt(1 / sum(w_h))

  delta   <- te_ls - te_h
  se_diff <- sqrt(se_ls^2 + se_h^2)
  z       <- if (se_diff > 0) delta / se_diff else 0
  p_value <- 2 * (1 - pnorm(abs(z)))

  sign_flip <- !isTRUE(all.equal(te_ls, 0)) &&
               !isTRUE(all.equal(te_h, 0)) &&
               sign(te_ls) != sign(te_h)

  judgement <- if (sign_flip)                       "serious"
               else if (p_value <= p_serious)       "serious"
               else if (p_value <= p_some)          "some_concerns"
               else                                 "no"

  list(judgement = judgement,
       te_ls = te_ls, te_h = te_h,
       se_ls = se_ls, se_h = se_h,
       k_ls = k_ls, k_h = k_h,
       p_value = p_value, sign_flip = sign_flip)
}

# Thin wrapper — returns only the judgement string.
judge_rob_direct_split_p <- function(rob_vec, te_vec, se_vec,
                                     p_some    = 0.10,
                                     p_serious = 0.05) {
  judge_rob_direct_split_p_v(rob_vec, te_vec, se_vec,
                             p_some = p_some,
                             p_serious = p_serious)$judgement
}
