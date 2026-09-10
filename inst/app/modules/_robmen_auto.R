# =============================================================================
# _robmen_auto.R — pure ROB-MEN auto-judgement helpers
# =============================================================================
# Sourced by inst/app/app.R before module_C_robmen.R, and by
# tests/testthat/test-robmen-auto.R. No shiny / DT / plotly imports so the
# rules can be unit-tested without loading the full app.
#
# What these helpers automate (Chiocchia et al. 2021, 2023)
# ----------------------------------------------------------
# 1. Group classification (A / B / C) from two counts per comparison:
#      k_reported — studies contributing outcome data (auto-derived)
#      k_sr       — all studies identified in the systematic review for the
#                   comparison (auto-filled = k_reported, user-editable)
# 2. Component 1 (within-study selective non-reporting): the ROB-ME Step 2
#    Q1 answer follows from k_sr vs k_reported. When studies are missing
#    (k_sr > k_reported) a PROVISIONAL direction is proposed from the
#    bias-favour ordering (which treatment missing evidence would favour);
#    optionally, and only when the direct evidence is statistically
#    significant, from the observed effect.
# 3. Component 2 (across-study bias):
#      k >= 10  — Egger's test (p < 0.05) with the direction of the
#                 intercept, made outcome-direction aware via small_values.
#      k <  10  — the review-level qualitative conditions listed in the
#                 ROB-MEN paper, scored as (conditions suggesting bias) minus
#                 (conditions suggesting no bias); direction from the
#                 bias-favour ordering (robmen_direction()).
# 4. Small-study-effect direction (reinforcing / not reinforcing) relative to
#    the biased-contribution direction, again small_values-aware.
#
# Direction convention
# --------------------
# Every effect `te` passed to these helpers is "t1 versus t2" on the analysis
# scale (t1 minus t2; log scale for OR/RR/HR), matching netmeta's TE matrices
# (TE[t1, t2]) and the canonicalised pairwise rows used in module C.
# `small_values` says whether a LOWER outcome value is "desirable" (symptom
# scores, mortality) or "undesirable" (remission, response). The treatment a
# given effect favours therefore depends on both the sign of `te` and
# `small_values`; robmen_favoured_treatment() is the single place that rule
# lives.
# =============================================================================

ROBMEN_NO_BIAS      <- "No bias detected"
ROBMEN_SUSP_PREFIX  <- "Suspected bias favouring "

robmen_bias_label <- function(treatment) paste0(ROBMEN_SUSP_PREFIX, treatment)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

.robmen_num1 <- function(x, default = NA_real_) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0 || is.na(v[1]) || !is.finite(v[1])) default else v[1]
}

# ---------------------------------------------------------------------------
# robmen_benefit_sign: +1 when a POSITIVE te (t1 higher than t2) favours t1,
# -1 when a NEGATIVE te favours t1.
# ---------------------------------------------------------------------------
robmen_benefit_sign <- function(small_values = "desirable") {
  sv <- tolower(trimws(as.character(small_values %||% "desirable")[1]))
  if (is.na(sv) || !nzchar(sv)) sv <- "desirable"
  if (identical(sv, "undesirable")) 1 else -1
}

# ---------------------------------------------------------------------------
# robmen_favoured_treatment: which of t1 / t2 does the effect `te` favour?
# Returns NA_character_ when te is NA / zero, or (require_ci = TRUE) when the
# 95% CI [lo, hi] includes the null.
# ---------------------------------------------------------------------------
robmen_favoured_treatment <- function(te, t1, t2, small_values = "desirable",
                                      lo = NA_real_, hi = NA_real_,
                                      require_ci = FALSE) {
  te <- .robmen_num1(te)
  if (is.na(te) || abs(te) < 1e-12) return(NA_character_)
  if (isTRUE(require_ci)) {
    lo <- .robmen_num1(lo); hi <- .robmen_num1(hi)
    if (is.na(lo) || is.na(hi) || (lo <= 0 && hi >= 0)) return(NA_character_)
  }
  if (te * robmen_benefit_sign(small_values) > 0) as.character(t1)
  else                                             as.character(t2)
}

# ---------------------------------------------------------------------------
# Bias-favour ordering.
#
# The ROB-MEN paper's "novel agent" condition is one instance of a general
# expectation: if missing evidence favours anyone, it favours the newer /
# more actively promoted / more "hoped-for" treatment. Reviewers can encode
# that expectation once as an ordering of treatments from MOST to LEAST
# likely to be favoured by bias (a newer drug before an older one; for
# psychotherapies, whatever order expert judgement suggests). The ordering
# then decides every auto "favouring X" direction that is not measured
# (Component 1 with missing studies; the qualitative Component 2 rule).
# Treatments left out of the ordering fall back to the observed effect.
#
# robmen_normalize_bias_order() accepts
#   - a character vector, most favoured first, or
#   - a named numeric vector such as approval years (higher = newer = more
#     favoured), which is sorted decreasingly.
# ---------------------------------------------------------------------------
robmen_normalize_bias_order <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (is.numeric(x) && !is.null(names(x))) {
    x <- x[!is.na(x) & nzchar(names(x))]
    x <- names(x)[order(-as.numeric(x))]
  }
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  unique(trimws(x))
}

# The treatment among t1 / t2 that comes first in `bias_order`; NA when
# either is absent from the ordering.
robmen_favoured_by_order <- function(t1, t2, bias_order = NULL) {
  ord <- robmen_normalize_bias_order(bias_order)
  if (length(ord) == 0) return(NA_character_)
  p1 <- match(as.character(t1), ord)
  p2 <- match(as.character(t2), ord)
  if (is.na(p1) || is.na(p2) || p1 == p2) return(NA_character_)
  if (p1 < p2) as.character(t1) else as.character(t2)
}

# One resolver for every unmeasured direction.
#   1. bias_order — both treatments ranked -> the higher-ranked one.
#   2. (only when effect_fallback = TRUE) the observed effect, and only when
#      its 95% CI [lo, hi] excludes the null: the mechanism behind both
#      selective non-reporting and publication bias is "non-significant
#      results go missing", so the reported evidence is exaggerated in its
#      own direction ONLY when it is clearly directional. A point estimate
#      whose CI covers the null carries no such information.
#   3. otherwise NA — the reviewer decides.
# Novel agents are deliberately NOT a direction source: they belong at the
# top of the ordering (robmen_order_conflicts() flags an ordering that
# contradicts the novel-agent list).
# Returns list(favoured, source) with source in "order" / "effect" / NA.
robmen_direction <- function(te, t1, t2, small_values = "desirable",
                             bias_order = NULL,
                             lo = NA_real_, hi = NA_real_,
                             effect_fallback = FALSE) {
  fav <- robmen_favoured_by_order(t1, t2, bias_order)
  if (!is.na(fav)) return(list(favoured = fav, source = "order"))
  if (isTRUE(effect_fallback)) {
    fav <- robmen_favoured_treatment(te, t1, t2, small_values,
                                     lo = lo, hi = hi, require_ci = TRUE)
    if (!is.na(fav)) return(list(favoured = fav, source = "effect"))
  }
  list(favoured = NA_character_, source = NA_character_)
}

.robmen_source_txt <- function(source, fav) {
  switch(as.character(source),
    order  = paste0("direction from the bias-favour ordering (", fav, " ranked higher)"),
    effect = paste0("direction proposed from the observed effect, which",
                    " significantly favours ", fav),
    "no direction available")
}

.robmen_no_direction_txt <- function(effect_fallback) {
  if (isTRUE(effect_fallback))
    "no direction available (treatments not both ranked and the observed effect is not significant)"
  else
    "no direction available (treatments not both in the bias-favour ordering)"
}

# Novel agents that the ordering ranks BELOW some other treatment: the two
# inputs then contradict each other (a novel agent is, by definition, the
# treatment bias would favour). Returns the offending novel agents.
robmen_order_conflicts <- function(bias_order, novel_agents) {
  ord <- robmen_normalize_bias_order(bias_order)
  nov <- unique(as.character(novel_agents %||% character(0)))
  nov <- nov[!is.na(nov) & nzchar(nov)]
  if (length(ord) < 2 || length(nov) == 0) return(character(0))
  pos_nov <- match(nov, ord)
  others  <- setdiff(ord, nov)
  if (length(others) == 0) return(character(0))
  top_other <- min(match(others, ord))
  nov[!is.na(pos_nov) & pos_nov > top_other]
}

# ---------------------------------------------------------------------------
# robmen_group_auto: A (observed for this outcome), B (observed for other
# outcomes only), C (unobserved). Vectorised over k_reported / k_sr.
# ---------------------------------------------------------------------------
robmen_group_auto <- function(k_reported, k_sr = NA) {
  k_reported <- suppressWarnings(as.numeric(k_reported))
  k_sr       <- suppressWarnings(as.numeric(k_sr))
  n <- max(length(k_reported), length(k_sr))
  k_reported <- rep_len(k_reported, n)
  k_sr       <- rep_len(k_sr, n)
  k_reported[is.na(k_reported)] <- 0
  ifelse(k_reported > 0, "A",
         ifelse(!is.na(k_sr) & k_sr > 0, "B", "C"))
}

# ---------------------------------------------------------------------------
# robmen_within_auto: Component 1 (within-study selective non-reporting).
#
#   k_sr <= k_reported (or NA)  -> Q1 = "no"  -> "No bias detected"
#   k_sr >  k_reported          -> Q1 = "yes" -> provisional
#        "Suspected bias favouring X" with X from robmen_direction();
#        when no direction is available -> "No bias detected", flagged
#        provisional so the reviewer sets Q2.
#
# te / lo / hi: the pooled DIRECT estimate of the comparison (Group A).
# effect_fallback: allow the observed effect as a direction source (see
#   robmen_direction). Callers pass FALSE for Group B, where there is no
#   direct evidence and the NMA estimate would be too weak a proxy.
# Returns a list:
#   rating, q1 ("no"/"yes"), favoured, source, provisional (logical),
#   n_missing, note
# ---------------------------------------------------------------------------
robmen_within_auto <- function(k_reported, k_sr, te = NA_real_,
                               t1 = "t1", t2 = "t2",
                               small_values = "desirable",
                               bias_order = NULL,
                               lo = NA_real_, hi = NA_real_,
                               effect_fallback = FALSE) {
  k_reported <- .robmen_num1(k_reported, default = 0)
  k_sr       <- .robmen_num1(k_sr, default = NA_real_)
  if (is.na(k_sr)) k_sr <- k_reported
  n_missing <- max(0, k_sr - k_reported)

  if (n_missing <= 0) {
    return(list(rating = ROBMEN_NO_BIAS, q1 = "no", favoured = NA_character_,
                source = NA_character_, provisional = FALSE, n_missing = 0,
                note = "All studies identified in the SR report this outcome (Q1 = No)."))
  }

  miss_txt <- paste0(n_missing, " SR stud", if (n_missing == 1) "y" else "ies",
                     " did not report this outcome (Q1 = Yes)")
  d <- robmen_direction(te, t1, t2, small_values, bias_order,
                        lo = lo, hi = hi, effect_fallback = effect_fallback)
  if (is.na(d$favoured)) {
    return(list(rating = ROBMEN_NO_BIAS, q1 = "yes", favoured = NA_character_,
                source = NA_character_, provisional = TRUE, n_missing = n_missing,
                note = paste0(miss_txt, "; ", .robmen_no_direction_txt(effect_fallback),
                              ". Set Q2 manually or add the bias-favour ordering.")))
  }
  list(rating = robmen_bias_label(d$favoured), q1 = "yes", favoured = d$favoured,
       source = d$source, provisional = TRUE, n_missing = n_missing,
       note = paste0(miss_txt, "; ", .robmen_source_txt(d$source, d$favoured),
                     ". Confirm with ROB-ME."))
}

# ---------------------------------------------------------------------------
# robmen_egger_auto: Component 2 for k >= 10 from Egger's test.
#   p >= alpha (or NA) -> "No bias detected"
#   p <  alpha         -> suspected bias favouring the treatment the
#                         small-study asymmetry (intercept sign) favours.
# The Egger intercept is on the same t1-vs-t2 scale as `te`: a negative
# intercept means small studies report LOWER values for t1.
# ---------------------------------------------------------------------------
robmen_egger_auto <- function(p, intercept, t1, t2,
                              small_values = "desirable", alpha = 0.05) {
  p <- .robmen_num1(p)
  if (is.na(p) || p >= alpha) return(ROBMEN_NO_BIAS)
  fav <- robmen_favoured_treatment(intercept, t1, t2, small_values)
  if (is.na(fav)) return(ROBMEN_NO_BIAS)
  robmen_bias_label(fav)
}

# ---------------------------------------------------------------------------
# robmen_across_qual_auto: Component 2 for k < 10 / Group C rows from the
# review-level conditions (Chiocchia 2021, Table 2).
#
# conditions: list with logical elements
#   no_grey_lit       — grey literature / unpublished studies NOT searched
#   prior_pub_bias    — previous evidence of publication bias in this field
#   registration      — tradition of prospective trial registration
#   unpub_consistent  — unpublished studies available and consistent
# novel_agents: treatments supported only by a few early trials; a
#   comparison involving one of them carries a bias-suggesting condition.
#   (Direction is NOT taken from this list — put novel agents at the top of
#   bias_order instead.)
# bias_order: treatments ordered from most to least favoured by bias.
# te / lo / hi + effect_fallback: see robmen_direction(); callers pass
#   effect_fallback = FALSE for Group C (no direct evidence).
#
# score = (#conditions suggesting bias) - (#conditions suggesting no bias)
#   score <= 0 -> "No bias detected"
#   score >  0 -> suspected bias favouring the treatment robmen_direction()
#                 returns; no direction -> "No bias detected" flagged
#                 provisional.
# ---------------------------------------------------------------------------
robmen_across_qual_auto <- function(te = NA_real_, t1 = "t1", t2 = "t2",
                                    small_values = "desirable",
                                    conditions = list(),
                                    novel_agents = character(0),
                                    bias_order = NULL,
                                    lo = NA_real_, hi = NA_real_,
                                    effect_fallback = FALSE) {
  flag <- function(nm) isTRUE(conditions[[nm]])
  novel_in <- intersect(as.character(novel_agents %||% character(0)),
                        c(as.character(t1), as.character(t2)))
  bias_pts   <- flag("no_grey_lit") + flag("prior_pub_bias") +
                (length(novel_in) >= 1)
  nobias_pts <- flag("registration") + flag("unpub_consistent")
  score <- bias_pts - nobias_pts

  reasons <- c(
    if (flag("no_grey_lit"))    "grey literature not searched",
    if (flag("prior_pub_bias")) "prior evidence of publication bias",
    if (length(novel_in) >= 1)  paste0("novel agent: ", paste(novel_in, collapse = ", ")),
    if (flag("registration"))   "prospective registration tradition (-)",
    if (flag("unpub_consistent")) "unpublished studies consistent (-)"
  )
  reason_txt <- if (length(reasons)) paste(reasons, collapse = "; ") else
    "no review-level conditions flagged"

  if (score <= 0) {
    return(list(rating = ROBMEN_NO_BIAS, favoured = NA_character_,
                source = NA_character_, provisional = FALSE, score = score,
                note = paste0("Qualitative auto: ", reason_txt, ".")))
  }

  d <- robmen_direction(te, t1, t2, small_values, bias_order,
                        lo = lo, hi = hi, effect_fallback = effect_fallback)
  if (is.na(d$favoured)) {
    return(list(rating = ROBMEN_NO_BIAS, favoured = NA_character_,
                source = NA_character_, provisional = TRUE, score = score,
                note = paste0("Qualitative auto: ", reason_txt,
                              " -> bias suspected but ",
                              .robmen_no_direction_txt(effect_fallback),
                              "; set manually or add the bias-favour ordering.")))
  }
  list(rating = robmen_bias_label(d$favoured), favoured = d$favoured,
       source = d$source, provisional = TRUE, score = score,
       note = paste0("Qualitative auto (provisional): ", reason_txt,
                     " -> ", .robmen_source_txt(d$source, d$favoured), "."))
}

# ---------------------------------------------------------------------------
# robmen_sse_auto: ⑤b small-study effects from the NMA vs NMR estimates.
#
#   nma_* — unadjusted NMA estimate + 95% CI (t1 vs t2)
#   nmr_* — NMR estimate extrapolated to the smallest observed variance
#   bias_favoured — treatment favoured by the biased contribution (④):
#                   NA when the contribution is balanced / absent
#
#   CIs overlap (or anything NA)       -> "No evidence of small-study effects"
#   otherwise the small-study component (nma_te - nmr_te) favours a
#   treatment; if it is the same treatment as `bias_favoured` -> reinforcing,
#   else not reinforcing. With no biased direction to compare against, fall
#   back to "shrinks toward the null" = reinforcing (legacy heuristic).
# ---------------------------------------------------------------------------
robmen_sse_auto <- function(nma_te, nma_lo, nma_hi,
                            nmr_te, nmr_lo, nmr_hi,
                            t1, t2, bias_favoured = NA_character_,
                            small_values = "desirable") {
  SSE_NONE <- "No evidence of small-study effects"
  SSE_IN   <- "Evidence of small-study effects \u2013 reinforcing biased contribution"
  SSE_NOT  <- "Evidence of small-study effects \u2013 not reinforcing biased contribution"

  vals <- vapply(list(nma_te, nma_lo, nma_hi, nmr_te, nmr_lo, nmr_hi),
                 .robmen_num1, numeric(1))
  if (anyNA(vals)) return(SSE_NONE)
  nma_te <- vals[1]; nma_lo <- vals[2]; nma_hi <- vals[3]
  nmr_te <- vals[4]; nmr_lo <- vals[5]; nmr_hi <- vals[6]

  overlaps <- nmr_hi >= nma_lo && nma_hi >= nmr_lo
  if (overlaps) return(SSE_NONE)

  sse_component <- nma_te - nmr_te
  sse_fav <- robmen_favoured_treatment(sse_component, t1, t2, small_values)

  if (!is.na(bias_favoured) && nzchar(bias_favoured)) {
    if (!is.na(sse_fav) && identical(sse_fav, as.character(bias_favoured)))
      return(SSE_IN)
    return(SSE_NOT)
  }
  # No biased direction: legacy shrinkage heuristic
  if (abs(nmr_te) < abs(nma_te) && sign(nmr_te) == sign(nma_te)) SSE_IN else SSE_NOT
}

# ---------------------------------------------------------------------------
# robmen_sr_reference: per-comparison SR totals from the data sheet.
#
# `sr_pairs` is the skeleton built by Module A (build_sr_pairs): one row per
# study x treatment pair present in the sheet, with `reported` = both arms
# carry outcome data. Aggregates to the canonical comparison key
# ("t1:t2", t1 < t2):
#   k_sr     — studies in the sheet that include the pair
#   n_sr     — total participants across those studies (NA when unknown)
#   k_rep    — studies with outcome data for the pair
#   missing  — labels of the studies without outcome data (";"-separated)
# Returns NULL when the sheet carries no unreported pair, so callers fall
# back to the "k_sr = k reporting" default and the user-editable cell.
# ---------------------------------------------------------------------------
robmen_sr_reference <- function(sr_pairs) {
  if (is.null(sr_pairs) || !is.data.frame(sr_pairs) || nrow(sr_pairs) == 0)
    return(NULL)
  need <- c("studlab", "t1", "t2", "reported")
  if (!all(need %in% names(sr_pairs))) return(NULL)
  rep_flag <- isTRUE_vec(sr_pairs$reported)
  if (all(rep_flag)) return(NULL)

  t1c <- pmin(as.character(sr_pairs$t1), as.character(sr_pairs$t2))
  t2c <- pmax(as.character(sr_pairs$t1), as.character(sr_pairs$t2))
  key <- paste(t1c, t2c, sep = ":")
  n1  <- if ("n1" %in% names(sr_pairs)) suppressWarnings(as.numeric(sr_pairs$n1)) else NA_real_
  n2  <- if ("n2" %in% names(sr_pairs)) suppressWarnings(as.numeric(sr_pairs$n2)) else NA_real_
  n_pair <- n1 + n2
  studlab <- as.character(sr_pairs$studlab)

  keys <- sort(unique(key))
  out <- lapply(keys, function(k) {
    idx  <- which(key == k)
    # one entry per study (a study lists each pair once, but be safe)
    st   <- studlab[idx]
    first <- !duplicated(st)
    idx  <- idx[first]; st <- st[first]
    n_tot <- if (all(is.na(n_pair[idx]))) NA_real_ else sum(n_pair[idx], na.rm = TRUE)
    miss <- st[!rep_flag[idx]]
    data.frame(comp_key = k,
               t1 = t1c[idx[1]], t2 = t2c[idx[1]],
               k_sr = length(idx),
               n_sr = n_tot,
               k_rep = sum(rep_flag[idx]),
               missing = paste(sort(miss), collapse = "; "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# robmen_auto_summary: one-line status counts for the pairwise table.
# `rows` is a data.frame with columns comp_key, grp, within_rating,
# within_provisional, across_rating, across_provisional, across_source
# ("egger" / "qual" / "").
# ---------------------------------------------------------------------------
robmen_auto_summary <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(list(n = 0L, n_a = 0L, n_b = 0L, n_c = 0L,
                n_within_auto = 0L, n_within_prov = 0L,
                n_across_egger = 0L, n_across_qual = 0L, n_across_prov = 0L,
                prov_keys = character(0)))
  }
  wp <- isTRUE_vec(rows$within_provisional)
  ap <- isTRUE_vec(rows$across_provisional)
  list(
    n              = nrow(rows),
    n_a            = sum(rows$grp == "A"),
    n_b            = sum(rows$grp == "B"),
    n_c            = sum(rows$grp == "C"),
    n_within_auto  = sum(rows$grp %in% c("A", "B") & nzchar(rows$within_rating) & !wp),
    n_within_prov  = sum(wp),
    n_across_egger = sum(rows$across_source == "egger"),
    n_across_qual  = sum(rows$across_source == "qual"),
    n_across_prov  = sum(ap),
    prov_keys      = rows$comp_key[wp | ap]
  )
}

isTRUE_vec <- function(x) {
  x <- as.logical(x)
  x[is.na(x)] <- FALSE
  x
}
