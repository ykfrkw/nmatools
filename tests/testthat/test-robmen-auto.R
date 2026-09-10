# Tests for the pure ROB-MEN auto-judgement rules in
# inst/app/modules/_robmen_auto.R (Group A/B/C classification, ROB-ME Q1
# from SR counts, Egger / qualitative across-study rules, SSE direction).

try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

helper_path <- system.file("app", "modules", "_robmen_auto.R",
                           package = "nmatools")
if (!nzchar(helper_path) || !file.exists(helper_path)) {
  helper_path <- testthat::test_path("..", "..", "inst", "app",
                                     "modules", "_robmen_auto.R")
}
source(helper_path, local = TRUE)

NO_BIAS <- "No bias detected"
SSE_NONE <- "No evidence of small-study effects"
SSE_IN   <- "Evidence of small-study effects \u2013 reinforcing biased contribution"
SSE_NOT  <- "Evidence of small-study effects \u2013 not reinforcing biased contribution"

# ---------------------------------------------------------------------------
# Direction convention
# ---------------------------------------------------------------------------
test_that("favoured treatment depends on sign and on small_values", {
  # te = A - B < 0: A has lower values
  expect_equal(robmen_favoured_treatment(-0.3, "A", "B", "desirable"),   "A")
  expect_equal(robmen_favoured_treatment(-0.3, "A", "B", "undesirable"), "B")
  expect_equal(robmen_favoured_treatment( 0.3, "A", "B", "desirable"),   "B")
  expect_equal(robmen_favoured_treatment( 0.3, "A", "B", "undesirable"), "A")
  expect_true(is.na(robmen_favoured_treatment(0,  "A", "B")))
  expect_true(is.na(robmen_favoured_treatment(NA, "A", "B")))
  # NULL / unknown small_values fall back to "desirable"
  expect_equal(robmen_favoured_treatment(-0.3, "A", "B", NULL), "A")
})

test_that("require_ci suppresses the direction when the CI includes the null", {
  expect_true(is.na(robmen_favoured_treatment(-0.3, "A", "B", "desirable",
                                              lo = -0.8, hi = 0.2,
                                              require_ci = TRUE)))
  expect_equal(robmen_favoured_treatment(-0.3, "A", "B", "desirable",
                                         lo = -0.5, hi = -0.1,
                                         require_ci = TRUE), "A")
})

# ---------------------------------------------------------------------------
# Group classification
# ---------------------------------------------------------------------------
test_that("groups follow the reporting and SR counts", {
  expect_equal(robmen_group_auto(3, 3), "A")
  expect_equal(robmen_group_auto(3, NA), "A")
  expect_equal(robmen_group_auto(0, 2), "B")
  expect_equal(robmen_group_auto(0, 0), "C")
  expect_equal(robmen_group_auto(0, NA), "C")
  expect_equal(robmen_group_auto(NA, NA), "C")
  expect_equal(robmen_group_auto(c(2, 0, 0), c(2, 1, NA)), c("A", "B", "C"))
})

# ---------------------------------------------------------------------------
# Component 1 — within-study (ROB-ME Q1 from k_sr vs k)
# ---------------------------------------------------------------------------
test_that("within-study auto is No bias when the SR count equals k", {
  r <- robmen_within_auto(5, 5, te = -0.4, t1 = "A", t2 = "B")
  expect_equal(r$rating, NO_BIAS)
  expect_equal(r$q1, "no")
  expect_false(r$provisional)
  # NA SR count behaves like "equal to k"
  r2 <- robmen_within_auto(5, NA, te = -0.4, t1 = "A", t2 = "B")
  expect_equal(r2$rating, NO_BIAS)
  expect_false(r2$provisional)
  # SR count below k (data entry slip) is not treated as missing studies
  r3 <- robmen_within_auto(5, 3, te = -0.4, t1 = "A", t2 = "B")
  expect_equal(r3$rating, NO_BIAS)
})

test_that("within-study auto proposes a provisional direction when studies are missing", {
  r <- robmen_within_auto(5, 7, te = -0.4, t1 = "A", t2 = "B",
                          small_values = "desirable")
  expect_equal(r$rating, "Suspected bias favouring A")
  expect_equal(r$q1, "yes")
  expect_true(r$provisional)
  expect_equal(r$n_missing, 2)
  expect_match(r$note, "2 SR studies")

  r_u <- robmen_within_auto(5, 7, te = -0.4, t1 = "A", t2 = "B",
                            small_values = "undesirable")
  expect_equal(r_u$rating, "Suspected bias favouring B")

  # Group B: k = 0, direction from the NMA estimate
  r_b <- robmen_within_auto(0, 1, te = 0.2, t1 = "A", t2 = "B",
                            small_values = "desirable")
  expect_equal(r_b$rating, "Suspected bias favouring B")
  expect_equal(r_b$n_missing, 1)
  expect_match(r_b$note, "1 SR study ")
})

test_that("within-study auto stays No bias but flagged when no direction is readable", {
  r <- robmen_within_auto(5, 6, te = NA, t1 = "A", t2 = "B")
  expect_equal(r$rating, NO_BIAS)
  expect_equal(r$q1, "yes")
  expect_true(r$provisional)
  expect_match(r$note, "set Q2 manually")
})

# ---------------------------------------------------------------------------
# Component 2 — Egger (k >= 10)
# ---------------------------------------------------------------------------
test_that("Egger auto uses p < 0.05 and an outcome-aware intercept direction", {
  expect_equal(robmen_egger_auto(0.20, -1.5, "A", "B", "desirable"), NO_BIAS)
  expect_equal(robmen_egger_auto(NA,   -1.5, "A", "B", "desirable"), NO_BIAS)
  # negative intercept: small studies report lower values for A
  expect_equal(robmen_egger_auto(0.01, -1.5, "A", "B", "desirable"),
               "Suspected bias favouring A")
  expect_equal(robmen_egger_auto(0.01, -1.5, "A", "B", "undesirable"),
               "Suspected bias favouring B")
  expect_equal(robmen_egger_auto(0.01,  1.5, "A", "B", "desirable"),
               "Suspected bias favouring B")
  # p significant but intercept NA / 0: no direction -> No bias
  expect_equal(robmen_egger_auto(0.01, NA, "A", "B"), NO_BIAS)
  expect_equal(robmen_egger_auto(0.01, 0,  "A", "B"), NO_BIAS)
  # custom alpha
  expect_equal(robmen_egger_auto(0.08, -1, "A", "B", alpha = 0.10),
               "Suspected bias favouring A")
})

# ---------------------------------------------------------------------------
# Component 2 — qualitative rule (k < 10 / Group C)
# ---------------------------------------------------------------------------
test_that("qualitative auto is No bias when nothing is flagged", {
  r <- robmen_across_qual_auto(-0.3, "A", "B", conditions = list())
  expect_equal(r$rating, NO_BIAS)
  expect_false(r$provisional)
  expect_equal(r$score, 0)
})

test_that("qualitative auto scores bias vs no-bias conditions", {
  cond_bias <- list(no_grey_lit = TRUE)
  r <- robmen_across_qual_auto(-0.3, "A", "B", "desirable", conditions = cond_bias)
  expect_equal(r$rating, "Suspected bias favouring A")
  expect_true(r$provisional)
  expect_equal(r$score, 1)

  # one bias condition offset by one no-bias condition -> No bias
  cond_tie <- list(no_grey_lit = TRUE, registration = TRUE)
  expect_equal(robmen_across_qual_auto(-0.3, "A", "B", conditions = cond_tie)$rating,
               NO_BIAS)

  # two bias conditions vs one no-bias -> suspected
  cond_2 <- list(no_grey_lit = TRUE, prior_pub_bias = TRUE, registration = TRUE)
  expect_equal(robmen_across_qual_auto(-0.3, "A", "B", conditions = cond_2)$rating,
               "Suspected bias favouring A")
  expect_equal(robmen_across_qual_auto(-0.3, "A", "B", "undesirable",
                                       conditions = cond_2)$rating,
               "Suspected bias favouring B")
})

test_that("a novel agent in the comparison counts as a bias condition and sets the direction", {
  # observed effect favours A, but B is the novel agent -> favouring B
  r <- robmen_across_qual_auto(-0.3, "A", "B", "desirable",
                               conditions = list(), novel_agents = "B")
  expect_equal(r$rating, "Suspected bias favouring B")
  expect_match(r$note, "novel agent: B")
  # both treatments novel: direction from the observed effect
  r2 <- robmen_across_qual_auto(-0.3, "A", "B", "desirable",
                                conditions = list(), novel_agents = c("A", "B"))
  expect_equal(r2$rating, "Suspected bias favouring A")
  # novel agent not in this comparison: no effect
  r3 <- robmen_across_qual_auto(-0.3, "A", "B", "desirable",
                                conditions = list(), novel_agents = "C")
  expect_equal(r3$rating, NO_BIAS)
})

test_that("qualitative auto with bias but no readable direction is flagged", {
  r <- robmen_across_qual_auto(NA, "A", "B",
                               conditions = list(prior_pub_bias = TRUE))
  expect_equal(r$rating, NO_BIAS)
  expect_true(r$provisional)
  expect_match(r$note, "set manually")
})

# ---------------------------------------------------------------------------
# Small-study effects direction
# ---------------------------------------------------------------------------
test_that("SSE auto is No evidence when CIs overlap or anything is missing", {
  expect_equal(robmen_sse_auto(-0.5, -0.8, -0.2, -0.3, -0.6, 0.0, "A", "B", "A"),
               SSE_NONE)
  expect_equal(robmen_sse_auto(-0.5, -0.8, -0.2, NA, NA, NA, "A", "B", "A"),
               SSE_NONE)
})

test_that("SSE auto compares the small-study component with the biased direction", {
  # unadjusted NMA -0.6 (favours A, desirable); adjusted -0.1 -> small-study
  # component -0.5 favours A. Bias favouring A -> reinforcing.
  expect_equal(robmen_sse_auto(-0.6, -0.8, -0.4, -0.1, -0.3, 0.1,
                               "A", "B", bias_favoured = "A",
                               small_values = "desirable"), SSE_IN)
  expect_equal(robmen_sse_auto(-0.6, -0.8, -0.4, -0.1, -0.3, 0.1,
                               "A", "B", bias_favoured = "B",
                               small_values = "desirable"), SSE_NOT)
  # Same numbers, undesirable outcome: the component now favours B
  expect_equal(robmen_sse_auto(-0.6, -0.8, -0.4, -0.1, -0.3, 0.1,
                               "A", "B", bias_favoured = "A",
                               small_values = "undesirable"), SSE_NOT)
  expect_equal(robmen_sse_auto(-0.6, -0.8, -0.4, -0.1, -0.3, 0.1,
                               "A", "B", bias_favoured = "B",
                               small_values = "undesirable"), SSE_IN)
})

test_that("SSE auto without a biased direction falls back to shrinkage", {
  # adjusted shrinks toward null, same sign -> reinforcing
  expect_equal(robmen_sse_auto(-0.6, -0.8, -0.4, -0.1, -0.3, 0.1,
                               "A", "B", bias_favoured = NA), SSE_IN)
  # adjusted grows -> not reinforcing
  expect_equal(robmen_sse_auto(-0.3, -0.4, -0.2, -0.9, -1.1, -0.7,
                               "A", "B", bias_favoured = NA), SSE_NOT)
})

# ---------------------------------------------------------------------------
# SR reference from the data-sheet skeleton
# ---------------------------------------------------------------------------
test_that("robmen_sr_reference aggregates per canonical comparison", {
  sk <- data.frame(
    studlab  = c("S1", "S2", "S3", "S3", "S3"),
    t1       = c("A", "B", "A", "A", "B"),
    t2       = c("B", "A", "B", "C", "C"),
    n1       = c(10L, 20L, 15L, 15L, 15L),
    n2       = c(12L, 21L, 16L, 14L, 14L),
    reported = c(TRUE, FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE)
  ref <- robmen_sr_reference(sk)
  expect_equal(ref$comp_key, c("A:B", "A:C", "B:C"))
  expect_equal(ref$k_sr,  c(3, 1, 1))
  expect_equal(ref$k_rep, c(2, 0, 0))
  expect_equal(ref$n_sr,  c(22 + 41 + 31, 29, 29))
  expect_equal(ref$missing, c("S2", "S3", "S3"))

  # all reported -> NULL; missing columns -> NULL
  sk$reported <- TRUE
  expect_null(robmen_sr_reference(sk))
  expect_null(robmen_sr_reference(sk[, c("studlab", "t1")]))
  expect_null(robmen_sr_reference(data.frame()))
})

# ---------------------------------------------------------------------------
# Status summary
# ---------------------------------------------------------------------------
test_that("auto summary counts groups, sources and provisional rows", {
  rows <- data.frame(
    comp_key           = c("A:B", "A:C", "B:C"),
    grp                = c("A", "B", "C"),
    within_rating      = c(NO_BIAS, "Suspected bias favouring A", ""),
    within_provisional = c(FALSE, TRUE, FALSE),
    across_rating      = c("Suspected bias favouring B", "", NO_BIAS),
    across_provisional = c(TRUE, FALSE, FALSE),
    across_source      = c("egger", "", "qual"),
    stringsAsFactors = FALSE
  )
  s <- robmen_auto_summary(rows)
  expect_equal(s$n, 3L)
  expect_equal(c(s$n_a, s$n_b, s$n_c), c(1L, 1L, 1L))
  expect_equal(s$n_within_auto, 1L)
  expect_equal(s$n_within_prov, 1L)
  expect_equal(s$n_across_egger, 1L)
  expect_equal(s$n_across_qual, 1L)
  expect_equal(s$n_across_prov, 1L)
  expect_setequal(s$prov_keys, c("A:B", "A:C"))

  empty <- robmen_auto_summary(NULL)
  expect_equal(empty$n, 0L)
  expect_length(empty$prov_keys, 0)
})
