# Tests for the "SR skeleton" that Module A keeps alongside the pairwise
# data: rows of the sheet whose outcome cells are blank are excluded from
# the NMA but recorded in $sr_pairs so ROB-MEN can pre-fill "Total
# identified in the SR" and Group B classification without manual entry.

for (.loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", .loc)))) break
}

module_dir <- testthat::test_path("..", "..", "inst", "app", "modules")
if (!dir.exists(module_dir)) {
  module_dir <- system.file("app", "modules", package = "nmatools")
}
source(file.path(module_dir, "utils.R"), local = TRUE, encoding = "UTF-8")
source(file.path(module_dir, "_robmen_auto.R"), local = TRUE, encoding = "UTF-8")
source(file.path(module_dir, "module_A_data_input.R"),
       local = TRUE, encoding = "UTF-8")

# S1: A vs B reported. S2: A vs B, outcome blank. S3: A/B/C three-arm, C arm
# blank -> A:B reported, A:C and B:C not. S4: B vs C, no n either.
binary_sheet <- function() {
  data.frame(
    studlab      = c("S1", "S1", "S2", "S2", "S3", "S3", "S3", "S4", "S4"),
    treat        = c("A", "B", "A", "B", "A", "B", "C", "B", "C"),
    n            = c(20L, 20L, 30L, 30L, 15L, 15L, 15L, NA, NA),
    event        = c(5L, 8L, NA, NA, 3L, 6L, NA, NA, NA),
    rob          = "low",
    indirectness = "low",
    stringsAsFactors = FALSE
  )
}

test_that("build_sr_pairs records every pair in the sheet with a reported flag", {
  arms <- binary_sheet()
  arms$has_outcome <- !is.na(arms$event) & !is.na(arms$n)
  sk <- build_sr_pairs(arms)

  expect_equal(nrow(sk), 6L)   # S1 1 + S2 1 + S3 3 + S4 1
  expect_true(all(sk$t1 < sk$t2))
  s3 <- sk[sk$studlab == "S3", ]
  expect_setequal(paste(s3$t1, s3$t2), c("A B", "A C", "B C"))
  expect_equal(s3$reported[s3$t1 == "A" & s3$t2 == "B"], TRUE)
  expect_equal(s3$reported[s3$t2 == "C"], c(FALSE, FALSE))
  expect_false(sk$reported[sk$studlab == "S2"])
  expect_true(is.na(sk$n1[sk$studlab == "S4"]))

  # single-arm studies contribute nothing; empty input is safe
  expect_equal(nrow(build_sr_pairs(arms[arms$studlab == "S1", ][1, ])), 0L)
  expect_equal(nrow(build_sr_pairs(NULL)), 0L)
})

test_that("convert_binary drops blank-outcome arms from the NMA but keeps them as SR pairs", {
  res <- convert_binary(binary_sheet(), "OR")

  expect_null(res$error)
  # NMA rows: S1 A:B and S3 A:B only
  expect_equal(nrow(res$data), 2L)
  expect_setequal(unique(res$data$studlab), c("S1", "S3"))

  expect_true(is.data.frame(res$sr_pairs))
  expect_equal(nrow(res$sr_pairs), 6L)
  expect_equal(res$n_unreported_studies, 3L)   # S2, S3 (C arm), S4
  expect_equal(res$n_unreported_pairs, 4L)
})

test_that("convert_continuous behaves the same with blank mean / sd", {
  df <- data.frame(
    studlab = c("S1", "S1", "S2", "S2"),
    treat   = c("A", "B", "A", "B"),
    n       = c(10L, 12L, 20L, 20L),
    mean    = c(1.2, 0.8, NA, NA),
    sd      = c(1, 1, NA, NA),
    rob = "low", indirectness = "low", stringsAsFactors = FALSE)
  res <- convert_continuous(df, "SMD")
  expect_null(res$error)
  expect_equal(nrow(res$data), 1L)
  expect_equal(res$n_unreported_studies, 1L)
  expect_equal(res$sr_pairs$reported, c(TRUE, FALSE))
})

test_that("a sheet without any outcome data is an error, not an empty NMA", {
  df <- binary_sheet()
  df$event <- NA_integer_
  res <- convert_binary(df, "OR")
  expect_null(res$data)
  expect_match(res$error, "No study has outcome data")
})

test_that("convert_pairwise keeps blank-y contrast rows as SR pairs", {
  df <- data.frame(
    studlab = c("S1", "S2", "S3"),
    t1 = c("A", "B", "A"), t2 = c("B", "A", "C"),
    y  = c(0.3, NA, 0.1), se = c(0.1, NA, 0.2),
    n1 = c(10L, 15L, 12L), n2 = c(11L, 14L, 12L),
    rob = "low", indirectness = "low", stringsAsFactors = FALSE)
  res <- convert_pairwise(df)
  expect_null(res$error)
  expect_equal(nrow(res$data), 2L)
  expect_equal(nrow(res$sr_pairs), 3L)
  # canonical ordering: S2 "B vs A" becomes A:B with n swapped
  s2 <- res$sr_pairs[res$sr_pairs$studlab == "S2", ]
  expect_equal(c(s2$t1, s2$t2), c("A", "B"))
  expect_equal(c(s2$n1, s2$n2), c(14L, 15L))
  expect_false(s2$reported)
  expect_equal(res$n_unreported_studies, 1L)
})

test_that("robmen_sr_reference aggregates the skeleton per comparison", {
  res <- convert_binary(binary_sheet(), "OR")
  ref <- robmen_sr_reference(res$sr_pairs)

  expect_true(is.data.frame(ref))
  expect_setequal(ref$comp_key, c("A:B", "A:C", "B:C"))
  ab <- ref[ref$comp_key == "A:B", ]
  expect_equal(ab$k_sr, 3)          # S1, S2, S3
  expect_equal(ab$k_rep, 2)         # S1, S3
  expect_equal(ab$n_sr, 40 + 60 + 30)
  expect_equal(ab$missing, "S2")
  bc <- ref[ref$comp_key == "B:C", ]
  expect_equal(bc$k_sr, 2)          # S3, S4
  expect_equal(bc$k_rep, 0)
  expect_equal(bc$missing, "S3; S4")
  expect_equal(bc$n_sr, 30)         # S4 has no n -> ignored, S3 counted

  # Group classification follows straight from the reference
  expect_equal(robmen_group_auto(ab$k_rep, ab$k_sr), "A")
  expect_equal(robmen_group_auto(bc$k_rep, bc$k_sr), "B")

  # A sheet with every outcome present yields NULL (fall back to the cells)
  full <- binary_sheet()[1:2, ]
  expect_null(robmen_sr_reference(convert_binary(full, "OR")$sr_pairs))
  expect_null(robmen_sr_reference(NULL))
})
