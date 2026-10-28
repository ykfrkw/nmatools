# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Purpose: identifiability of main effects and interactions from
#          within-study contrasts (review item R7), on a toy network built
#          so that every interaction status occurs, and on the bundled CBT-I
#          example (17 components: design rank 121 of 153).
# Inputs:  toy network below; inst/extdata/bcnma/data_CBTICNMA.csv
# Outputs: none (testthat expectations)
# Depends: rjags + MCMCvis for the fitting test (skipped otherwise)

TOY_COMPONENTS <- c("A", "B", "C", "D")
# Contrasts: S1 B + AB; S2 C + BC; S3 A + AC; S4 A - D; S5 B + BC + BD (CD in
# both arms). Main effects have rank 4; AB adds the fifth dimension, after
# which AC, BC, BD are linear combinations (aliased); AD never co-occurs.
toy_network <- data.frame(
  study = rep(c("S1", "S2", "S3", "S4", "S5"), each = 2),
  treat = c("A", "A+B", "B", "B+C", "C", "A+C", "D", "A", "C+D", "B+C+D"),
  r = c(10, 14, 12, 15, 9, 13, 11, 12, 10, 16), n = 50,
  stringsAsFactors = FALSE
)

test_that("interaction statuses on the toy network", {
  prep <- .bcnma_bin_prepare_data(toy_network, "study", "treat", "r", "n", TOY_COMPONENTS)
  identifiability <- .classify_identifiability(prep, candidate_pairs = 1:6)
  status <- stats::setNames(identifiability$interaction_status$status,
                            identifiability$interaction_status$component_pair)
  expect_equal(unname(status[c("A - B", "A - C", "A - D", "B - C", "B - D", "C - D")]),
               unname(BCNMA_INTERACTION_STATUS[c("estimable", "aliased", "never_copresent",
                                                 "aliased", "aliased", "zero_contrast")]))
  expect_equal(identifiability$ssvs_pairs, 1L)
  expect_equal(identifiability$main_rank, 4)
  expect_equal(identifiability$design_rank, 5)
  expect_warning(.check_identifiability(prep, 1:6, "ssvs"), "5 of 6 candidate interactions")
  expect_silent(.check_identifiability(prep, 1:6, "none"))
})

test_that("non-identifiable main effects are an error naming them", {
  aliased_rows <- small_bin_data[small_bin_data$study != "S4", ]   # A, B only via B - A
  prep <- .bcnma_bin_prepare_data(aliased_rows, "study", "treat", "r", "n", SIM_COMPONENTS)
  expect_error(.classify_identifiability(prep, integer(0)), "aliased.*B")
  constant_d <- transform(toy_network, treat = paste0(treat, "+E"))
  prep <- .bcnma_bin_prepare_data(constant_d, "study", "treat", "r", "n",
                                  c(TOY_COMPONENTS, "E"))
  expect_error(.classify_identifiability(prep, integer(0)), "no within-study contrast.*: E")
})

test_that("the CBT-I example has design rank 121 of 153", {
  raw_data <- utils::read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
                                          package = "nmatools"))
  names(raw_data)[1] <- "study"
  components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
                  "w", "ns", "he", "tg", "ind", "gp", "ff", "ae")
  prep <- suppressWarnings(.bcnma_bin_prepare_data(
    raw_data, "study", "treatment_component", "r", "n", components, inactive = "dt"))
  identifiability <- .classify_identifiability(prep, seq_len(136))
  expect_equal(identifiability$main_rank, 17)
  expect_equal(identifiability$design_rank, 121)
  expect_equal(identifiability$n_design_columns, 153)
  status_counts <- table(identifiability$interaction_status$status)
  expect_equal(status_counts[[BCNMA_INTERACTION_STATUS[["never_copresent"]]]], 4)
  expect_equal(status_counts[[BCNMA_INTERACTION_STATUS[["aliased"]]]], 28)
})

test_that("fixed interactions are stored, absent from SSVS and never selected", {
  skip_if_not_installed("rjags")
  skip_if_not_installed("MCMCvis")
  expect_warning(fit <- muffle_mcmc_warnings(bcnma_bin(
    toy_network, study, treat, r, n, components = TOY_COMPONENTS, interactions = "ssvs",
    n.chains = 2, n.adapt = 100, n.burnin = 0, n.iter = 20, seed = 1, quiet = TRUE)),
    "fixed to gamma = 0")
  expect_equal(fit$identifiability$ssvs_pairs, 1L)
  expect_true("gamma[1]" %in% names(fit$samps_ssvs))
  expect_false("gamma[2]" %in% names(fit$samps_ssvs))
  interaction_table <- summarise_interactions(fit, threshold = 0)
  expect_equal(sum(interaction_table$selected), 1)
  expect_true("status" %in% names(interaction_table))
})
