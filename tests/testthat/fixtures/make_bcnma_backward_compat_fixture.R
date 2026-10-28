# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# One-off generator for bcnma_backward_compat.rds: outputs of bcnma 0.1.0
# (commit 2214ac1, the state before component values / effect modifiers were
# added), so that tests/testthat/test-bcnma-backward-compat.R can check that
# the new code reproduces them exactly when `component_values` and
# `modifiers` are NULL. No JAGS run is needed: model strings and JAGS data
# come from the internal builders, and compare_combinations() is evaluated on
# fixed synthetic draws.
#
# Provenance: the fixture was built from bcnma 0.1.0, commit 2214ac1 of
# https://github.com/ykfrkw/bcnma (the standalone package that was merged
# into nmatools 0.3.0). It was generated in the bcnma repository as
# backward_compat.rds and copied here unchanged under its new name.
#
# To regenerate, run from the nmatools package root with the bcnma 0.1.0
# sources extracted elsewhere:
#   git clone https://github.com/ykfrkw/bcnma /tmp/bcnma_repo
#   mkdir -p /tmp/bcnma_010
#   git -C /tmp/bcnma_repo archive 2214ac1 R | tar -x -C /tmp/bcnma_010
#   BCNMA_BASE_DIR=/tmp/bcnma_010 \
#     Rscript tests/testthat/fixtures/make_bcnma_backward_compat_fixture.R
# Inputs : bcnma 0.1.0 R/ sources (BCNMA_BASE_DIR)
# Outputs: tests/testthat/fixtures/bcnma_backward_compat.rds
# Depends: base R only (sources are sys.source()d, not installed).

BASE_DIR <- Sys.getenv("BCNMA_BASE_DIR")
if (!dir.exists(file.path(BASE_DIR, "R"))) {
  stop("Set BCNMA_BASE_DIR to a directory containing the bcnma 0.1.0 R/ sources.")
}

FIXTURE_PATH     <- "tests/testthat/fixtures/bcnma_backward_compat.rds"
SMALL_COMPONENTS <- c("A", "B", "C")
DRAW_SEED        <- 20261028
N_DRAWS          <- 200
COMPARE_COMBOS   <- list(c("A", "C"), c(1L, 3L), c("A", "B", "C"))
COMPARE_AGAINST  <- list("B", character(0))

base <- new.env()
for (file_name in list.files(file.path(BASE_DIR, "R"), pattern = "[.]R$", full.names = TRUE)) {
  sys.source(file_name, base)
}

# Same small networks as in tests/testthat/helper-data.R
small_bin_data <- data.frame(
  study = c("S2", "S1", "S2", "S3", "S1", "S3"),
  treat = c("A", "A", "A+C", "B", "B+C", "B+C"),
  r = c(10, 12, 15, 9, 14, 18), n = 50,
  stringsAsFactors = FALSE
)
small_cont_data <- data.frame(
  study = c("S2", "S1", "S2", "S3", "S1", "S3"),
  treat = c("A", "A", "A+C", "B", "B+C", "B+C"),
  mean  = c(-2.0, -1.5, -4.1, -2.2, -3.9, -5.0),
  sd    = c(4.0, 3.5, 4.2, 3.9, 4.4, 4.1), n = 50,
  stringsAsFactors = FALSE
)

old_bin  <- base$.bcnma_bin_prepare_data(small_bin_data, "study", "treat", "r", "n",
                                         SMALL_COMPONENTS)
old_cont <- base$.bcnma_cont_prepare_data(small_cont_data, "study", "treat", "mean", "sd", "n",
                                          SMALL_COMPONENTS, "+", "SMD", "within")
jags_data_bin  <- old_bin[c("r", "n", "xA", "xB", "na", "Ns", "Nc")]
jags_data_cont <- list(y = old_cont$y, prec = old_cont$prec, xA = old_cont$xA,
                       xB = old_cont$xB, na = old_cont$na, Ns = old_cont$Ns, Nc = old_cont$Nc)

# compare_combinations() only reads posterior draws, so fixed synthetic draws
# are enough to pin down the old arithmetic for both classes.
set.seed(DRAW_SEED)
draw_columns <- function(prefix, n_columns) {
  draws <- as.data.frame(matrix(rnorm(N_DRAWS * n_columns, sd = 0.5), ncol = n_columns))
  names(draws) <- paste0(prefix, "[", seq_len(n_columns), "]")
  draws
}
main_draws        <- draw_columns("d", 3)
interaction_draws <- draw_columns("gamma", 3)
make_fit <- function(class_name, sm) {
  structure(list(components = SMALL_COMPONENTS, n_components = 3L, Ninter = 3L, sm = sm,
                 samps_additive = main_draws,
                 samps_lasso    = cbind(main_draws, interaction_draws)),
            class = class_name)
}
compare_fit_bin  <- make_fit("bcnma_bin", "OR")
compare_fit_cont <- make_fit("bcnma_cont", "SMD")

compare_cases <- expand.grid(combo = seq_along(COMPARE_COMBOS), against = seq_along(COMPARE_AGAINST),
                             model = c("lasso", "additive"), stringsAsFactors = FALSE)
run_old_compare <- function(fit) {
  lapply(seq_len(nrow(compare_cases)), function(case_index) {
    case <- compare_cases[case_index, ]
    # Methods are called directly: S3 dispatch does not search `base`.
    old_method <- get(paste0("compare_combinations.", class(fit)), envir = base)
    capture.output(result <- old_method(
      fit, COMPARE_COMBOS[[case$combo]], COMPARE_AGAINST[[case$against]], model = case$model))
    result
  })
}

saveRDS(list(
  generated_from             = "bcnma 0.1.0, commit 2214ac136ff5ab895bb6f154c289596e3c94a52f",
  model_additive_bin_4       = base$.jags_model_additive(4),
  model_ssvs_bin_4_6         = base$.jags_model_ssvs(4, 6),
  model_ssvs_bin_4_6_partial = base$.jags_model_ssvs(4, 6, included = c(1, 6), prob_active = 0.8),
  model_additive_cont_4      = base$.jags_model_additive_cont(4),
  model_ssvs_cont_4_6        = base$.jags_model_ssvs_cont(4, 6),
  model_ssvs_cont_4_6_partial = base$.jags_model_ssvs_cont(4, 6, included = c(1, 6),
                                                           prob_active = 0.8, g = 50),
  small_bin_data             = small_bin_data,
  small_cont_data            = small_cont_data,
  jags_data_bin              = jags_data_bin,
  jags_data_cont             = jags_data_cont,
  compare_combos             = COMPARE_COMBOS,
  compare_against            = COMPARE_AGAINST,
  compare_cases              = compare_cases,
  compare_fit_bin            = compare_fit_bin,
  compare_fit_cont           = compare_fit_cont,
  compare_results_bin        = run_old_compare(compare_fit_bin),
  compare_results_cont       = run_old_compare(compare_fit_cont)
), FIXTURE_PATH)
message("Wrote ", FIXTURE_PATH)
