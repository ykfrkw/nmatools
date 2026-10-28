# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-10-28
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Shared test data and helpers: small hand-made networks (binary and
# continuous), simulated networks whose component C is modified by provider
# proficiency, and a fitting wrapper that silences the expected one-chain
# Rhat warning, for the test-bcnma-*.R files. Loaded automatically by
# testthat before the test files.

SIM_SEED       <- 20261028
SIM_COMPONENTS <- c("A", "B", "C")
TRUE_BETA_BIN  <- 0.5
TRUE_BETA_CONT <- 0.3
PROFICIENCY_CENTER <- 3
SHORT_MCMC <- list(n.chains = 1, n.adapt = 500, n.burnin = 1000, n.iter = 3000)
# Two-arm designs whose within-study contrasts (C; A; B + C - A) identify
# all three main effects.
SIM_DESIGNS <- list(c("A", "A+C"), c("B", "A+B"), c("A", "B+C"))
SIM_COMPONENT_EFFECTS <- c(A = 0.2, B = 0.3, C = 0.4)
TINY_MCMC  <- list(n.chains = 1, n.adapt = 100, n.burnin = 0, n.iter = 10)

small_bin_data <- data.frame(
  study = c("S2", "S1", "S2", "S3", "S1", "S3"),
  treat = c("A", "A", "A+C", "B", "B+C", "B+C"),
  r = c(10, 12, 15, 9, 14, 18), n = 50,
  level = c(NA, NA, "high", NA, "mid", "low"),
  stringsAsFactors = FALSE
)

small_cont_data <- data.frame(
  study = c("S2", "S1", "S2", "S3", "S1", "S3"),
  treat = c("A", "A", "A+C", "B", "B+C", "B+C"),
  mean  = c(-2.0, -1.5, -4.1, -2.2, -3.9, -5.0),
  sd    = c(4.0, 3.5, 4.2, 3.9, 4.4, 4.1), n = 50,
  level = c(NA, NA, "high", NA, "mid", "low"),
  stringsAsFactors = FALSE
)

# Component values (e.g. sessions) aligned with the rows of the small data
dose_values <- cbind(A = c(1, 1, 1, 0, 0, 0), B = c(0, 0, 0, 1, 1, 1), C = c(0, 0, 2.5, 0, 4, 7))

prepare_small_bin <- function(data = small_bin_data, values = NULL) {
  prep <- .bcnma_bin_prepare_data(data, "study", "treat", "r", "n", SIM_COMPONENTS, "+")
  .apply_component_values(prep, values)
}

prepare_small_cont <- function(data = small_cont_data, values = NULL) {
  prep <- .bcnma_cont_prepare_data(data, "study", "treat", "mean", "sd", "n",
                                   SIM_COMPONENTS, "+", "SMD", "within")
  .apply_component_values(prep, values)
}

# 30 two-arm studies (SIM_DESIGNS); the effect of C changes by `true_beta`
# per unit of the study's provider proficiency. outcome = "binary" gives
# r / n; "continuous" gives mean / sd / n (SD = 1, so MD and SMD coincide
# approximately).
simulate_proficiency_studies <- function(outcome = c("binary", "continuous"),
                                         n_studies = 30, arm_size = 150,
                                         true_beta = NULL) {
  outcome <- match.arg(outcome)
  if (is.null(true_beta)) true_beta <- if (outcome == "binary") TRUE_BETA_BIN else TRUE_BETA_CONT
  set.seed(SIM_SEED)
  component_effect <- SIM_COMPONENT_EFFECTS
  arm_rows <- lapply(seq_len(n_studies), function(study_number) {
    design      <- SIM_DESIGNS[[(study_number - 1) %% length(SIM_DESIGNS) + 1]]
    proficiency <- stats::runif(1, min = 1, max = 5)
    baseline    <- stats::rnorm(1, mean = -0.5, sd = 0.3)
    linear_predictor <- vapply(design, function(treat_label) {
      parts <- strsplit(treat_label, "+", fixed = TRUE)[[1]]
      has_c <- "C" %in% parts
      baseline + sum(component_effect[parts]) +
        has_c * true_beta * (proficiency - PROFICIENCY_CENTER)
    }, numeric(1))
    arms <- data.frame(study = sprintf("S%02d", study_number), treat = design, n = arm_size,
                       proficiency = proficiency,
                       proficiency_level = as.character(cut(proficiency, c(0, 2.33, 3.67, 6),
                                                            labels = c("low", "mid", "high"))),
                       stringsAsFactors = FALSE)
    if (outcome == "binary") {
      arms$r <- stats::rbinom(2, arm_size, stats::plogis(linear_predictor))
    } else {
      arms$mean <- stats::rnorm(2, linear_predictor, 1 / sqrt(arm_size))
      arms$sd   <- 1
    }
    arms
  })
  do.call(rbind, arm_rows)
}

# Binary network without modifier effect and with large arms, for the
# comparison with netmeta::discomb().
simulate_identifiable_network <- function() {
  simulate_proficiency_studies("binary", arm_size = 300, true_beta = 0)
}

# One-chain test runs make MCMCsummary warn that Rhat is unavailable; muffle
# only that warning so any other warning still surfaces.
quietly_fit <- function(expr) {
  withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) {
      if (grepl("Rhat", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

