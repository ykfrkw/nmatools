# Tests pinning the between-study variance (tau^2) estimator defaults:
# REML everywhere (matching the CINeMA GUI), user-overridable via
# netmeta_args, with deliberate DL exceptions in the rare-event panel.

tau_mk_arms <- function(id, t, ...) {
  data.frame(id = id, t = t, ..., stringsAsFactors = FALSE)
}

tau_binary_net <- function() {
  rbind(
    tau_mk_arms("s1", c("A", "B"), r = c(20, 30), n = c(100, 100)),
    tau_mk_arms("s2", c("A", "B"), r = c(18, 38), n = c(100, 100)),
    tau_mk_arms("s3", c("A", "B"), r = c(22, 25), n = c(100, 100)),
    tau_mk_arms("s4", c("A", "C"), r = c(20, 15), n = c(100, 100)),
    tau_mk_arms("s5", c("A", "C"), r = c(23, 9),  n = c(100, 100)),
    tau_mk_arms("s6", c("B", "C"), r = c(30, 16), n = c(100, 100)),
    tau_mk_arms("s7", c("B", "C"), r = c(28, 24), n = c(100, 100))
  )
}

tau_continuous_net <- function() {
  sd_arm <- c(2, 2)
  n_arm  <- c(50, 50)
  rbind(
    tau_mk_arms("s1", c("A", "B"), m = c(10, 8.0), s = sd_arm, n = n_arm),
    tau_mk_arms("s2", c("A", "B"), m = c(10, 9.5), s = sd_arm, n = n_arm),
    tau_mk_arms("s3", c("A", "B"), m = c(10, 7.2), s = sd_arm, n = n_arm),
    tau_mk_arms("s4", c("A", "C"), m = c(10, 9.0), s = sd_arm, n = n_arm),
    tau_mk_arms("s5", c("A", "C"), m = c(10, 7.5), s = sd_arm, n = n_arm),
    tau_mk_arms("s6", c("B", "C"), m = c(8,  8.6), s = sd_arm, n = n_arm),
    tau_mk_arms("s7", c("B", "C"), m = c(8,  9.9), s = sd_arm, n = n_arm)
  )
}

# Run netmetawrap quietly and return the saved netmeta object.
tau_run_wrap <- function(data, is_binary, netmeta_args = list()) {
  out <- withr::local_tempdir(.local_envir = parent.frame())
  common_args <- list(
    data = data, studlab = "id", treat = "t", n = "n",
    outcome = "tau", reference.group = "A", small.values = "undesirable",
    path = out, trim = FALSE, netmeta_args = netmeta_args
  )
  type_args <- if (is_binary) {
    list(event = "r", sm = "OR", rare_events = "never")
  } else {
    list(mean_col = "m", sd_col = "s", sm = "MD")
  }
  suppressWarnings(suppressMessages(
    do.call(nmatools::netmetawrap, c(common_args, type_args))
  ))
  rds <- list.files(out, pattern = "^netmeta_.*\\.rds$",
                    recursive = TRUE, full.names = TRUE)
  expect_length(rds, 1L)
  readRDS(rds)
}

test_that("netmetawrap continuous defaults to REML and honours override", {
  testthat::skip_on_cran()
  d <- tau_continuous_net()
  expect_identical(tau_run_wrap(d, is_binary = FALSE)$method.tau, "REML")
  overridden <- tau_run_wrap(d, is_binary = FALSE,
                             netmeta_args = list(method.tau = "DL"))
  expect_identical(overridden$method.tau, "DL")
})

test_that("netmetawrap binary IV defaults to REML and restores settings", {
  testthat::skip_on_cran()
  loadNamespace("netmeta")
  tau_before <- meta::gs("method.tau.netmeta")
  d <- tau_binary_net()
  expect_identical(tau_run_wrap(d, is_binary = TRUE)$method.tau, "REML")
  expect_identical(meta::gs("method.tau.netmeta"), tau_before)
  overridden <- tau_run_wrap(d, is_binary = TRUE,
                             netmeta_args = list(method.tau = "DL"))
  expect_identical(overridden$method.tau, "DL")
  expect_identical(meta::gs("method.tau.netmeta"), tau_before)
})

test_that(".with_netmeta_tau restores the setting on error", {
  loadNamespace("netmeta")
  tau_before <- meta::gs("method.tau.netmeta")
  expect_error(
    nmatools:::.with_netmeta_tau("REML", stop("boom")),
    "boom"
  )
  expect_identical(meta::gs("method.tau.netmeta"), tau_before)
  seen_inside <- nmatools:::.with_netmeta_tau(
    "REML", meta::gs("method.tau.netmeta")
  )
  expect_identical(seen_inside, "REML")
})

test_that("rare panel: IV_RE_CC is DL, MH fits without tau", {
  pw <- meta::pairwise(data = tau_binary_net(), studlab = id, treat = t,
                       n = n, event = r, sm = "OR")
  specs <- nmatools:::.rare_nma_method_specs()
  names(specs) <- vapply(specs, `[[`, character(1L), "id")
  fit_spec <- function(id) {
    nmatools:::.fit_one_rare_nma(pw, specs[[id]], sm = "OR",
                                 reference.group = "A",
                                 small.values = "undesirable")
  }
  # A non-DL global setting proves IV_RE_CC pins DL itself.
  loadNamespace("netmeta")
  tau_before <- meta::gs("method.tau.netmeta")
  meta::settings.meta(method.tau.netmeta = "REML", quietly = TRUE)
  withr::defer(
    meta::settings.meta(method.tau.netmeta = tau_before, quietly = TRUE)
  )
  iv_re <- fit_spec("IV_RE_CC")
  expect_null(iv_re$error)
  expect_identical(iv_re$fit$method.tau, "DL")
  mh <- fit_spec("MH")
  expect_null(mh$error)
  expect_s3_class(mh$fit, "netmetabin")
})

test_that("build_w2i_netmeta uses REML", {
  testthat::skip_on_cran()
  net <- suppressWarnings(nmatools::build_w2i_netmeta("remission_lt"))
  expect_identical(net$method.tau, "REML")
})
