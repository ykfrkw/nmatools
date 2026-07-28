# Tests for the run_nma_batch() argument API: `...` shared args, precedence,
# fail-fast validation and outcome-named results.
#
# netmetawrap() is called as a bare symbol via do.call(netmetawrap, args), so
# local_mocked_bindings() intercepts it without running any real analysis.

# Record every call netmetawrap() receives and return the merged args.
mock_recorder <- function(env) {
  function(...) {
    args <- list(...)
    env$calls <- c(env$calls, list(args))
    invisible(args)
  }
}

test_that("`...` supplies shared args and params_list entries override them", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  res <- suppressMessages(nmatools::run_nma_batch(
    list(
      list(outcome = "o1", event = "r"),
      list(outcome = "o2", event = "r2", sm = "RR")
    ),
    data    = d,
    studlab = "id",
    treat   = "t",
    n       = "n",
    sm      = "OR"
  ))

  expect_length(rec$calls, 2L)
  expect_identical(rec$calls[[1]]$studlab, "id")
  expect_identical(rec$calls[[1]]$treat, "t")
  expect_identical(rec$calls[[1]]$n, "n")
  expect_identical(rec$calls[[1]]$sm, "OR")
  expect_identical(rec$calls[[1]]$outcome, "o1")
  expect_identical(rec$calls[[1]]$event, "r")
  expect_equal(rec$calls[[1]]$data, d)

  # per-element key overrides `...`
  expect_identical(rec$calls[[2]]$sm, "RR")
  expect_identical(rec$calls[[2]]$event, "r2")
  expect_length(res, 2L)
})

test_that(".default_args has the lowest precedence", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  suppressMessages(nmatools::run_nma_batch(
    list(list(outcome = "o1", event = "r", path = "from_params")),
    studlab = "id_dots",
    path    = "from_dots",
    .default_args = list(
      data    = d,
      studlab = "id_default",
      treat   = "t",
      n       = "n",
      sm      = "OR",
      path    = "from_default"
    )
  ))

  expect_length(rec$calls, 1L)
  expect_identical(rec$calls[[1]]$studlab, "id_dots")     # ... beats defaults
  expect_identical(rec$calls[[1]]$path, "from_params")    # params beats both
  expect_identical(rec$calls[[1]]$treat, "t")             # default carried over
})

test_that("the old .default_args-only call style still works", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  suppressMessages(nmatools::run_nma_batch(
    params_list = list(
      list(outcome = "o1", event = "r", small.values = "undesirable"),
      list(outcome = "o2", event = "r2", small.values = "desirable")
    ),
    .default_args = list(
      data            = d,
      studlab         = "id",
      treat           = "t",
      n               = "n",
      sm              = "OR",
      reference.group = "P",
      path            = "./outputs"
    )
  ))

  expect_length(rec$calls, 2L)
  expect_identical(rec$calls[[2]]$reference.group, "P")
  expect_identical(rec$calls[[2]]$small.values, "desirable")
})

test_that("a single parameter set is auto-wrapped", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  res <- suppressMessages(nmatools::run_nma_batch(
    list(outcome = "solo", event = "r"),
    data    = d,
    studlab = "id",
    treat   = "t",
    n       = "n",
    sm      = "OR"
  ))

  expect_length(rec$calls, 1L)
  expect_identical(rec$calls[[1]]$outcome, "solo")
  expect_named(res, "solo")
})

test_that("unknown keys error before any netmetawrap call", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(
        list(outcome = "o1", event = "r"),
        list(outcome = "o2", event = "r2", smd = "SMD")
      ),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "smd"
  )
  expect_length(rec$calls, 0L)   # nothing ran

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "o1", event = "r")),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR",
      typo_arg = 1
    )),
    "typo_arg"
  )
  expect_length(rec$calls, 0L)
})

test_that("a missing or empty outcome errors", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(event = "r")),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "outcome"
  )
  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "", event = "r")),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "outcome"
  )
  expect_length(rec$calls, 0L)
})

test_that("missing data errors before running", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "o1", event = "r")),
      studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "data"
  )
  expect_length(rec$calls, 0L)
})

test_that("non-string column values error and mention quoting", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "o1", event = "r")),
      data = d, studlab = 1, treat = "t", n = "n", sm = "OR"
    )),
    "quoted column name"
  )
  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "o1", event = c("r", "r2"))),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "quoted column name"
  )
  expect_length(rec$calls, 0L)
})

test_that("symbol column values pass validation", {
  # A symbol survives do.call() -> substitute() inside netmetawrap(), so it is
  # accepted. The mock must not force `...` (that would evaluate the symbol).
  testthat::local_mocked_bindings(netmetawrap = function(...) ...names())

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  res <- suppressMessages(nmatools::run_nma_batch(
    list(list(outcome = "o1", event = quote(r))),
    data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
  ))

  expect_true("event" %in% res$o1)
})

test_that("params_list elements must be named lists", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(netmetawrap = mock_recorder(rec))

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  expect_error(
    suppressMessages(nmatools::run_nma_batch(
      list(list(outcome = "o1", event = "r"), list("o2", "r2")),
      data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
    )),
    "params_list\\[\\[2\\]\\]"
  )
  expect_length(rec$calls, 0L)
})

test_that("results are named by outcome and a failing outcome yields NULL", {
  rec <- new.env(parent = emptyenv())
  rec$calls <- list()
  testthat::local_mocked_bindings(
    netmetawrap = function(...) {
      args <- list(...)
      rec$calls <- c(rec$calls, list(args))
      if (identical(args$outcome, "bad")) stop("boom")
      args$outcome
    }
  )

  d <- data.frame(id = "s1", t = "A", n = 10, r = 2)

  res <- suppressMessages(nmatools::run_nma_batch(
    list(
      list(outcome = "good1", event = "r"),
      list(outcome = "bad",   event = "r"),
      list(outcome = "good2", event = "r")
    ),
    data = d, studlab = "id", treat = "t", n = "n", sm = "OR"
  ))

  expect_named(res, c("good1", "bad", "good2"))
  expect_identical(res$good1, "good1")
  expect_null(res$bad)
  expect_identical(res$good2, "good2")
  expect_length(rec$calls, 3L)   # the batch continued past the failure
})
