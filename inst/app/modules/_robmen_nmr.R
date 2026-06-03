# Pure ROB-MEN network meta-regression helper.

robmen_nmr_result_template <- function(treatments, status = "unfitted") {
  treatments <- sort(unique(as.character(treatments)))
  if (length(treatments) < 2) {
    return(data.frame(
      comparison = character(0),
      t1         = character(0),
      t2         = character(0),
      nmr_te     = numeric(0),
      nmr_lo     = numeric(0),
      nmr_hi     = numeric(0),
      status     = character(0),
      stringsAsFactors = FALSE
    ))
  }

  pair_mat <- utils::combn(treatments, 2)
  data.frame(
    comparison = paste(pair_mat[1, ], pair_mat[2, ], sep = " vs "),
    t1         = pair_mat[1, ],
    t2         = pair_mat[2, ],
    nmr_te     = NA_real_,
    nmr_lo     = NA_real_,
    nmr_hi     = NA_real_,
    status     = status,
    stringsAsFactors = FALSE
  )
}

robmen_nmr_components <- function(t1, t2, treatments) {
  treatments <- sort(unique(as.character(treatments)))
  adj <- stats::setNames(vector("list", length(treatments)), treatments)
  for (trt in treatments) adj[[trt]] <- character(0)

  for (i in seq_along(t1)) {
    a <- as.character(t1[i])
    b <- as.character(t2[i])
    if (!a %in% treatments || !b %in% treatments || identical(a, b)) next
    adj[[a]] <- unique(c(adj[[a]], b))
    adj[[b]] <- unique(c(adj[[b]], a))
  }

  remaining <- treatments
  comps <- list()
  while (length(remaining) > 0) {
    seed <- remaining[1]
    seen <- character(0)
    queue <- seed
    while (length(queue) > 0) {
      cur <- queue[1]
      queue <- queue[-1]
      if (cur %in% seen) next
      seen <- c(seen, cur)
      queue <- unique(c(queue, setdiff(adj[[cur]], seen)))
    }
    comps[[length(comps) + 1L]] <- sort(seen)
    remaining <- setdiff(remaining, seen)
  }
  comps
}

robmen_nmr_design <- function(dat, treatments, reference, xc,
                              coef_type = "common") {
  basic <- setdiff(treatments, reference)
  if (length(basic) < 1) return(NULL)

  Xb <- matrix(0, nrow = nrow(dat), ncol = length(basic))
  colnames(Xb) <- paste0("b_", make.names(basic, unique = TRUE))
  for (i in seq_len(nrow(dat))) {
    if (dat$t1[i] != reference) {
      Xb[i, match(dat$t1[i], basic)] <- Xb[i, match(dat$t1[i], basic)] - 1
    }
    if (dat$t2[i] != reference) {
      Xb[i, match(dat$t2[i], basic)] <- Xb[i, match(dat$t2[i], basic)] + 1
    }
  }

  if (coef_type == "unrelated") {
    Xs <- sweep(Xb, 1, xc, "*")
    colnames(Xs) <- paste0("s_", make.names(basic, unique = TRUE))
    X <- cbind(Xb, Xs)
  } else if (all(abs(xc) < sqrt(.Machine$double.eps))) {
    X <- Xb
  } else {
    X <- cbind(Xb, nmr_xc = xc)
  }

  list(X = X, basic = basic)
}

robmen_nmr_fit <- function(dat, X, model_type) {
  if (nrow(X) < ncol(X) || qr(X)$rank < ncol(X)) return(NULL)

  fit <- tryCatch({
    if (model_type == "random") {
      metafor::rma.mv(yi = y, V = vi, mods = ~ X - 1,
                      random = ~ 1 | studlab, method = "REML", data = dat)
    } else {
      metafor::rma.mv(yi = y, V = vi, mods = ~ X - 1,
                      method = "FE", data = dat)
    }
  }, error = function(e) NULL)

  if (is.null(fit) && model_type == "random") {
    fit <- tryCatch(
      metafor::rma.mv(yi = y, V = vi, mods = ~ X - 1,
                      method = "FE", data = dat),
      error = function(e) NULL)
  }
  if (is.null(fit)) return(NULL)

  b <- tryCatch(stats::coef(fit), error = function(e) NULL)
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(b) || is.null(V) || any(!is.finite(b)) || any(!is.finite(V))) {
    if (model_type != "random") return(NULL)
    fit <- tryCatch(
      metafor::rma.mv(yi = y, V = vi, mods = ~ X - 1,
                      method = "FE", data = dat),
      error = function(e) NULL)
    if (is.null(fit)) return(NULL)
  }

  fit
}

compute_network_nmr <- function(pairwise_df, treatments, reference = NULL,
                                model_type = "fixed",
                                covar = "variance",
                                coef_type = "common",
                                extrapolate_to = "min") {
  if (is.null(model_type) || length(model_type) == 0 || is.na(model_type[1])) {
    model_type <- "fixed"
  }
  model_type <- as.character(model_type[1])
  if (model_type == "common") model_type <- "fixed"
  model_type <- match.arg(model_type, c("fixed", "random"))

  covar <- match.arg(covar, c("variance", "se"))
  coef_type <- match.arg(coef_type, c("common", "unrelated"))
  extrapolate_to <- match.arg(extrapolate_to, c("min"))

  out <- robmen_nmr_result_template(treatments, status = "unfitted")
  if (nrow(out) == 0) return(out)

  if (!requireNamespace("metafor", quietly = TRUE) ||
      is.null(pairwise_df) || nrow(pairwise_df) < 2) {
    return(out)
  }

  required <- c("t1", "t2", "y", "se")
  if (!all(required %in% names(pairwise_df))) return(out)

  dat <- data.frame(
    t1      = as.character(pairwise_df$t1),
    t2      = as.character(pairwise_df$t2),
    y       = suppressWarnings(as.numeric(pairwise_df$y)),
    se      = suppressWarnings(as.numeric(pairwise_df$se)),
    studlab = if ("studlab" %in% names(pairwise_df)) {
      as.character(pairwise_df$studlab)
    } else {
      paste0("study_", seq_len(nrow(pairwise_df)))
    },
    stringsAsFactors = FALSE
  )
  dat$studlab[is.na(dat$studlab) | !nzchar(dat$studlab)] <-
    paste0("study_", which(is.na(dat$studlab) | !nzchar(dat$studlab)))

  all_treatments <- sort(unique(as.character(treatments)))
  dat <- dat[dat$t1 %in% all_treatments &
             dat$t2 %in% all_treatments &
             dat$t1 != dat$t2 &
             is.finite(dat$y) &
             is.finite(dat$se) &
             dat$se > 0, , drop = FALSE]
  if (nrow(dat) < 2 || length(unique(dat$studlab)) < 2) return(out)

  dat$vi <- dat$se^2
  dat$x <- if (covar == "variance") dat$vi else dat$se

  observed <- sort(unique(c(dat$t1, dat$t2)))
  comps <- robmen_nmr_components(dat$t1, dat$t2, observed)
  z <- stats::qnorm(0.975)

  for (comp_trts in comps) {
    if (length(comp_trts) < 2) next
    sub <- dat[dat$t1 %in% comp_trts & dat$t2 %in% comp_trts, , drop = FALSE]
    if (nrow(sub) < 2 || length(unique(sub$studlab)) < 2) next

    ref <- if (!is.null(reference) && length(reference) > 0 &&
               !is.na(reference[1]) && reference[1] %in% comp_trts) {
      as.character(reference[1])
    } else {
      comp_trts[1]
    }
    x_min <- min(sub$x, na.rm = TRUE)
    sub$xc <- sub$x - x_min

    status <- "ok"
    design <- robmen_nmr_design(sub, comp_trts, ref, sub$xc, coef_type)
    fit <- if (!is.null(design)) robmen_nmr_fit(sub, design$X, model_type) else NULL

    if (is.null(fit) && coef_type == "unrelated") {
      design <- robmen_nmr_design(sub, comp_trts, ref, sub$xc, "common")
      fit <- if (!is.null(design)) robmen_nmr_fit(sub, design$X, model_type) else NULL
      status <- if (!is.null(fit)) "fallback_common" else "unfitted"
    }
    if (is.null(fit) || is.null(design)) next

    basic <- design$basic
    n_basic <- length(basic)
    b <- stats::coef(fit)
    V <- stats::vcov(fit)
    if (length(b) < n_basic || nrow(V) < n_basic || ncol(V) < n_basic) next
    b_basic <- b[seq_len(n_basic)]
    V_basic <- V[seq_len(n_basic), seq_len(n_basic), drop = FALSE]

    row_idx <- which(out$t1 %in% comp_trts & out$t2 %in% comp_trts)
    for (idx in row_idx) {
      L <- rep(0, n_basic)
      if (out$t1[idx] != ref) L[match(out$t1[idx], basic)] <-
          L[match(out$t1[idx], basic)] - 1
      if (out$t2[idx] != ref) L[match(out$t2[idx], basic)] <-
          L[match(out$t2[idx], basic)] + 1

      est <- sum(L * b_basic)
      var <- as.numeric(t(L) %*% V_basic %*% L)
      if (!is.finite(est) || !is.finite(var) ||
          var < -sqrt(.Machine$double.eps)) next

      se <- sqrt(max(var, 0))
      out$nmr_te[idx] <- est
      out$nmr_lo[idx] <- est - z * se
      out$nmr_hi[idx] <- est + z * se
      out$status[idx] <- status
    }
  }

  out
}
