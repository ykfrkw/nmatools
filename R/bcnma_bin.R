# © 2026 Yuki Furukawa. All rights reserved.
# Author: Yuki Furukawa
# Date: 2026-03-09
# Contact: furukawa.y.psy@gmail.com
#
# File description:
# Main bcnma_bin() function — Bayesian component NMA with LASSO/SSVS interactions.
# Interface is modelled after netmeta() / discomb().

#' Bayesian Component Network Meta-Analysis with LASSO Interactions
#'
#' Fits a Bayesian random-effects component NMA (cNMA) using JAGS. Supports
#' three interaction models:
#' - `"none"`: fully additive (no interactions)
#' - `"lasso"`: all pairwise interactions with equiprobable SSVS priors
#' - `"lasso_informative"`: SSVS priors for `active_components` interactions only;
#'   all other interactions forced to zero
#'
#' Requires the JAGS system library and the suggested packages rjags and
#' MCMCvis; an informative error is raised when they are missing.
#'
#' @param data               data.frame with one row per arm per study
#' @param studlab             unquoted column name for study identifier
#' @param treat               unquoted column name for treatment/component combination string
#' @param event               unquoted column name for event count (integer)
#' @param n                   unquoted column name for arm sample size (integer)
#' @param components          character vector of component abbreviations (must match substrings in `treat`)
#' @param component_separator character separating components in the `treat` column (default `"+"`)
#' @param reference.group     character: reference component/combination for printing (optional)
#' @param interactions        character: `"none"`, `"lasso"` (default), or `"lasso_informative"`
#' @param active_components   character vector of components whose pairwise interactions
#'                            are modelled (only used when `interactions = "lasso_informative"`)
#' @param lasso_prob          numeric: prior inclusion probability for active interactions (default 0.5)
#' @param g                   numeric: spike-slab variance ratio; slab variance = g * spike variance (default 100)
#' @param tau_prior           numeric c(mean, precision): parameters of lognormal prior on tau (default
#'                            c(-1.67, 1/1.472^2) following Turner et al. 2015)
#' @param n.chains            integer: number of MCMC chains (default 3)
#' @param n.adapt             integer: JAGS adaptation iterations (default 2000)
#' @param n.burnin            integer: additional burn-in iterations after adaptation (default 5000)
#' @param n.iter              integer: sampling iterations per chain (default 20000)
#' @param n.thin              integer: thinning interval (default 1)
#' @param seed                integer or NULL: random seed for reproducibility
#' @param quiet               logical: suppress progress messages (default FALSE)
#' @param component_values    optional numeric matrix or data.frame with `nrow(data)`
#'   rows in the same order as `data` and column names exactly equal to
#'   `components`. Replaces the 0/1 indicators parsed from `treat` (e.g. number of
#'   sessions), so `d[k]` becomes the log-OR per unit of component k (linear
#'   dose-response). SSVS interactions still use presence (value != 0). Default
#'   `NULL` keeps the 0/1 indicators.
#' @param modifiers           optional list of arm-level effect-modifier
#'   specifications (a single specification may be passed without the outer
#'   list). Each is a list with `column` (arm-level column of `data`),
#'   `components` (components whose effect is modified; one term per component
#'   and level step), `type` (`"continuous"` or `"ordinal"`) and optionally
#'   `label` (default `column`) and `prior_sd` (default 10; `beta ~ N(0,
#'   prior_sd^2)`). Continuous: `center` (default: mean over arms where the
#'   component is present); the code is `value - center`. Ordinal: `levels`
#'   (required, lowest first) and `monotone` (`"none"`, `"increasing"`,
#'   `"decreasing"`); cumulative coding, so `beta_l` is the increment from level
#'   l to l+1. `NA` is allowed only in arms where the modified component is
#'   absent. The term `beta_q * (wA[i,k,q] - wA[i,1,q])` is added to the
#'   consistency equation, so `d[k]` of a modified component is its effect at
#'   the center (continuous) or reference level (ordinal). Modifier effects are
#'   identified mainly by between-study variation and are observational
#'   (ecological) associations. Default `NULL`: no modifiers.
#'
#' @return object of class `bcnma_bin` (a list) with elements:
#' \describe{
#'   \item{components}{character vector of component names}
#'   \item{n_components}{integer Nc}
#'   \item{Ns}{integer number of studies}
#'   \item{interactions_model}{character: which interaction model was fitted}
#'   \item{summary_additive}{MCMCsummary data.frame for additive model (if fitted)}
#'   \item{summary_lasso}{MCMCsummary data.frame for LASSO model (if fitted)}
#'   \item{samps_additive}{data.frame of combined MCMC samples, additive model}
#'   \item{samps_lasso}{data.frame of combined MCMC samples, LASSO model}
#'   \item{interaction_inclusion_prob}{named numeric: \code{mean(Ind[k])} per interaction}
#'   \item{interaction_effects}{named numeric: \code{median(gamma[k])} per interaction}
#'   \item{component_values}{validated component value matrix (NULL if not supplied)}
#'   \item{modifier_terms}{data.frame of modifier (beta) terms (NULL without modifiers)}
#'   \item{modifier_effects}{list(additive, lasso) of beta posterior summaries
#'     (NULL without modifiers)}
#'   \item{jags_data}{list passed to JAGS}
#'   \item{call}{matched call}
#' }
#'
#' @examples
#' \dontrun{
#' library(nmatools)
#'
#' # Load the bundled CBTICNMA example data
#' dat <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
#'                             package = "nmatools"))
#'
#' components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
#'                 "w",  "ns", "he", "tg", "ind", "gp", "ff", "ae")
#'
#' fit <- bcnma_bin(
#'   data         = dat,
#'   studlab      = study,
#'   treat        = treatment_component,
#'   event        = r,
#'   n            = n,
#'   components   = components,
#'   interactions = "lasso",
#'   seed         = 42
#' )
#' print(fit)
#' forest(fit)
#'
#' # Compare full CBT-I vs. sleep hygiene only
#' compare_combinations(fit,
#'   combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
#'   combo2 = c("se"))
#' }
#'
#' @export
bcnma_bin <- function(data,
                  studlab,
                  treat,
                  event,
                  n,
                  components,
                  component_separator = "+",
                  reference.group     = NULL,
                  interactions        = c("lasso", "none", "lasso_informative"),
                  active_components   = NULL,
                  lasso_prob          = 0.5,
                  g                   = 100,
                  tau_prior           = c(-1.67, 1 / 1.472^2),
                  n.chains            = 3,
                  n.adapt             = 2000,
                  n.burnin            = 5000,
                  n.iter              = 20000,
                  n.thin              = 1,
                  seed                = NULL,
                  quiet               = FALSE,
                  component_values    = NULL,
                  modifiers           = NULL) {

  .bcnma_require_mcmc_packages("bcnma_bin")
  cl <- match.call()

  # ---- Input validation ----
  interactions <- match.arg(interactions)

  studlab_col <- deparse(substitute(studlab))
  treat_col   <- deparse(substitute(treat))
  event_col   <- deparse(substitute(event))
  n_col       <- deparse(substitute(n))

  # Allow quoted or unquoted column names
  for (col in c(studlab_col, treat_col, event_col, n_col)) {
    col_clean <- gsub('"', '', col)
    if (!col_clean %in% names(data)) {
      stop(sprintf("Column '%s' not found in data.", col_clean))
    }
  }
  studlab_col <- gsub('"', '', studlab_col)
  treat_col   <- gsub('"', '', treat_col)
  event_col   <- gsub('"', '', event_col)
  n_col       <- gsub('"', '', n_col)

  if (!is.character(components) || length(components) < 2) {
    stop("'components' must be a character vector of length >= 2.")
  }

  if (interactions == "lasso_informative") {
    if (is.null(active_components)) {
      stop("'active_components' must be specified when interactions = 'lasso_informative'.")
    }
    unknown <- setdiff(active_components, components)
    if (length(unknown) > 0) {
      stop("'active_components' contains names not in 'components': ",
           paste(unknown, collapse = ", "))
    }
  }

  component_values <- .validate_component_values(component_values, nrow(data), components)
  modifier_specs   <- .normalise_modifier_specs(modifiers, data, components)

  # ---- Data preparation ----
  if (!quiet) message("Preparing data...")
  prep <- .bcnma_bin_prepare_data(data, studlab_col, treat_col, event_col, n_col,
                               components, component_separator)
  prep     <- .apply_component_values(prep, component_values)
  modifier_design <- .build_modifier_design(modifier_specs, data, prep)
  Ns       <- prep$Ns
  Nc       <- prep$Nc
  Ninter   <- Nc * (Nc - 1L) / 2L

  # ---- Determine included interactions ----
  if (interactions == "lasso_informative") {
    included <- all.interactions(active_components, Nc, components)
    if (length(included) == 0) {
      stop("No pairwise interactions found among 'active_components'. ",
           "Need at least 2 active components.")
    }
  } else {
    included <- seq_len(Ninter)
  }

  # ---- Seed ----
  if (!is.null(seed)) set.seed(seed)

  # ---- Common JAGS data ----
  jags_data_base <- list(
    r  = prep$r,
    n  = prep$n,
    xA = prep$xA,
    xB = prep$xB,
    na = prep$na,
    Ns = Ns,
    Nc = Nc
  )
  # wA/Q are added only with modifiers so that the data stay identical otherwise
  if (!is.null(modifier_design)) {
    jags_data_base <- c(jags_data_base, list(wA = modifier_design$wA, Q = modifier_design$Q))
  }
  modifier_priors <- modifier_design$priors
  mcmc <- list(n.chains = n.chains, n.adapt = n.adapt,
               n.burnin = n.burnin, n.iter  = n.iter, n.thin = n.thin)

  # ---- Results containers ----
  samps_lasso      <- NULL
  summary_lasso    <- NULL

  # =====================================================================
  # MODEL 1: Additive (always fitted when interactions = "none";
  #          also fitted alongside LASSO models for comparison)
  # =====================================================================
  if (!quiet) message("\n--- Fitting additive model (no interactions) ---")

  model_add <- .jags_model_additive(Nc, tau_prior, modifier_priors = modifier_priors)
  params_add <- .with_modifier_params(c("d", "ORd", "tau"), modifier_design)
  fit_add <- .bcnma_fit_model(model_add, jags_data_base, params_add, mcmc, quiet)
  summary_additive <- fit_add$summary
  samps_additive   <- fit_add$samps

  # =====================================================================
  # MODEL 2: SSVS / LASSO (interactions = "lasso" or "lasso_informative")
  # =====================================================================
  if (interactions != "none") {
    if (!quiet) {
      model_label <- if (interactions == "lasso") "LASSO (equiprobable)" else "LASSO (informative)"
      message(sprintf("\n--- Fitting %s interaction model ---", model_label))
      message(sprintf("  %d active interactions out of %d total", length(included), Ninter))
    }

    # Presence, not dose products, so gamma stays the co-presence log-OR
    component_presence <- (prep$xA != 0) * 1L
    interactions_arr <- .build_interaction_array(component_presence, Ns, prep$max_arms, Nc)

    jags_data_ssvs <- c(jags_data_base, list(interactions = interactions_arr))

    model_ssvs <- .jags_model_ssvs(
      Nc, Ninter,
      included        = included,
      prob_active     = lasso_prob,
      g               = g,
      tau_prior       = tau_prior,
      modifier_priors = modifier_priors
    )

    # Always sample gamma and Ind; eta only when all are active
    params_ssvs <- .with_modifier_params(c("d", "ORd", "gamma", "Ind", "tau", "eta"),
                                         modifier_design)
    fit_ssvs <- .bcnma_fit_model(model_ssvs, jags_data_ssvs, params_ssvs, mcmc, quiet)
    summary_lasso <- fit_ssvs$summary
    samps_lasso   <- fit_ssvs$samps

    interaction_summary <- .extract_interaction_summary(samps_lasso, Ninter, components)
    inter_incl_prob <- interaction_summary$inclusion_prob
    inter_effects   <- interaction_summary$effects

  } else {
    inter_incl_prob <- NULL
    inter_effects   <- NULL
  }

  if (!quiet) message("\nDone.")

  # ---- Build return object ----
  result <- c(
    list(
      call                       = cl,
      components                 = components,
      n_components               = Nc,
      Ns                         = Ns,
      Ninter                     = Ninter,
      sm                         = "OR",
      interactions_model         = interactions,
      active_components          = if (interactions == "lasso_informative") active_components else NULL,
      included_interactions      = included,
      reference.group            = reference.group,
      summary_additive           = summary_additive,
      summary_lasso              = summary_lasso,
      samps_additive             = samps_additive,
      samps_lasso                = samps_lasso,
      interaction_inclusion_prob = inter_incl_prob,
      interaction_effects        = inter_effects,
      jags_data                  = jags_data_base,
      mcmc_settings              = mcmc
    ),
    .modifier_results(component_values, modifier_specs, modifier_design,
                      samps_additive, samps_lasso, exponentiate = TRUE)
  )
  structure(result, class = "bcnma_bin")
}
