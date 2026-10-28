[Manual home](README.md) › Bayesian component NMA

# 17. Bayesian component NMA — `bcnma_bin()` and `bcnma_cont()`

> [!WARNING]
> **The Bayesian component NMA functions (`bcnma_bin()`, `bcnma_cont()` and
> related helpers) are under active development.** A methodological review is
> in progress; defaults (priors, labelling of the heterogeneity parameter,
> handling of mean differences, reproducibility of `seed`) and outputs may
> change without notice. Do not use them for published analyses yet. The
> frequentist NMA tools in this package are not affected.

A component network meta-analysis (cNMA) splits each treatment into the
components it is made of — for example the elements of a CBT-I package — and
estimates the effect of each component rather than of each whole treatment.
nmatools fits **Bayesian random-effects cNMA** in JAGS for binary outcomes
(`bcnma_bin()`, odds ratio) and continuous outcomes (`bcnma_cont()`, SMD or
MD), with three ways of handling interactions between components, optional
non-binary component values (doses), and arm-level effect modifiers of
specific components.

These functions were developed as the standalone package **bcnma**
([github.com/ykfrkw/bcnma](https://github.com/ykfrkw/bcnma)), which is now
superseded by nmatools. Function names, arguments, and results are unchanged:
existing scripts work after replacing `library(bcnma)` with
`library(nmatools)`. The rest of nmatools (the frequentist `netmeta` pipeline,
the GUI, and the visualizations) is independent of this chapter.

| Function | Outcome | Effect measure | Likelihood |
|---|---|---|---|
| `bcnma_bin()` | Binary | OR (log-OR in `d[k]`) | Binomial |
| `bcnma_cont()` | Continuous | SMD or MD | Normal (arm-level) |

## Requirements: JAGS, rjags, MCMCvis

Model fitting needs the **JAGS** system library and two R packages that
nmatools lists under *Suggests*, so they are not installed automatically and
nothing else in nmatools needs them:

1. Install JAGS from <https://mcmc-jags.sourceforge.io/> (on macOS also
   available via Homebrew: `brew install jags`).
2. Install the R packages:

```r
install.packages(c("rjags", "MCMCvis"))
install.packages("ggrepel")   # optional: non-overlapping labels in plot_interactions()
```

Without them, `bcnma_bin()` and `bcnma_cont()` stop at once with an error
naming what to install. The index helpers (`place()`, `which.place()`,
`all.interactions()`) and the methods for an already-fitted object work
without JAGS.

## Input data

Long format, **one row per arm per study**. The `treat` column lists the
components of each arm joined by `component_separator` (default `"+"`), and
`components` is the character vector of all component abbreviations (any
number; their order defines `d[1]`, `d[2]`, ...).

| study | treatment_component | r | n |
|---|---|---|---|
| Smith2001 | se+sd+cr | 20 | 50 |
| Smith2001 | w | 10 | 50 |
| Jones2010 | se+sd+cr+th+sr+sc | 30 | 60 |
| Jones2010 | se | 15 | 55 |

For `bcnma_cont()` the outcome columns are `mean`, `sd`, and `n` instead of
`event` and `n`. Column arguments may be given unquoted (`studlab = study`) or
quoted.

The CBT-I component NMA dataset (Furukawa et al. 2024, *JAMA Psychiatry*) is
bundled as an example:

```r
library(nmatools)
library(dplyr)

dat_raw <- read.csv(system.file("extdata", "bcnma", "data_CBTICNMA.csv",
                                package = "nmatools"))
names(dat_raw)[1] <- "study"   # fix the BOM character in the first column name

dat <- dat_raw |>
  select(study, treatment_component, r, n) |>
  group_by(study, treatment_component) |>
  summarise(r = sum(r), n = sum(n), .groups = "drop")

components <- c("se", "sd", "cr", "th", "cw", "sr", "sc", "re", "pi",
                "w",  "ns", "he", "tg", "ind", "gp", "ff", "ae")
```

A complete worked script is installed with the package:
`system.file("extdata", "bcnma", "example_CBTICNMA.R", package = "nmatools")`.

## Fitting a model

```r
fit <- bcnma_bin(
  data         = dat,
  studlab      = study,
  treat        = treatment_component,
  event        = r,
  n            = n,
  components   = components,
  interactions = "lasso",
  seed         = 42
)
print(fit)
```

For a continuous outcome:

```r
fit_cont <- bcnma_cont(
  data         = dat_cont,
  studlab      = study,
  treat        = treatment_component,
  mean         = mean_change,   # arm mean (e.g. change from baseline)
  sd           = sd_change,     # arm SD
  n            = n,
  components   = components,
  sm           = "SMD",         # "SMD" (default) or "MD"
  pooled_sd    = "within",      # pool the SD over all arms (default) or "control"
  interactions = "lasso",
  seed         = 42
)
```

With `sm = "SMD"`, each study's arm means and standard errors are divided by
the within-study pooled SD and multiplied by Hedges' small-sample correction
J = 1 − 3/(4 df − 1), so every within-study contrast is Hedges' *g* and
`d[k]` is on the SMD scale. The pooled SD is treated as known. With
`pooled_sd = "control"` the SD of **arm 1** — the first row of the study in
`data` — is used, so the data order defines the comparator. Every arm needs
a positive SD and *n*; a missing or non-positive SD is an error. Change
scores and final values are not distinguished: do not mix them on the SMD
scale without a deliberate choice. `sm = "MD"` keeps
the raw scale but centres each study's means on its arm 1 (an exact location
shift that the study baseline absorbs), and scales the default priors by the
median within-study pooled SD *s*: `d[k] ~ N(0, (10 s)^2)` and a half-normal
prior on the SD tau with scale *s* (instead of the log-normal prior on tau²
used for SMD and OR). Supply `tau_prior = list(family = "half_normal_tau",
sd = ...)` or `list(family = "lognormal_tau2", meanlog = ..., precision =
...)` to override it; the prior used is stored in
`fit$model_settings$tau_prior`.

### Interaction models

| `interactions` | Model |
|---|---|
| `"none"` | Fully additive: a combination's effect is the sum of its components' effects. |
| `"lasso"` (default) | Every pairwise interaction gets a spike-and-slab (SSVS) prior with equal inclusion probability `lasso_prob` (default 0.5); the data decide which interactions matter. |
| `"lasso_informative"` | Only interactions among `active_components` get SSVS priors; all others are fixed to zero. |

The additive model is always fitted as well, so `print(fit)` shows the
additive and the LASSO estimates side by side.

### Key arguments

| Argument | Default | Description |
|---|---|---|
| `component_separator` | `"+"` | Separator in the `treat` column. |
| `active_components` | `NULL` | Required for `"lasso_informative"`. |
| `lasso_prob` | `0.5` | Prior probability that an interaction is included. |
| `g` | `100` | Spike-slab variance ratio (slab variance = `g` × spike variance). |
| `tau_prior` | `NULL`: log-normal `c(-1.67, 1/1.472^2)` on τ² (binary), `c(-1.0, 1/0.8^2)` on τ² (SMD), half-normal on τ with SD = median pooled SD (MD) | Prior on the between-study heterogeneity; log-normal priors are on the **variance** τ² (JAGS node `tau2`) and `tau = sqrt(tau2)` is reported as the SD. The binary and SMD defaults are package defaults labelled *not from a (verified) published category*: prefer a published, outcome-specific prior (Turner et al. 2015 for log-OR; Rhodes et al. 2015 for SMD). |
| `n.chains`, `n.adapt`, `n.burnin`, `n.iter`, `n.thin` | `3`, `2000`, `5000`, `20000`, `1` | MCMC settings (4 chains are advisable for LASSO models). |
| `seed` | `NULL` | Random seed for reproducibility. |
| `component_values` | `NULL` | Optional component values (doses); see below. |
| `modifiers` | `NULL` | Optional arm-level effect modifiers; see below. |

## Results

```r
print(fit)                       # component effects, additive and LASSO models
print(fit, model = "lasso")      # one model only
summary(fit)                     # adds the call, MCMC settings, modifier terms
forest(fit)                      # component OR (or SMD / MD) forest plot (ggplot2)
plot_interactions(fit)           # median gamma vs. inclusion probability
summarise_interactions(fit)      # data frame of all pairs, sorted by |gamma|
```

`forest()` is the `meta::forest()` generic, re-exported by nmatools; the
`bcnma_bin` and `bcnma_cont` methods return a `ggplot` object invisibly.

Each component row is the effect of that component against "component
absent", pooled over the network: log-OR and OR with 95% credible interval
(CrI) for `bcnma_bin()`, SMD or MD for `bcnma_cont()`; `tau (SD)` is the
between-study standard deviation and `tau2 (variance)` its square (the prior
is placed on `tau2`). For interactions, `inclusion_prob` is the posterior
probability that the interaction is non-negligible (values ≥ 0.5 are
conventionally "selected"), and `median_gamma` its size and direction:
negative means the combination works less well than the sum of its parts,
positive means better. If the additive and LASSO component estimates are
similar, interactions add little; if they diverge, prefer the LASSO model for
comparing combinations.

### Comparing combinations

`compare_combinations()` gives the posterior effect of combination A against
combination B, including the main effects, the interaction terms of the LASSO
model, and any effect modifiers:

```r
# Full CBT-I package vs. sleep hygiene only
compare_combinations(fit,
  combo1 = c("se", "sd", "cr", "th", "sr", "sc"),
  combo2 = c("se"))

# A combination vs. no component
compare_combinations(fit, combo1 = c("cr", "th", "sr"), combo2 = character(0))

# Additive model only (no interaction adjustment)
compare_combinations(fit, combo1 = c("se", "sd", "cr", "th"), combo2 = "se",
                     model = "additive")
```

The full posterior draws are returned in `$effect_samples`; the raw samples
are in `fit$samps_additive` and `fit$samps_lasso` (columns `d[k]`, `ORd[k]`,
`gamma[k]`, `Ind[k]`, `tau`, `tau2`, ...). `place()`, `which.place()` and
`all.interactions()` map component pairs to interaction indices:

```r
k <- place("cr", "th", Nc = length(components), components = components)
quantile(exp(fit$samps_lasso[[paste0("gamma[", k, "]")]]), c(0.025, 0.5, 0.975))
which.place(k, components)
```

## Component values (doses) and effect modifiers

Two optional arguments extend the linear predictor. With both left at `NULL`
the model is exactly the plain cNMA above (identical to bcnma 0.1.0). For arm
*k* of study *i* (arm 1 is the comparator):

```
mean[i,k] = sum_j d[j]    * (x[i,k,j] - x[i,1,j])
          + sum_q beta[q] * (w[i,k,q] - w[i,1,q])
          [+ SSVS interaction terms gamma]
```

### `component_values`

A numeric matrix (or data frame) with `nrow(data)` rows in the order of
`data` and column names exactly equal to `components` — for example the
number of sessions of each component. It replaces the 0/1 indicators parsed
from `treat`, so `d[j]` becomes the effect **per unit** of component *j*
(linear dose-response). The `treat` column is still required; a warning is
given when a column is zero in every arm or when the values and the `treat`
label disagree on presence. SSVS interactions are always defined on presence
(value ≠ 0).

### `modifiers`

A list of specifications, each adding terms that modify the effect of one or
more components according to an arm-level covariate:

```r
modifiers = list(
  list(column     = "proficiency",      # arm-level column of `data`
       components = c("th", "cr"),      # one term per component (and level step)
       type       = "continuous",
       center     = 3,                  # default: mean over arms with the component
       prior_sd   = 10,                 # beta ~ N(0, prior_sd^2); default 10
       label      = "prof"),            # name in outputs and in `at` (default: column)
  list(column     = "therapist_level",
       components = "th",
       type       = "ordinal",
       levels     = c("trainee", "certified", "expert"),   # lowest first; required
       monotone   = "increasing",       # "none" (default), "increasing", "decreasing"
       label      = "skill")
)
```

| Type | Terms per component | Code where the component is present | Meaning of `beta` |
|---|---|---|---|
| continuous | 1 | value − `center` | Change in the component effect per covariate unit. |
| ordinal (*L* levels) | *L* − 1 | term *l*: 1 if level ≥ *l* + 1 (cumulative coding) | Increment from level *l* to *l* + 1. |

`monotone = "increasing"` gives every ordinal increment a half-normal prior
(`"decreasing"` the mirror image). `NA` in the covariate is allowed only in
arms where the modified component is absent.

### Interpretation

- **`d[c]` of a modified component is its effect at the center** (continuous)
  **or at the reference, lowest level** (ordinal), not an average over the
  covariate. The effect at value *v* is `d[c] + beta * (v - center)`; at
  ordinal level *m* it is `d[c]` plus the increments below *m*.
- `beta` is on the log-OR scale for `bcnma_bin()` (an OR column is also
  printed) and on the SMD / MD scale for `bcnma_cont()`.
- Results are in `fit$modifier_terms` (term definitions) and
  `fit$modifier_effects$additive` / `$lasso` (posterior median and 95% CrI);
  `print()` and `summary()` show both.
- `compare_combinations()` accepts named numeric combinations (component
  values, e.g. `c(th = 2, se = 1)`) and evaluates modified components at
  `at = list(label = value)`. Both combinations use the same `at`; modifiers
  not named are evaluated at their center / reference level.

```r
fit_mod <- bcnma_bin(
  data = dat, studlab = study, treat = treatment_component, event = r, n = n,
  components   = components,
  interactions = "none",
  modifiers    = list(
    list(column = "therapist_level", components = "th", type = "ordinal",
         levels = c("trainee", "certified", "expert"), monotone = "increasing",
         label = "skill"))
)
fit_mod$modifier_effects$additive

# 'th' delivered by an expert vs. no component
compare_combinations(fit_mod, combo1 = "th", model = "additive",
                     at = list(skill = "expert"))
```

Section 6 of the bundled `example_CBTICNMA.R` uses programme duration as a
continuous modifier of sleep restriction.

### Caveat: between-study (ecological) information

When the covariate takes the same value in every arm of a study — typical of
a provider's proficiency or a study-level programme duration — `beta` is
identified only by how the component's within-study contrast varies **across
studies**. Such estimates are observational: they are open to ecological bias
and to confounding by other study characteristics, even though the component
effects themselves rest on randomised contrasts. The fitting functions print
a note to this effect whenever modifiers are used. A term whose within-study
contrast is zero in every study (for example, the component appears in all
arms at the same level) is not identifiable; a warning is given and its
posterior is the prior.

## References

- Furukawa Y, Sakata M, Yamamoto R, et al. Components and delivery formats of
  cognitive behavioral therapy for chronic insomnia in adults: a systematic
  review and component network meta-analysis. *JAMA Psychiatry*.
  2024;81(4):357–365. https://doi.org/10.1001/jamapsychiatry.2023.5060
- Turner RM, Jackson D, Wei Y, Thompson SG, Higgins JPT. Predictive
  distributions for between-study heterogeneity and simple methods for their
  application in Bayesian meta-analysis. *Stat Med*. 2015;34(6):984–998.
- Rhodes KM, Turner RM, Higgins JPT. Predictive distributions were developed
  for the extent of heterogeneity in meta-analyses of continuous outcome data.
  *J Clin Epidemiol*. 2015;68(1):52–60.
- George EI, McCulloch RE. Variable selection via Gibbs sampling. *J Am Stat
  Assoc*. 1993;88(423):881–889.

The SSVS approach for component NMA was developed with conceptual guidance
from Orestis Efthimiou.

---
Prev: [16. Function reference](16-function-reference.md) · Next: [Manual home](README.md)
