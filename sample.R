# ============================================================
# nmatools — Quick-Start Sample Script
# ============================================================
# Setup: open nmatools.Rproj so the working directory is the package root.
# Usage: open this file and run line-by-line with Cmd+Enter.
# ============================================================

# ── Step 1: load the package in development mode ────────────────────────────
devtools::load_all()   # No install required; loads everything under R/.

# ── Step 2: inspect the bundled sample data ─────────────────────────────────
d <- load_w2i()
head(d)
# id            t    n  r  n_dropout  r_pt  n_dropout_pt  rob  indirectness
# Gross2011  CBT-I  20  9          3    12             2    L             1
# Gross2011  Pharmacotherapy  10  4   2     5             1    L             1
# ...

# Column names and treatment counts
colnames(d)
table(d$t)   # treatments and the number of arm rows per treatment

# ── Step 3: single outcome (one-shot run) ───────────────────────────────────
# Column names are quoted strings everywhere in nmatools.
netmetawrap(
  data            = d,
  studlab         = "id",
  treat           = "t",
  outcome         = "remission_lt",  # also used as the output folder name
  n               = "n",
  event           = "r",
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",   # "desirable" = smaller is better (e.g. dropout)
  path            = "./outputs"      # auto-created if missing; default if omitted
)
# → all results land under outputs/remission_lt/

# ── Step 4: batch run across multiple outcomes (binary only) ────────────────
# Put per-outcome arguments in params_list.
# Put shared arguments (column names, sm, etc.) once, directly in the call.
params_list <- list(
  # binary outcomes: n + event + sm
  list(outcome = "remission_lt",  n = "n", event = "r",            sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_lt",    n = "n", event = "n_dropout",    sm = "OR", small.values = "desirable"),
  list(outcome = "remission_pt",  n = "n", event = "r_pt",         sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_pt",    n = "n", event = "n_dropout_pt", sm = "OR", small.values = "desirable")
)

run_nma_batch(
  params_list,
  data            = d,
  studlab         = "id",
  treat           = "t",
  reference.group = "Pharmacotherapy",
  path            = "./outputs"
)
# → outputs/ ends up with four sub-directories, one per outcome
# Returned invisibly as a list named by outcome, e.g. res[["remission_lt"]].

# The older .default_args style does exactly the same thing:
# run_nma_batch(
#   params_list   = params_list,
#   .default_args = list(
#     data            = d,
#     studlab         = "id",
#     treat           = "t",
#     reference.group = "Pharmacotherapy",
#     path            = "./outputs"
#   )
# )

# ── Step 4b: mixed binary + continuous outcomes ─────────────────────────────
# Continuous outcomes need n + mean_col + sd_col + sm.
# (Columns below are illustrative; rename to match your data.)
params_mixed <- list(
  list(
    outcome      = "remission",
    n            = "n",
    event        = "r",
    sm           = "OR",
    small.values = "undesirable"
  ),
  list(
    outcome      = "sleep_efficiency",   # continuous outcome
    n            = "n_cont",
    mean_col     = "se_mean",
    sd_col       = "se_sd",
    sm           = "SMD",
    small.values = "desirable"
  )
)

# run_nma_batch(
#   params_mixed,
#   data    = my_data,                # replace with your real data frame
#   studlab = "study",
#   treat   = "treatment",
#   path    = "./outputs"
# )

# ── Step 5: overriding default arguments ────────────────────────────────────
netmetawrap(
  data            = d,
  studlab         = "id",
  treat           = "t",
  outcome         = "remission_lt_custom",
  n               = "n",
  event           = "r",
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",
  path            = "./outputs",
  # override arguments passed to forest()
  forest_args = list(
    leftcols = c("studlab", "n.trts")
  ),
  # override arguments passed to netmetabin() / netmeta()
  netmeta_args = list(
    incr = 0.5   # raise the continuity correction from 0.001 to 0.5
  )
)

# ── Step 6: open the output folder ──────────────────────────────────────────
system("open outputs")   # macOS: opens outputs/ in Finder

# ============================================================
# See sample_viz.R for examples of the visualization functions
# (color_league, color_forest, color_netgraph, kilim, vitruvian,
# min_context, part_context).
# ============================================================
