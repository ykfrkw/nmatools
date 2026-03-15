# ============================================================
# NMA Analysis Template — powered by nmatools
# ============================================================
# Copy this file to your project root and edit as needed.
# Recommended project layout:
#
#   my_project/
#   ├── data/          <- arm-level input data (CSV / Excel)
#   ├── outputs/       <- netmetawrap() writes here automatically
#   ├── utils/         <- helper scripts (this file goes here)
#   └── run_analysis.R <- main script (source this file)
# ============================================================

library(nmatools)

# ── 1. Load data ──────────────────────────────────────────────────────────────
# Replace with your own data:
#   d <- readxl::read_xlsx("data/mydata.xlsx")
#   d <- read.csv("data/mydata.csv")
d <- nmatools::load_w2i()   # built-in sample data

# ── 2. Define outcomes ────────────────────────────────────────────────────────
# Each list element = one outcome.
# Keys in params_list override .default_args.

params_list <- list(
  list(
    outcome      = "remission_lt",       # output folder name
    event        = "r",                  # event column (binary)
    small.values = "undesirable"         # higher OR = better remission
  ),
  list(
    outcome      = "dropout_lt",
    event        = "n_dropout",
    small.values = "desirable"           # higher OR = more dropout = bad
  ),
  list(
    outcome      = "remission_pt",
    event        = "r_pt",
    small.values = "undesirable"
  ),
  list(
    outcome      = "dropout_pt",
    event        = "n_dropout_pt",
    small.values = "desirable"
  )
)

# ── 3. Run batch ──────────────────────────────────────────────────────────────
run_nma_batch(
  params_list   = params_list,
  .default_args = list(
    data            = d,
    studlab         = id,                # unquoted column name
    treat           = t,
    n               = n,
    sm              = "OR",
    reference.group = "Pharmacotherapy",
    path            = "./outputs"        # created automatically
  )
)

# ── 4. Single outcome (alternative) ──────────────────────────────────────────
# netmetawrap(
#   data            = d,
#   studlab         = id,
#   treat           = t,
#   outcome         = "remission_lt",
#   n               = n,
#   event           = r,
#   sm              = "OR",
#   reference.group = "Pharmacotherapy",
#   small.values    = "undesirable",
#   path            = "./outputs",
#   # Override defaults if needed:
#   forest_args     = list(leftcols = c("studlab", "n.trts", "rob"))
# )
