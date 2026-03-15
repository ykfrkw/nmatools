# ============================================================
# nmatools — Quick-Start Sample Script
# ============================================================
# 前提: nmatools.Rproj を開く → 作業ディレクトリが自動でパッケージルートになる
# 使い方: このファイルを開き、Cmd+Enter で 1 行ずつ実行する
# ============================================================

# ── Step 1: パッケージを開発モードで読み込む ─────────────────────────────────
devtools::load_all()   # インストール不要。R/ 以下のすべてを即座にロード

# ── Step 2: サンプルデータを確認する ─────────────────────────────────────────
d <- load_w2i()
head(d)
# id            t    n  r  n_dropout  r_pt  n_dropout_pt  rob  indirectness
# Gross2011  CBT-I  20  9          3    12             2    L             1
# Gross2011  Pharmacotherapy  10  4   2     5             1    L             1
# ...

# 列名・治療の確認
colnames(d)
table(d$t)   # 治療の種類と行数

# ── Step 3: 1 アウトカム（単発実行）────────────────────────────────────────
# studlab = id のように列名をクォートなしで指定できる（meta::pairwise と同じ書き方）
netmetawrap(
  data            = d,
  studlab         = id,              # 列名はクォートなしでOK
  treat           = t,
  outcome         = "remission_lt",  # 出力フォルダ名にも使われる
  n               = n,
  event           = r,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",   # "desirable" = 小さいほど良い（dropout など）
  path            = "./outputs"      # なければ自動作成。省略可（デフォルト）
)
# → outputs/remission_lt/ に結果がまとめて出力される

# ── Step 4: 複数アウトカムを一括実行（binary のみ）───────────────────────────
# アウトカムごとに変わる引数だけ params_list に書く
# 列名・sm など共通の引数は .default_args に一度だけ書く
params_list <- list(
  # binary アウトカム: n + event + sm を指定
  list(outcome = "remission_lt",  n = "n", event = "r",            sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_lt",    n = "n", event = "n_dropout",    sm = "OR", small.values = "desirable"),
  list(outcome = "remission_pt",  n = "n", event = "r_pt",         sm = "OR", small.values = "undesirable"),
  list(outcome = "dropout_pt",    n = "n", event = "n_dropout_pt", sm = "OR", small.values = "desirable")
)

run_nma_batch(
  params_list   = params_list,
  .default_args = list(
    data            = d,
    studlab         = "id",          # 文字列でも可（do.call 経由のため）
    treat           = "t",
    reference.group = "Pharmacotherapy",
    path            = "./outputs"
  )
)
# → outputs/ 以下に 4 つのサブディレクトリが作成される

# ── Step 4b: binary + continuous 混在の場合 ───────────────────────────────────
# continuous アウトカム: n + mean_col + sd_col + sm を指定する
# （以下は架空のデータ例。実際のデータに合わせて列名を変更する）
params_mixed <- list(
  list(
    outcome      = "remission",
    n            = "n",
    event        = "r",
    sm           = "OR",
    small.values = "undesirable"
  ),
  list(
    outcome      = "sleep_efficiency",   # continuous アウトカム
    n            = "n_cont",
    mean_col     = "se_mean",
    sd_col       = "se_sd",
    sm           = "SMD",
    small.values = "desirable"
  )
)

# run_nma_batch(
#   params_list   = params_mixed,
#   .default_args = list(
#     data    = my_data,              # 実際のデータフレームに置き換え
#     studlab = "study",
#     treat   = "treatment",
#     path    = "./outputs"
#   )
# )

# ── Step 5: 引数のカスタマイズ例 ─────────────────────────────────────────────
netmetawrap(
  data            = d,
  studlab         = id,
  treat           = t,
  outcome         = "remission_lt_custom",
  n               = n,
  event           = r,
  sm              = "OR",
  reference.group = "Pharmacotherapy",
  small.values    = "undesirable",
  path            = "./outputs",
  # forest() へ渡す引数を上書き
  forest_args = list(
    leftcols = c("studlab", "n.trts")
  ),
  # netmetabin() / netmeta() へ渡す引数を上書き
  netmeta_args = list(
    incr = 0.5   # continuity correction を 0.001 → 0.5 に変更
  )
)

# ── Step 6: 出力フォルダを開く ────────────────────────────────────────────────
system("open outputs")   # macOS: Finder で outputs/ を開く
