## ============================================================
## 累積リスク解析（一括実行版：解析・論文の図表・補足図表）
##   旧 A2A3analysis05_cumrisk_all.R / 06_cumrisk_upset.R / 07_cumrisk_figures.R /
##   08_cumrisk_supplement.R を 1 本にまとめたもの
##
##   Case = A2 (case = 1), Control = CT (case = 0)
##   連続変数は CT 群・性別ごとの Mean + 2SD で二値化
##
##   1回の実行で ANALYSES の解析（主解析・A・B）をすべて行い、それぞれの中で
##   感度分析（欠損・閾値・精神疾患の除外・連続値を残した得点）も行う
##
##   総リスク数（表4）の結果は、群間比較（A2A3analysis03）と同じく
##   交互作用 → 群の主効果（男女を同じ重みで平均）→ 性別ごとの群差 の順に示す。
##   群ごとの性差は参考として出力するが、解釈しない
##
##   出力（OUT_DIR）
##     cumrisk_tables.xlsx   … 論文の表と、図の元の数値（1つの表・図 = 1シート）
##       Table 3 / Table 4 / Table 5 / Table S4 / Table S5 / Table S6 / Table S7 / Table S8 / Table S10
##       Distribution values / OR values / Distribution AB values / UpSet values / LOO values / Checks
##     venn_values_<解析>.xlsx … ベン図の数値（以前の venn_values.xlsx と同じ形）
##       main = 図5、CTonly = 図 S11、bio = 図 S12
##       シート F_domain / F_iii / M_domain / M_iii（label, Case, sig, Control）と all_numbers
##     cumrisk_numeric.xlsx  … 確認用の数値（丸める前の値）
##     作業用の図（図番号は付けない）
##       distribution_main.pdf, distribution_AB.pdf, forest_OR.pdf (+ forest_OR_per_factor / forest_OR_domains),
##       upset_<解析>.pdf, leave_one_out.pdf
##     submission/ … 論文投稿用の図（タイトル・注なし、Arial 7–8 pt、Scientific Reports の段組の幅）
##       Fig3  総リスク数の分布（主解析）               Fig4  1因子あたり OR（A）と5領域の OR（B）
##       FigS8 総リスク数の分布（データセット A・B）    FigS9 leave-one-out
##       FigS10 5領域の組み合わせ（UpSet プロット、主解析）
##       （ragg パッケージがあれば 600 dpi の TIFF も出力）
##     scored_venn_<解析>.csv … 参加者ごとの因子・領域・ベン図の領域（以前の scored_venn.csv）
##     cumrisk_scored.rds    … 解析ごとの参加者単位のデータ（因子の 0/1、総リスク数など）
##     cumrisk_sessioninfo.txt
##
##   表・補足表の番号は改訂原稿に合わせている
##     Table 3  個別因子・領域・下位領域（性別ごと、Fisher）     Table S4  表3のデータセット A・B
##     Table 4  総リスク数（線形モデル）                         Table S5  表4の補足（A・B、未調整、負の二項）
##     Table 5  ロジスティック回帰モデル 1〜5                     Table S6  総リスク数のカテゴリ別の分布と OR
##                                                              Table S7  解析内の感度分析（図4a の数値）
##                                                              Table S8  頑健性とモデル3の診断
##                                                              Table S10 連続変数の閾値
##     （Table 1 参加者の特徴、Table 2 変数一覧、Table S1〜S3 は別に作成）
##     欠測の表（Table S9）は A2A3analysis10_missing.R で作成する
## ============================================================

rm(list = ls())
Sys.setenv(LANGUAGE = "en_US.UTF-8")

library(tidyverse)
library(logistf)      # Firth のロジスティック回帰
library(emmeans)      # 表4：交互作用・群の主効果・性別ごとの群差と効果量 d
# sandwich（HC3）と MASS（負の二項回帰）は sandwich:: / MASS:: で呼び出す
library(exact2x2)     # Fisher の正確検定（P 値と一致する信頼区間、条件付き最尤推定のオッズ比）
library(writexl)      # 確認用の数値 (cumrisk_numeric.xlsx)
library(openxlsx)     # 書式を整えた表 (cumrisk_tables.xlsx)
library(patchwork)    # 図の結合

# 群の主効果（性別について平均）は意図した推定なので、交互作用の注意を表示しない
emm_options(msg.interaction = FALSE)


## ============================================================
## 0. 設定
## ============================================================

OUT_DIR <- "Results/cumrisk"

# 実行する解析（上から順に実行。主解析を必ず1行目に置く）
#   group_type: "CTA3" = A3 を CT に含める / "CT" = A3 を除外
#   subset    : "all"  = 全員 / "bio" = 非血縁関係のある回答者を除外
ANALYSES <- tribble(
  ~name,    ~group_type, ~subset, ~short, ~long,
  "main",   "CTA3",      "all",   "Main", "Main analysis",
  "CTonly", "CT",        "all",   "A",    "A: parenting program participants excluded",
  "bio",    "CTA3",      "bio",   "B",    "B: participants with a non-kin relationship excluded"
)

THR_REF       <- "own"   # A・B での閾値: "own" = その解析の CT 群から計算 / "main" = 主解析の閾値に固定
THR_STRICT    <- TRUE    # TRUE: x > 閾値 を該当 / FALSE: x >= 閾値
MISSING_RULE  <- "zero"  # 累積指標での欠損（主解析）: "zero" = 該当なし
P_ADJUST      <- "BH"    # 多重比較の補正方法（p.adjust の method）
FDR_BY_GENDER <- TRUE    # TRUE: 性別ごとに補正
GENDER_LEVELS <- c("F", "M")
GENDER_LABELS <- c(F = "Women", M = "Men")
MALE_LABEL    <- "M"
CASE_GROUP    <- "A2"

K_BREAKS <- c(-Inf, 0, 1, 2, 4, Inf)        # 総リスク数のカテゴリ
K_LABELS <- c("0", "1", "2", "3–4", "5+")
TOP_ITEM <- "nonkin"                        # 頑健性の確認で別に入れる因子
N_BOOT   <- 200                             # モデル3のブートストラップ回数（0 で省略）
SEED     <- 20261016

# 解析内の感度分析（結果を見る前に決めておく）
#   threshold: 連続変数の閾値 / missing: 欠損の扱い / drop: 除く因子
#   score    : "count" = 0/1 の合計 / "pctl" = 連続変数を CT 群（同性）での順位（0〜1）で加える
SENS_SPECS <- tribble(
  ~spec,             ~threshold, ~missing,     ~drop,    ~score,
  "main",            "m2sd",     MISSING_RULE, NA,       "count",
  "missing_adverse", "m2sd",     "adverse",    NA,       "count",   # Case → 0, CT → 1
  "thr_m1sd",        "m1sd",     MISSING_RULE, NA,       "count",
  "thr_p90",         "p90",      MISSING_RULE, NA,       "count",
  "drop_mental",     "m2sd",     MISSING_RULE, "mental", "count",
  "pctl_score",      "m2sd",     MISSING_RULE, NA,       "pctl"
)
SPEC_LABELS <- c(main            = "Main specification",
                 missing_adverse = "Missing: least favorable",
                 thr_m1sd        = "Threshold: mean + 1 SD",
                 thr_p90         = "Threshold: 90th percentile",
                 drop_mental     = "Mental disorders excluded",
                 pctl_score      = "Continuous factors not dichotomized")

# 因子の定義（表3の行順）
#   domain/sub: 領域・下位領域（表3・ベン図）
#   d5        : 5領域（モデル3・4、UpSet）D1 発達 / D2 神経生物 / D3 非血縁 / D4 子の要因 / D5 その他の養育困難
#   unit/digits: 閾値の表示用（連続変数のみ）
ITEMS <- tribble(
  ~var,            ~label,                                  ~domain, ~domain_label,                  ~sub,   ~sub_label,                                          ~d5,  ~type,  ~unit,   ~digits,
  "U15_pat_abs",   "U15 Paternal absence",                  "i",     "(i) Developmental factors",    NA,     NA,                                                  "D1", "cont", "years", 1L,
  "U15_mat_abs",   "U15 Maternal absence",                  "i",     "(i) Developmental factors",    NA,     NA,                                                  "D1", "cont", "years", 1L,
  "neglect",       "Neglect",                               "i",     "(i) Developmental factors",    NA,     NA,                                                  "D1", "cont", "",      2L,
  "phys_abuse",    "Physical abuse",                        "i",     "(i) Developmental factors",    NA,     NA,                                                  "D1", "cont", "",      2L,
  "early_1st",     "Early age at having 1st child",         "i",     "(i) Developmental factors",    NA,     NA,                                                  "D1", "bin",  NA,      NA,
  "sensory",       "Sensory impairment",                    "ii",    "(ii) Neurobiological factors", NA,     NA,                                                  "D2", "bin",  NA,      NA,
  "dev",           "Developmental disorders",               "ii",    "(ii) Neurobiological factors", NA,     NA,                                                  "D2", "bin",  NA,      NA,
  "mental",        "Mental disorders",                      "ii",    "(ii) Neurobiological factors", NA,     NA,                                                  "D2", "bin",  NA,      NA,
  "head",          "Head/neck trauma",                      "ii",    "(ii) Neurobiological factors", NA,     NA,                                                  "D2", "bin",  NA,      NA,
  "substance",     "Substance abuse",                       "ii",    "(ii) Neurobiological factors", NA,     NA,                                                  "D2", "bin",  NA,      NA,
  "nonkin",        "Non-kin relation",                      "iii",   "(iii) Environmental factors",  "iii1", "(iii-1) Non-kin relationship",                      "D3", "bin",  NA,      NA,
  "children4",     "4 or more children",                    "iii",   "(iii) Environmental factors",  "iii2", "(iii-2) Child factors",                             "D4", "bin",  NA,      NA,
  "multiple",      "Multiple birth",                        "iii",   "(iii) Environmental factors",  "iii2", "(iii-2) Child factors",                             "D4", "bin",  NA,      NA,
  "child_med",     "Medical issues of the child",           "iii",   "(iii) Environmental factors",  "iii2", "(iii-2) Child factors",                             "D4", "bin",  NA,      NA,
  "economic",      "Economic hardship",                     "iii",   "(iii) Environmental factors",  "iii3", "(iii-3) Other hardships in the rearing conditions", "D5", "bin",  NA,      NA,
  "debt",          "Debt burden",                           "iii",   "(iii) Environmental factors",  "iii3", "(iii-3) Other hardships in the rearing conditions", "D5", "bin",  NA,      NA,
  "single_parent", "Child having a legally single parent",  "iii",   "(iii) Environmental factors",  "iii3", "(iii-3) Other hardships in the rearing conditions", "D5", "bin",  NA,      NA,
  "isolation",     "Isolated parenting",                    "iii",   "(iii) Environmental factors",  "iii3", "(iii-3) Other hardships in the rearing conditions", "D5", "cont", "",      2L,
  "ipv",           "Experiencing IPV",                      "iii",   "(iii) Environmental factors",  "iii3", "(iii-3) Other hardships in the rearing conditions", "D5", "bin",  NA,      NA
)
ITEM_LABELS <- set_names(ITEMS$label, ITEMS$var)
CONT_UNITS  <- c(U15_pat_abs = "years", U15_mat_abs = "years", neglect = "score",
                 phys_abuse = "score", isolation = "score")

D5_LABELS <- c(D1 = "D1 Developmental", D2 = "D2 Neurobiological", D3 = "D3 Non-kin relation",
               D4 = "D4 Child factors", D5 = "D5 Other hardships")

# 図の設定
EST_COLORS   <- c(common = "black", F = "#B07800", M = "#009E73")   # 図4a（OR の図）
EST_SHAPES   <- c(common = 15,      F = 16,        M = 17)
EST_LABELS   <- c(common = "Women and men (common)", F = "Women", M = "Men")
K_COLORS     <- set_names(c("#DCE6F2", "#A9C4E3", "#6F9FD1", "#3B74B5", "#1F4E8C"), K_LABELS)  # 総リスク数の分布の図
UPSET_ANALYSES <- c("main")      # UpSet プロットを描く解析
UPSET_COLORS   <- c(Case = "#D55E00", Control = "#0072B2")
UPSET_LABELS   <- c(cnt_D1 = "Developmental", cnt_D2 = "Neurobiological", cnt_D3 = "Non-kin relation",
                    cnt_D4 = "Child factors", cnt_D5 = "Other hardships")

# 元データ（fulldata）から解析用の変数を作る
make_vars <- function(fulldata) {
  fulldata %>%
    transmute(
      ID, Group, Gender,
      age = age_subject,

      U15_pat_abs = `B3_01_15歳まで実父不在年数`, # year
      U15_mat_abs = `B3_02_15歳まで実母不在年数`, # year
      neglect     = `C2_01_ネグレクトスコア`,     # score
      phys_abuse  = `C2_04_身体的虐待スコア`,     # score
      early_1st   = as.numeric(`I1_04_第一子出生or同居開始年齢` <= 20),

      sensory   = as.numeric(`E2_00_感覚障害15歳まで` == 1 | `E3_00_感覚障害16歳から` == 1),
      dev       = as.numeric(`E2_01_発達障害15歳まで` == 1 | `E3_01_発達障害16歳から` == 1),
      mental    = as.numeric(`E2_02_他精神疾患15歳まで` == 1 | `E3_02_他精神疾患16歳から` == 1),
      head      = as.numeric(`E2_05_頭部外傷15歳まで` == 1 | `E3_05_頭部外傷16歳から` == 1),
      substance = as.numeric(`E4_02_物質嗜癖` == 1),

      nonkin = `I4_03_自分orパートナーと非血縁の子`,

      children4 = `I3_08_4人以上同時養育`,
      multiple  = `I3_09_多胎`,
      child_med = as.numeric(`J1_02_病気障害発達の遅れスコア` == 1),

      economic = case_when(
        `H2_01_生保非課税でないが大人2人子2人300万以下` == 1 |
          grepl("生活保護|非課税", `A4_03_回答者の世帯経済状況`)      ~ 1,         # どちらかで該当が確定
        `A4_03_回答者の世帯経済状況` == "不明" |
          is.na(`A4_03_回答者の世帯経済状況`) |
          is.na(`H2_01_生保非課税でないが大人2人子2人300万以下`)      ~ NA_real_,  # 判断できない
        TRUE                                                            ~ 0
      ),
      debt          = `H2_02_借金による困難有無`,
      single_parent = `I2_01_当時子の親がひとり親`,
      isolation     = `I2_05_孤立した子育てスコア`, # score
      ipv           = `I2_06_DV被害`
    )
}

# ベン図の定義（図の配置に合わせた領域ラベル）
#   3つの円: left / top / right（left_top = 左∩上のみ, top_right = 上∩右のみ,
#            left_right = 左∩右のみ, all = 3つすべて）
#   2つの円: left / right（left_right = 左∩右）
make_venns <- function(subset) {
  v <- list(
    domain = list(
      left = "dom_i", top = "dom_ii", right = "dom_iii",
      set_labels    = c(left = "i_Development01", top = "ii_Neuro01",
                        right = "iii_Environment01"),
      region_labels = c(left = "A", top = "B", right = "C",
                        left_top = "D", top_right = "E", left_right = "F", all = "G")
    ),
    iii = list(
      left = "sub_iii2", top = "sub_iii1", right = "sub_iii3",
      set_labels    = c(left = "iii2_ChildFactor01", top = "iii1_NonKin01",
                        right = "iii3_OtherHardships01"),
      region_labels = c(left = "b", top = "a", right = "c",
                        left_top = "d", top_right = "f", left_right = "e", all = "g")
    )
  )
  # 非血縁関係を除外した解析では iii1 の円が空になるので2つの円にする
  if (subset == "bio") {
    v$iii <- list(
      left = "sub_iii2", right = "sub_iii3",
      set_labels    = c(left = "iii2_ChildFactor01", right = "iii3_OtherHardships01"),
      region_labels = c(left = "b", right = "c", left_right = "e")
    )
  }
  v
}
SET_VAR_ALIAS <- c(sub_iii1 = "nonkin")   # 表3では sub_iii1 は因子 nonkin として検定


## ============================================================
## 1. 共通の関数
## ============================================================

# ---- 多重比較・欠損・閾値 ----------------------------------------------
adjust_p <- function(p, ok) {               # 推定可能な検定だけを補正
  Padj <- rep(NA_real_, length(p))
  Padj[ok] <- p.adjust(p[ok], method = P_ADJUST)
  Padj
}

fill_missing <- function(x, case, rule) switch(rule,
  zero    = coalesce(x, 0),
  one     = coalesce(x, 1),
  adverse = coalesce(x, if_else(case == 1, 0, 1)))

thr_value <- function(x, method) switch(method,
  m2sd = mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE),
  m1sd = mean(x, na.rm = TRUE) + 1 * sd(x, na.rm = TRUE),
  p90  = quantile(x, 0.90, na.rm = TRUE, names = FALSE))

# 閾値表（Gender, var, method, threshold）で連続変数を二値化
dichotomize_with <- function(dat, vars, thr, thr_method = "m2sd") {
  for (v in vars) {
    th <- thr %>%
      filter(var == v, method == thr_method) %>%
      select(Gender, .th = threshold)
    dat <- dat %>%
      left_join(th, by = "Gender") %>%
      mutate(!!v := as.integer(if (THR_STRICT) .data[[v]] > .th else .data[[v]] >= .th)) %>%
      select(-.th)
  }
  dat
}

# CT 群（同性）のうち x より小さい人の割合（0〜1）。最小値は 0 になる
pct_rank <- function(x, ref) {
  ref <- sort(ref[!is.na(ref)])
  out <- findInterval(x, ref, left.open = TRUE) / length(ref)
  out[is.na(x)] <- NA_real_
  out
}

# ---- 累積指標の作成 ------------------------------------------------------
#   si : 連続変数の *_raw 列を持つデータ
#   戻り値: 因子の 0/1（欠損は missing で埋める）、k（総リスク数）、5領域の数 cnt_D*、
#           該当領域数 breadth、k_cat、dom_* / sub_*（いずれか該当）
build_index <- function(si, thr, itm, threshold = "m2sd", missing = MISSING_RULE,
                        drop = NA, score = "count") {
  itm  <- filter(itm, !var %in% drop)
  cont <- itm$var[itm$type == "cont"]
  d    <- si
  for (v in cont) d[[v]] <- d[[str_c(v, "_raw")]]

  if (score == "count") {
    d <- dichotomize_with(d, cont, thr, threshold)
  } else {
    d <- d %>%
      group_by(Gender) %>%
      mutate(across(all_of(cont), ~ pct_rank(.x, .x[case == 0]))) %>%
      ungroup()
  }

  d <- d %>% mutate(across(all_of(itm$var), ~ fill_missing(.x, case, missing)))
  d$k <- rowSums(d[itm$var])

  for (dm in unique(itm$d5))
    d[[str_c("cnt_", dm)]] <- rowSums(d[itm$var[itm$d5 == dm]])
  d$breadth <- rowSums(d[str_c("cnt_", unique(itm$d5))] > 0)
  d$k_cat   <- cut(d$k, K_BREAKS, labels = K_LABELS)

  for (dm in unique(itm$domain))
    d[[str_c("dom_", dm)]] <- as.integer(rowSums(d[itm$var[itm$domain == dm]]) > 0)
  for (sb in unique(na.omit(itm$sub)))
    d[[str_c("sub_", sb)]] <- as.integer(rowSums(d[itm$var[itm$sub %in% sb]]) > 0)
  d
}

# ---- ロジスティック回帰 --------------------------------------------------
# Firth のロジスティック回帰（OR, 95%CI, p）。式に含まれる変数の欠損は除く
firth_tab <- function(data, formula, pl = TRUE) {
  d <- drop_na(data, all_of(intersect(all.vars(formula), names(data))))
  f <- logistf(formula, data = d, pl = pl)
  tibble(term   = names(f$coefficients),
         OR     = exp(f$coefficients),
         lower  = exp(f$ci.lower),
         upper  = exp(f$ci.upper),
         p      = f$prob,
         n      = nrow(d),
         n_case = sum(d$case == 1)) %>%
    filter(term != "(Intercept)")
}

# 性別ごとの 1 因子あたり OR（交互作用モデルを、性別ごとの傾きで書き直したもの）
#   case ~ male + age10 + x_F + x_M  （x_F = x × 女性, x_M = x × 男性）
sex_slopes <- function(d, x = "k") {
  d %>%
    mutate(x_F = .data[[x]] * (1 - male), x_M = .data[[x]] * male) %>%
    firth_tab(case ~ male + age10 + x_F + x_M) %>%
    filter(term %in% c("x_F", "x_M")) %>%
    mutate(term = if_else(term == "x_F", "per factor (F)", "per factor (M)"))
}

# 説明変数どうしの VIF
vif_manual <- function(d, vars) {
  d <- drop_na(d, all_of(vars))
  map_dfr(vars, function(v) {
    r2 <- summary(lm(reformulate(setdiff(vars, v), response = v), data = d))$r.squared
    tibble(term = v, VIF = 1 / (1 - r2))
  })
}

# 群別の度数表（Control / Case の2列）
count_by_group <- function(data, var) {
  data %>%
    mutate(grp = factor(case, 0:1, c("Control", "Case"))) %>%
    count({{ var }}, grp) %>%
    pivot_wider(names_from = grp, values_from = n, values_fill = 0)
}

trend_p <- function(tb) prop.trend.test(tb$Case, tb$Case + tb$Control)$p.value

# ---- 表4：総リスク数（群間比較と同じ方法：HC3、調整済み標準化対比 d）------
#   k ~ Group * Gender (+ 中心化した年齢) の線形回帰
#   対比：交互作用 (A2 − CT)[男性] − (A2 − CT)[女性]
#         群の主効果 A2 − CT（男女の群差を同じ重みで平均）
#         性別ごとの群差 A2 − CT（単純主効果）
#         群ごとの性差 男性 − 女性（参考。解釈しない）
#   検定と CI は HC3（計算できなければ通常の共分散）
#   d = 対比 / 残差 SD。d の CI は eff_size で残差 SD の不確かさも考慮
#   ※ dB の Gender は「F, M」の順、Group は「CT, A2」の順の因子にしておくこと
fit_lm3 <- function(dB, adjust_age = TRUE) {
  f <- k ~ Group * Gender
  if (adjust_age) f <- update(f, . ~ . + age_c)
  fit <- lm(f, data = dB)
  V   <- tryCatch(sandwich::vcovHC(fit, type = "HC3"), error = function(e) NULL)
  hc3 <- !is.null(V) && all(is.finite(V))
  if (!hc3) V <- vcov(fit)
  list(fit = fit, V = V, hc3 = hc3)
}

# 調整済み平均（adjusted mean）：上と同じモデルから、年齢を平均（age_c = 0）に固定した値。CI は HC3
#   Gender = "All" … 男女を同じ重みで平均した群ごとの値（差が表4の「群の主効果」に一致）
#   Gender = F / M … 群 × 性別ごとの値（差が表4の「性別ごとの群差」に一致）
lm_group_means <- function(dB, adjust_age = TRUE) {
  m    <- fit_lm3(dB, adjust_age)
  emm  <- emmeans(m$fit, ~ Group * Gender, vcov. = m$V)
  tidy <- function(x) {
    s <- as.data.frame(summary(x, infer = TRUE))
    if (!"Gender" %in% names(s)) s$Gender <- "All"
    transmute(s, Group = as.character(Group), Gender = as.character(Gender),
              adj_mean = emmean, adj_low = lower.CL, adj_high = upper.CL)
  }
  bind_rows(tidy(emmeans(emm, ~ Group)), tidy(emm))
}

lm_effects <- function(dB, adjust_age = TRUE) {
  m   <- fit_lm3(dB, adjust_age)
  fit <- m$fit; V <- m$V; hc3 <- m$hc3
  sig <- sigma(fit)
  edf <- df.residual(fit)

  one <- function(ct, labels) {
    s  <- as.data.frame(summary(ct, infer = TRUE))
    es <- as.data.frame(eff_size(ct, method = "identity", sigma = sig, edf = edf))
    tibble(effect = labels(s),
           est = s$estimate, est_low = s$lower.CL, est_high = s$upper.CL, p = s$p.value,
           d = es$effect.size, d_low = es$lower.CL, d_high = es$upper.CL)
  }

  emm <- emmeans(fit, ~ Group * Gender, vcov. = V)

  bind_rows(
    one(contrast(emm, interaction = c("revpairwise", "revpairwise")),
        function(s) "interaction"),
    one(contrast(emmeans(emm, ~ Group), "revpairwise"),
        function(s) "main effect of group"),
    one(contrast(emm, "revpairwise", by = "Gender"),
        function(s) str_c("group difference (", as.character(s$Gender), ")")),
    one(contrast(emm, "revpairwise", by = "Group"),
        function(s) str_c("gender difference (", as.character(s$Group), ")"))
  ) %>%
    mutate(model = if (adjust_age) "age-adjusted" else "unadjusted",
           vcov  = if (hc3) "HC3" else "model-based",
           n     = nobs(fit), .before = 1)
}

# 負の二項回帰（感度分析）：交互作用は尤度比検定、群差は平均の比（A2 / CT）
nb_effects <- function(dB) {
  nb1 <- tryCatch(MASS::glm.nb(k ~ Group * Gender + age_c, data = dB), error = function(e) NULL)
  nb0 <- tryCatch(MASS::glm.nb(k ~ Group + Gender + age_c, data = dB), error = function(e) NULL)
  if (is.null(nb1) || is.null(nb0)) {
    warning("負の二項回帰を推定できませんでした")
    return(tibble(effect = character(0), ratio = numeric(0), low = numeric(0),
                  high = numeric(0), p = numeric(0)))
  }
  ratio_tab <- function(ct, labels) {
    s <- as.data.frame(summary(ct, infer = TRUE, type = "response"))
    tibble(effect = labels(s), ratio = s$ratio, low = s$asymp.LCL, high = s$asymp.UCL, p = s$p.value)
  }
  emm <- emmeans(nb1, ~ Group * Gender)
  bind_rows(
    tibble(effect = "interaction", ratio = NA_real_, low = NA_real_, high = NA_real_,
           p = anova(nb0, nb1)$`Pr(Chi)`[2]),
    ratio_tab(contrast(emmeans(emm, ~ Group), "revpairwise"), function(s) "main effect of group"),
    ratio_tab(contrast(emm, "revpairwise", by = "Gender"),
              function(s) str_c("group difference (", as.character(s$Gender), ")"))
  )
}

# ---- 表3の行構成と Fisher の正確検定 ---------------------------------------
make_rows <- function(itm) {
  map_dfr(unique(itm$domain), function(dm) {
    it_d <- filter(itm, domain == dm)
    body <- map_dfr(unique(it_d$sub), function(sb) {
      it_s <- if (is.na(sb)) filter(it_d, is.na(sub)) else filter(it_d, sub == sb)
      items_rows <- tibble(level = "item", label = it_s$label, var = it_s$var)
      if (is.na(sb)) return(items_rows)
      sub_row <- tibble(level = "subdomain", label = it_s$sub_label[1],
                        var = if (nrow(it_s) > 1) str_c("sub_", sb) else NA_character_)
      bind_rows(sub_row, items_rows)
    })
    bind_rows(tibble(level = "domain", label = it_d$domain_label[1],
                     var = str_c("dom_", dm)),
              body)
  }) %>%
    mutate(row = row_number())
}

fisher_stats <- function(d, v) {
  d  <- filter(d, !is.na(.data[[v]]))
  n1 <- sum(d$case == 1); a <- sum(d[[v]][d$case == 1])
  n0 <- sum(d$case == 0); c <- sum(d[[v]][d$case == 0])
  # exact2x2（tsmethod = "minlike"）: 両側 P は Fisher の正確検定と同じ。CI はその P と一致する (Fay 2010)
  ft <- exact2x2(matrix(c(a, n1 - a, c, n0 - c), nrow = 2, byrow = TRUE), tsmethod = "minlike")
  tibble(case_n = a, case_N = n1, ctrl_n = c, ctrl_N = n0,
         p = ft$p.value, OR = unname(ft$estimate),
         lower = ft$conf.int[1], upper = ft$conf.int[2],
         estimable = !(a + c == 0 | a + c == n1 + n0))
}

# ---- ベン図 ------------------------------------------------------------------
set_vars <- function(v) {
  pos <- intersect(c("left", "top", "right"), names(v$set_labels))
  set_names(unlist(v[pos]), v$set_labels[pos])
}
region_levels <- function(v) c(sort(unname(v$region_labels)), "None")

venn_region <- function(d, v) {
  L  <- d[[v$left]]  == 1
  R  <- d[[v$right]] == 1
  rl <- v$region_labels
  if (is.null(v$top)) {
    reg <- case_when(L & R ~ rl[["left_right"]],
                     L     ~ rl[["left"]],
                     R     ~ rl[["right"]],
                     TRUE  ~ "None")
  } else {
    U <- d[[v$top]] == 1
    reg <- case_when(L & U & R ~ rl[["all"]],
                     L & U     ~ rl[["left_top"]],
                     U & R     ~ rl[["top_right"]],
                     L & R     ~ rl[["left_right"]],
                     L         ~ rl[["left"]],
                     U         ~ rl[["top"]],
                     R         ~ rl[["right"]],
                     TRUE      ~ "None")
  }
  factor(reg, levels = region_levels(v))
}

venn_stats <- function(d, v, nm) {
  reg  <- d[[str_c("region_", nm)]]
  labs <- region_levels(v)
  sv   <- set_vars(v)
  ind  <- c(list(case = d$case),
            map(sv, ~ as.integer(d[[.x]] == 1)),
            set_names(map(labs, ~ as.integer(reg == .x)), labs))
  as_tibble(ind) %>%
    pivot_longer(-case, names_to = "label", values_to = "x") %>%
    group_by(label) %>%
    summarise(
      Case_n    = sum(x[case == 1]), Case_N    = sum(case == 1),
      Control_n = sum(x[case == 0]), Control_N = sum(case == 0),
      p = exact2x2(matrix(c(Case_n, Case_N - Case_n,
                               Control_n, Control_N - Control_n),
                             nrow = 2, byrow = TRUE), tsmethod = "minlike")$p.value,
      .groups = "drop") %>%
    mutate(type = case_when(label %in% v$set_labels ~ "set",
                            label == "None"         ~ "none",
                            TRUE                    ~ "region"),
           Case_pct    = Case_n / Case_N,
           Control_pct = Control_n / Control_N,
           var         = unname(sv[label]),
           estimable   = Case_n + Control_n > 0 &
                         Case_n + Control_n < Case_N + Control_N) %>%
    arrange(factor(type, c("set", "region", "none")), label) %>%
    relocate(type, label)
}

# ---- 表示用の書式 ------------------------------------------------------------
fmt_np   <- function(n, N) if_else(n == 0, "0 (0%)", sprintf("%d (%.2f%%)", n, 100 * n / N))
np_txt   <- function(n, N) sprintf("%d (%.1f%%)", as.integer(n), 100 * n / N)
fmt_num  <- function(x) case_when(is.na(x) ~ NA_character_, is.infinite(x) ~ "∞", x == 0 ~ "0",
                                  TRUE ~ sprintf("%.2f", x))
fmt_or   <- function(or, lo, hi, est = rep(TRUE, length(or)))
  if_else(est, str_c(fmt_num(or), " [", fmt_num(lo), ", ", fmt_num(hi), "]"), "NE [0, ∞]")
ci_txt   <- function(x, lo, hi) if_else(is.na(x), "–", sprintf("%.2f [%.2f, %.2f]", x, lo, hi))
p_num    <- function(p) case_when(is.na(p) ~ "–", p < .001 ~ "< .001",
                                  TRUE ~ str_remove(sprintf("%.3f", p), "^0"))
p_star   <- function(p) str_c(p_num(p), case_when(is.na(p) ~ "", p < .001 ~ "***",
                                                  p < .01 ~ "**", p < .05 ~ "*", TRUE ~ ""))
sig_mark <- function(p) case_when(p < .001 ~ "***", p < .01 ~ "**",
                                  p < .05  ~ "*",   p < .10 ~ "+",
                                  TRUE     ~ " ")
indent   <- function(level, label) case_when(level == "item"      ~ str_c("    ", label),
                                             level == "subdomain" ~ str_c("  ",   label),
                                             TRUE                 ~ label)
sex_name <- function(g) unname(GENDER_LABELS[g])
an_short <- function(an) ANALYSES$short[match(an, ANALYSES$name)]
an_long  <- function(an) ANALYSES$long[match(an, ANALYSES$name)]


## ============================================================
## 2. 解析1回分（ファイルには書かず、結果をリストで返す）
## ============================================================
run_analysis <- function(fulldata, an, thr_main = NULL) {

  message("\n==== 解析: ", an$name, "（", an$group_type, ", ", an$subset, "）====")

  ## ---- 2-1. データ -----------------------------------------------------
  df <- make_vars(fulldata) %>% mutate(Group = as.character(Group))
  df <- if (an$group_type == "CTA3") {
    mutate(df, Group = if_else(Group == "A3", "CT", Group))
  } else {
    filter(df, Group != "A3")
  }

  # 非血縁関係のある回答者の除外（データ読み込み側で除外済みなら何もしない）
  if (an$subset == "bio") {
    n_nk <- sum(df$nonkin %in% 1)
    if (n_nk > 0) message("非血縁関係のある回答者を除外: ", n_nk, "人")
    df <- filter(df, !nonkin %in% 1)
  }

  itm <- if (an$subset == "bio") filter(ITEMS, var != "nonkin") else ITEMS
  cont_vars <- itm$var[itm$type == "cont"]
  bin_vars  <- itm$var[itm$type == "bin"]

  dat0 <- df %>%
    filter(Group %in% c("CT", CASE_GROUP)) %>%
    mutate(Group  = factor(Group, levels = c("CT", CASE_GROUP)),
           case   = as.integer(Group == CASE_GROUP),
           Gender = as.character(Gender),
           male   = as.integer(Gender == MALE_LABEL),
           age10  = (age - mean(age, na.rm = TRUE)) / 10)   # 年齢（平均で中心化、10歳単位）

  genders <- intersect(GENDER_LEVELS, unique(dat0$Gender))

  bad_bin <- bin_vars %>% keep(~ any(!dat0[[.x]] %in% c(0, 1, NA)))
  if (length(bad_bin) > 0)
    warning("0/1以外の値を含む二値変数: ", str_c(bad_bin, collapse = ", "))

  # 人数と、1つ以上の因子（または年齢）に欠損のある人（確認用）
  N_tab <- dat0 %>% count(Gender, Group, name = "N")
  incomplete <- dat0 %>%
    filter(if_any(all_of(c(itm$var, "age")), is.na)) %>%
    rowwise() %>%
    mutate(missing_vars = str_c(c(itm$var, "age")[is.na(c_across(all_of(c(itm$var, "age"))))],
                                collapse = ", ")) %>%
    ungroup() %>%
    select(ID, Group, Gender, missing_vars)

  ## ---- 2-2. 閾値・二値化・累積指標 -------------------------------------
  thr_own <- dat0 %>%
    filter(case == 0) %>%
    pivot_longer(all_of(cont_vars), names_to = "var", values_to = "x") %>%
    group_by(Gender, var) %>%
    summarise(m2sd = thr_value(x, "m2sd"),
              m1sd = thr_value(x, "m1sd"),
              p90  = thr_value(x, "p90"), .groups = "drop") %>%
    pivot_longer(c(m2sd, m1sd, p90), names_to = "method", values_to = "threshold")

  thr <- if (THR_REF == "main" && !is.null(thr_main)) thr_main else thr_own

  # 因子の 0/1（欠損は残す：表3の個別因子用）
  scored_items <- dat0 %>%
    mutate(across(all_of(cont_vars), ~ .x, .names = "{.col}_raw")) %>%
    dichotomize_with(cont_vars, thr, "m2sd")

  # 累積指標（欠損は MISSING_RULE で埋める）
  idx <- build_index(scored_items, thr, itm) %>%
    mutate(n_val = rowSums(!is.na(scored_items[itm$var])))   # 回答のあった因子の数

  d5_vars <- str_c("cnt_", unique(itm$d5))

  ## ---- 2-3. 表3：個別因子・領域・下位領域（性別ごと Fisher）------------
  tab2_dat <- scored_items %>%
    select(Gender, case, all_of(itm$var)) %>%
    bind_cols(select(idx, starts_with(c("dom_", "sub_"))))

  row_spec  <- make_rows(itm)
  stat_vars <- row_spec$var[!is.na(row_spec$var)]

  stats2 <- expand_grid(Gender = genders, var = stat_vars) %>%
    mutate(res = map2(Gender, var, ~ fisher_stats(filter(tab2_dat, Gender == .x), .y))) %>%
    unnest(res) %>%
    mutate(family = if_else(str_detect(var, "^(dom|sub)_"), "summary", "item")) %>%
    mutate(p_adj = adjust_p(p, estimable),
           .by = all_of(c("family", if (FDR_BY_GENDER) "Gender")))

  # 表3：男女を縦に並べる（女性のブロック → 男性のブロック）
  table2 <- map_dfr(genders, function(g) {
    nn <- tab2_dat %>% filter(Gender == g) %>% count(case)
    head <- tibble(Factor = str_c(sex_name(g),
                                  " (case n = ", nn$n[nn$case == 1],
                                  ", control n = ", nn$n[nn$case == 0], ")"))
    thr_g <- thr %>%
      filter(method == "m2sd", Gender == g) %>%
      left_join(select(itm, var, unit, digits), by = "var") %>%
      transmute(var, thr_txt = str_c(sprintf("%.*f", digits, threshold),
                                     if_else(unit == "", "", str_c(" ", unit))))
    body <- row_spec %>%
      left_join(filter(stats2, Gender == g), by = "var") %>%
      left_join(thr_g, by = "var") %>%
      arrange(row) %>%
      transmute(
        Factor                    = indent(level, label),
        `Case, n (%)`             = if_else(is.na(case_n), "", fmt_np(case_n, case_N)),
        `Control, n (%)`          = if_else(is.na(ctrl_n), "", fmt_np(ctrl_n, ctrl_N)),
        P                         = case_when(is.na(case_n) ~ "", !estimable ~ "–", TRUE ~ p_num(p)),
        `Padj (FDR)`              = case_when(is.na(case_n) ~ "", TRUE ~ p_star(p_adj)),
        `OR [95% CI]`             = if_else(is.na(case_n), "", fmt_or(OR, lower, upper, estimable)),
        `Threshold (mean + 2 SD)` = coalesce(thr_txt, ""))
    bind_rows(head, body)
  })

  ## ---- 2-4. 表4：総リスク数 ------------------------------------------
  dB <- idx %>%
    filter(!is.na(age)) %>%
    mutate(Gender = factor(Gender, levels = genders),
           age_c  = age - mean(age))

  eff3 <- bind_rows(lm_effects(dB, adjust_age = TRUE),
                    lm_effects(dB, adjust_age = FALSE))   # 未調整モデルは補足表 S5 に
  nb3  <- nb_effects(dB)
  adj3 <- lm_group_means(dB, adjust_age = TRUE)            # 表4の主効果の行に入れる調整済み平均
  adj_txt <- function(g, s = "All") {
    x <- filter(adj3, Group == g, Gender == s)
    ci_txt(x$adj_mean, x$adj_low, x$adj_high)
  }

  desc3 <- idx %>%
    group_by(Gender) %>%
    summarise(case_N = sum(case == 1), case_mean = mean(k[case == 1]), case_sd = sd(k[case == 1]),
              ctrl_N = sum(case == 0), ctrl_mean = mean(k[case == 0]), ctrl_sd = sd(k[case == 0]),
              .groups = "drop") %>%
    arrange(match(Gender, genders))

  # 表4：交互作用 → 群の主効果 → 性別ごとの群差 → 群ごとの性差（参考）
  e3 <- filter(eff3, model == "age-adjusted")
  row3 <- function(label, e) {
    x <- filter(e3, effect == e)
    tibble(Comparison = label,
           `Difference [95% CI]` = ci_txt(x$est, x$est_low, x$est_high),
           `d or Δd [95% CI]`    = ci_txt(x$d, x$d_low, x$d_high),
           P                     = p_star(x$p))
  }
  grp3 <- desc3 %>%
    left_join(filter(e3, str_detect(effect, "^group difference")) %>%
                mutate(Gender = str_match(effect, "\\((.*)\\)")[, 2]),
              by = "Gender") %>%
    transmute(Comparison            = str_c("  ", sex_name(Gender), " (case n = ", case_N,
                                            ", control n = ", ctrl_N, ")"),
              `Case, adjusted mean [95% CI]`    = map_chr(Gender, ~ adj_txt(CASE_GROUP, .x)),
              `Control, adjusted mean [95% CI]` = map_chr(Gender, ~ adj_txt("CT", .x)),
              `Difference [95% CI]` = ci_txt(est, est_low, est_high),
              `d or Δd [95% CI]`    = ci_txt(d, d_low, d_high),
              P                     = p_star(p))
  table3 <- bind_rows(
    row3("Group × gender interaction (men − women)", "interaction"),
    row3("Main effect of group (case − control)", "main effect of group") %>%
      mutate(`Case, adjusted mean [95% CI]`    = adj_txt(CASE_GROUP),   # 男女を同じ重みで平均
             `Control, adjusted mean [95% CI]` = adj_txt("CT"), .after = Comparison),
    tibble(Comparison = "Group difference (case − control) by gender"),
    grp3,
    tibble(Comparison = "Gender difference (men − women), for reference"),
    row3("  Control group", "gender difference (CT)"),
    row3(str_c("  Case group"), str_c("gender difference (", CASE_GROUP, ")"))
  ) %>%
    select(Comparison, `Case, adjusted mean [95% CI]`, `Control, adjusted mean [95% CI]`,
           `Difference [95% CI]`, `d or Δd [95% CI]`, P)

  ## ---- 2-5. ベン図（図5、補足図 S11・S12）-----------------------------
  venns <- make_venns(an$subset)
  region_cols <- str_c("region_", names(venns))
  idx <- idx %>%
    select(-any_of(region_cols)) %>%
    bind_cols(imap_dfc(venns, ~ venn_region(idx, .x)) %>%
                rename_with(~ str_c("region_", .x)))

  venn_long <- expand_grid(Gender = genders, venn = names(venns)) %>%
    mutate(res = map2(Gender, venn,
                      ~ venn_stats(filter(idx, Gender == .x), venns[[.y]], .y))) %>%
    unnest(res)

  stats2_adj <- stats2 %>% select(Gender, var_A = var, p_adj_A = p_adj)
  venn_long <- venn_long %>%
    mutate(family = if_else(type == "set", "set", "region")) %>%
    mutate(p_adj = adjust_p(p, estimable),
           .by = all_of(c("venn", "family", if (FDR_BY_GENDER) "Gender"))) %>%
    mutate(var_A = coalesce(unname(SET_VAR_ALIAS[var]), var)) %>%
    left_join(stats2_adj, by = c("Gender", "var_A")) %>%
    # 円の横の割合（領域・下位領域）は表3の補正後の値を使う
    mutate(p_adj   = if_else(type == "set", p_adj_A, p_adj),
           sig_raw = sig_mark(p),
           sig     = sig_mark(p_adj)) %>%
    select(-var_A, -p_adj_A)

  # 図の作成用（以前の venn_values.xlsx と同じ形）
  #   パネルごとに1シート（F_domain, F_iii, M_domain, M_iii）：列は label / Case / sig / Control
  #   1行目は N（人数）、以降は円・領域・None の割合（0〜1、数値）
  #   最後に all_numbers シート（venn_long そのもの）
  venn_tabs <- expand_grid(Gender = genders, venn = names(venns)) %>%
    mutate(name = str_c(Gender, "_", venn),
           tab  = map2(Gender, venn, function(g, vn) {
             x <- filter(venn_long, Gender == g, venn == vn)
             bind_rows(tibble(label = "N", Case = as.numeric(x$Case_N[1]), sig = NA_character_,
                              Control = as.numeric(x$Control_N[1])),
                       transmute(x, label, Case = Case_pct, sig, Control = Control_pct))
           }))
  venn_sheets <- c(set_names(venn_tabs$tab, venn_tabs$name), list(all_numbers = venn_long))

  ## ---- 2-6. 総リスク数のカテゴリ（図3、補足図 S8、補足表 S6）---------
  by_sex  <- split(idx, idx$Gender)[genders]
  k_table <- map_dfr(by_sex, ~ count_by_group(mutate(.x, k_cat = droplevels(k_cat)), k_cat),
                     .id = "Gender") %>%
    mutate(k_cat = as.character(k_cat))
  trend   <- map_dfr(by_sex, ~ tibble(trend_p = trend_p(
                       count_by_group(mutate(.x, k_cat = droplevels(k_cat)), k_cat))),
                     .id = "Gender")

  ## ---- 2-7. モデル1〜5（男女をまとめ、年齢・性別で調整、Firth）--------
  dM <- idx %>% filter(!is.na(age10))
  dM_cat <- mutate(dM, k_cat = droplevels(k_cat))
  f_m4 <- reformulate(c(d5_vars, "age10", "male"), response = "case")

  models <- bind_rows(
    firth_tab(dM, case ~ k + age10 + male)                %>% mutate(model = "M1 total"),
    firth_tab(dM, case ~ k * male + age10)                %>% mutate(model = "M2 interaction"),
    sex_slopes(dM, "k")                                   %>% mutate(model = "M2 sex-specific"),
    firth_tab(dM, case ~ breadth + age10 + male)          %>% mutate(model = "M3a breadth only"),
    firth_tab(dM, case ~ k + breadth + age10 + male)      %>% mutate(model = "M3 total + breadth"),
    firth_tab(dM, f_m4)                                   %>% mutate(model = "M4 domains"),
    firth_tab(dM, case ~ k + I(k^2) + age10 + male)       %>% mutate(model = "M5 quadratic"),
    firth_tab(dM_cat, case ~ k_cat + age10 + male)        %>% mutate(model = "M5b categories")
  ) %>%
    relocate(model) %>%
    mutate(term = dplyr::recode(term, !!!set_names(D5_LABELS, str_c("cnt_", names(D5_LABELS)))))

  # モデル3の診断：相関・VIF・ブートストラップ
  diag3 <- bind_rows(
    tibble(item = "cor(k, breadth): all",     value = cor(dM$k, dM$breadth)),
    tibble(item = "cor(k, breadth): Control", value = with(filter(dM, case == 0), cor(k, breadth))),
    tibble(item = "cor(k, breadth): Case",    value = with(filter(dM, case == 1), cor(k, breadth))),
    vif_manual(dM, c("k", "breadth", "age10", "male")) %>% transmute(item = str_c("VIF: ", term), value = VIF)
  )

  boot3 <- NULL
  if (N_BOOT > 0) {
    set.seed(SEED)
    boot <- map_dfr(seq_len(N_BOOT), function(b) {
      db <- dM %>% group_by(case, male) %>% slice_sample(prop = 1, replace = TRUE) %>% ungroup()
      f  <- tryCatch(logistf(case ~ k + breadth + age10 + male, data = db, pl = FALSE),
                     error = function(e) NULL)
      if (is.null(f)) return(NULL)
      tibble(b = b, term = names(f$coefficients), est = unname(f$coefficients))
    })
    boot3 <- boot %>%
      filter(term %in% c("k", "breadth")) %>%
      group_by(term) %>%
      summarise(OR_median = exp(median(est)),
                OR_2.5    = exp(quantile(est, .025)),
                OR_97.5   = exp(quantile(est, .975)),
                prop_OR_above_1 = mean(est > 0),
                n_boot = n(), .groups = "drop")
  }

  ## ---- 2-8. 頑健性（年齢・性別で調整）---------------------------------
  top <- if (TOP_ITEM %in% itm$var) {
    dM %>%
      mutate(k_wo_top = k - .data[[TOP_ITEM]]) %>%
      firth_tab(reformulate(c(TOP_ITEM, "k_wo_top", "age10", "male"), response = "case"))
  } else NULL

  loo <- map_dfr(itm$var, function(v) {
    dM %>%
      mutate(kk = k - .data[[v]]) %>%
      firth_tab(case ~ kk + age10 + male) %>%
      filter(term == "kk") %>%
      mutate(dropped = v, .before = 1)
  })

  ## ---- 2-9. 感度分析（解析内）------------------------------------------
  sens <- SENS_SPECS %>%
    pmap_dfr(function(spec, threshold, missing, drop, score) {
      d <- build_index(scored_items, thr, itm, threshold, missing, drop, score) %>%
        filter(!is.na(age10))
      bind_rows(
        firth_tab(d, case ~ k + age10 + male) %>% filter(term == "k") %>%
          mutate(term = "per factor (common)"),
        sex_slopes(d, "k"),
        firth_tab(d, case ~ k * male + age10) %>% filter(term == "k:male") %>%
          mutate(term = "interaction (M/F ratio)")
      ) %>%
        mutate(spec = spec,
               case_mean = mean(d$k[d$case == 1]), case_sd = sd(d$k[d$case == 1]),
               ctrl_mean = mean(d$k[d$case == 0]), ctrl_sd = sd(d$k[d$case == 0]),
               .before = 1)
    })

  list(thr = thr, thr_own = thr_own, N = N_tab, incomplete = incomplete,
       stats2 = stats2, table2 = table2,
       eff3 = eff3, nb3 = nb3, desc3 = desc3, adj3 = adj3, table3 = table3,
       venn_long = venn_long, venn_sheets = venn_sheets,
       k_table = k_table, trend = trend,
       models = models, diag3 = diag3, boot3 = boot3, top = top, loo = loo, sens = sens,
       idx = idx, itm = itm)
}


## ============================================================
## 3. すべての解析を実行
## ============================================================
stopifnot(ANALYSES$name[1] == "main",
          ANALYSES$group_type[1] == "CTA3", ANALYSES$subset[1] == "all",
          THR_REF %in% c("own", "main"),
          MISSING_RULE %in% c("zero", "adverse", "one"))

results  <- list()
thr_main <- NULL

for (i in seq_len(nrow(ANALYSES))) {
  an <- ANALYSES[i, ]
  # データ読み込み（読み込み側で setting_subset / group_type を使う）
  setting_subset <- an$subset
  group_type     <- an$group_type
  source("Scripts/A2A3data_import.R")

  results[[an$name]] <- run_analysis(fulldata, an, thr_main = thr_main)
  if (an$name == "main") thr_main <- results$main$thr_own
}

collect <- function(part) bind_rows(map(results, part), .id = "analysis")
an_names <- ANALYSES$name

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out <- function(f) file.path(OUT_DIR, f)


## ============================================================
## 4. 論文の表
## ============================================================
tables <- list()

## ---- Table 3：個別因子・領域・下位領域（主解析）---------------------------
tables$`Table 3` <- results$main$table2

## ---- Table 4：総リスク数（主解析、年齢調整モデル）-----------------------
tables$`Table 4` <- results$main$table3

## ---- Table 5：ロジスティック回帰モデル 1〜5（Main / A / B）---------------
models_all <- collect("models")
MODEL_ROWS <- tribble(
  ~model,               ~term,                 ~label,
  "M1 total",           "k",                   "Model 1: per additional factor",
  "M2 interaction",     "k:male",              "Model 2: risk count × gender (men/women)",
  "M2 sex-specific",    "per factor (F)",      "Model 2: per additional factor, women",
  "M2 sex-specific",    "per factor (M)",      "Model 2: per additional factor, men",
  "M3a breadth only",   "breadth",             "Model 3a: per additional domain (alone)",
  "M3 total + breadth", "k",                   "Model 3: per additional factor",
  "M3 total + breadth", "breadth",             "Model 3: per additional domain",
  "M4 domains",         "D1 Developmental",    "Model 4: developmental",
  "M4 domains",         "D2 Neurobiological",  "Model 4: neurobiological",
  "M4 domains",         "D3 Non-kin relation", "Model 4: non-kin relation",
  "M4 domains",         "D4 Child factors",    "Model 4: child factors",
  "M4 domains",         "D5 Other hardships",  "Model 4: other hardships",
  "M5 quadratic",       "k",                   "Model 5: linear term",
  "M5 quadratic",       "I(k^2)",              "Model 5: quadratic term"
) %>%
  mutate(row = row_number())

t4_body <- expand_grid(MODEL_ROWS, analysis = an_names) %>%
  left_join(models_all, by = c("model", "term", "analysis")) %>%
  transmute(row, label, analysis,
            `OR [95% CI]` = if_else(is.na(OR), "–", fmt_or(OR, lower, upper)),
            P = p_num(p))
t4_n <- models_all %>%
  filter(model == "M1 total", term == "k") %>%
  transmute(analysis, `OR [95% CI]` = str_c("n = ", n, " (cases = ", n_case, ")"), P = "") %>%
  mutate(row = 0L, label = "Participants")

tables$`Table 5` <- bind_rows(t4_n, t4_body) %>%
  mutate(analysis = factor(analysis, levels = an_names)) %>%
  arrange(row, analysis) %>%
  mutate(analysis = an_short(as.character(analysis))) %>%
  pivot_wider(names_from = analysis, values_from = c(`OR [95% CI]`, P),
              names_glue = "{analysis}: {.value}", names_vary = "slowest") %>%
  select(-row) %>%
  rename(`Model / term` = label)


## ============================================================
## 5. 補足表
## ============================================================

## ---- Table S10：連続変数の閾値 ----------------------------------------------
METHOD_LABELS <- c(m2sd = "Mean + 2 SD (main)", m1sd = "Mean + 1 SD", p90 = "90th percentile")
tables$`Table S10` <- collect("thr") %>%
  mutate(Factor = ITEM_LABELS[var],
         Unit   = CONT_UNITS[var],
         Method = METHOD_LABELS[method],
         col    = str_c(an_short(analysis), ": ", sex_name(Gender)),
         value  = sprintf("%.2f", threshold)) %>%
  arrange(match(var, names(CONT_UNITS)), match(method, names(METHOD_LABELS))) %>%
  select(Factor, Unit, Method, col, value) %>%
  pivot_wider(names_from = col, values_from = value) %>%
  select(Factor, Unit, Method,
         any_of(as.vector(t(outer(ANALYSES$short, unname(GENDER_LABELS), str_c, sep = ": ")))))

## ---- Table S4：表3のデータセット A・B -----------------------------------------
tables$`Table S4` <- map_dfr(setdiff(an_names, "main"), function(an) {
  bind_rows(tibble(Factor = an_long(an)), results[[an]]$table2)
})

## ---- Table S5：表4の補足（全データセット、年齢調整・未調整、負の二項回帰）----
eff_label <- function(e) case_when(
  e == "interaction"                  ~ "Group × gender interaction (men − women)",
  e == "main effect of group"         ~ "Main effect of group (case − control)",
  str_detect(e, "^group difference")  ~ str_c("Group difference (case − control), ",
                                              sex_name(str_match(e, "\\((.*)\\)")[, 2])),
  str_detect(e, "^gender difference") ~ str_c("Gender difference (men − women), ",
                                              if_else(str_detect(e, CASE_GROUP), "case group",
                                                      "control group"), " (reference)"),
  TRUE ~ e)
eff_order <- c("interaction", "main effect of group", "group difference (F)", "group difference (M)",
               "gender difference (CT)", str_c("gender difference (", CASE_GROUP, ")"))

eff3_all <- collect("eff3")
nb3_all  <- collect("nb3")

s7_lm <- eff3_all %>%
  transmute(analysis, effect,
            Model = if_else(model == "age-adjusted", "Linear model, age-adjusted", "Linear model, unadjusted"),
            Comparison = eff_label(effect),
            `Estimate [95% CI]` = ci_txt(est, est_low, est_high),
            `d or Δd [95% CI]`  = ci_txt(d, d_low, d_high),
            P = p_star(p), `Standard errors` = vcov)
s7_nb <- nb3_all %>%
  transmute(analysis, effect,
            Model = "Negative binomial, age-adjusted",
            Comparison = case_when(effect == "interaction" ~ "Group × gender interaction (likelihood-ratio test)",
                                   effect == "main effect of group" ~ "Main effect of group: ratio of means (case / control)",
                                   TRUE ~ str_c("Ratio of means (case / control), ",
                                                sex_name(str_match(effect, "\\((.*)\\)")[, 2]))),
            `Estimate [95% CI]` = if_else(is.na(ratio), "", ci_txt(ratio, low, high)),
            `d or Δd [95% CI]`  = "",
            P = p_star(p), `Standard errors` = "model-based")

tables$`Table S5` <- bind_rows(s7_lm, s7_nb) %>%
  mutate(order_m = match(Model, c("Linear model, age-adjusted", "Linear model, unadjusted",
                                  "Negative binomial, age-adjusted")),
         order_e = match(effect, eff_order)) %>%
  arrange(match(analysis, an_names), order_m, order_e) %>%
  transmute(Dataset = an_short(analysis), Model, Comparison,
            `Estimate [95% CI]`, `d or Δd [95% CI]`, P, `Standard errors`)

## ---- Table S7：解析内の感度分析（図4a の数値）--------------------------------
sens_all <- collect("sens")
tables$`Table S7` <- sens_all %>%
  mutate(term = dplyr::recode(term, "per factor (common)" = "Common", "per factor (F)" = "Women",
                              "per factor (M)" = "Men", "interaction (M/F ratio)" = "Interaction"),
         val  = if_else(term == "Interaction",
                        str_c(ci_txt(OR, lower, upper), ", P = ", p_num(p)),
                        ci_txt(OR, lower, upper))) %>%
  select(analysis, spec, case_mean, case_sd, ctrl_mean, ctrl_sd, term, val) %>%
  pivot_wider(names_from = term, values_from = val) %>%
  arrange(match(analysis, an_names), match(spec, names(SPEC_LABELS))) %>%
  transmute(Dataset = an_short(analysis), Specification = SPEC_LABELS[spec],
            `Count, case, mean ± SD`    = sprintf("%.2f ± %.2f", case_mean, case_sd),
            `Count, control, mean ± SD` = sprintf("%.2f ± %.2f", ctrl_mean, ctrl_sd),
            `OR, common [95% CI]` = Common, `OR, women [95% CI]` = Women,
            `OR, men [95% CI]` = Men, `Interaction (men/women) [95% CI], P` = Interaction)

## ---- Table S6：総リスク数のカテゴリ別の分布と OR（モデル5b）、傾向検定 -------
k_all     <- collect("k_table")
trend_all <- collect("trend")
cat_or <- models_all %>%
  filter(model == "M5b categories", str_detect(term, "^k_cat")) %>%
  transmute(analysis, k_cat = str_remove(term, "^k_cat"), OR_txt = ci_txt(OR, lower, upper))

s9_body <- k_all %>%
  pivot_longer(c(Case, Control), names_to = "grp", values_to = "n") %>%
  group_by(analysis, Gender, grp) %>%
  mutate(val = np_txt(n, sum(n))) %>%
  ungroup() %>%
  mutate(col = str_c(sex_name(Gender), ", ", tolower(grp))) %>%
  select(analysis, k_cat, col, val) %>%
  pivot_wider(names_from = col, values_from = val) %>%
  left_join(cat_or, by = c("analysis", "k_cat")) %>%
  mutate(OR_txt = coalesce(OR_txt, "1 (reference)")) %>%
  arrange(match(analysis, an_names), match(k_cat, K_LABELS)) %>%
  transmute(analysis, Dataset = an_short(analysis), `Number of risk factors` = k_cat,
            `Women, case`, `Women, control`, `Men, case`, `Men, control`,
            `OR [95% CI] (Model 5b)` = OR_txt)
s9_trend <- trend_all %>%
  group_by(analysis) %>%
  summarise(`Women, case` = p_num(trend_p[Gender == "F"]),
            `Men, case`   = p_num(trend_p[Gender == "M"]), .groups = "drop") %>%
  transmute(analysis, Dataset = an_short(analysis), `Number of risk factors` = "Trend test (P)",
            `Women, case`, `Men, case`)
tables$`Table S6` <- bind_rows(s9_body, s9_trend) %>%
  arrange(match(analysis, an_names)) %>%
  select(-analysis)

## ---- Table S8：頑健性の確認とモデル3の診断 -------------------------------
top_all  <- collect("top")
diag_all <- collect("diag3")
boot_all <- collect("boot3")

s10_top <- top_all %>%
  filter(term %in% c(TOP_ITEM, "k_wo_top")) %>%
  transmute(analysis, Section = "Non-kin relation and the remaining risk count",
            Item  = if_else(term == "k_wo_top", "Risk count excluding non-kin relation (per factor)",
                            "Non-kin relation (present vs absent)"),
            Value = str_c(ci_txt(OR, lower, upper), ", P = ", p_num(p)))
s10_diag <- diag_all %>%
  transmute(analysis, Section = "Model 3: collinearity",
            Item  = str_replace_all(item, c("cor\\(k, breadth\\)" = "Correlation (risk count, breadth)",
                                            "VIF: k" = "VIF: risk count", "VIF: breadth" = "VIF: risk breadth",
                                            "VIF: age10" = "VIF: age", "VIF: male" = "VIF: gender")),
            Value = sprintf("%.2f", value))
s10_boot <- if (nrow(boot_all) > 0) boot_all %>%
  transmute(analysis, Section = "Model 3: bootstrap",
            Item  = dplyr::recode(term, k = "Risk count (per factor)", breadth = "Risk breadth (per domain)"),
            Value = str_c("median OR ", sprintf("%.2f", OR_median),
                          " [2.5–97.5th percentile ", sprintf("%.2f", OR_2.5), ", ",
                          sprintf("%.2f", OR_97.5), "]; OR > 1 in ",
                          sprintf("%.1f", 100 * prop_OR_above_1), "% of ", n_boot, " replicates")) else NULL

tables$`Table S8` <- bind_rows(s10_top, s10_diag, s10_boot) %>%
  mutate(sec_order = match(Section, c("Non-kin relation and the remaining risk count",
                                      "Model 3: collinearity", "Model 3: bootstrap"))) %>%
  arrange(match(analysis, an_names), sec_order) %>%
  transmute(Dataset = an_short(analysis), Section, Item, Value)


## ============================================================
## 6. 図
## ============================================================

## ---- 総リスク数の分布（主解析：Fig 3、データセット A・B：Fig S8）----------
dist_values <- function(an) {
  results[[an]]$k_table %>%
    pivot_longer(c(Control, Case), names_to = "grp", values_to = "n") %>%
    group_by(Gender, grp) %>%
    mutate(N = sum(n), pct = n / N) %>%
    ungroup() %>%
    mutate(analysis = an, .before = 1)
}
plot_distribution <- function(an, title = NULL) {
  d <- dist_values(an) %>%
    mutate(k_cat    = factor(k_cat, levels = rev(K_LABELS)),     # 0 が下になるように
           grp      = factor(grp, levels = c("Control", "Case")),
           Gender   = factor(Gender, levels = names(GENDER_LABELS), labels = GENDER_LABELS),
           label    = if_else(pct >= 0.08, scales::percent(pct, accuracy = 1), ""),
           text_col = if_else(k_cat %in% c("3–4", "5+"), "white", "grey15")) %>%
    arrange(Gender, grp, match(k_cat, K_LABELS)) %>%
    group_by(Gender, grp) %>%
    mutate(ypos = cumsum(pct) - pct / 2) %>%
    ungroup()
  x_lab <- d %>% distinct(Gender, grp, N) %>%
    mutate(lab = str_c(as.character(grp), "\n(n = ", N, ")"))

  ggplot(d, aes(grp, pct, fill = k_cat)) +
    geom_col(width = 0.65, colour = "white", linewidth = 0.5) +
    geom_text(aes(y = ypos, label = label, colour = text_col), size = 3) +
    geom_text(data = x_lab, aes(grp, -0.02, label = lab), inherit.aes = FALSE,
              vjust = 1, size = 3, lineheight = 0.9) +
    facet_wrap(~ Gender) +
    scale_fill_manual(values = K_COLORS, breaks = K_LABELS, name = "Number of\nrisk factors") +
    scale_colour_identity() +
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = c(0.12, 0.02))) +
    labs(x = NULL, y = "Participants (%)", title = title) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 11))
}

ggsave(out("distribution_main.pdf"), plot_distribution("main"), width = 6, height = 4.5)
p_s6 <- plot_distribution("CTonly", an_long("CTonly")) / plot_distribution("bio", an_long("bio")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")
ggsave(out("distribution_AB.pdf"), p_s6, width = 6.5, height = 8.5)

## ---- Fig 4：A 1因子あたり OR（主解析＋感度分析）、B 5領域の OR（モデル4）---
# Main analysis（主解析の主要な仕様）を目立たせる
#   a: 「Main specification」の行を上に少し離して置き、薄い灰色の帯を敷き、項目名・P 値を太字にする。
#      点と線も主要な仕様だけ太く大きくし、感度分析の仕様は細く小さく、少し薄くする
#   b: 主解析（Main）は大きな黒丸と太い線、データセット A・B は小さな灰色の印と細い線
SPEC_GAP <- 0.6                                        # Main specification と他の仕様の間の追加の間隔
SPEC_Y   <- set_names(c(length(SPEC_LABELS) + SPEC_GAP, rev(seq_len(length(SPEC_LABELS) - 1))),
                      names(SPEC_LABELS))              # 各仕様の y 位置（Main specification が一番上）
spec_axis_labels <- as.expression(map(names(SPEC_LABELS), function(s)
  if (s == "main") bquote(bold(.(unname(SPEC_LABELS[s])))) else unname(SPEC_LABELS[s])))

est5 <- sens_all %>%
  filter(term %in% c("per factor (common)", "per factor (F)", "per factor (M)")) %>%
  mutate(est = dplyr::recode(term, "per factor (common)" = "common",
                             "per factor (F)" = "F", "per factor (M)" = "M"),
         is_main = spec == "main",
         y      = unname(SPEC_Y[spec]) +
                  case_when(est == "common" ~ 0.25, est == "F" ~ 0, TRUE ~ -0.25),
         analysis_f = factor(an_long(analysis), levels = ANALYSES$long),
         est      = factor(est, levels = names(EST_COLORS)))
int5 <- sens_all %>%
  filter(term == "interaction (M/F ratio)") %>%
  mutate(y          = unname(SPEC_Y[spec]),
         label      = str_c("P = ", p_num(p)),
         face       = if_else(spec == "main", "bold", "plain"),
         analysis_f = factor(an_long(analysis), levels = ANALYSES$long))

X_MIN <- 1; X_MAX <- 7; X_TXT <- 8.5
p5a <- ggplot(est5, aes(OR, y, colour = est, shape = est)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = SPEC_Y[["main"]] - 0.45,
           ymax = SPEC_Y[["main"]] + 0.45, fill = "grey92") +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  # 感度分析の仕様：細い線・小さい点・少し薄く
  geom_linerange(data = filter(est5, !is_main), aes(xmin = lower, xmax = upper),
                 linewidth = 0.35, alpha = 0.7) +
  geom_point(data = filter(est5, !is_main), size = 1.2, alpha = 0.7) +
  # 主要な仕様：太い線・大きい点
  geom_linerange(data = filter(est5, is_main), aes(xmin = lower, xmax = upper), linewidth = 0.6) +
  geom_point(data = filter(est5, is_main), size = 1.6) +
  geom_text(data = int5, aes(x = X_TXT, y = y, label = label, fontface = face), inherit.aes = FALSE,
            hjust = 0, size = 2.8, colour = "grey20") +
  facet_wrap(~ analysis_f, ncol = 1) +
  scale_x_log10(breaks = c(1, 1.5, 2, 3, 4, 6)) +
  scale_y_continuous(breaks = unname(SPEC_Y), labels = spec_axis_labels,
                     expand = expansion(add = 0.5)) +
  scale_colour_manual(values = EST_COLORS, labels = EST_LABELS, name = NULL) +
  scale_shape_manual(values = EST_SHAPES, labels = EST_LABELS, name = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 2.2, alpha = 1, linewidth = 0.8)),
         shape  = guide_legend(override.aes = list(size = 2.2, alpha = 1, linewidth = 0.8))) +
  coord_cartesian(xlim = c(X_MIN, X_MAX), clip = "off") +
  labs(x = "OR per additional risk factor (Firth, adjusted for age and gender; 95% CI, log scale)",
       y = NULL, caption = "Right: P for the risk count × gender interaction") +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(hjust = 0, face = "bold"),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.margin = margin(5, 60, 5, 5))

dom_levels <- unname(D5_LABELS)
dom5 <- models_all %>%
  filter(model == "M4 domains", term %in% dom_levels) %>%
  mutate(an_i = match(analysis, an_names),
         y    = length(dom_levels) + 1 - match(term, dom_levels) + (2 - an_i) * 0.25,
         analysis_f = factor(an_short(analysis), levels = ANALYSES$short))
# 印の形・大きさ・色・線の太さ（Main を大きな黒丸、A・B を小さな灰色の印に）
DOM_SHAPES <- set_names(c(16, 15, 17), ANALYSES$short)
DOM_SIZES  <- set_names(c(1.6, 1.1, 1.1), ANALYSES$short)
DOM_COLS   <- set_names(c("black", "grey50", "grey50"), ANALYSES$short)
DOM_LWD    <- set_names(c(0.6, 0.35, 0.35), ANALYSES$short)
p5b <- ggplot(dom5, aes(OR, y, shape = analysis_f, colour = analysis_f)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_linerange(aes(xmin = lower, xmax = upper, linewidth = analysis_f), show.legend = FALSE) +
  geom_point(aes(size = analysis_f)) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)) +
  scale_y_continuous(breaks = seq_along(dom_levels), labels = rev(dom_levels),
                     expand = expansion(add = 0.5)) +
  scale_shape_manual(values = DOM_SHAPES, name = NULL) +
  scale_size_manual(values = DOM_SIZES, name = NULL) +
  scale_colour_manual(values = DOM_COLS, name = NULL) +
  scale_linewidth_manual(values = DOM_LWD, guide = "none") +
  labs(x = "OR per additional factor within the domain (Model 4; 95% CI, log scale)", y = NULL) +
  theme_bw() +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(out("forest_OR_per_factor.pdf"), p5a, width = 9, height = 7)
ggsave(out("forest_OR_domains.pdf"), p5b, width = 7, height = 3.8)
ggsave(out("forest_OR.pdf"),
       p5a / p5b + plot_layout(heights = c(3, 1.1)) + plot_annotation(tag_levels = "a"),
       width = 9, height = 10)

## ---- 5領域の組み合わせ（UpSet プロット；主解析は Fig S10）--------------------
upset_plots <- list()   # 投稿用の図を作るために、解析ごとに上段・下段の図を残しておく
make_upset <- function(an) {
  scored <- results[[an]]$idx
  dvars  <- intersect(names(UPSET_LABELS), names(scored))   # B では D3 がないので4領域
  dlab   <- UPSET_LABELS[dvars]

  d <- scored %>%
    transmute(Gender = factor(Gender, levels = GENDER_LEVELS),
              grp    = if_else(case == 1, "Case", "Control"),
              across(all_of(dvars), ~ .x > 0))
  in_mat   <- as.matrix(d[dvars])
  d$combo  <- apply(in_mat, 1, function(r) if (any(r)) paste(dlab[r], collapse = " & ") else "None")
  d$degree <- rowSums(in_mat)

  N <- d %>% count(Gender, grp, name = "N")
  tab <- d %>%
    count(Gender, grp, combo, degree) %>%
    complete(Gender, grp, nesting(combo, degree), fill = list(n = 0)) %>%
    left_join(N, by = c("Gender", "grp")) %>%
    mutate(pct = n / N)

  # 観察されたすべての組み合わせを個別に表示する（少数の組み合わせもまとめない）
  plot_tab <- tab %>% rename(combo2 = combo)

  # 並び順：該当領域数の少ない順、同じ数の中では事件群の割合の高い順
  ord <- plot_tab %>%
    group_by(combo2, degree) %>%
    summarise(case_pct = sum(n[grp == "Case"]) / sum(N[grp == "Case"]), .groups = "drop") %>%
    arrange(degree, desc(case_pct))
  lv <- ord$combo2
  plot_tab <- plot_tab %>% mutate(combo2 = factor(combo2, levels = lv))

  facet_lab <- N %>%
    pivot_wider(names_from = grp, values_from = N) %>%
    mutate(lab = str_glue("{GENDER_LABELS[as.character(Gender)]} (case n = {Case}, control n = {Control})"))
  facet_lab <- set_names(facet_lab$lab, as.character(facet_lab$Gender))

  p_bar <- ggplot(plot_tab, aes(combo2, pct, fill = grp)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.3, size = 2.5) +
    facet_wrap(~ Gender, ncol = 1, labeller = as_labeller(facet_lab)) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = UPSET_COLORS, labels = c(Case = "Case group", Control = "Control group")) +
    labs(x = NULL, y = "Participants (%)", fill = NULL) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), legend.position = "top")

  dom_lv <- rev(unname(dlab))
  mat <- expand_grid(combo2 = lv, domain = unname(dlab)) %>%
    mutate(in_set = map2_lgl(combo2, domain, ~ .y %in% str_split(.x, " & ")[[1]]),
           combo2 = factor(combo2, levels = lv),
           y      = match(domain, dom_lv))
  seg <- mat %>% filter(in_set) %>% group_by(combo2) %>%
    summarise(ymin = min(y), ymax = max(y), .groups = "drop")

  p_mat <- ggplot(mat, aes(combo2, y)) +
    geom_segment(data = seg, aes(x = combo2, xend = combo2, y = ymin, yend = ymax),
                 inherit.aes = FALSE, linewidth = 0.8) +
    geom_point(aes(colour = in_set), size = 3) +
    scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey85"), guide = "none") +
    scale_y_continuous(breaks = seq_along(dom_lv), labels = dom_lv, expand = expansion(add = 0.5)) +
    scale_x_discrete(drop = FALSE, labels = function(x) if_else(x == "None", x, "")) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  p <- p_bar / p_mat + plot_layout(heights = c(4, 1.3))
  upset_plots[[an]] <<- list(bar = p_bar, mat = p_mat, n_combo = length(lv))
  f <- str_c("upset_", an, ".pdf")
  ggsave(out(f), p, width = max(6, 0.5 * length(lv) + 2.5), height = 7)
  plot_tab %>% arrange(Gender, combo2, grp) %>% mutate(analysis = an, .before = 1)
}
upset_values <- map_dfr(UPSET_ANALYSES, make_upset)

## ---- leave-one-out（Main・A・B；Fig S9）----------------------------------
loo_all <- collect("loo")
m1_all  <- models_all %>% filter(model == "M1 total", term == "k") %>% select(analysis, OR_m1 = OR)

loo_plot <- loo_all %>%
  mutate(Factor     = factor(unname(ITEM_LABELS[dropped]), levels = rev(unname(ITEM_LABELS))),
         analysis_f = factor(an_long(analysis), levels = ANALYSES$long))
p_s8 <- ggplot(loo_plot, aes(OR, Factor)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_vline(data = mutate(m1_all, analysis_f = factor(an_long(analysis), levels = ANALYSES$long)),
             aes(xintercept = OR_m1), colour = "grey60") +
  geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ analysis_f, nrow = 1, labeller = label_wrap_gen(28)) +
  scale_x_log10(breaks = c(1, 1.5, 2, 3, 4, 6)) +
  coord_cartesian(xlim = c(1, 6.5)) +
  labs(x = "OR per additional risk factor after removing the factor (95% CI, log scale)",
       y = "Factor removed") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
ggsave(out("leave_one_out.pdf"), p_s8, width = 10, height = 6)


## ---- 論文投稿用の図（Scientific Reports）--------------------------------------
#   作業用の図からタイトル・注（caption）を消し、文字を論文用の大きさ（7–8 pt、Arial）にして、
#   mm 単位の大きさで OUT_DIR/submission/ に保存する（説明は論文の figure legend に書く）
#   幅は Scientific Reports の段組に合わせる（1.5 段 120 mm、2 段 183 mm）
SUB_DIR      <- file.path(OUT_DIR, "submission")
FIG_FONT     <- "Arial"
FIG_FONT_PT  <- 7      # 目盛り・項目名・凡例・パネルの見出し（pt）
FIG_TITLE_PT <- 8      # 軸の見出し（pt）
FIG_LABEL_MM <- 2.2    # 図の中の数字（geom_text、mm 単位。2.2 mm ≈ 6.3 pt）
dir.create(SUB_DIR, recursive = TRUE, showWarnings = FALSE)

# PDF の出力装置：Mac は quartz（Arial を定義して使う）、それ以外は cairo_pdf
if (Sys.info()[["sysname"]] == "Darwin") {
  quartzFonts(Arial = c("ArialMT", "Arial-BoldMT", "Arial-ItalicMT", "Arial-BoldItalicMT"))
  sub_pdf <- function(filename, width, height, ...)
    grDevices::quartz(type = "pdf", file = filename, width = width, height = height, family = FIG_FONT)
} else {
  sub_pdf <- function(filename, width, height, ...)
    grDevices::cairo_pdf(filename, width = width, height = height, family = FIG_FONT)
}

# 1つの ggplot の文字を投稿用にそろえる（タイトル・注を消し、geom_text の大きさも変える）
to_submission <- function(p, label_mm = FIG_LABEL_MM) {
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomText") && !is.null(p$layers[[i]]$aes_params$size))
      p$layers[[i]]$aes_params$size <- label_mm
  }
  p +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(text         = element_text(family = FIG_FONT, size = FIG_FONT_PT, colour = "black"),
          axis.text    = element_text(size = FIG_FONT_PT, colour = "black"),
          axis.title   = element_text(size = FIG_TITLE_PT),
          strip.text   = element_text(size = FIG_FONT_PT),
          legend.text  = element_text(size = FIG_FONT_PT),
          legend.title = element_text(size = FIG_FONT_PT),
          plot.margin  = margin(1.5, 2, 1.5, 1.5, unit = "mm"))
}

save_submission <- function(p, name, width_mm, height_mm) {
  ggsave(file.path(SUB_DIR, str_c(name, ".pdf")), p, device = sub_pdf,
         width = width_mm / 25.4, height = height_mm / 25.4)
  if (requireNamespace("ragg", quietly = TRUE))
    ggsave(file.path(SUB_DIR, str_c(name, ".tiff")), p, device = ragg::agg_tiff,
           width = width_mm, height = height_mm, units = "mm", dpi = 600, compression = "lzw")
  message("投稿用の図: ", file.path(SUB_DIR, name), " (", width_mm, " × ", height_mm, " mm)")
}
# パネル記号（A, B）は太字 8 pt
tag_theme <- theme(plot.tag = element_text(family = FIG_FONT, size = 8, face = "bold"))

# Fig 3：総リスク数の分布（主解析）。1.5 段幅
save_submission(to_submission(plot_distribution("main")), "Fig3", width_mm = 120, height_mm = 80)

# Fig 4：A 1因子あたり OR（主解析と感度分析）、B 5領域の OR（モデル4）。2 段幅
#   A の右の P 値（交互作用）の説明は figure legend に書く
fig4 <- (to_submission(p5a) + theme(plot.margin = margin(1.5, 14, 1.5, 1.5, unit = "mm"))) /
  to_submission(p5b) +
  plot_layout(heights = c(3, 1.1)) + plot_annotation(tag_levels = "a") & tag_theme
save_submission(fig4, "Fig4", width_mm = 183, height_mm = 210)

# Fig S8：総リスク数の分布（データセット A・B）。パネルの見出しは消し、A・B の記号で示す
figS8 <- to_submission(plot_distribution("CTonly")) / to_submission(plot_distribution("bio")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") & tag_theme
save_submission(figS8, "FigS8", width_mm = 120, height_mm = 150)

# Fig S9：leave-one-out（Main・A・B）。2 段幅
save_submission(to_submission(p_s8), "FigS9", width_mm = 183, height_mm = 110)

# Fig S10：5領域の組み合わせ（UpSet プロット、主解析）。2 段幅
#   上段の棒の上の人数は 1.8 mm（約 5 pt）、下段の点は少し小さくする
up <- upset_plots[["main"]]
up_mat <- to_submission(up$mat)
for (i in seq_along(up_mat$layers)) {
  if (inherits(up_mat$layers[[i]]$geom, "GeomPoint"))   up_mat$layers[[i]]$aes_params$size      <- 2
  if (inherits(up_mat$layers[[i]]$geom, "GeomSegment")) up_mat$layers[[i]]$aes_params$linewidth <- 0.5
}
figS10 <- to_submission(up$bar, label_mm = 1.8) / up_mat + plot_layout(heights = c(4, 1.3))
save_submission(figS10, "FigS10", width_mm = 183, height_mm = 150)


## ============================================================
## 7. 図の元の数値と確認用のシート
## ============================================================
fig_sheets <- list(
  `Distribution values`    = dist_values("main") %>% mutate(Gender = sex_name(Gender)),
  `OR values`              = bind_rows(
    est5 %>% transmute(Panel = "A", Dataset = an_short(analysis), Specification = SPEC_LABELS[spec],
                       Estimate = as.character(EST_LABELS[as.character(est)]), OR, lower, upper, p),
    int5 %>% transmute(Panel = "A", Dataset = an_short(analysis), Specification = SPEC_LABELS[spec],
                       Estimate = "Interaction (men/women)", OR, lower, upper, p),
    dom5 %>% transmute(Panel = "B", Dataset = an_short(analysis), Specification = "Model 4",
                       Estimate = term, OR, lower, upper, p)),
  `Distribution AB values` = bind_rows(dist_values("CTonly"), dist_values("bio")) %>%
    mutate(Dataset = an_short(analysis), Gender = sex_name(Gender), .before = 1) %>% select(-analysis),
  `UpSet values`           = upset_values %>% mutate(Dataset = an_short(analysis), .before = 1) %>% select(-analysis),
  `LOO values`             = loo_all %>% transmute(Dataset = an_short(analysis),
                                           `Factor removed` = ITEM_LABELS[dropped], OR, lower, upper, p)
)

# 確認用：人数、欠損のある人、表4で使った標準誤差
checks <- bind_rows(
  collect("N") %>%
    left_join(collect("incomplete") %>% count(analysis, Gender, Group, name = "n_incomplete"),
              by = c("analysis", "Gender", "Group")) %>%
    mutate(n_incomplete = coalesce(n_incomplete, 0L)) %>%
    transmute(Dataset = an_short(analysis), Item = str_c(sex_name(Gender), ", ",
                                                         if_else(Group == CASE_GROUP, "case", "control")),
              Value = str_c("N = ", N, "; participants with any missing factor or age = ", n_incomplete)),
  eff3_all %>% distinct(analysis, model, vcov) %>%
    transmute(Dataset = an_short(analysis), Item = str_c("Table 4 / S5 standard errors (", model, ")"),
              Value = vcov)
) %>%
  arrange(match(Dataset, ANALYSES$short))

## ---- 書式を整えた Excel（論文の表 + 図の数値 + 確認用）---------------------
#   表のシート: 表題（1行目）、列見出し（上下に罫線・太字）、本体、最後の行の下に罫線
#              1列目の先頭の空白は字下げに変換。1列目以外が空の行（見出し行）は太字
#   図の数値と確認用のシート: 列見出しを太字にした一覧（数値は数値のまま）
FONT <- "Arial"

TABLE_TITLES <- c(
  `Table 3`   = "Table 3. Proportions of participants with each risk factor, domain, and subdomain by group and gender",
  `Table 4`   = "Table 4. Total number of risk factors by group and gender",
  `Table 5`   = "Table 5. Logistic regression models of case-group membership",
  `Table S10`  = "Supplementary Table S10. Thresholds for the continuous risk factors",
  `Table S4`  = "Supplementary Table S4. Proportions of participants with each risk factor, domain, and subdomain in the sensitivity datasets",
  `Table S5`  = "Supplementary Table S5. Supplementary results for the total number of risk factors",
  `Table S7`  = "Supplementary Table S7. Sensitivity analyses of the association between the number of risk factors and case-group membership",
  `Table S6`  = "Supplementary Table S6. Distribution of the total number of risk factors and ORs by category",
  `Table S8` = "Supplementary Table S8. Robustness checks and diagnostics for Model 3")

st_title  <- createStyle(fontName = FONT, fontSize = 10, textDecoration = "bold")
st_body   <- createStyle(fontName = FONT, fontSize = 9, valign = "center")
st_head   <- createStyle(fontName = FONT, fontSize = 9, textDecoration = "bold", valign = "center",
                         halign = "center", wrapText = TRUE, border = "TopBottom", borderColour = "black")
st_head1  <- createStyle(halign = "left")                    # 1列目の見出しは左寄せ
st_center <- createStyle(halign = "center")
st_sect   <- createStyle(textDecoration = "bold")            # 見出し行（性別・データセットなど）
st_last   <- createStyle(border = "Bottom", borderColour = "black")
st_plain_head <- createStyle(fontName = FONT, fontSize = 9, textDecoration = "bold",
                             border = "Bottom", borderColour = "black")

# 表の列幅（文字数の目安から決める。1列目は広め）
auto_widths <- function(tbl) {
  w <- map_dbl(seq_along(tbl), function(j) {
    x <- c(names(tbl)[j], as.character(tbl[[j]]))
    min(max(nchar(x, type = "width"), na.rm = TRUE) * 0.95 + 2, if (j == 1) 60 else 26)
  })
  pmax(w, 8)
}

add_table_sheet <- function(wb, sheet, tbl, title) {
  tbl <- mutate(tbl, across(everything(), as.character))
  first  <- coalesce(tbl[[1]], "")
  indent <- (nchar(first) - nchar(str_remove(first, "^ +"))) %/% 2   # 先頭の空白 2つで 1段
  tbl[[1]] <- str_remove(first, "^ +")
  is_sect <- if (ncol(tbl) > 1) apply(as.matrix(tbl[-1]), 1, function(r) all(is.na(r) | r == "")) else rep(FALSE, nrow(tbl))

  addWorksheet(wb, sheet, gridLines = FALSE)
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet, st_title, rows = 1, cols = 1)

  r0 <- 3                                              # 列見出しの行
  writeData(wb, sheet, tbl, startRow = r0, colNames = TRUE, keepNA = FALSE)
  nr <- nrow(tbl); nc <- ncol(tbl); body <- (r0 + 1):(r0 + nr)

  addStyle(wb, sheet, st_body, rows = c(r0, body), cols = 1:nc, gridExpand = TRUE)
  addStyle(wb, sheet, st_head, rows = r0, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, st_head1, rows = r0, cols = 1, stack = TRUE)
  if (nc > 1) addStyle(wb, sheet, st_center, rows = body, cols = 2:nc, gridExpand = TRUE, stack = TRUE)
  if (any(is_sect)) addStyle(wb, sheet, st_sect, rows = body[is_sect], cols = 1, stack = TRUE)
  for (lv in setdiff(unique(indent), 0))
    addStyle(wb, sheet, createStyle(indent = lv), rows = body[indent == lv], cols = 1, stack = TRUE)
  addStyle(wb, sheet, st_last, rows = max(body), cols = 1:nc, gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet, cols = 1:nc, widths = auto_widths(tbl))
  setRowHeights(wb, sheet, rows = r0, heights = 28)
}

add_plain_sheet <- function(wb, sheet, tbl) {
  tbl <- mutate(tbl, across(everything(), ~ if (is.numeric(.x)) .x else as.character(.x)))
  addWorksheet(wb, sheet, gridLines = FALSE)
  writeData(wb, sheet, tbl, startRow = 1, colNames = TRUE, keepNA = FALSE)
  addStyle(wb, sheet, st_body, rows = 1:(nrow(tbl) + 1), cols = 1:ncol(tbl), gridExpand = TRUE)
  addStyle(wb, sheet, st_plain_head, rows = 1, cols = 1:ncol(tbl), gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet, cols = 1:ncol(tbl), widths = "auto")
  freezePane(wb, sheet, firstRow = TRUE)
}

wb <- createWorkbook()
modifyBaseFont(wb, fontName = FONT, fontSize = 9)
# シートは論文の表番号の順に並べる
tables <- tables[c("Table 3", "Table 4", "Table 5",
                   str_c("Table S", c(4, 5, 6, 7, 8, 10)))]
iwalk(tables, ~ add_table_sheet(wb, .y, .x, TABLE_TITLES[[.y]]))
iwalk(fig_sheets, ~ add_plain_sheet(wb, .y, .x))
add_plain_sheet(wb, "Checks", checks)
saveWorkbook(wb, out("cumrisk_tables.xlsx"), overwrite = TRUE)

## ---- ベン図の数値（解析ごとに1ファイル。以前の <解析>/venn_values.xlsx と同じ形）----
#   venn_values_main.xlsx（図5）、venn_values_CTonly.xlsx（図 S11）、venn_values_bio.xlsx（図 S12）
iwalk(results, ~ write_xlsx(.x$venn_sheets, out(str_c("venn_values_", .y, ".xlsx"))))

## ---- ベン図の作成用：参加者ごとの因子・領域・ベン図の領域（解析ごとに1ファイル）----
#   以前の <解析>/scored_venn.csv と同じ内容
iwalk(results, function(res, an) {
  res$idx %>%
    select(-any_of(c("age10", "male"))) %>%
    relocate(ID, Group, Gender, case, age) %>%
    relocate(starts_with("region_"), .after = last_col()) %>%
    write_excel_csv(out(str_c("scored_venn_", an, ".csv")), na = "")
})

# 確認用の数値（丸める前の値）
write_xlsx(list(
  `Table 3 numeric`  = collect("stats2"),
  `Table 4 lm`       = eff3_all,
  `Table 4 nb`       = nb3_all,
  `Table 4 desc`     = collect("desc3"),
  `Table 4 adj mean` = collect("adj3"),
  `Models`           = models_all,
  `Sensitivity`      = sens_all,
  `Leave-one-out`    = loo_all,
  `Robust top`       = top_all,
  `Model3 diag`      = diag_all,
  `Model3 bootstrap` = boot_all,
  `Thresholds`       = collect("thr"),
  `Incomplete cases` = collect("incomplete")
), out("cumrisk_numeric.xlsx"))

saveRDS(map(results, "idx"), out("cumrisk_scored.rds"))
writeLines(capture.output(sessionInfo()), out("cumrisk_sessioninfo.txt"))

# 主な結果の一覧（コンソール）
sens_all %>%
  filter(term %in% c("per factor (common)", "interaction (M/F ratio)")) %>%
  transmute(Dataset = an_short(analysis), spec, term, `OR [95% CI]` = ci_txt(OR, lower, upper), P = p_num(p)) %>%
  print(n = Inf, width = Inf)

message("\nすべて完了: ", normalizePath(OUT_DIR))
