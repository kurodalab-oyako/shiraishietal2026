# color table for the Group × Gender interaction analysis ###########################################################
# A2A3analysis03_interaction.R の結果 (Results/interaction/lm.Rdata) から、論文の図用の色付き表 (xlsx) を作成する
#
# 列: 変数名 (短縮) | 変数名 [単位] | 群差 d (主効果, 女性, 男性) | 交互作用 Δd | 性差 d (一般群, 事件群) | 該当者数
#     群の主効果 = 男女の群差を同じ重みで平均した値。結果は「交互作用 → 主効果 → 性別ごとの群差」の順に読む
# 色: 群差 = 事件群で高い (橙) / 低い (青)、性差 = 男性で高い (緑) / 女性で高い (黄)、|d| = 0.2 / 0.5 / 0.8 で4段階
#     交互作用は色を付けず、FDR 補正後に有意なら太字 (星印も付ける)
# 印: 主解析のシートのみ、感度分析で有意性が変わったセルに A / B / C を付ける
#     (主解析と感度分析の両方で推定できた対比のみが対象。推定不能になった場合は印を付けない)
#
# シート: データセット (main / noA3 / bio) × モデル (age / crude) ごとに 1 枚
#        + combined: 年齢 (結果変数) だけ未調整モデル、他の変数は年齢調整モデルの結果を載せたシート (論文の本文用)
rm(list = ls())
Sys.setenv(LANGUAGE = "en_US.UTF-8")

library(tidyverse)
library(openxlsx)
library(effectsize)


## settings ####################################################################################################
dir_in        = file.path("Results", "interaction")
file_main     = file.path(dir_in, "lm.Rdata")              # 主解析 (setting_binary_method = "lm")
file_logistic = file.path(dir_in, "logistic.Rdata")        # 感度分析 (ロジスティック回帰); なければ印 C は付けない
file_setting  = "A2A3variables_setting.xlsx"               # 表の並び順 (for_table) とカテゴリー (sheet "category")
file_out      = file.path(dir_in, "lm_colortable.xlsx")

category_col  = NULL     # カテゴリー名の列 (NULL: "category_eng" があれば使用、なければ "category")
d_breaks      = c(.2, .5, .8)
digits        = 2

table_datasets = c("main", "noA3", "bio")
table_models   = c("age", "crude")
# 年齢だけ未調整モデル、他は年齢調整モデルの結果をまとめたシートを作るデータセット (シート名: <データセット>_combined)
table_combined_datasets = c("main")
# 最後の列に、群×性別のいずれかのセルが全員0または全員1の二値変数について、各セルの該当者数を載せる
# (ロジスティック回帰で分離となる変数。主解析で推定不能 (N/A) とした対比の根拠にもなる)
setting_sep_col = TRUE

# 列幅 (効果量の列は見出しを短くしたので狭くできる)
width_short = 22    # 変数名 (短縮)
width_name  = 40    # 変数名 [単位]
width_d     = 10    # 効果量の各列
width_sep   = 10    # 該当者数の列


## labels ######################################################################################################
font = "Arial"
lab = list(
  dataset = c(main = "Main analysis (control group includes parenting program participants)",
              noA3 = "Sensitivity analysis 1 (parenting program participants excluded)",
              bio  = "Sensitivity analysis 2 (non-biological parent–child relationships excluded)"),
  model = c(age = "Age-adjusted model", crude = "Unadjusted model",
            combined = "Age-adjusted model (age: unadjusted model)"),
  var_short = "Variable", var_name = "Description [unit]",
  # 見出し (3行目: 効果の種類、4行目: 列、5行目: n)
  grp = "Group difference, d\n(case − control)", int = "Interaction", sex = "Gender difference, d\n(men − women)",
  main = "Main", women = "Women", men = "Men", int_sub = "Δd", control = "Control", case = "Case",
  na_fit = "N/A", na_age = "—",
  legend = "Effect size |d|", lev = c("Very small (< 0.2)", "Small (0.2–0.5)", "Medium (0.5–0.8)", "Large (≥ 0.8)"),
  leg_cols = c("Higher in case", "Lower in case", "Higher in men", "Higher in women"),
  sep = "All 0 or all 1:\nevents/n")

# 色 (Very small, Small, Medium, Large)
pal = list(case_high = c("#FDF3E7", "#F6CF95", "#EFA640", "#E8771E"),
           case_low  = c("#EEF5FD", "#CFE3FA", "#7FB5F5", "#3E6FE8"),
           men       = c("#EFF8F1", "#CDEFD5", "#97D2A6", "#5BAA75"),
           women     = c("#FEFCEB", "#FDF6C0", "#FBEE85", "#F5E234"),
           na        = "#F2F2F2")


## load results ################################################################################################
load_env = function(f){ e = new.env(); load(f, envir = e); e }
res = load_env(file_main)
res_log = if(file.exists(file_logistic)) load_env(file_logistic) else NULL

if(!"GroupMain" %in% res$table_contrasts$effect){
  stop("lm.Rdata に群の主効果 (GroupMain) がありません。改訂後の A2A3analysis03_interaction.R を実行し直してください。")
}

vcov_setting = res$table_settings$value[res$table_settings$item == "vcov for emmeans"]
# analysis03 の setting_constant_na (TRUE なら、値が一定のセルの組の対比は N/A になっている)
const_na = identical(res$table_settings$value[res$table_settings$item == "constant cell pairs set to NA (lm)"], "TRUE")
var_age = res$table_settings$value[res$table_settings$item == "covariate (age)"]
if(length(var_age) != 1) var_age = "age_subject"
is_age_outcome = function(v) v == var_age | grepl("^A2_01", v)   # analysis03 と同じ定義

or_to_d = function(or){ d = oddsratio_to_d(or); if_else(is.finite(d), d, NA_real_) }
conv_d  = function(x, is_or){ out = x; out[is_or] = or_to_d(x[is_or]); out }

prep_contrasts = function(tc){
  tc %>%
    mutate(across(c(dataset, model, variable), as.character),
           is_or = scale %in% c("odds ratio", "ratio of odds ratios"),
           d     = ES_d,
           d_lwr = conv_d(ES_lwr, is_or),
           d_upr = conv_d(ES_upr, is_or),
           sig   = if_else(is.na(p_adj), NA, p_adj < .05),   # 推定不能 (P値なし) は NA のまま (「有意でない」と区別)
           col   = case_when(effect == "Interaction" ~ "Int",
                             effect == "GroupMain"   ~ "Main",
                             effect == "Group"       ~ paste0("Group_", subgroup),
                             effect %in% c("Gender", "Sex") ~ paste0("Gender_", subgroup),
                             TRUE ~ NA_character_)) %>%
    # constant: 対比する 2 セルとも値が一定で同じ値 (analysis03 で付与; 古い Rdata にはないので FALSE)
    mutate(constant = if("constant" %in% names(tc)) coalesce(as.logical(constant), FALSE) else FALSE) %>%
    select(dataset, model, variable, col, d, d_lwr, d_upr, p_adj, sig, constant)
}
con = prep_contrasts(res$table_contrasts)

# 表の列に割り当てられない結果 (col = NA) や、1 つのセルに複数の結果があると表や比較が正しくならないので確認
check_contrasts = function(x, label){
  bad = filter(x, is.na(col))
  if(nrow(bad) > 0){
    print(bad, n = 20)
    stop(label, " に表の列へ割り当てられない結果があります (effect / subgroup を確認してください)。")
  }
  dup = x %>% count(dataset, model, variable, col) %>% filter(n > 1)
  if(nrow(dup) > 0){
    print(dup, n = 20)
    stop(label, " に同じセルの結果が複数あります (上の表を参照)。同じ Rdata を2回読み込んでいないか確認してください。")
  }
}
# combined: 年齢 (結果変数) は未調整モデル、他の変数は年齢調整モデルの結果を model = "combined" として追加
add_combined = function(x){
  comb = x %>%
    filter((model == "age" & !is_age_outcome(variable)) | (model == "crude" & is_age_outcome(variable))) %>%
    mutate(model = "combined")
  bind_rows(x, comb)
}
con = add_combined(con)
check_contrasts(con, basename(file_main))

status = res$table_status %>%
  mutate(across(c(dataset, model, variable), as.character)) %>%
  select(dataset, model, variable, ok, any_of("vcov"))
if(!"vcov" %in% names(status)) status$vcov = ifelse(status$ok, vcov_setting, NA_character_)   # 古い Rdata 用
status = add_combined(status)


## sensitivity marks (主解析と有意性が異なる場合) ###########################################################
sig_of = function(x, ds) filter(x, dataset == ds) %>% select(model, variable, col, sig)
ref = sig_of(con, "main")
compare_sig = function(other, letter){
  inner_join(ref, other, by = c("model", "variable", "col"), suffix = c("", "_o")) %>%
    filter(!is.na(sig), !is.na(sig_o), sig != sig_o) %>%   # 両方で推定できた対比のみ比較
    transmute(model, variable, col, letter = letter)
}
bi_vars = res$settings_vars$variable[res$settings_vars$datatype %in% "bi"]
marks = bind_rows(
  compare_sig(sig_of(con, "noA3"), "A"),
  compare_sig(sig_of(con, "bio"),  "B"),
  if(!is.null(res_log)){
    {con_log = add_combined(prep_contrasts(res_log$table_contrasts)); check_contrasts(con_log, basename(file_logistic))
     compare_sig(sig_of(con_log, "main") %>% filter(variable %in% bi_vars), "C")}
  }) %>%
  group_by(model, variable, col) %>%
  summarise(mark = paste(sort(unique(letter)), collapse = ""), .groups = "drop")


## cells with all 0 or all 1 (最後の列) ##########################################################################
# 群×性別のいずれかのセルが全員0または全員1だった二値変数 (lm.Rdata のセル集計) について、
# セルごとの該当者数/解析人数を「女性: 一般群, 事件群 / 男性: 一般群, 事件群」の形で示す
use_sep = setting_sep_col
sep_tbl = if(use_sep){
  res$table_cells %>%
    mutate(across(c(dataset, model, variable, Group, Gender), as.character),
           cell = paste(Group, Gender, sep = "_"),
           nn   = paste0(round(n_yes), "/", n)) %>%
    filter(variable %in% bi_vars) %>%
    group_by(dataset, model, variable) %>%
    filter(any(n_yes == 0 | n_yes == n)) %>%
    ungroup() %>%
    select(dataset, model, variable, cell, nn) %>%
    pivot_wider(names_from = cell, values_from = nn) %>%
    mutate(txt = sprintf("W: %s, %s\nM: %s, %s", CT_F, A2_F, CT_M, A2_M)) %>%
    select(dataset, model, variable, txt) %>%
    add_combined()
}else NULL


## rows: 並び順とカテゴリー #################################################################################
vlist = readxl::read_excel(file_setting, sheet = "variable list", skip = 1, na = c("", "NA")) %>%
  filter(data_prep2 == 1) %>%
  transmute(variable = variable_R, for_table = as.numeric(for_table))

cats0 = readxl::read_excel(file_setting, sheet = "category")
cat_col = if(!is.null(category_col)) category_col else
  if("category_eng" %in% names(cats0)) "category_eng" else "category"
cats = cats0 %>%
  filter(!is.na(for_table), !is.na(.data[[cat_col]])) %>%
  transmute(key = as.numeric(for_table), label = as.character(.data[[cat_col]]), type = "cat") %>%
  distinct(key, .keep_all = TRUE)

vars_tbl = res$settings_vars %>%
  mutate(order0 = row_number()) %>%
  left_join(vlist, by = "variable") %>%
  transmute(key = coalesce(for_table, 1e6 + order0),    # for_table がない変数は最後に
            variable, short = variable_short, name = variable_name, type = "var")

rows = bind_rows(cats, vars_tbl) %>%
  arrange(key, desc(type == "cat")) %>%
  mutate(block = cumsum(type == "cat")) %>%
  group_by(block) %>% filter(any(type == "var")) %>% ungroup()   # 変数のないカテゴリーは除く


## helpers #####################################################################################################
p_stars = function(p) case_when(is.na(p) ~ "", p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
fmt = function(x) formatC(x, format = "f", digits = digits)
level_of = function(d) findInterval(abs(d), d_breaks) + 1   # 1 = very small ... 4 = large

fill_of = function(col, d){
  if(is.na(d) || col == "Int") return(NA_character_)
  lv = level_of(d)
  if(col == "Main" || startsWith(col, "Group_")) return(if(d >= 0) pal$case_high[lv] else pal$case_low[lv])
  if(startsWith(col, "Gender_"))                 return(if(d >= 0) pal$men[lv] else pal$women[lv])
  NA_character_
}

# 効果量の列 (表の 3 列目から)
cell_cols = c("Main", "Group_F", "Group_M", "Int", "Gender_CT", "Gender_A2")
col_of    = setNames(seq_along(cell_cols) + 2, cell_cols)   # 列番号

# 5 行目の n: 群差の列は「事件群/一般群」、性差の列は「女性/男性」
n_row = function(ds){
  n = res$table_N %>% mutate(dataset = as.character(dataset)) %>% filter(dataset == ds)
  g = function(x) if(x %in% names(n)) n[[x]][1] else NA
  c(Main      = sprintf("%s/%s", g("A2_F") + g("A2_M"), g("CT_F") + g("CT_M")),
    Group_F   = sprintf("%s/%s", g("A2_F"), g("CT_F")),
    Group_M   = sprintf("%s/%s", g("A2_M"), g("CT_M")),
    Int       = "",
    Gender_CT = sprintf("%s/%s", g("CT_F"), g("CT_M")),
    Gender_A2 = sprintf("%s/%s", g("A2_F"), g("A2_M")))
}


## styles ######################################################################################################
st_base   = createStyle(fontName = font, fontSize = 9, valign = "center", wrapText = TRUE)
st_center = createStyle(halign = "center")
st_bold   = createStyle(textDecoration = "bold")
st_title  = createStyle(fontName = font, fontSize = 11, textDecoration = "bold")
st_sub    = createStyle(fontName = font, fontSize = 9)
st_head   = createStyle(fontName = font, fontSize = 9, textDecoration = "bold", halign = "center",
                        valign = "center", wrapText = TRUE)
st_head_n = createStyle(fontName = font, fontSize = 8, halign = "center", valign = "center",
                        border = "Bottom", borderColour = "#404040")
st_cat    = createStyle(fontName = font, fontSize = 10, textDecoration = "bold", valign = "bottom",
                        border = "Bottom", borderColour = "#404040")
st_row    = createStyle(border = "Bottom", borderColour = "#BFBFBF")
st_note   = createStyle(fontName = font, fontSize = 9, valign = "top", wrapText = TRUE)
st_num    = createStyle(fontSize = 10)   # 数値の入ったセル
st_short  = createStyle(fontSize = 10)   # 変数名 (短縮) の列
st_sep    = createStyle(fontSize = 8, halign = "left")   # 全員0または全員1のセルがある変数の該当者数の列


## build sheets ################################################################################################
wb = createWorkbook()

make_sheet = function(ds, md){
  sh = paste(ds, md, sep = "_")
  addWorksheet(wb, sh, gridLines = FALSE)
  n_eff = length(cell_cols)
  ncol  = 2 + n_eff + if(use_sep) 1 else 0
  c_sep = 2 + n_eff + 1
  sep_s = if(use_sep) filter(sep_tbl, dataset == ds, model == md) else NULL

  ## 見出し (1–2 行目: タイトル、3–5 行目: 列見出し)
  writeData(wb, sh, lab$dataset[[ds]], startRow = 1, startCol = 1)
  writeData(wb, sh, lab$model[[md]],   startRow = 2, startCol = 1)
  addStyle(wb, sh, st_title, rows = 1, cols = 1)
  addStyle(wb, sh, st_sub,   rows = 2, cols = 1)

  head3 = c(lab$var_short, lab$var_name, lab$grp, "", "", lab$int, lab$sex, "", if(use_sep) lab$sep)
  head4 = c("", "", lab$main, lab$women, lab$men, lab$int_sub, lab$control, lab$case, if(use_sep) "")
  head5 = c("", "", n_row(ds), if(use_sep) "")
  writeData(wb, sh, t(head3), startRow = 3, colNames = FALSE)
  writeData(wb, sh, t(head4), startRow = 4, colNames = FALSE)
  writeData(wb, sh, t(head5), startRow = 5, colNames = FALSE)
  mergeCells(wb, sh, cols = 1, rows = 3:5)
  mergeCells(wb, sh, cols = 2, rows = 3:5)
  mergeCells(wb, sh, cols = col_of[["Main"]]:col_of[["Group_M"]], rows = 3)
  mergeCells(wb, sh, cols = col_of[["Gender_CT"]]:col_of[["Gender_A2"]], rows = 3)
  if(use_sep) mergeCells(wb, sh, cols = c_sep, rows = 3:5)
  addStyle(wb, sh, st_head,   rows = 3:4, cols = 1:ncol, gridExpand = TRUE)
  addStyle(wb, sh, st_head_n, rows = 5,   cols = 1:ncol, gridExpand = TRUE)
  addStyle(wb, sh, createStyle(fontName = font, fontSize = 9, textDecoration = "bold", valign = "center",
                               wrapText = TRUE, border = "Bottom", borderColour = "#404040"),
           rows = 3:5, cols = c(1:2, if(use_sep) c_sep), gridExpand = TRUE)

  ## 本体
  con_s = filter(con, dataset == ds, model == md)
  st_s  = filter(status, dataset == ds, model == md)
  mk_s  = if(ds == "main") filter(marks, model == md) else marks[0, ]

  r0 = 6
  r  = r0
  fills = list(); bolds = list(); cat_rows = c(); var_rows = c(); missing_status = c()
  for(i in seq_len(nrow(rows))){
    if(rows$type[i] == "cat"){
      writeData(wb, sh, rows$label[i], startRow = r, startCol = 1)
      mergeCells(wb, sh, cols = 1:ncol, rows = r)
      cat_rows = c(cat_rows, r)
    }else{
      v = rows$variable[i]
      ok = st_s$ok[st_s$variable == v]
      # 結果がないのは年齢の年齢調整モデルだけのはず。それ以外は変数名の不一致などの可能性
      if(length(ok) == 0 && !(md == "age" && isTRUE(is_age_outcome(v)))) missing_status = c(missing_status, v)
      vals = map_chr(cell_cols, function(cc){
        if(length(ok) == 0) return(lab$na_age)        # モデルを当てはめていない (年齢の年齢調整など)
        if(!isTRUE(ok[1]))  return(lab$na_fit)        # 推定できなかった
        x = filter(con_s, variable == v, col == cc)
        if(nrow(x) == 0 || is.na(x$d)) return(lab$na_fit)
        m = mk_s$mark[mk_s$variable == v & mk_s$col == cc]
        m = if(length(m) == 0) "" else paste0(" ", m)   # 印の前にスペース (例: 0.45** AB)
        dg = if(isTRUE(x$constant)) "†" else ""        # 対比する 2 セルとも値が一定 (例: 両群とも全員 0)
        sprintf("%s%s%s%s\n[%s, %s]", fmt(x$d), p_stars(x$p_adj), dg, m, fmt(x$d_lwr), fmt(x$d_upr))
      })
      sep_txt = if(use_sep){ s = sep_s$txt[sep_s$variable == v]; if(length(s) == 0) "" else s[1] } else NULL
      writeData(wb, sh, t(c(rows$short[i], rows$name[i], vals, sep_txt)), startRow = r, colNames = FALSE)
      for(j in seq_along(cell_cols)){
        x = filter(con_s, variable == v, col == cell_cols[j])
        f = if(vals[j] %in% c(lab$na_fit, lab$na_age)) pal$na else if(nrow(x) == 1) fill_of(cell_cols[j], x$d) else NA
        if(!is.na(f)) fills[[length(fills) + 1]] = c(r, col_of[[cell_cols[j]]], f)
        if(nrow(x) == 1 && cell_cols[j] == "Int" && isTRUE(x$sig)) bolds[[length(bolds) + 1]] = c(r, col_of[["Int"]])
      }
      var_rows = c(var_rows, r)
    }
    r = r + 1
  }
  last = r - 1
  if(length(missing_status) > 0){
    warning(sh, ": 解析結果がない変数があります (変数名の不一致の可能性): ", paste(missing_status, collapse = ", "))
  }

  c_eff = unname(col_of)
  addStyle(wb, sh, st_base, rows = r0:last, cols = 1:ncol, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sh, st_center, rows = r0:last, cols = 3:ncol, gridExpand = TRUE, stack = TRUE)
  if(length(var_rows) > 0) addStyle(wb, sh, st_row, rows = var_rows, cols = 1:ncol, gridExpand = TRUE, stack = TRUE)
  if(length(var_rows) > 0) addStyle(wb, sh, st_num, rows = var_rows, cols = c_eff, gridExpand = TRUE, stack = TRUE)
  if(length(var_rows) > 0 && use_sep) addStyle(wb, sh, st_sep, rows = var_rows, cols = c_sep, stack = TRUE)
  if(length(var_rows) > 0) addStyle(wb, sh, st_short, rows = var_rows, cols = 1, stack = TRUE)
  if(length(cat_rows) > 0) addStyle(wb, sh, st_cat, rows = cat_rows, cols = 1:ncol, gridExpand = TRUE, stack = TRUE)
  for(f in fills){
    addStyle(wb, sh, createStyle(fgFill = f[3]), rows = as.integer(f[1]), cols = as.integer(f[2]), stack = TRUE)
  }
  for(b in bolds) addStyle(wb, sh, st_bold, rows = b[1], cols = b[2], stack = TRUE)

  ## 凡例 (群差の色は「主効果」「女性」の列の下、性差の色は「一般群」「事件群」の列の下)
  lr = last + 2
  leg_c = c(col_of[["Main"]], col_of[["Group_F"]], col_of[["Gender_CT"]], col_of[["Gender_A2"]])
  writeData(wb, sh, lab$legend, startRow = lr, startCol = 2)
  for(k in seq_along(leg_c)) writeData(wb, sh, lab$leg_cols[k], startRow = lr, startCol = leg_c[k])
  addStyle(wb, sh, createStyle(fontName = font, fontSize = 8, textDecoration = "bold", halign = "center",
                               wrapText = TRUE), rows = lr, cols = 2:ncol, gridExpand = TRUE)
  for(k in 4:1){
    rr = lr + (5 - k)
    writeData(wb, sh, lab$lev[k], startRow = rr, startCol = 2)
    addStyle(wb, sh, createStyle(fontName = font, fontSize = 9, halign = "right"), rows = rr, cols = 2)
    sw = list(c(leg_c[1], pal$case_high[k]), c(leg_c[2], pal$case_low[k]),
              c(leg_c[3], pal$men[k]), c(leg_c[4], pal$women[k]))
    for(s in sw) addStyle(wb, sh, createStyle(fgFill = s[2], border = "TopBottomLeftRight", borderColour = "#BFBFBF"),
                          rows = rr, cols = as.integer(s[1]))
  }

  ## 注
  # 対比に使った分散共分散行列 (HC3 が計算できず通常の標準誤差に切り替えた変数があれば明記)
  vc_s = filter(st_s, ok %in% TRUE)
  n_classic = sum(vc_s$vcov == "classic", na.rm = TRUE)
  n_hc3     = sum(vc_s$vcov == "HC3", na.rm = TRUE)
  vcov_txt = if(n_hc3 > 0 && n_classic == 0) "HC3 heteroskedasticity-consistent standard errors" else
    if(n_hc3 > 0) paste0("HC3 heteroskedasticity-consistent standard errors (conventional standard errors for ",
                         n_classic, " variable", if(n_classic > 1) "s" else "", " for which HC3 could not be computed)") else
      "conventional standard errors"

  notes = c(
    "Values: d [95% CI]. d = covariate-adjusted contrast divided by the residual SD of the model (y ~ Group × Gender [+ age]).",
    paste0("Group difference: Main = main effect of group (group differences in women and men averaged with equal weights); ",
           "Women and Men = group difference within each gender (simple main effects). ",
           "Interaction Δd = group difference in men − group difference in women (no fill); significant interactions after FDR correction ",
           "are shown in bold with asterisks. Gender differences are shown for reference and were not interpreted."),
    "n: case/control for group differences; women/men for gender differences.",
    paste0("Tests and CIs of the contrasts were based on ", vcov_txt, "; the CIs of d also incorporate uncertainty in the residual SD. ",
           "* / ** / ***: FDR-adjusted p < .05 / .01 / .001 (Benjamini–Hochberg, applied for each effect across all variables). ",
           "CIs are not adjusted for multiplicity."),
    "Binary variables were analysed as linear probability models. Effect-size categories (colors) are visual guides only and were not treated as thresholds of substantive importance.",
    paste0("Letters (main analysis only): significance changed when A = parenting program participants were excluded; ",
           "B = participants whose child was not biologically related to them and/or their partner were excluded; ",
           "C = binary variables were analysed by logistic regression. Letters are given only for contrasts estimable in both analyses; ",
           "contrasts that became non-estimable in a sensitivity analysis are not marked."),
    paste0(lab$na_fit, ": could not be estimated (",
           if(const_na) "when both cells being contrasted had all 0 [or all 1], and the interaction for that variable; " else "",
           "in the logistic regression sensitivity analysis, contrasts involving any cell with all 0 or all 1); ", lab$na_age,
           ": not applicable (e.g., age-adjusted model for age). n in the column headers is the total number of participants in the dataset; ",
           "the analysed n may be smaller for each variable due to missing data.")
  )
  if(any(con_s$constant)){
    notes = c(notes,
      paste0("†: both cells being contrasted had all 0 (or all 1), so the data contain no information on this contrast ",
             "(also marked for the interaction and the main effect of group). In the age-adjusted model, a nonzero estimate reflects only the age adjustment ",
             "(the age coefficient estimated from the other cells), and the CI does not reflect the actual uncertainty; these values should not be interpreted."))
  }
  if(use_sep){
    notes = c(notes,
      paste0("Last column: for binary variables with all 0 or all 1 in at least one group-by-gender cell, ",
             "the number of participants with the outcome / analysed n in each cell (W, women; M, men; control, case). ",
             "Contrasts involving such cells were non-estimable in the logistic regression sensitivity analysis."))
  }
  if(md == "combined"){
    notes = c(notes,
      "Age is shown from the unadjusted model because it cannot be adjusted for itself; all other variables are from the age-adjusted model (FDR correction was applied within each model).")
  }
  # 注は 1 行ずつ全列を結合して折り返す (行の高さは文字数から概算)
  widths = c(width_short, width_name, rep(width_d, n_eff), if(use_sep) width_sep)
  chars_per_line = floor(sum(widths) * 1.1)
  nr = lr + 6
  for(k in seq_along(notes)){
    rr = nr + k - 1
    writeData(wb, sh, notes[k], startRow = rr, startCol = 1)
    mergeCells(wb, sh, cols = 1:ncol, rows = rr)
    addStyle(wb, sh, st_note, rows = rr, cols = 1:ncol, gridExpand = TRUE)
    setRowHeights(wb, sh, rows = rr, heights = 13 * ceiling(nchar(notes[k]) / chars_per_line) + 3)
  }

  ## 幅と高さ
  setColWidths(wb, sh, cols = 1:ncol, widths = widths)
  setRowHeights(wb, sh, rows = 3, heights = 26)   # 効果の種類 (2行)
  setRowHeights(wb, sh, rows = 4, heights = 14)
  setRowHeights(wb, sh, rows = 5, heights = 13)
  if(length(var_rows) > 0) setRowHeights(wb, sh, rows = var_rows, heights = 30)
  if(length(cat_rows) > 0) setRowHeights(wb, sh, rows = cat_rows, heights = 20)
  freezePane(wb, sh, firstActiveRow = r0, firstActiveCol = 3)
}

for(ds in table_combined_datasets) make_sheet(ds, "combined")   # 論文の本文用 (先頭のシート)
for(ds in table_datasets) for(md in table_models) make_sheet(ds, md)

saveWorkbook(wb, file_out, overwrite = TRUE)
message("保存しました: ", normalizePath(file_out))
