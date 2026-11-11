# group comparison: Group (CT vs A2) x Gender interaction ######################################################
# 一般群 (CT) と事件群 (A2) の群間比較: 群 × 性別 の交互作用を含む回帰分析
#
# データセット
#   main : 主解析      CT 群に A3 群 (子育て支援プログラム参加群) を含める (CT = CT + A3)
#   noA3 : 感度分析 1  CT 群から A3 群を除く (CT = CT のみ)
#   bio  : 感度分析 2  自分またはパートナーが子と非血縁である回答者を除く (群分けは main と同じ)
#
# モデル (各変数・各データセットについて)
#   crude : y ~ Group * Gender
#   age   : y ~ Group * Gender + age   (年齢は中心化; 年齢そのものが結果変数の場合は当てはめない)
#
# 検定 (結果の報告順: 交互作用 → 群の主効果 → 性別ごとの群差 [単純主効果])
#   - Type III 検定 (Group, Gender とも sum contrast)。lm は通常の F 検定と HC3 (頑健な標準誤差) の F 検定
#   - emmeans による対比
#       Interaction : (A2 − CT)[男性] − (A2 − CT)[女性]
#       GroupMain   : 群の主効果 (A2 − CT)。男女の群差を同じ重みで平均した値 (交互作用を含むモデルのまま推定)
#       Group       : 性別ごとの群差 (A2 − CT) = 群の単純主効果
#       Gender      : 群ごとの性差 (M − F)。性別は男女比の違いを考慮するために含めたもので、解釈しない
#   - FDR 補正: データセット × モデル × 効果 (× 下位群) ごとに、全変数を 1 つのファミリーとして補正
#
# 効果量
#   lm       : Type III の各項は偏 omega^2、emmeans の対比は標準化平均差 d (= 対比 / 残差 SD)
#   logistic : オッズ比 (交互作用はオッズ比の比)、色分け用に d へ換算した値 (ES_d) も出力
#
# 投稿用の図 (Results/interaction/submission/; 主解析・年齢調整モデルのみ)
#   setting_binary_method = "lm"       で実行 → Fig2.pdf  (群の主効果の forest plot)
#                                              FigS3.pdf (男女の群差 d の散布図)
#   setting_binary_method = "logistic" で実行 → FigS4.pdf (二値変数の群の主効果の forest plot)
#   タイトル・副題・注は消し (説明は論文の figure legend に書く)、フォントは 7–8 pt、
#   幅は Scientific Reports の段組 (1.5 段 120 mm / 2 段 183 mm) に合わせる
#   ragg パッケージがあれば 600 dpi の TIFF も出力する
#
# Before starting, clear all objects from workspace & restart R!
rm(list = ls())
Sys.setenv(LANGUAGE = "en_US.UTF-8")

library(tidyverse)
library(broom)
library(car)
library(emmeans)
library(effectsize)
library(sandwich)

# 群の主効果 (性別について平均) は意図した推定なので、emmeans の交互作用の注意を表示しない
emm_options(msg.interaction = FALSE)


## settings ####################################################################################################
file_data       = "Data_latest/A2A3analysis_maindata.Rdata" # fulldata_main を含む
file_setting     = "A2A3variables_setting.xlsx"            # for_comp == 1 の変数を解析
dir_out          = file.path("Results", "interaction")     # 出力先 (なければ作成)

var_age    = "age_subject"                                 # 共変量 (年齢)
var_nonbio = "I4_03_自分orパートナーと非血縁の子"            # 1 = 自分またはパートナーが子と非血縁
setting_nonbio_keep_na = FALSE   # 感度分析 2 で var_nonbio が欠測の回答者を残すか (FALSE: 除外)

# 年齢そのものが結果変数の場合は年齢調整モデルを当てはめない
is_age_outcome = function(v) v == var_age | grepl("^A2_01", v)

# 二値変数 (datatype == "bi") の解析方法
#   "lm"       : 主解析 (全変数を連続値として線形回帰)
#   "logistic" : 感度分析 (二値変数のみロジスティック回帰、連続変数は lm のまま)
setting_binary_method = "lm"

# emmeans (下位検定) の分散共分散行列: "classic" / "HC3" (HC3 は lm のみ)
setting_vcov = "HC3"

# 線形モデルで、対比する 2 つのセルがどちらも値が一定かつ同じ値 (例: 女性は両群とも全員 0) の場合の扱い
#   FALSE (既定): 値はそのまま残し、Contrasts の constant 列に TRUE を付ける (表・図では † で示す)
#   TRUE        : その対比 (と交互作用) を推定不能 (NA) にし、FDR 補正からも除く
setting_constant_na = FALSE

setting_p_adjust   = "BH"     # FDR 補正の方法
setting_save_models = FALSE    # TRUE にするとモデルオブジェクトも .Rdata に保存 (ファイルが大きくなる)

# 図 (男女の群差 d の散布図) の設定
setting_plot_label    = "name"    # 点のラベル: "name" (変数の短縮名) / "number" (番号; 対応表を CSV で出力)
setting_plot_label_ns = TRUE      # FALSE にすると、男女とも群差が有意でない変数 (白抜きの点) にはラベルを付けない
# setting_binary_method = "logistic" のとき、図にロジスティック回帰で解析した二値変数だけを載せるか
# (FALSE: 線形回帰のままの連続変数も載せる。連続変数の結果は "lm" の図と同じ)
setting_plot_logistic_only = TRUE
# 図の上で d の正負を反転する変数 (値が小さいほどリスクを表す変数)。ラベルに (R) を付ける
# 変数名 (variable_R) または短縮名 (variable_short_eng) で指定する
plot_reverse_vars = c("Age at having 1st child")


## variable list ###############################################################################################
settings_vars = readxl::read_excel(file_setting, sheet = "variable list", skip = 1, na = c("", "NA")) %>%
  filter(data_prep2 == 1, for_comp == 1) %>%
  arrange(sort) %>%
  transmute(variable       = variable_R,
            variable_name  = variable_name_eng,
            variable_short = variable_short_eng,
            datatype)


## data ########################################################################################################
load(file_data)   # fulldata_main
fulldata = fulldata_main %>% 
  mutate(Gender = factor(as.character(Gender), levels = c("F", "M")))

temp_missing = setdiff(c(settings_vars$variable, var_age, var_nonbio), names(fulldata))
if(length(temp_missing) > 0){
  stop("データにない変数があります: ", paste(temp_missing, collapse = ", "))
}

get_info = function(col) if(col %in% names(fulldata)) as.character(fulldata[[1, col]]) else NA_character_
file_original = get_info("OriginalFile")
date_original = get_info("OriginalDataDate")

# 群の再コード: A2 以外 (CT, A3) を CT とする
recode_group = function(data){
  mutate(data, Group = factor(case_when(Group == "A2" ~ "A2", 
                                        Group %in% c("CT", "A3") ~ "CT"), 
                              levels = c("CT", "A2")))
}

data_main = recode_group(fulldata)
data_noA3 = fulldata %>% filter(Group %in% c("CT", "A2")) %>% recode_group()

temp_nonbio = data_main[[var_nonbio]]
temp_keep = if(setting_nonbio_keep_na) is.na(temp_nonbio) | temp_nonbio == 0 else 
  !is.na(temp_nonbio) & temp_nonbio == 0
data_bio = data_main[temp_keep, ]

datasets = list(main = data_main, noA3 = data_noA3, bio = data_bio)

labels_dataset = c(main = "Main (control group includes parenting program participants)",
                   noA3 = "Sensitivity 1 (parenting program participants excluded)",
                   bio  = "Sensitivity 2 (non-biological parent–child relationships excluded)")

# データセットごとの人数 (群 × 性別)
table_N = imap_dfr(datasets, ~ count(.x, Group, Gender, name = "N") %>% mutate(dataset = .y)) %>% 
  mutate(Group = as.character(Group), Gender = as.character(Gender)) %>% 
  pivot_wider(names_from = c(Group, Gender), values_from = N, names_sep = "_") %>% 
  mutate(dataset_label = labels_dataset[dataset]) %>% 
  relocate(dataset, dataset_label)

table_exclusion = tibble(dataset = "bio", 
                         n_main = nrow(data_main), 
                         n_nonbio = sum(temp_nonbio != 0, na.rm = TRUE), 
                         n_nonbio_NA = sum(is.na(temp_nonbio)), 
                         NA_kept = setting_nonbio_keep_na, 
                         n_bio = nrow(data_bio))


## helper functions ############################################################################################

# モデル当てはめ時のエラーと警告を捕捉する
fit_capture = function(expr){
  warn = character(0)
  fit = withCallingHandlers(
    tryCatch(expr, error = function(e) e), 
    warning = function(w){
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(fit = fit, 
       error = inherits(fit, "error"), 
       warning = if(length(warn) == 0) NA_character_ else paste(unique(warn), collapse = " | "))
}

# emmeans の summary の列名をそろえる (lm / glm, 推定周辺平均 / 対比 で列名が異なるため)
std_names = c(emmean = "estimate", prob = "estimate", response = "estimate", 
              odds.ratio = "estimate", ratio = "estimate", 
              lower.CL = "lwr", upper.CL = "upr", asymp.LCL = "lwr", asymp.UCL = "upr", 
              t.ratio = "statistic", z.ratio = "statistic", p.value = "p")
tidy_emm = function(x){
  df = as.data.frame(x)
  hit = names(df) %in% names(std_names)
  names(df)[hit] = std_names[names(df)[hit]]
  as_tibble(df)
}

# オッズ比を d に換算 (完全分離などで有限でない場合は NA)
or_to_d = function(or){ d = oddsratio_to_d(or); if_else(is.finite(d), d, NA_real_) }

p_stars = function(p){
  case_when(is.na(p) ~ NA_character_, p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
}


## analysis for one variable × one dataset × one model #########################################################
analyze_one = function(data, v, datatype, model_type){
  is_bi    = isTRUE(datatype == "bi")
  logistic = setting_binary_method == "logistic" && is_bi
  use_age  = model_type == "age"
  method   = if(logistic) "logistic" else "lm"
  
  dat = tibble(y = data[[v]], Group = data$Group, Gender = data$Gender, age = data[[var_age]]) %>% 
    filter(!is.na(y), !is.na(Group), !is.na(Gender))
  if(use_age) dat = filter(dat, !is.na(age)) %>% mutate(age = age - mean(age))  # 年齢を中心化
  notes = character(0)   # 注意事項の記録 (Status の note 列)

  # 群 × 性別のセルごとの記述統計
  cells = dat %>% 
    group_by(Group, Gender, .drop = FALSE) %>% 
    summarise(n     = n(), 
              mean  = if(n() > 0) mean(y) else NA_real_, 
              sd    = if(n() > 1) sd(y) else NA_real_, 
              n_yes = if(is_bi) as.numeric(sum(y == 1)) else NA_real_, 
              .groups = "drop") %>% 
    mutate(Group = as.character(Group), Gender = as.character(Gender))
  
  vcov_used = if(logistic) "model-based" else "classic"   # 下位検定 (emmeans) の分散共分散行列; HC3 が使えたら更新
  make_status = function(ok, reason = NA_character_, warning = NA_character_, note = NA_character_){
    tibble(method = method, n = nrow(dat), ok = ok, reason = reason, warning = warning, note = note, 
           vcov = if(ok) vcov_used else NA_character_)
  }
  out = list(status = NULL, anova = NULL, contrasts = NULL, cells = cells, model = NULL)
  
  ### 回帰可能かの確認
  reason = NA_character_
  if(any(cells$n < 2)){
    reason = "群×性別に2人未満のセルがある"
  }else if(!logistic && sd(dat$y) == 0){
    reason = "全員が同じ値"
  }else if(logistic && !all(dat$y %in% c(0, 1))){
    reason = paste0("0/1 以外の値がある: ", paste(sort(unique(dat$y[!dat$y %in% c(0, 1)])), collapse = ", "))
  }else if(logistic && length(unique(dat$y)) < 2){
    reason = "全員が同じ値"
  }
  if(!is.na(reason)){
    out$status = make_status(FALSE, reason)
    return(out)
  }
  
  # ロジスティック回帰: セル内で全員 0 または全員 1 (準完全分離) のセル
  # 変数全体は除外せず、そのセルを含む対比だけを推定不可 (NA) にする (下の「分離の処理」)
  # (glm() は分離があってもエラーにならず、極端に大きな係数・標準誤差を返すため、そのままでは使えない)
  sep_cells = if(logistic) filter(cells, n_yes == 0 | n_yes == n) else cells[0, ]
  sep_key   = paste(sep_cells$Group, sep_cells$Gender)
  
  ### 当てはめ (sum contrast; emmeans の結果は contrast の種類によらない)
  # 式の環境には解析用データだけを置く (emmeans がデータを再取得するため & 保存サイズを抑えるため)
  env_fit = new.env(parent = globalenv())
  env_fit$dat = dat
  fml = as.formula(if(use_age) "y ~ Group * Gender + age" else "y ~ Group * Gender", env = env_fit)
  ctr = list(Group = "contr.sum", Gender = "contr.sum")
  
  f = fit_capture(
    if(logistic) glm(fml, data = dat, family = binomial, contrasts = ctr) 
    else lm(fml, data = dat, contrasts = ctr)
  )
  if(f$error){
    out$status = make_status(FALSE, paste("当てはめエラー:", conditionMessage(f$fit)))
    return(out)
  }
  fit = f$fit
  if(anyNA(coef(fit))){
    out$status = make_status(FALSE, "推定できない係数がある", f$warning)
    return(out)
  }
  if(logistic){
    if(!isTRUE(fit$converged)){
      out$status = make_status(FALSE, "ロジスティック回帰が収束しなかった", f$warning)
      return(out)
    }
    # 年齢を含むモデルでは、セル内に 0 と 1 があっても年齢によって分離が起こりうる
    pr = fitted(fit)
    if(nrow(sep_cells) == 0 && any(pr < 1e-8 | pr > 1 - 1e-8)){
      notes = c(notes, "予測確率が 0 または 1 に極めて近い個人がいる (年齢による分離の可能性; 推定値の解釈に注意)")
    }
  }
  
  ### Type III 検定と下位検定
  post = tryCatch({

    ## Type III
    if(logistic){
      aov3 = car::Anova(fit, type = 3, test.statistic = "LR")
      anova_tbl = tidy(aov3) %>%
        transmute(term, df, statistic, p = p.value, p_HC3 = NA_real_,
                  ES = NA_real_, ES_lwr = NA_real_, ES_upr = NA_real_, ES_lev = NA_character_, 
                  ES_method = NA_character_)
    }else{
      aov3 = car::Anova(fit, type = 3)
      # HC3 はセル内の残差がほぼすべて 0 (セル内で全員同じ値) の場合などに計算できない → NA にする
      aov3_hc3 = tryCatch(car::Anova(fit, type = 3, white.adjust = "hc3"), error = function(e) NULL)
      hc3_tbl = if(is.null(aov3_hc3)){
        notes = c(notes, "HC3 の検定は計算不可 (p_HC3 = NA)")
        tibble(term = character(0), p_HC3 = numeric(0))
      }else{
        transmute(tidy(aov3_hc3), term, p_HC3 = p.value)
      }
      # 偏 omega^2 (CI は effectsize の既定の片側 95%、上限は 1)
      omega = omega_squared(aov3, partial = TRUE, verbose = FALSE) %>% 
        as_tibble() %>% 
        select(term = Parameter, ES = any_of(c("Omega2_partial", "Omega2")), 
               ES_lwr = CI_low, ES_upr = CI_high)
      anova_tbl = tidy(aov3) %>% 
        transmute(term, df, statistic, p = p.value) %>% 
        left_join(hc3_tbl, by = "term") %>%
        left_join(omega, by = "term") %>% 
        mutate(ES_lev = as.character(interpret_omega_squared(ES, rules = "field2013")), 
               ES_method = "partial omega-squared from conventional Type III SS (not HC3)")
    }
    anova_tbl = anova_tbl %>% 
      filter(!term %in% c("(Intercept)", "Residuals")) %>% 
      # car パッケージの recode() と衝突するので dplyr:: を明示する
      mutate(term = dplyr::recode(term, "Group:Gender" = "Group x Gender", "age" = "Age"))

    ## emmeans
    # HC3 を指定した場合、計算できない (行列が特異・NA を含む) ときは通常の分散共分散行列を使う
    vc = NULL
    if(!logistic && setting_vcov == "HC3"){
      vc = tryCatch(sandwich::vcovHC(fit, type = "HC3"), error = function(e) NULL)
      if(!is.null(vc) && (anyNA(vc) || qr(vc)$rank < ncol(vc))) vc = NULL
      if(is.null(vc)) notes = c(notes, "emmeans の HC3 は計算不可のため通常の分散を使用")
      if(!is.null(vc)) vcov_used = "HC3"
    }
    emm_cells = if(is.null(vc)) emmeans(fit, ~ Group * Gender) else
      emmeans(fit, ~ Group * Gender, vcov. = vc)
    
    con_int   = contrast(emm_cells, interaction = c("revpairwise", "revpairwise"))  # (A2−CT)[M] − (A2−CT)[F]
    # 群の主効果: 性別について同じ重みで平均した推定周辺平均の差 (A2 − CT)
    # (同じモデル・同じ分散共分散行列から求めるので、HC3 の Type III 検定の Group 項と同じ検定になる)
    con_main  = contrast(emmeans(emm_cells, ~ Group), method = "revpairwise", adjust = "none")
    con_group = contrast(emm_cells, method = "revpairwise", by = "Gender", adjust = "none")  # A2 − CT | 性別
    con_gender = contrast(emm_cells, method = "revpairwise", by = "Group",  adjust = "none")  # M − F | 群
    
    make_con = function(con, effect, label, by_var = NULL){
      s = tidy_emm(
        if(logistic) summary(con, infer = c(TRUE, TRUE), type = "response", adjust = "none") 
        else summary(con, infer = c(TRUE, TRUE), adjust = "none")
      )
      # ロジスティック回帰 (漸近 z 検定) では df 列がない場合がある
      df_value = if("df" %in% names(s)) s$df else rep(NA_real_, nrow(s))
      # 交互作用はロジスティック回帰では「オッズ比の比」(OR[M] / OR[F]; 帰無値 1)
      scale_value = if(!logistic) "mean difference" else if(effect == "Interaction") "ratio of odds ratios" else "odds ratio"
      res = tibble(effect   = effect, 
                   subgroup = if(is.null(by_var)) NA_character_ else as.character(s[[by_var]]), 
                   contrast = label, 
                   scale    = scale_value, 
                   estimate = s$estimate, SE = s$SE, df = df_value, 
                   lwr = s$lwr, upr = s$upr, statistic = s$statistic, p = s$p)
      if(logistic){
        mutate(res, ES = estimate, ES_lwr = lwr, ES_upr = upr, 
               ES_lev = as.character(interpret_oddsratio(ES, rules = "cohen1988", log = FALSE)), 
               ES_d = or_to_d(ES), 
               ES_method = "odds ratio; ES_d = approximate conversion to d (log OR × √3 / π)")
      }else{
        # 標準化平均差 d = 対比 / モデルの残差 SD (残差 SD の不確かさも CI に反映)
        e = tidy_emm(summary(eff_size(con, sigma = sigma(fit), edf = df.residual(fit), 
                                      method = "identity"), infer = c(TRUE, FALSE)))
        mutate(res, ES = e$effect.size, ES_lwr = e$lwr, ES_upr = e$upr, 
               ES_lev = as.character(interpret_cohens_d(ES, rules = "cohen1988")), 
               ES_d = ES, 
               ES_method = "adjusted contrast divided by model residual SD")
      }
    }
    
    con_tbl = bind_rows(
      make_con(con_int,   "Interaction", "(A2 - CT)[M] - (A2 - CT)[F]"),
      make_con(con_main,  "GroupMain",   "A2 - CT (averaged over gender)"),
      make_con(con_group, "Group",       "A2 - CT", by_var = "Gender"),
      make_con(con_gender, "Gender",     "M - F",   by_var = "Group")
    ) %>% 
      mutate(vcov = vcov_used)
    
    ## 符号の確認: emmeans の対比が想定どおり (A2 − CT, M − F) になっているか、セル平均から直接計算して照合
    # (lm: 平均の尺度、glm: ロジットの尺度)。一致しなければ止める (Status に「検定エラー」として記録される)
    cm = tidy_emm(summary(emm_cells))
    cell_mean = function(gr, ge) cm$estimate[cm$Group == gr & cm$Gender == ge]
    expect = c(int   = (cell_mean("A2", "M") - cell_mean("CT", "M")) - (cell_mean("A2", "F") - cell_mean("CT", "F")),
               main  = ((cell_mean("A2", "F") - cell_mean("CT", "F")) + (cell_mean("A2", "M") - cell_mean("CT", "M"))) / 2,
               grp_F = cell_mean("A2", "F") - cell_mean("CT", "F"),
               gen_CT = cell_mean("CT", "M") - cell_mean("CT", "F"))
    got = c(int    = tidy_emm(summary(con_int))$estimate,
            main   = tidy_emm(summary(con_main))$estimate,
            grp_F  = with(tidy_emm(summary(con_group)), estimate[Gender == "F"]),
            gen_CT = with(tidy_emm(summary(con_gender)), estimate[Group == "CT"]))
    if(any(abs(got - expect) > 1e-6 * pmax(1, abs(expect)))){
      stop("emmeans の対比の向きが想定 (A2 − CT, M − F) と異なります")
    }
    
    emm_tbl = tidy_emm(if(logistic) summary(emm_cells, type = "response") else summary(emm_cells)) %>% 
      transmute(Group = as.character(Group), Gender = as.character(Gender), 
                emm = estimate, emm_SE = SE, emm_lwr = lwr, emm_upr = upr)
    
    ## 分離の処理: 分離したセルを含む対比は推定値・P値を NA にする (FDR 補正の対象からも外れる)
    #   交互作用・群の主効果: 4 セルすべてを含む → 分離が1つでもあれば NA
    #   性別ごとの群差 (A2 − CT | 性別 g): セル (CT, g), (A2, g)
    #   群ごとの性差   (M − F | 群 k)    : セル (k, F), (k, M)
    con_tbl = con_tbl %>%
      mutate(separation = case_when(
        length(sep_key) == 0 ~ FALSE,
        effect %in% c("Interaction", "GroupMain") ~ TRUE,
        effect == "Group"  ~ paste("CT", subgroup) %in% sep_key | paste("A2", subgroup) %in% sep_key, 
        effect == "Gender" ~ paste(subgroup, "F") %in% sep_key | paste(subgroup, "M") %in% sep_key, 
        TRUE ~ FALSE))
    if(any(con_tbl$separation)){
      con_tbl = con_tbl %>% 
        mutate(across(c(estimate, SE, lwr, upr, statistic, p, ES, ES_lwr, ES_upr, ES_d), 
                      ~ if_else(separation, NA_real_, .)), 
               ES_lev = if_else(separation, NA_character_, ES_lev))
      emm_tbl = emm_tbl %>% 
        mutate(sep = paste(Group, Gender) %in% sep_key, 
               across(c(emm, emm_SE, emm_lwr, emm_upr), ~ if_else(sep, NA_real_, .))) %>% 
        select(-sep)
      notes = c(notes, paste0("分離 (全員0または全員1) のセル: ", 
                              paste(sep_cells$Group, sep_cells$Gender, 
                                    paste0(sep_cells$n_yes, "/", sep_cells$n), collapse = ", "), 
                              " → そのセルを含む対比は NA"))
    }
    
    ## 値が一定のセルの組 (線形モデル): 対比する 2 つのセルがどちらもセル内で値が一定で、かつ同じ値の場合
    #   (例: 女性は両群とも全員 0) その対比にはデータの情報がない。年齢調整モデルの推定値は他のセルから推定した
    #   年齢の傾きによる見かけの値になり、標準誤差もほぼ 0 になる。
    #   constant = TRUE を付ける (交互作用と群の主効果は性別ごとの群差を含むので、どれかが該当すればそれらにも付ける)。
    #   setting_constant_na = TRUE のときは推定値・P値も NA にする (FDR 補正の対象からも外れる)。
    #   (ロジスティック回帰では、このようなセルは上の「分離の処理」で NA になっている)
    con_tbl = mutate(con_tbl, constant = FALSE)
    if(!logistic){
      cell_key = paste(cells$Group, cells$Gender)
      is_const = function(k){ i = match(k, cell_key); !is.na(i) && cells$n[i] >= 2 && isTRUE(cells$sd[i] == 0) }
      const_pair = function(k1, k2) is_const(k1) && is_const(k2) &&
        isTRUE(all.equal(cells$mean[match(k1, cell_key)], cells$mean[match(k2, cell_key)]))
      con_tbl = con_tbl %>%
        mutate(constant = case_when(
          effect == "Group"  ~ map_lgl(subgroup, ~ const_pair(paste("CT", .x), paste("A2", .x))),
          effect == "Gender" ~ map_lgl(subgroup, ~ const_pair(paste(.x, "F"), paste(.x, "M"))),
          TRUE ~ FALSE))
      if(any(con_tbl$constant)){
        const_lab = with(filter(con_tbl, constant), paste0(effect, "[", subgroup, "]"))
        con_tbl = mutate(con_tbl, constant = constant | effect == "Interaction" |
                           (effect == "GroupMain" & any(effect == "Group" & constant)))
        if(setting_constant_na){
          con_tbl = con_tbl %>%
            mutate(across(c(estimate, SE, lwr, upr, statistic, p, ES, ES_lwr, ES_upr, ES_d),
                          ~ if_else(constant, NA_real_, .)),
                   ES_lev = if_else(constant, NA_character_, ES_lev))
        }
        notes = c(notes, paste0("対比する2セルとも値が一定で同じ値: ", paste(const_lab, collapse = ", "),
                                if(setting_constant_na) " → その対比と交互作用は NA" else
                                  " → 値は残し constant = TRUE (年齢調整による見かけの値; 信頼区間は不確かさを反映しない)"))
      }
    }

    list(anova = anova_tbl, contrasts = con_tbl, emm = emm_tbl)
  }, error = function(e) e)
  
  if(inherits(post, "error")){
    out$status = make_status(FALSE, paste("検定エラー:", conditionMessage(post)), f$warning)
    return(out)
  }
  
  out$status    = make_status(TRUE, NA_character_, f$warning,
                              note = if(length(notes) == 0) NA_character_ else paste(notes, collapse = " | "))
  out$anova     = post$anova
  out$contrasts = post$contrasts
  out$cells     = left_join(cells, post$emm, by = c("Group", "Gender"))
  if(setting_save_models) out$model = fit
  out
}


## run #########################################################################################################
jobs = expand_grid(dataset = names(datasets), 
                   variable = settings_vars$variable, 
                   model = c("crude", "age")) %>% 
  filter(!(model == "age" & is_age_outcome(variable)))

run_job = function(dataset, variable, model){
  datatype = settings_vars$datatype[settings_vars$variable == variable][1]
  res = analyze_one(datasets[[dataset]], variable, datatype, model)
  tbls = map(res[c("status", "anova", "contrasts", "cells")], function(x){
    if(is.null(x)) NULL else 
      mutate(x, dataset = !!dataset, variable = !!variable, model = !!model, .before = 1)
  })
  c(tbls, list(model_fit = res$model))
}

results = vector("list", nrow(jobs))
for(i in seq_len(nrow(jobs))){
  if(i %% 50 == 1) message(sprintf("[%d/%d] %s / %s / %s", 
                                   i, nrow(jobs), jobs$dataset[i], jobs$variable[i], jobs$model[i]))
  results[[i]] = run_job(jobs$dataset[i], jobs$variable[i], jobs$model[i])
}


## tables ######################################################################################################
add_labels = function(tbl){
  tbl %>% 
    left_join(select(settings_vars, variable, variable_name, variable_short, datatype), by = "variable") %>% 
    mutate(dataset_label = labels_dataset[dataset], 
           dataset  = factor(dataset, levels = names(datasets)), 
           model    = factor(model, levels = c("crude", "age")), 
           variable = factor(variable, levels = settings_vars$variable)) %>% 
    relocate(dataset, dataset_label, model, variable, variable_name, variable_short, datatype)
}

table_status = map_dfr(results, "status") %>% add_labels() %>% arrange(dataset, model, variable)

# 下位検定 (emmeans): データセット × モデル × 効果 × 下位群ごとに全変数で FDR 補正
table_contrasts = map_dfr(results, "contrasts") %>% 
  group_by(dataset, model, effect, subgroup) %>% 
  mutate(n_tests = sum(!is.na(p)), 
         p_adj = p.adjust(p, method = setting_p_adjust)) %>% 
  ungroup() %>% 
  mutate(sig = p_stars(p), sig_adj = p_stars(p_adj)) %>% 
  add_labels() %>% 
  arrange(dataset, model, variable, factor(effect, levels = c("Interaction", "GroupMain", "Group", "Gender")), subgroup)

# Type III: データセット × モデル × 項ごとに全変数で FDR 補正
table_anova = map_dfr(results, "anova") %>% 
  group_by(dataset, model, term) %>% 
  mutate(n_tests = sum(!is.na(p)), 
         p_adj = p.adjust(p, method = setting_p_adjust), 
         p_HC3_adj = p.adjust(p_HC3, method = setting_p_adjust)) %>% 
  ungroup() %>% 
  mutate(sig = p_stars(p), sig_adj = p_stars(p_adj), 
         sig_HC3 = p_stars(p_HC3), sig_HC3_adj = p_stars(p_HC3_adj)) %>% 
  add_labels() %>% 
  arrange(dataset, model, variable, term)

# 群 × 性別のセルの記述統計と推定周辺平均
table_cells = map_dfr(results, "cells") %>% add_labels() %>% arrange(dataset, model, variable, Group, Gender)

# 要約表 (1 行 = データセット × モデル × 変数)
table_summary = table_contrasts %>% 
  mutate(key = if_else(is.na(subgroup), effect, paste(effect, subgroup, sep = "_"))) %>% 
  select(dataset, model, variable, key, estimate, lwr, upr, ES, ES_lwr, ES_upr, p, p_adj) %>% 
  pivot_wider(names_from = key, values_from = c(estimate, lwr, upr, ES, ES_lwr, ES_upr, p, p_adj), 
              names_glue = "{key}_{.value}") %>% 
  right_join(select(table_status, dataset, dataset_label, model, variable, variable_name, 
                    variable_short, datatype, method, n, ok, reason, note),
             by = c("dataset", "model", "variable")) %>%
  select(dataset, dataset_label, model, variable, variable_name, variable_short, datatype,
         method, n, ok, reason, note,
         starts_with("Interaction_"), starts_with("GroupMain_"), starts_with("Group_F_"), starts_with("Group_M_"),
         starts_with("Gender_CT_"), starts_with("Gender_A2_")) %>% 
  arrange(dataset, model, variable)

table_settings = tibble(
  item = c("data file", "original file", "original data date", "variable settings", 
           "covariate (age)", "binary variables", "vcov for emmeans", "p adjustment", 
           "non-biological variable", "keep NA in non-biological variable",
           "constant cell pairs set to NA (lm)", "created"),
  value = c(file_data, file_original, date_original, file_setting,
            var_age, setting_binary_method, setting_vcov, setting_p_adjust,
            var_nonbio, as.character(setting_nonbio_keep_na),
            as.character(setting_constant_na), format(Sys.time())))

message(sprintf("回帰不可: %d / %d", sum(!table_status$ok), nrow(table_status)))


## export ######################################################################################################
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)   # Results/interaction を作成
# 出力ファイル名は解析方法 ("lm" / "logistic") のみ
# 例: lm.xlsx, lm.Rdata, lm_sessioninfo.txt, lm_ESplot_main_age.pdf
file_name = setting_binary_method

to_xlsx = function(tbl) mutate(tbl, across(where(is.factor), as.character))
writexl::write_xlsx(
  list(Summary   = table_summary, 
       Contrasts = table_contrasts, 
       TypeIII   = table_anova, 
       Cells     = table_cells, 
       Status    = table_status, 
       N         = table_N, 
       Exclusion = table_exclusion, 
       Settings  = table_settings) %>% map(to_xlsx), 
  path = file.path(dir_out, paste0(file_name, ".xlsx")), 
  format_headers = FALSE)

if(setting_save_models){
  models = set_names(map(results, "model_fit"), 
                     paste(jobs$dataset, jobs$variable, jobs$model, sep = "|"))
  save(table_summary, table_contrasts, table_anova, table_cells, table_status, table_N, 
       table_exclusion, table_settings, settings_vars, models, 
       file = file.path(dir_out, paste0(file_name, ".Rdata")))
}else{
  save(table_summary, table_contrasts, table_anova, table_cells, table_status, table_N, 
       table_exclusion, table_settings, settings_vars, 
       file = file.path(dir_out, paste0(file_name, ".Rdata")))
}

writeLines(capture.output(sessionInfo()), file.path(dir_out, paste0(file_name, "_sessioninfo.txt")))
message("保存先: ", normalizePath(dir_out))


## plots: 男女の群差 (d) の散布図 ##############################################################################
# x = 男性の群差、y = 女性の群差 (いずれも d: 事件群 − 一般群、交互作用モデルの共通の残差 SD で標準化)
# データセット (主解析・感度分析1・感度分析2) × モデル (未調整・年齢調整) ごとに 1 ファイル
if(TRUE){
  library(ggrepel)
  source("Scripts/Japanese_font.R")   # Mac: quartzFonts で "Arial" を定義するために使用

  is_mac = Sys.info()[["sysname"]] == "Darwin"
  plot_font = "Arial"
  # Mac は quartz (Japanese_font.R のフォント定義を使用)、それ以外は cairo_pdf で保存
  pdf_device = if(is_mac){
    function(filename, width, height, ...){
      grDevices::quartz(type = "pdf", file = filename, width = width, height = height, family = plot_font)
    }
  }else{
    function(filename, width, height, ...){
      grDevices::cairo_pdf(filename, width = width, height = height, family = plot_font)
    }
  }
  
  ## 図中の文言
  txt = list(model = c(crude = "Unadjusted model", age = "Age-adjusted model"), 
             int = c("Interaction n.s.", "Interaction significant"), 
             grp = c("Significant in both genders", "Significant in women only", 
                     "Significant in men only", "Not significant in either gender"), 
             legend_grp = "Group difference (FDR < .05)", legend_int = "Group × gender interaction (FDR < .05)", 
             x = "Group difference in men (d: Case − Control)", 
             y = "Group difference in women (d: Case − Control)", 
             sub = "Dashed line: equal difference in women and men; grid: d = ±0.2, ±0.5, ±0.8; bars: unadjusted 95% CIs; fill and shape: FDR-adjusted p-values", 
             caption = paste("A significant group difference in one gender but not the other does not by itself", 
                             "indicate a gender difference; gender differences were evaluated by the", 
                             "Group × gender interaction (point shape)."), 
             caption_or = "For binary variables, d was approximately converted from odds ratios (log OR × √3 / π).",
             # Used when no point has a significant interaction (no shape legend)
             sub_noint = "Dashed line: equal difference in women and men; grid: d = ±0.2, ±0.5, ±0.8; bars: unadjusted 95% CIs; fill: FDR-adjusted p-values",
             caption_noint = paste("A significant group difference in one gender but not the other does not by itself",
                                   "indicate a gender difference; gender differences were evaluated by the",
                                   "Group × gender interaction, none of which was significant after FDR correction."),
             dropped = "Not shown because the group difference was non-estimable in at least one gender: ",
             flag = paste("†: all participants in both groups of one gender had the same value (e.g., no events);",
                          "the data contain no information on the group difference in that gender, a nonzero estimate",
                          "reflects only the age adjustment, and its CI does not reflect the actual uncertainty."),
             rev = "(R)", rev_note = "(R) = reverse: sign of d reversed")
  # 塗り: 群差の有意性 (有意差なしは白抜き)、形: 交互作用の有意性 (○ = NS、◇ = 有意)
  fill_grp  = setNames(c("#7B3294", "#E66101", "#3B8BC9", "white"), txt$grp)
  shape_int = setNames(c(21, 23), txt$int)
  size_int  = setNames(c(1.8, 2.6), txt$int)
  
  ## 投稿用の図 (Scientific Reports) ------------------------------------------------------------------------
  # 作業用の図からタイトル・副題・注を消し、フォントを論文用の大きさ (7–8 pt) にして、mm 単位の大きさで保存する
  # (図の説明は論文の figure legend に書く。変数名の † と (R)、凡例、軸の見出しは図に残す)
  sub_dir = file.path(dir_out, "submission")
  fig_font_pt  = 7     # 目盛り・変数名・凡例の文字 (pt)
  fig_title_pt = 8     # 軸の見出し (pt)
  save_submission = function(p, name, width_mm, height_mm){
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
    p2 = p +
      labs(title = NULL, subtitle = NULL, caption = NULL) +
      theme(text         = element_text(family = plot_font, size = fig_font_pt, colour = "black"),
            axis.text    = element_text(size = fig_font_pt, colour = "black"),
            axis.text.y  = element_text(size = fig_font_pt, colour = "black"),
            axis.title   = element_text(size = fig_title_pt),
            legend.text  = element_text(size = fig_font_pt),
            legend.title = element_text(size = fig_font_pt),
            plot.margin  = margin(2, 3, 2, 2, unit = "mm"))
    ggsave(file.path(sub_dir, paste0(name, ".pdf")), plot = p2, device = pdf_device,
           width = width_mm / 25.4, height = height_mm / 25.4)
    if(requireNamespace("ragg", quietly = TRUE)){
      ggsave(file.path(sub_dir, paste0(name, ".tiff")), plot = p2, device = ragg::agg_tiff,
             width = width_mm, height = height_mm, units = "mm", dpi = 600, compression = "lzw")
    }
    message("投稿用の図: ", file.path(sub_dir, name), " (", width_mm, " × ", round(height_mm), " mm)")
  }
  is_submission = function(ds, md) ds == "main" && md == "age"

  # オッズ比の場合 (setting_binary_method = "logistic") は信頼区間も d に換算
  to_d = function(x, is_or){ out = x; out[is_or] = or_to_d(x[is_or]); out }
  
  # 反転する変数 (変数名・短縮名のどちらで指定してもよい)
  reverse_ids = settings_vars %>% 
    filter(variable %in% plot_reverse_vars | variable_short %in% plot_reverse_vars) %>% 
    pull(variable)
  not_found = setdiff(plot_reverse_vars, 
                      unlist(select(filter(settings_vars, variable %in% reverse_ids), 
                                    variable, variable_short)))
  if(length(not_found) > 0) warning("plot_reverse_vars に該当する変数がありません: ", 
                                    paste(not_found, collapse = ", "))
  
  # ロジスティック回帰の感度分析では、ロジスティック回帰で解析した変数だけを図に載せる
  plot_vars = if(setting_binary_method == "logistic" && setting_plot_logistic_only){
    table_status %>% filter(method == "logistic") %>% distinct(variable) %>% pull(variable) %>% as.character()
  }else{
    as.character(settings_vars$variable)
  }
  
  plot_base = table_contrasts %>% 
    filter(effect == "Group", as.character(variable) %in% plot_vars) %>% 
    mutate(is_or = scale %in% c("odds ratio", "ratio of odds ratios"), 
           d     = ES_d, 
           d_lwr = to_d(ES_lwr, is_or), 
           d_upr = to_d(ES_upr, is_or)) %>% 
    select(dataset, model, variable, variable_short, subgroup, d, d_lwr, d_upr, p_adj) %>% 
    pivot_wider(names_from = subgroup, values_from = c(d, d_lwr, d_upr, p_adj)) %>%   # d_F, p_adj_M など
    left_join(table_contrasts %>% 
                filter(effect == "Interaction") %>% 
                select(dataset, model, variable, p_int_adj = p_adj), 
              by = c("dataset", "model", "variable"))
  # 男女どちらかの群差が推定不能 (NA) の変数は図に載せず、図の注に変数名を記載する
  plot_dropped = plot_base %>%
    filter(is.na(d_M) | is.na(d_F)) %>%
    mutate(name = coalesce(variable_short, as.character(variable))) %>%
    select(dataset, model, name)
  # 対比する 2 セルとも値が一定 (例: 女性は両群とも全員 0) の群差がある変数: ラベルに † を付け、図の注に記載
  plot_flag = if("constant" %in% names(table_contrasts)){
    table_contrasts %>%
      filter(effect == "Group", constant %in% TRUE) %>%
      distinct(dataset, model, variable) %>%
      mutate(flag = TRUE)
  }else tibble(dataset = character(0), model = character(0), variable = character(0), flag = logical(0))
  plot_base = plot_base %>%
    filter(!is.na(d_M), !is.na(d_F)) %>%
    left_join(plot_flag, by = c("dataset", "model", "variable")) %>%
    mutate(flag = coalesce(flag, FALSE)) %>%
    mutate(
      # 向きの反転 (点と信頼区間の両端を反転)
      reversed = as.character(variable) %in% reverse_ids, 
      sgn = if_else(reversed, -1, 1), 
      across(c(d_F, d_M), ~ sgn * .), 
      lwr_F = if_else(reversed, -d_upr_F, d_lwr_F), upr_F = if_else(reversed, -d_lwr_F, d_upr_F), 
      lwr_M = if_else(reversed, -d_upr_M, d_lwr_M), upr_M = if_else(reversed, -d_lwr_M, d_upr_M), 
      # 有意性
      int_sig = factor(if_else(!is.na(p_int_adj) & p_int_adj < .05, txt$int[2], txt$int[1]), 
                       levels = txt$int), 
      sig_F = !is.na(p_adj_F) & p_adj_F < .05, 
      sig_M = !is.na(p_adj_M) & p_adj_M < .05, 
      grp_sig = factor(case_when(sig_F & sig_M ~ txt$grp[1], 
                                 sig_F         ~ txt$grp[2], 
                                 sig_M         ~ txt$grp[3], 
                                 TRUE          ~ txt$grp[4]), 
                       levels = txt$grp), 
      # ラベル: 番号 (変数一覧の順) または短縮名
      no = match(as.character(variable), settings_vars$variable), 
      name = coalesce(variable_short, as.character(variable)), 
      label = if(setting_plot_label == "number") as.character(no) else
        paste0(str_wrap(name, width = 16), if_else(reversed, paste0(" ", txt$rev), "")),
      label = paste0(label, if_else(flag, " †", "")),
      label = if(setting_plot_label_ns) label else if_else(grp_sig == txt$grp[4], "", label))
  
  # 番号ラベルの場合は対応表を出力
  if(setting_plot_label == "number"){
    settings_vars %>% 
      transmute(no = row_number(), variable, variable_short, variable_name, 
                reversed = variable %in% reverse_ids) %>% 
      readr::write_excel_csv(file.path(dir_out, paste0(file_name, "_ESplot_key.csv")))
  }
  
  # 凡例のラベルに件数を付ける (該当なしのカテゴリも件数 0 として表示)
  label_n = function(levels, x) paste0(levels, " (", table(factor(x, levels = levels))[levels], ")")
  d_lines = c(-.8, -.5, -.2, .2, .5, .8)
  
  for(ds in names(datasets)){
    for(md in c("crude", "age")){
      pd = filter(plot_base, dataset == ds, model == md)
      if(nrow(pd) == 0) next
      
      # 軸の範囲: 点が収まる範囲 (0 は必ず含める)。x と y で別々に決め、縦横比は 1:1 に保つ
      rng = function(v){ r = range(c(0, v), na.rm = TRUE); r + c(-1, 1) * max(.12, .08 * diff(r)) }
      xr = rng(pd$d_M); yr = rng(pd$d_F)

      # 交互作用が有意な点がなければ、形の凡例を出さず (すべて丸)、副題と注もそれに合わせる
      has_int = any(pd$int_sig == txt$int[2])
      shape_guide = if(has_int) guide_legend(order = 2, nrow = 2, override.aes = list(fill = "grey70", size = 2.5)) else "none"
      sub_txt     = if(has_int) txt$sub     else txt$sub_noint
      caption_txt = if(has_int) txt$caption else txt$caption_noint
      # 推定不能のため図に載せなかった変数
      dropped = plot_dropped$name[plot_dropped$dataset == ds & plot_dropped$model == md]
      if(length(dropped) > 0){
        caption_txt = paste0(caption_txt, " ", txt$dropped, paste(dropped, collapse = ", "), ".")
      }
      if(any(pd$flag)) caption_txt = paste0(caption_txt, " ", txt$flag)

      p = ggplot(pd, aes(x = d_M, y = d_F)) + 
        geom_hline(yintercept = d_lines, linewidth = .2, color = "darkseagreen2") + 
        geom_vline(xintercept = d_lines, linewidth = .2, color = "darkseagreen2") + 
        geom_hline(yintercept = 0, linewidth = .3, color = "grey20") + 
        geom_vline(xintercept = 0, linewidth = .3, color = "grey20") + 
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = .3, color = "grey40") + 
        geom_errorbar(aes(ymin = lwr_F, ymax = upr_F), width = 0, linewidth = .2, 
                      color = "#4C5E7F", alpha = .4, na.rm = TRUE) + 
        geom_errorbar(aes(xmin = lwr_M, xmax = upr_M), width = 0, linewidth = .2, 
                      color = "#4C5E7F", alpha = .4, orientation = "y", na.rm = TRUE) + 
        geom_point(aes(fill = grp_sig, shape = int_sig, size = int_sig), color = "grey15", stroke = .4, 
                   show.legend = TRUE) +   # 図にないカテゴリも凡例にマーク・色を表示 
        geom_text_repel(aes(label = label), family = plot_font, 
                        size = if(setting_plot_label == "number") 2.4 else 2.1, lineheight = .85, 
                        segment.size = .1, segment.color = "grey40", min.segment.length = 0, 
                        box.padding = .25, point.padding = .15, force = 2, force_pull = .5, 
                        max.time = 3, max.iter = 50000, max.overlaps = Inf, seed = 1) + 
        scale_fill_manual(values = fill_grp, labels = label_n(txt$grp, pd$grp_sig), drop = FALSE, 
                          name = txt$legend_grp) + 
        scale_shape_manual(values = shape_int, labels = label_n(txt$int, pd$int_sig), drop = FALSE, 
                           name = txt$legend_int) + 
        scale_size_manual(values = size_int, guide = "none", drop = FALSE) + 
        guides(fill  = guide_legend(order = 1, nrow = 2, override.aes = list(shape = 21, size = 2.5)), 
               shape = shape_guide) +
        coord_equal(xlim = xr, ylim = yr, expand = FALSE) + 
        labs(title = labels_dataset[[ds]], 
             subtitle = paste0(txt$model[[md]], "\n", sub_txt,
                               if(any(pd$reversed)) paste0("\n", txt$rev_note) else ""), 
             caption = str_wrap(paste0(caption_txt,
                                       if(setting_binary_method == "logistic") paste0(" ", txt$caption_or) else ""), 
                                width = 140), 
             x = txt$x, y = txt$y) + 
        theme_bw(base_family = plot_font, base_size = 9) + 
        theme(panel.grid = element_blank(), 
              legend.position = "bottom", 
              legend.box = "horizontal", 
              legend.title = element_text(size = 8), 
              legend.text = element_text(size = 7.5), 
              plot.title = element_text(face = "bold", size = 10), 
              plot.subtitle = element_text(size = 7.5, lineheight = 1.1), 
              plot.caption = element_text(size = 7, hjust = 0, lineheight = 1.1))
      
      ggsave(file.path(dir_out, paste0(file_name, "_ESplot_", ds, "_", md, ".pdf")),
             plot = p, device = pdf_device, width = 8, height = 9)

      # 投稿用: Fig S3 (線形回帰の主解析・年齢調整モデル)。2 段幅 183 mm
      if(setting_binary_method == "lm" && is_submission(ds, md)){
        save_submission(p, "FigS3", width_mm = 183, height_mm = 200)
      }
    }
  }


  ## forest plot: 群の主効果 (d: 事件群 − 一般群、男女を同じ重みで平均) ##########################################
  # d の大きい順に上から並べる。散布図と同じく plot_reverse_vars の変数は正負を反転して (R) を付ける
  # 塗り: FDR 補正後に有意 = 黒、有意でない = 白抜き
  # 推定不能の変数は載せず、注に変数名を記載。値が一定のセルを含む対比 (constant) には † を付ける
  fp_wrap = 90   # subtitle と caption を折り返す文字数 (図の幅 6.5 インチに合わせた目安)
  txt_fp = list(x = "Main effect of group (d: Case − Control)",
                sig = c("Significant (FDR < .05)", "Not significant"),
                sub = paste("d = group difference averaged over women and men (equal weights);",
                            "bars: unadjusted 95% CIs; grid: d = ±0.2, ±0.5, ±0.8"),
                dropped = "Not shown because the main effect of group was non-estimable: ",
                flag = paste("†: all participants in both groups of at least one gender had the same value;",
                             "the estimate partly reflects the age adjustment and should not be interpreted."))
  fill_fp = setNames(c("black", "white"), txt_fp$sig)

  fp_base = table_contrasts %>%
    filter(effect == "GroupMain", as.character(variable) %in% plot_vars) %>%
    mutate(is_or = scale %in% c("odds ratio", "ratio of odds ratios"),
           d     = ES_d,
           d_lwr = to_d(ES_lwr, is_or),
           d_upr = to_d(ES_upr, is_or),
           constant = if("constant" %in% names(.)) coalesce(as.logical(constant), FALSE) else FALSE,
           name  = coalesce(variable_short, as.character(variable)))
  fp_dropped = fp_base %>% filter(is.na(d)) %>% select(dataset, model, name)
  fp_base = fp_base %>%
    filter(!is.na(d)) %>%
    mutate(reversed = as.character(variable) %in% reverse_ids,
           sgn = if_else(reversed, -1, 1),
           d_plot = sgn * d,
           lwr = if_else(reversed, -d_upr, d_lwr),
           upr = if_else(reversed, -d_lwr, d_upr),
           sig = !is.na(p_adj) & p_adj < .05,
           fill = factor(if_else(sig, txt_fp$sig[1], txt_fp$sig[2]), levels = txt_fp$sig),
           label = paste0(name, if_else(reversed, paste0(" ", txt$rev), ""), if_else(constant, " †", "")))

  for(ds in names(datasets)){
    for(md in c("crude", "age")){
      pd = filter(fp_base, dataset == ds, model == md)
      if(nrow(pd) == 0) next
      # d の大きい順に上から (ggplot の y 軸は下から並ぶので、昇順の因子にする)
      pd = pd %>% arrange(d_plot) %>% mutate(label = factor(label, levels = unique(label)))

      caption_txt = ""
      dropped = fp_dropped$name[fp_dropped$dataset == ds & fp_dropped$model == md]
      if(length(dropped) > 0) caption_txt = paste0(txt_fp$dropped, paste(dropped, collapse = ", "), ".")
      if(any(pd$constant)) caption_txt = paste(caption_txt, txt_fp$flag)
      if(any(pd$reversed)) caption_txt = paste(caption_txt, txt$rev_note)
      if(setting_binary_method == "logistic") caption_txt = paste(caption_txt, txt$caption_or,
        "The odds ratio for the main effect is the geometric mean of the gender-specific odds ratios.")

      p = ggplot(pd, aes(x = d_plot, y = label)) +
        geom_vline(xintercept = d_lines[d_lines >= min(pd$lwr, na.rm = TRUE) & 
                                          d_lines <= max(pd$upr, na.rm = TRUE)], 
                   linewidth = .2, color = "darkseagreen2") +
        geom_vline(xintercept = 0, linewidth = .3, color = "grey20") +
        geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0, linewidth = .35, color = "#4C5E7F",
                      orientation = "y", na.rm = TRUE) +
        geom_point(aes(fill = fill), shape = 21, size = 2.2, color = "grey15", stroke = .4, show.legend = TRUE) +
        scale_fill_manual(values = fill_fp, labels = label_n(txt_fp$sig, pd$fill), drop = FALSE, name = NULL) +
        guides(fill = guide_legend(override.aes = list(size = 2.5))) +
        labs(title = labels_dataset[[ds]],
             subtitle = paste0(txt$model[[md]], "\n", str_wrap(txt_fp$sub, width = fp_wrap)),
             caption = str_wrap(trimws(caption_txt), width = fp_wrap),
             x = txt_fp$x, y = NULL) +
        theme_bw(base_family = plot_font, base_size = 9) +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              legend.text = element_text(size = 7.5),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(face = "bold", size = 10),
              plot.subtitle = element_text(size = 7.5, lineheight = 1.1),
              plot.caption = element_text(size = 7, hjust = 0, lineheight = 1.1))

      ggsave(file.path(dir_out, paste0(file_name, "_forest_main_", ds, "_", md, ".pdf")),
             plot = p, device = pdf_device, width = 6.5, height = 1.6 + .22 * nrow(pd))

      # 投稿用: Fig 2 (線形回帰) / Fig S4 (ロジスティック回帰)。主解析・年齢調整モデル。1.5 段幅 120 mm
      #   高さ = 軸と凡例のぶん 22 mm + 1 変数あたり 4.2 mm
      if(is_submission(ds, md)){
        save_submission(p, if(setting_binary_method == "lm") "Fig2" else "FigS4",
                        width_mm = 120, height_mm = 22 + 4.2 * nrow(pd))
      }
    }
  }
  message("図を保存しました: ", normalizePath(dir_out))
}

warnings()
