# Supplementary figures: comparison to gov data, etc. ###################################################
# 子ども虐待による死亡事例等の検証について https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000198645.html
# 2019年　国民生活基礎調査の概況 https://www.mhlw.go.jp/toukei/list/20-21kekka.html
# 矯正統計調査 https://www.e-stat.go.jp/

## settings ####
rm(list = ls())

## setting: warnings set in English
Sys.setenv(LANGUAGE = "en_US.UTF-8")

## setting: load libraries
library(tidyverse); library(readr); library(broom); library(exact2x2); library(lemon); library(patchwork)

## setting: A3をCTと分けるか
# settings_A3 = "CTvsA2" # A3をCTと分ける
settings_A3 = "CTA3vsA2" # A3をCTと一緒にCTとする

## setting: load custom functions & ggplot themes
source("Scripts/A2A3functions.R")
source("Scripts/A2A3ggplot_settings.R")

temp_file_name = paste0("A2A3_supp_", settings_A3, "_", date_original_chr)

## import tables & data
load("Reports/A2A3_demographic.Rdata")
# ls()

### settings: factors & levels ####
# factors_group2
factors_groups_recode = set_names(factors_group2, nm = c("Case Group", "Control Group"))
factors_groups_short_recode = set_names(factors_group2, nm = c("Case", "Control"))

factors_groups_CMFR = c("CT", "A2", "CMFR_nozero", "CMFR_all")
factors_groups_CMFR_recode = set_names(factors_groups_CMFR, 
                                       nm = c("Control Group", "Case Group", 
                                              "CMFR cases (≥ 1 m/o)", "CMFR cases (all ages)"))

factors_groups_CMFR3 = factors_groups_CMFR[factors_groups_CMFR != "CT"]
factors_groups_CMFR_recode3 = factors_groups_CMFR_recode[factors_groups_CMFR_recode != "CT"]

factors_groups_CMFR2 = c("A2", "CMFR")
factors_groups_CMFR2_recode = set_names(factors_groups_CMFR2, nm = c("Case Group", "CMFR cases"))

factors_groups_MOJ = c("MOJ", "A2", "CT", "Census")
factors_groups_MOJ_recode = set_names(factors_groups_MOJ, 
                                      nm = c("Penal Code offenders", 
                                             "Case Group", "Control Group", "Census2020 30-39 y/o"))

factors_groups_CSLC = c("CSLC_withchild", "CSLC_singlemom")
factors_groups_CSLC_recode = 
  set_names(factors_groups_CSLC, 
            nm = c("Households with child(ren)<br>2019 CSLC data", 
                   "Single-mother<br>households, 2019 CSLC data"))

factors_gender = c("F", "M")

## custom functions ####
myfunc_df_setup = function(category_name, factors_recode, CT = FALSE, dataname_eng = NA_character_){
  # category_name = "kagaisya"; factors_recode = factors_kagaisya_recode
  # CT = FALSE; dataname_eng = NA_character_
  # category_name = "age_child_y"; factors_recode = factors_age_child_recode
  # CT = FALSE; dataname_eng = NA_character_
  
  ## カテゴリ表
  freq_CMFR_temp = readxl::read_excel("Data_latest/CMFR_shibouzirei_data.xlsx", sheet = category_name, na = c("", "NA")) %>% 
    mutate(category = fct_recode(as.factor(category), !!!factors_recode) %>% 
             suppressWarnings())
  
  # 厚労省データ 全数
  freq_CMFR_all = transmute(freq_CMFR_temp, Group = "CMFR_all", category, N = N_all, prop = prop.table(N))

  # 厚労省データ 0ヶ月児を除く
  freq_CMFR_nozero = transmute(freq_CMFR_temp, Group = "CMFR_nozero", category, N = N_nozero, prop = prop.table(N))
  
  if(!CT){
    factors_groups = factors_groups_CMFR[factors_groups_CMFR %in% c("A2", "CMFR_nozero")]
    factors_groups_recode = factors_groups_CMFR_recode[factors_groups_CMFR_recode %in% factors_groups]
    # A2詳細データ
    freq_CTA3A2 = data_A2details %>% 
      rename(category := all_of(category_name)) %>% 
      group_by(category) %>% 
      summarise(.groups = "drop", N = sum(!is.na(ID_temp), na.rm = T)) %>% 
      filter(!is.na(category)) %>% 
      transmute(Group = "A2", category = fct_recode(as.factor(category), !!!factors_recode) %>% suppressWarnings(), 
                N, prop = prop.table(N)) %>% 
      ungroup()
  }else{
    factors_groups = factors_groups_CMFR[factors_groups_CMFR %in% c("CT", "A2", "CMFR_nozero")]
    factors_groups_recode = factors_groups_CMFR_recode[factors_groups_CMFR_recode %in% factors_groups]
    
    # CTA2データ
    if(settings_A3 == "CTA3vsA2") freq_temp = data_CTA3vsA2 else freq_temp = data_CTvsA2
    
    freq_CTA3A2 = freq_temp %>% 
      rename(category := all_of(category_name)) %>% 
      group_by(Group, category) %>% 
      summarise(.groups = "drop", N = sum(!is.na(ID), na.rm = T)) %>% 
      filter(!is.na(category)) %>% 
      group_by(Group) %>% 
      transmute(Group = fct_drop(Group), category = fct_recode(category, !!!factors_recode), N, prop = prop.table(N)) %>% 
      ungroup()
  }
  
  # 統合
  freq_combined_NA = full_join(freq_CMFR_all, freq_CMFR_nozero, by = join_by(Group, category, N, prop)) %>% 
    full_join(freq_CTA3A2, by = join_by(Group, category, N, prop)) %>% 
    mutate(dataname = dataname_eng, Group = fct_drop(factor(Group, levels = factors_groups_CMFR))) %>% 
    select(dataname, everything())
  n_combined_NA = freq_combined_NA %>% 
    group_by(Group) %>% 
    summarize(.groups = "drop", N_Group = sum(N)) %>%  
    ungroup()
  freq_combined_NA = left_join(freq_combined_NA, n_combined_NA) %>% suppressMessages()
  
  
  ## 個表データ
  if(CT){
    list_freq = list(freq_CMFR_all, freq_CMFR_nozero, 
                     filter(freq_CTA3A2, Group == "A2"), filter(freq_CTA3A2, Group == "CT"))
  }else{
    list_freq = list(freq_CMFR_all, freq_CMFR_nozero, filter(freq_CTA3A2, Group == "A2"))
  }
  
  data_combined_NA = 
    map_df(list_freq, 
           function(x){
             # x = list_freq[[1]] # test
             map_df(names(factors_recode),
                    function(y){
                      # y = names(factors_recode)[2] # test
                      temp1 = filter(x, category == y)
                      tibble(ID = NA_character_, Group = temp1$Group, category = rep(temp1$category, temp1$N))
                    }
             ) 
           } %>% 
             mutate(ID = paste(Group, row_number(), sep = "_"), 
                    Group = fct_drop(factor(Group, levels = factors_groups_CMFR)), 
                    category = factor(category, levels = names(factors_recode)))) 
  
  freq_combined = filter(freq_combined_NA, category != "Unknown") %>% 
    group_by(Group) %>% 
    mutate(category = fct_drop(category), prop = prop.table(N)) %>% 
    ungroup() %>% 
    arrange(Group)
  n_combined = freq_combined %>% 
    group_by(Group) %>% 
    summarize(.groups = "drop", N_Group = sum(N)) %>%  
    ungroup()
  freq_combined = left_join(freq_combined, n_combined) %>% suppressMessages()
  data_combined = filter(data_combined_NA, category != "Unknown") %>% 
    mutate(category = fct_drop(category)) %>% 
    arrange(Group)
  
  list(freq_combined = freq_combined, n_combined = n_combined, data_combined = data_combined, 
       freq_combined_NA = freq_combined_NA, n_combined_NA = n_combined_NA, data_combined_NA = data_combined_NA)
}

myfunc_fisher = 
  function(dataset, factors, dataname = NA_character_, dataname_eng = NA_character_, CMFR = TRUE){
    # dataset = data_keizaisimple2; factors = factors_keizaisimple2_eng; dataname = "keizaisimple2"; CMFR = TRUE # test
    # dataset = data_setaisimple; factors = factors_setaisimple_eng; dataname = "setaisimple"; CMFR = TRUE # test
    # dataset = data_oyafuzai; factors = factors_oyafuzai_eng; dataname = "oyafuzai"; CMFR = FALSE # test
    # dataset = data_syogai; factors = factor(c(0, 1)); dataname = "syogai"; CMFR = TRUE # test
    
    # factors = factors[factors != "Unknown"]
    factors = factors[!(factors %in% c("Other", "Unknown"))]
    temp_dataset = dataset %>% 
      filter(Group != "A3") %>% 
      filter(!(category %in% c("Other", "Unknown"))) %>% 
      {if(CMFR) filter(., Group != "CMFR_all") else .} %>% 
      mutate(Group = fct_drop(factor(Group, levels = factors_groups_CMFR)), category = fct_drop(category) %>% 
               suppressWarnings())
    
    if(CMFR){
      fisherCMFR_A2 = map_df(factors, 
                             function(x){
                               # x = factors[2] # test
                               temp_dataset1 = temp_dataset %>% 
                                 filter(Group %in% c("A2", "CMFR_nozero")) %>% 
                                 mutate(Group = fct_drop(Group), 
                                        category = if_else(category == x, x, "Else"))
                               fisher1 = fisher.exact(temp_dataset1$category, temp_dataset1$Group, alternative = "two.sided", 
                                                      conf.int = TRUE, conf.level = 0.95)
                               temp_dataset2 = temp_dataset1 %>% 
                                 mutate(Group = fct_rev(Group))
                               fisher2 = fisher.exact(temp_dataset2$category, temp_dataset2$Group, alternative = "two.sided", 
                                                      conf.int = TRUE, conf.level = 0.95)
                               tab_fisher1 = mutate(tidy(fisher1), comp = "A2 vs CMFR_nozero")
                               tab_fisher2 = mutate(tidy(fisher2), comp = "CMFR_nozero vs A2")
                               full_join(tab_fisher1, tab_fisher2) %>% suppressMessages() %>% 
                                 transmute(dataname = dataname_eng, category = x, comp, 
                                           OR = estimate, OR_lwr = conf.low, OR_upr = conf.high, 
                                           p.value, sig = p.asterisk2(p.value), method, func = "exact2x2::fisher.exact")
                             }
      ) %>% 
        arrange(comp)
    }
    
    
    if(!CMFR){
      fisherCTA2 = map_df(factors, 
                          function(y){
                            # y = factors[1] # test
                            temp_dataset1 = dataset %>% 
                              filter(Group %in% c("CT", "A2")) %>% 
                              mutate(Group = fct_drop(Group), 
                                     category = if_else(category == y, y, "Else"))
                            fisher1 = fisher.exact(temp_dataset1$category, temp_dataset1$Group, alternative = "two.sided", 
                                                   conf.int = TRUE, conf.level = 0.95)
                            temp_dataset2 = temp_dataset1 %>% 
                              mutate(Group = fct_rev(Group))
                            fisher2 = fisher.exact(temp_dataset2$category, temp_dataset2$Group, alternative = "two.sided", 
                                                   conf.int = TRUE, conf.level = 0.95)
                            tab_fisher1 = mutate(tidy(fisher1), comp = "CT vs A2")
                            tab_fisher2 = mutate(tidy(fisher2), comp = "A2 vs CT")
                            full_join(tab_fisher1, tab_fisher2) %>% suppressMessages() %>% 
                              transmute(dataname = dataname_eng, category = y, comp, 
                                        OR = estimate, OR_lwr = conf.low, OR_upr = conf.high, 
                                        p.value, sig = p.asterisk2(p.value), method, func = "exact2x2::fisher.exact")
                          }
      ) %>% arrange(comp)
      if(CMFR){full_join(fisherCMFR_A2, fisherCTA2, 
                         by = join_by(dataname, category, comp, OR, OR_lwr, OR_upr, p.value, sig, method, func))
        
      }else fisherCTA2
      
    }else fisherCMFR_A2
  }

## ggplot themes ####
mytheme_legend = theme(legend.location = "plot", 
                       legend.position = "right", 
                       legend.justification.right = c(0, 1), 
                       legend.margin = margin(t = 0, b = 0, l = 2.5, r = 0, unit = "mm"),
                       legend.box.spacing = unit(1.5, units = "mm"), 
                       legend.key = element_rect(color = "#1A1A1A", linewidth = .00001), 
                       legend.key.height = unit(3, units = "mm"), 
                       legend.key.width = unit(3, units = "mm"), 
                       legend.key.spacing.y = unit(1.5, units = "mm"), 
                       legend.text = element_markdown(color = "#0D0D0D", size = 8,
                                                      margin = margin(t = 0, b = 0, l = 1.25, r = 0, unit = "mm")),
                       legend.title = element_markdown(color = "#0D0D0D", face = "bold", size = 8, lineheight = 1.25, 
                                                       margin = margin(t = 0, b = 1.5, l = 0, r = 0, unit = "mm")))

mytheme_main = theme(text = element_text(family = "Arial", color = "#0D0D0D"), 
                     line = element_line(color = "#1A1A1A", linewidth = .3), 
                     rect = element_rect(color = "#1A1A1A", linewidth = .3), 
                     
                     # axis
                     axis.line.y = element_line(color = "#1A1A1A", linewidth = .3), 
                     axis.line.x = element_line(color = "#1A1A1A", linewidth = .3), 
                     
                     axis.ticks.y = element_line(color = "#1A1A1A", linewidth = .3),
                     axis.ticks.x = element_line(color = "#1A1A1A", linewidth = .3),
                     
                     axis.text.y = element_text(color = "#0D0D0D", size = 8),
                     axis.text.x = element_text(color = "#0D0D0D", size = 8),
                     
                     axis.title.y = element_text(color = "#0D0D0D", size = 9, 
                                                 margin = margin(t = 0, b = 0, l = 0, r = 2, unit = "mm")),
                     axis.title.x = element_text(color = "#0D0D0D", size = 9,
                                                 margin = margin(t = 1.25, b = 0, l = 0, r = 0, unit = "mm")), 
                     
                     # panel
                     panel.background = element_rect(color = NA, fill = "#FFFFFF"),
                     panel.grid.major.y = element_line(color = "#BDC3C7", linewidth = .4), 
                     panel.grid.major.x = element_line(color = "#BDC3C7", linewidth = .4), 
                     
                     # plot
                     plot.margin = margin(t = 5, b = 5, l = 5, r = 5, unit = "mm"),
                     plot.background = element_rect(color = NA, fill = "transparent"))

myfunc_stackedplot = 
  function(dataset, factors, groups_recode, fill, color, label_x = "% of cases", geom_txt = TRUE, category_name = ""){
    # dataset = data_gyakutai; factors = factors_gyakutai_eng; groups_recode = factors_groups_CMFR_recode # test
    # fill = color_gyakutai_fill; color = color_gyakutai_color; label_x = "% of cases" # test
    # geom_txt = TRUE; category_name = "Title" # test
    
    factors = factors[factors != "Unknown"]
    dataset_plot = dataset %>% 
      filter(category != "Unknown") %>% 
      mutate(Group = fct_recode(Group, !!!groups_recode) %>% suppressWarnings())
    plot = 
      ggplot(data = dataset_plot,
             aes(y = Group, fill = category)) +
      geom_bar(stat = "count", position = position_fill(reverse = TRUE),
               color = "#1A1A1A", 
               show.legend = c(fill = TRUE, color = TRUE),
               # show.legend = TRUE,
               linewidth = .25, width = .8, orientation = "y") +
      scale_fill_manual(values = fill, drop = FALSE) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1.01), breaks = seq(0, 1, by = .1),
                         labels = scales::percent_format(suffix = "")) +
      labs(x = label_x) + 
      theme_classic(base_family = "Arial") + 
      mytheme_legend + 
      mytheme_main + 
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_markdown(color = "#0D0D0D", face = "bold", size = 9, lineheight = 1.15, 
                                           margin = margin(t = 0, b = 0, l = 0, r = .8, unit = "mm")), 
            axis.title.y = element_blank(), 
            panel.grid.major.y = element_blank())
    
    if(geom_txt){
      plot + 
        geom_text(aes(label = after_stat(count), color = category),
                  stat = "count", position = position_fill(reverse = TRUE, vjust = .5),
                  show.legend = c(fill = FALSE, color = FALSE),
                  # show.legend = FALSE, 
                  size = 3, fontface = "plain", orientation = "y") +
        scale_color_manual(values = color, drop = FALSE) + 
        guides(color = "none", 
               fill = guide_legend(title = category_name))
    }else{
      plot + 
        guides(color = "none", 
               fill = guide_legend(title = category_name))
    }
  }



# 厚労省 死亡事例群との比較 ###################################################
## A2 被害児の年齢 for 厚労省 死亡事例群との比較 ####
# 註：ここでは、月齢が分かっている場合でも、年齢だけで解析 (死亡事例群では一部しか月齢がわからないため)
factors_age_child = 0:17
factors_age_child_recode = set_names(as.character(factors_age_child), nm = str_c(factors_age_child, "yo"))
factors_age_child_eng = names(factors_age_child_recode)

## data frames
list_age_child = myfunc_df_setup("age_child_y", factors_age_child_recode, 
                                 dataname_eng = "Age of victimized children")
freq_age_child = list_age_child$freq_combined # カテゴリ表
data_age_child = list_age_child$data_combined %>%  # 個表データ
  mutate(age = as.integer(str_remove_all(category, "yo")))
n_age_child = list_age_child$n_combined

## t-test & K-S test
data_age_child_2groups = filter(data_age_child, Group != "CMFR_all") %>% 
  mutate(Group = fct_drop(Group))

(age_child_Levenetest = car::leveneTest(age ~ Group, data = data_age_child_2groups))
(age_child_Welchtest = t.test(age ~ Group, data = data_age_child_2groups, var.equal = F))
(age_child_ttest = t.test(age ~ Group, data = data_age_child_2groups, var.equal = T))
(age_child_KStest = ks.test(age ~ Group, data = data_age_child_2groups))

## plot (histogram)
factors_recode_temp = factors_groups_CMFR_recode3
names(factors_recode_temp) = paste0(names(factors_recode_temp), ", N = ", n_age_child$N_Group)
color_groups_color = set_names(c("#990099", "#4DC4FF", "#BFE4FF"), names(factors_recode_temp))

data_age_child_plot = data_age_child %>% 
  mutate(Group = fct_recode(Group, !!!factors_recode_temp) %>% suppressWarnings())

plot_age_child_main = 
  ggplot(data_age_child_plot, 
         aes(x = age, y = after_stat(density), color = Group)) + 
  geom_freqpoly(binwidth = 1, linewidth = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, .55), breaks = seq(0, 1, by = .1),
                     labels = scales::percent_format(suffix = "")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 18.25),
                     breaks = seq(0, 18, by = 1)) + 
  scale_color_manual(values = color_groups_color, 
                     labels = , 
                     guide = guide_legend(reverse = TRUE)) +
  labs(y = "% of cases", x = "Age of victimized children (years old)") + 
  theme_classic(base_family = "Arial") + 
  mytheme_main + 
  theme(panel.grid.major.y = element_line(color = "#ecf0f1", linewidth = .4), 
        panel.grid.major.x = element_line(color = "#ecf0f1", linewidth = .4), 
        
        # axis
        axis.line = element_line(color = "#1A1A1A", linewidth = .4), 
        axis.ticks = element_line(color = "#1A1A1A", linewidth = .4), 
        
        # legend
        legend.position = "inside", 
        legend.justification = c(1, 1), 
        legend.position.inside = c(1, 1), 
        legend.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"), 
        legend.box.margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm"),
        legend.key = element_rect(fill = "transparent", color = NA), 
        legend.key.width = unit(5, "mm"), 
        legend.key.height = unit(5, "mm"), 
        legend.key.spacing.y = unit(0, "mm"), 
        legend.title = element_blank(), 
        
        legend.margin = margin(t = 5, b = 5, l = 5, r = 5, unit = "mm"), 
        legend.text = element_text(face = "bold", color = "#0D0D0D", size = 10, 
                                   margin = margin(t = 0, b = 0, l = 2, r = 0, unit = "mm")), 
        # plot
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm"))
# plot_age_child_main



## 主な虐待の類型 gyakutai: 厚労省 死亡事例群 vs A2 ###################################################
# factors_gyakutai
factors_gyakutai_recode = set_names(factors_gyakutai, 
                                    nm = c("Physical abuse", "Neglect", "Psychological abuse", "Sexual abuse"))
factors_gyakutai_eng = names(factors_gyakutai_recode)
dataname_eng_gyakutai = "Types of abuse"

## data frames
list_gyakutai = myfunc_df_setup("gyakutai", factors_gyakutai_recode, 
                                dataname_eng = dataname_eng_gyakutai)
freq_gyakutai = list_gyakutai$freq_combined # カテゴリ表
data_gyakutai = list_gyakutai$data_combined # 個表データ
n_gyakutai = list_gyakutai$n_combined

## Fisher's exact test ("Sexual abuse" を除く)
res_gyakutai = myfunc_fisher(dataset = data_gyakutai, 
                             factors = factors_gyakutai_eng[1:3], 
                             dataname = "gyakutai", dataname_eng = dataname_eng_gyakutai, CMFR = TRUE)
# res_gyakutai

## plot
factors_recode_temp = factors_groups_CMFR_recode3
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>N = ", n_gyakutai$N_Group)

color_gyakutai_fill = set_names(c("#990099", "#4DC4FF", "#BFE4FF", "#FFFFFF"), factors_gyakutai_eng)
color_gyakutai_color = set_names(c("#ECF0F1", "#0D0D0D", "#0D0D0D", "#0D0D0D"), factors_gyakutai_eng)

plot_gyakutai = 
  myfunc_stackedplot(data_gyakutai, factors_gyakutai_eng, groups_recode = factors_recode_temp, 
                     fill = color_gyakutai_fill, color = color_gyakutai_color, 
                     category_name = dataname_eng_gyakutai)
# plot_gyakutai



## 主たる加害者 kagaisya: 厚労省 死亡事例群 vs A2 #### 2025.01.20 非血縁女性は「その他」に統合 #### 
factors_kagaisya = factors_kagaisya[factors_kagaisya != "非血縁女性"]
factors_kagaisya_recode = set_names(factors_kagaisya, 
                                    nm = c("Genetic mother & father", 
                                           "Genetic mother", "Genetic father", 
                                           "Non-kin man", "Genetic mother & non-kin man", "Other"))
factors_kagaisya_eng = names(factors_kagaisya_recode)
dataname_eng_kagaisya = "Principal perpetrator"

## data frames
list_kagaisya = myfunc_df_setup("kagaisya", factors_kagaisya_recode, 
                                dataname_eng = dataname_eng_kagaisya)
freq_kagaisya = list_kagaisya$freq_combined # カテゴリ表
data_kagaisya = list_kagaisya$data_combined # 個表データ
n_kagaisya = list_kagaisya$n_combined

## Fisher's exact test
res_kagaisya = myfunc_fisher(dataset = data_kagaisya, factors = factors_kagaisya_eng, 
                             dataname = "kagaisya", dataname_eng = dataname_eng_kagaisya)
# res_kagaisya

## plot
factors_recode_temp = factors_groups_CMFR_recode3
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>N = ", n_kagaisya$N_Group)

color_kagaisya_fill = set_names(c("#990099", "#C9ACE6", "#BFE4FF", "#77D9A8", "#D8F255", "#FFFFFF"), 
                                factors_kagaisya_eng)
color_kagaisya_color = set_names(c("#ECF0F1", "#0D0D0D", "#0D0D0D", "#0D0D0D", "#0D0D0D", "#0D0D0D"), 
                                 factors_kagaisya_eng)
plot_kagaisya = 
  myfunc_stackedplot(data_kagaisya, factors_kagaisya_eng, groups_recode = factors_recode_temp, 
                     fill = color_kagaisya_fill, color = color_kagaisya_color, 
                     category_name = dataname_eng_kagaisya)
# plot_kagaisya



## 子の疾患・障害 syogai: 厚労省 死亡事例群 vs A2 ###################################################
factors_syogaitype_eng = c("Physical disorder", "Physical disability", "Intellectual disability", 
                           "Developmental disorder", "Delay in physical growth")
factors_syogaitype_recode_eng = set_names(factors_syogaitype_jpn, nm = factors_syogaitype_eng)

# factors_utagai
factors_utagai_recode = set_names(factors_utagai, nm = c("Diagnosed", "Suspected but not diagnosed", "None"))
factors_utagai_eng = names(factors_utagai_recode)

## data frames
CMFR_syogai = readxl::read_excel("Data_latest/CMFR_shibouzirei_data.xlsx", sheet = "syogai", na = c("", "NA")) %>% 
  mutate(category = factor(category, levels = factors_syogaitype_jpn))
freq_syogai = full_join(mutate(CMFR_syogai, Group = "CMFR"), mutate(A2_syogai, Group = "A2")) %>% 
  suppressMessages() %>% 
  rename(dataname = category) %>% 
  pivot_longer(cols = -c(Group, dataname), names_to = "category", values_to = "N") %>% 
  group_by(dataname, Group) %>% 
  mutate(dataname = fct_recode(dataname, !!!factors_syogaitype_recode_eng), 
         Group = factor(Group, levels = factors_groups_CMFR2), 
         category = fct_recode(factor(category, levels = factors_utagai), !!!factors_utagai_recode), 
         prop = prop.table(N)) %>%
  ungroup() %>% 
  arrange(dataname, Group, category)
n_syogai = freq_syogai %>% 
  group_by(dataname, Group) %>%
  summarize(.groups = "drop", N_Group = sum(N)) %>% 
  ungroup()
freq_syogai = left_join(freq_syogai, n_syogai) %>% suppressMessages()
# freq_syogai
# n_syogai

list_list_freq = map(factors_syogaitype_eng, 
                     function(type_syogai){
                       map(factors_groups_CMFR2_recode, 
                           ~ filter(freq_syogai, dataname %in% type_syogai, Group %in% .))
                     })
names(list_list_freq) = factors_syogaitype_eng
list_freq = unlist(list_list_freq, recursive = FALSE)

data_syogai = 
  map_df(list_freq, 
         function(df){
           # df = list_freq[[1]] # test
           map_df(factors_utagai_eng,
                  function(utagai){
                    # utagai = factors_utagai_eng[1] # test
                    temp1 = filter(df, category == utagai)
                    tibble(dataname = temp1$dataname, ID = NA_character_, Group = temp1$Group, 
                           category = rep(temp1$category, temp1$N))
                  }
           ) %>% 
             mutate(ID = paste(Group, dataname, row_number(), sep = "_"), 
                    Group = fct_drop(factor(Group, levels = factors_groups_CMFR2)), 
                    category = factor(category, levels = factors_utagai_eng))
         })

## Fisher's exact test
data_syogai01 = mutate(data_syogai, 
                       category = factor(if_else(category == "None", 0, 1)))
res_syogai = 
  map_df(factors_syogaitype_eng, 
         function(temp_type){
           # temp_type = factors_syogaitype_eng[3] # test
           temp_dataset1 = filter(data_syogai01, dataname %in% temp_type)
           fisher1 = fisher.exact(temp_dataset1$category, temp_dataset1$Group, alternative = "two.sided", 
                                  conf.int = TRUE, conf.level = 0.95)
           temp_dataset2 = temp_dataset1 %>% 
             mutate(Group = fct_rev(Group))
           fisher2 = fisher.exact(temp_dataset2$category, temp_dataset2$Group, alternative = "two.sided", 
                                  conf.int = TRUE, conf.level = 0.95)
           tab_fisher1 = mutate(tidy(fisher1), comp = "A2 vs CMFR")
           tab_fisher2 = mutate(tidy(fisher2), comp = "CMFR vs A2")
           full_join(tab_fisher1, tab_fisher2) %>% suppressMessages() %>% 
             transmute(dataname = temp_type, category = "Y/N", comp, 
                       OR = estimate, OR_lwr = conf.low, OR_upr = conf.high, 
                       p.value, sig = p.asterisk2(p.value), method, func = "exact2x2::fisher.exact")
         }) %>% arrange(comp)
# res_syogai

## plot
color_syogai_fill = set_names(c("#4DC4FF", "#BFE4FF", "#FFFFFF"), factors_utagai_eng)
color_syogai_color = set_names(c("#0D0D0D", "#0D0D0D", "#0D0D0D"), factors_utagai_eng)

plotlist_syogai = 
  map(factors_syogaitype_eng, 
      function(type_syogai){
        # type_syogai = factors_syogaitype_eng[5] # test
        temp_n = filter(n_syogai, dataname %in% type_syogai)
        factors_recode_temp = set_names(factors_groups_CMFR2_recode, 
                                        paste0(names(factors_groups_CMFR2_recode), ", N = ", temp_n$N_Group))
        temp_data = filter(data_syogai, dataname %in% type_syogai) %>% 
          mutate(Group = fct_rev(Group))
        myfunc_stackedplot(temp_data, factors_utagai_eng, groups_recode = factors_recode_temp,
                           fill = color_syogai_fill, color = color_syogai_color) + 
          labs(title = temp_n$dataname[1]) + 
          theme(axis.text.y = element_markdown(color = "#0D0D0D", face = "bold", size = 8, lineheight = 1.15, 
                                               margin = margin(t = 0, b = 0, l = 0, r = .8, unit = "mm")), 
                legend.position = "none", 
                plot.title.position = "plot", 
                plot.title = element_text(family = "Arial", color = "#0D0D0D", face = "bold", size = 9, 
                                          margin = margin(t = 1, b = .5, l = 2, r = 0, unit = "mm")))
      }
  )
# plotlist_syogai[[2]]



## 世帯の状況 (家族構成) setaisimple: 厚労省 死亡事例群 vs A2 ###################################################
# factors_setaisimple
factors_setaisimple_recode = set_names(factors_setaisimple, 
                                       nm = c("Both genetic parents", "Single parent", 
                                              "Genetic parent & non-kin caregiver", "Other"))
factors_setaisimple_eng = names(factors_setaisimple_recode)
dataname_setaisimple_eng = "Types of the caregivers"

## data frames
list_setaisimple = myfunc_df_setup("setaisimple", factors_setaisimple_recode, CT = TRUE, 
                                   dataname_eng = dataname_setaisimple_eng)
freq_setaisimple = list_setaisimple$freq_combined # カテゴリ表
data_setaisimple = list_setaisimple$data_combined # 個表データ
n_setaisimple = list_setaisimple$n_combined

## Fisher's exact test
res_setaisimple = myfunc_fisher(data_setaisimple, factors_setaisimple_eng, CMFR = TRUE, 
                                dataname = "setai", dataname_eng = dataname_setaisimple_eng)
# res_setaisimple

## plot
factors_recode_temp = factors_groups_CMFR_recode
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>N = ", n_setaisimple$N_Group)

color_setaisimple_fill = set_names(c("#FFFF99", "#FFCA80", "#F6AA00", "#FFFFFF"), factors_setaisimple_eng)
color_setaisimple_color = set_names(c("#0D0D0D", "#0D0D0D", "#ECF0F1", "#0D0D0D"), factors_setaisimple_eng)

plot_setaisimple = 
  myfunc_stackedplot(data_setaisimple, factors_setaisimple_eng, groups_recode = factors_recode_temp, 
                     fill = color_setaisimple_fill, color = color_setaisimple_color, 
                     category_name = "Types of the caregivers<br>the target children had lived with")
# plot_setaisimple



## 養育者の家庭の経済状況 keizaisimple: 国民生活基礎調査 (CSLC) ###################################################
# factors_keizaisimple
factors_keizaisimple_recode = set_names(factors_keizaisimple, nm = c("< 5M yen/year", "≥ 5M yen/year", "Unknown"))
factors_keizaisimple_eng = names(factors_keizaisimple_recode)
category_name_keizaisimple = "Annual household income"

## CSLC カテゴリ表 (%*10)
freq_CSLC = readxl::read_excel("Data_latest/CMFR_shibouzirei_data.xlsx", sheet = "CSLC_keizaisimple", na = c("", "NA")) %>% 
  mutate(category = fct_recode(as.factor(category), !!!factors_keizaisimple_recode) %>% 
           suppressWarnings(), 
         pct_singlemom = pct_singlemom*10, 
         pct_withchild = pct_withchild*10) %>% 
  rename_with(~ str_replace_all(., "pct", "CSLC")) %>% 
  pivot_longer(cols = -category, names_to = "Group", values_to = "N") %>% 
  group_by(Group) %>% 
  transmute(dataname = category_name_keizaisimple, Group = factor(Group, levels = factors_groups_CSLC),
            category, N, prop = prop.table(N)) %>%
  ungroup() %>% 
  arrange(Group)

## CSLC 個票データ (%*10)
data_CSLC = map_df(list(filter(freq_CSLC, Group == "CSLC_withchild"), 
                        filter(freq_CSLC, Group == "CSLC_singlemom")), 
                   ~ map_df(factors_keizaisimple_eng[factors_keizaisimple_eng != "Unknown"],
                            function(x){
                              temp1 = filter(., category == x)
                              tibble(ID = NA_character_, Group = temp1$Group, category = rep(temp1$category, temp1$N))
                            }
                   ) %>% 
                     mutate(ID = paste(Group, row_number(), sep = "_"), 
                            Group = factor(Group, levels = factors_groups_CSLC), 
                            category = factor(category, levels = names(factors_keizaisimple_recode)))) %>% 
  filter(category != "Unknown") %>% 
  mutate(category = fct_drop(category))
freq_CSLC = mutate(freq_CSLC, N = NA_real_, N_Group = NA_real_)

## plot
# factors_groups_CSLC_recode
color_keizaisimple_fill = set_names(c("#03AF7A", "#FFFFFF"), 
                                    factors_keizaisimple_eng[factors_keizaisimple_eng != "Unknown"])
color_keizaisimple_color = set_names(c("#ECF0F1", "#0D0D0D"), 
                                     factors_keizaisimple_eng[factors_keizaisimple_eng != "Unknown"])
plot_CSLC = 
  myfunc_stackedplot(data_CSLC, factors_keizaisimple_eng, 
                     groups_recode = factors_groups_CSLC_recode, 
                     fill = color_keizaisimple_fill, color = color_keizaisimple_color, 
                     label_x = "% of households", geom_txt = FALSE, category_name = category_name_keizaisimple) + 
  geom_text(aes(label = paste0(after_stat(round(count/10, 0)), "%"), 
                color = category), 
            stat = "count", position = position_fill(reverse = TRUE, vjust = .5), 
            show.legend = c(fill = NA, color = NA), 
            size = 3, fontface = "plain", orientation = "y") +
  scale_color_manual(values = color_keizaisimple_color) + 
  guides(color = "none", 
         fill = guide_legend(title = category_name_keizaisimple))
# plot_CSLC



## 養育者の家庭の経済状況 keizaisimple2: 厚労省 死亡事例群 vs A2, CT ###################################################
# factors_keizaisimple2
factors_keizaisimple2_recode = set_names(factors_keizaisimple2, 
                                         nm = c("Livelihood assistance recipient", "Non-taxable", 
                                                "Taxable (< 5M yen/year)", "Taxable (≥ 5M yen/year)", "Unknown"))
factors_keizaisimple2_eng = names(factors_keizaisimple2_recode)
dataname_keizaisimple2_eng = "Annual household income etc."

## data frames
list_keizaisimple2 = myfunc_df_setup("keizaisimple2", factors_keizaisimple2_recode, CT = TRUE, 
                                     dataname_eng = dataname_keizaisimple2_eng)
freq_keizaisimple2 = list_keizaisimple2$freq_combined # カテゴリ表
data_keizaisimple2 = list_keizaisimple2$data_combined # 個表データ
n_keizaisimple2 = freq_keizaisimple2 %>% 
  group_by(Group) %>% 
  summarize(.groups = "drop", N = sum(N)) %>%  
  ungroup()

## Fisher's exact test
res_keizaisimple2 = myfunc_fisher(data_keizaisimple2, factors_keizaisimple2_eng, CMFR = TRUE, 
                                  dataname = "keizaisimple2", dataname_eng = dataname_keizaisimple2_eng)
# res_keizaisimple2

## plot
factors_recode_temp = factors_groups_CMFR_recode
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>N = ", n_keizaisimple2$N)

color_keizaisimple2_fill = set_names(c("#D8F255", "#77D9A8", "#03AF7A", "#FFFFFF", "#FFFFFF"), 
                                     factors_keizaisimple2_eng)
color_keizaisimple2_color = set_names(c("#0D0D0D", "#0D0D0D", "#ECF0F1", "#0D0D0D", "#0D0D0D"), 
                                      factors_keizaisimple2_eng)

plot_keizaisimple2 = 
  myfunc_stackedplot(data_keizaisimple2, factors_keizaisimple2_eng, 
                     groups_recode = factors_recode_temp, 
                     fill = color_keizaisimple2_fill, color = color_keizaisimple2_color, 
                     category_name = "Annual household income &<br>recepients of livelihood asistance<br>or tax exemption")
# plot_keizaisimple2



## 回答者の実父母の不在 oyafuzai: CT, A2, A3 ####
# factors_oyafuzai
factors_oyafuzai_recode = set_names(factors_oyafuzai, 
                                    nm = c("Both genetic parents absent", "Genetic mother absent", "Genetic father absent", 
                                           "Both present", "Unknown"))
factors_oyafuzai_eng = names(factors_oyafuzai_recode)

category_name_oyafuzai = "Experience of at least one year of separation<br> from either of their genetic parents<br> by age 15"
dataname_eng_oyafuzai = "Separation from genetic parents"

## data frames
freq_oyafuzai0 = summary_all %>% 
  filter(title_code == "oyafuzai") %>% # カテゴリ表
  transmute(dataname = dataname_eng_oyafuzai, Group, Gender, 
            category = fct_recode(category, !!!factors_oyafuzai_recode), N, prop) %>% 
  select(dataname, everything())
if(settings_A3 == "CTvsA2"){
  data_oyafuzai0 = data_CTvsA2
  freq_oyafuzai1 = filter(freq_oyafuzai0, Group %in% factors_group2) %>% 
    mutate(Group = fct_drop(Group))
}else if(settings_A3 == "CTA3vsA2"){
  data_oyafuzai0 = data_CTA3vsA2
  freq_oyafuzai1 = filter(freq_oyafuzai0, Group %in% c("A2", "CTA3")) %>% 
    mutate(Group = factor(if_else(Group == "CTA3", "CT", Group), levels = factors_group2))
}
n_oyafuzai = freq_oyafuzai1 %>% 
  group_by(Group, Gender) %>% 
  summarize(.groups = "drop", N_Group = sum(N)) %>%  
  ungroup() %>% 
  arrange(Group)
freq_oyafuzai = left_join(freq_oyafuzai1, n_oyafuzai) %>% suppressMessages()

data_oyafuzai = data_oyafuzai0 %>%  # 個表データ
  transmute(ID, Group, Gender, category = fct_recode(oyafuzai, !!!factors_oyafuzai_recode)) %>% 
  filter(category != "Unknown") %>% 
  mutate(category = fct_drop(category))
n_oyafuzaiF = filter(n_oyafuzai, Gender == "F")
n_oyafuzaiM = filter(n_oyafuzai, Gender == "M")

## Fisher's exact test
res_oyafuzaiF = 
  myfunc_fisher(dataset = filter(data_oyafuzai, Gender == "F"), 
                factors = factors_oyafuzai_eng[factors_oyafuzai_eng != "Unknown"], 
                dataname = "oyafuzai", dataname_eng = dataname_eng_oyafuzai, CMFR = FALSE) %>% 
  mutate(Gender = "F")
res_oyafuzaiM = 
  myfunc_fisher(dataset = filter(data_oyafuzai, Gender == "M"), 
                factors = factors_oyafuzai_eng[factors_oyafuzai_eng != "Unknown"], 
                dataname = "oyafuzai", dataname_eng = dataname_eng_oyafuzai, CMFR = FALSE) %>% 
  mutate(Gender = "M")
res_oyafuzai = bind_rows(res_oyafuzaiF, res_oyafuzaiM) %>% 
  select(dataname, Gender, everything())

## plot
color_oyafuzai_fill = set_names(c("#990099", "#C9ACE6", "#BFE4FF", "#FFFFFF", "#FFFFFF"), factors_oyafuzai_eng)
color_oyafuzai_color = set_names(c("#ECF0F1", "#0D0D0D", "#0D0D0D", "#0D0D0D", "#0D0D0D"), factors_oyafuzai_eng)

factors_recode_temp = factors_groups_recode
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>Women, N = ", n_oyafuzaiF$N_Group)
plot_oyafuzaiF = 
  myfunc_stackedplot(filter(data_oyafuzai, Gender == "F"), factors_oyafuzai_eng, 
                     groups_recode = factors_recode_temp,
                     fill = color_oyafuzai_fill, color = color_oyafuzai_color,
                     category_name = category_name_oyafuzai)

factors_recode_temp = factors_groups_recode
names(factors_recode_temp) = paste0(names(factors_recode_temp), "<br>Men, N = ", n_oyafuzaiM$N_Group)
plot_oyafuzaiM = 
  myfunc_stackedplot(filter(data_oyafuzai, Gender == "M"), factors_oyafuzai_eng, 
                     groups_recode = factors_recode_temp,
                     fill = color_oyafuzai_fill, color = color_oyafuzai_color,
                     category_name = category_name_oyafuzai)
# plot_oyafuzaiF / plot_oyafuzaiM +
#   plot_layout(guides = "collect", axis = "collect")
rm(list = ls(pattern = "res_oyafuzaiF|res_oyafuzaiM"))



## 学歴 gakurekisimple : 法務省 矯正統計調査 [新受刑者 新受刑者の罪名別 教育程度] vs A2, CT #####################
## import MOJ data
MOJ_gakureki= read_csv("Data_original/FEH_00250005_200923163327.csv",
                       skip = 27, locale = locale(encoding = "shift-jis"), show_col_types = FALSE) %>%
  filter(`時間軸(年次)` == "2016年", `罪名` == "刑法犯_計", `性別` != "総数", 
         !(`教育程度` == "総数" & `学校` == "総数")) %>%
  transmute(Group = "MOJ", 
            Gender = factor(if_else(`性別` == "女", "F", "M"), levels = factors_gender),
            gakureki = str_remove(paste0(`学校`, `教育程度`), "総数"), 
            gakurekisimple = 
              case_when(gakureki %in% c("不就学", "小学校中退", "小学校卒業", "中学校中退", "中学校卒業", 
                                        "高等学校中退", "高等学校在学") ~ "高卒未満", 
                        gakureki %in% c("高等学校卒業", "大学中退", "大学在学") ~ "高卒", 
                        gakureki %in% c("大学卒業") ~ "大卒以上", 
                        gakureki == "不詳" ~ "不明") %>%
              factor(levels = factors_gakurekisimple),
            N = value)
MOJ_gakurekisimple = MOJ_gakureki %>% 
  filter(gakurekisimple != "不明") %>% 
  mutate(category = fct_drop(gakurekisimple)) %>% 
  group_by(Group, Gender, category, .drop = F) %>% 
  summarise(.groups = "drop", N = sum(N, na.rm = T)) %>% 
  mutate(prop = prop.table(N)) %>% 
  ungroup()

## import Census 2020 data
Census_gakurekisimple = 
  readxl::read_excel("Data_latest/kokusei2020.xlsx", sheet = "gakurekisimple") %>% 
  transmute(Group = "Census", 
            Gender = factor(Gender, levels = factors_gender), 
            category = factor(gakurekisimple, levels = factors_gakurekisimple[1:3]), 
            N) %>% 
  group_by(Group, Gender, .drop = F) %>% 
  mutate(prop = prop.table(N)) %>% 
  ungroup()
# Census_gakurekisimple

## factors
# factors_gakurekisimple
factors_gakurekisimple_recode = set_names(factors_gakurekisimple[factors_gakurekisimple != "不明"], 
                                          nm = c("Junior-high graduate or less", "High school graduate", 
                                                 "University/junior college or higher"))
factors_gakurekisimple_eng = names(factors_gakurekisimple_recode)
category_name_gakurekisimple = "Educational attainment"

factors_gendergroup = c("F_MOJ", "F_A2", "F_CT", "F_Census", 
                        "M_MOJ", "M_A2", "M_CT", "M_Census")

# カテゴリ表
CTA2_gakurekisimple = summary_all %>% 
  filter(title_code == "gakurekisimple", category != "不明")

if(settings_A3 == "CTvsA2"){
  CTA2_gakurekisimple = filter(CTA2_gakurekisimple, Group %in% factors_group2) %>% 
    mutate(Group = fct_drop(Group))
}else if(settings_A3 == "CTA3vsA2"){
  CTA2_gakurekisimple = filter(CTA2_gakurekisimple, Group %in% c("A2", "CTA3")) %>% 
    mutate(Group = factor(if_else(Group == "CTA3", "CT", Group), levels = factors_group2))
}
freq_gakurekisimple =
  full_join(CTA2_gakurekisimple, MOJ_gakurekisimple, by = join_by(Group, Gender, category, N, prop)) %>% 
  full_join(Census_gakurekisimple, by = join_by(Group, Gender, category, N, prop)) %>% 
  transmute(dataname = category_name_gakurekisimple, 
            Group = factor(Group, levels = factors_groups_MOJ), 
            Gender, 
            Gender_Group = factor(paste0(Gender, "_", Group), levels = factors_gendergroup), 
            category = fct_recode(factor(category, levels = factors_gakurekisimple_recode), 
                                  !!!factors_gakurekisimple_recode), N, prop) %>% 
  select(dataname, everything()) %>% 
  arrange(Gender_Group)

## 個表データ
data_gakurekisimple = 
  map_df(factors_gendergroup,  
         function(a){
           # a = factors_gendergroup[1] #test
           temp_df = filter(freq_gakurekisimple, Gender_Group %in% a)
           
           map_df(factors_gakurekisimple_eng, #[-length(factors_gakurekisimple_eng)],
                  function(b){
                    # b = factors_gakurekisimple_eng[1] #test
                    temp1 = filter(temp_df, category == b)
                    tibble(ID = NA_character_, Gender_Group = temp1$Gender_Group, 
                           Group = temp1$Group, Gender = temp1$Gender,
                           category = rep(temp1$category, temp1$N))
                    
                  }) %>% 
             mutate(ID = paste(Gender_Group, row_number(), sep = "_"))
         }
  )

## N
n_gakurekisimple = freq_gakurekisimple %>% 
  group_by(Group, Gender) %>% 
  summarize(.groups = "drop", N_Group = sum(N)) %>%  
  ungroup() %>% 
  arrange(Group)
freq_gakurekisimple = left_join(freq_gakurekisimple, n_gakurekisimple) %>% suppressMessages()
n_gakurekisimpleF = filter(n_gakurekisimple, Gender == "F")
n_gakurekisimpleM = filter(n_gakurekisimple, Gender == "M")

## Fisher's exact test
names(factors_gender) = factors_gender

res_gakurekisimple = 
  map_dfr(factors_gender, .id = "Gender", 
          function(c){
            # c = factors_gender[1] # test
            temp_dataset = data_gakurekisimple %>% 
              filter(Gender == c)
            
            fisherMOJ_A2 = 
              map_df(factors_gakurekisimple_eng, 
                     function(d){
                       # d = factors_gakurekisimple_eng[1] # test
                       temp_dataset = temp_dataset %>% 
                         filter(Group %in% c("MOJ", "A2")) %>% 
                         mutate(Group = fct_drop(Group), 
                                category = if_else(category == d, d, "Else"))
                       fisher.exact(temp_dataset$category, temp_dataset$Group, alternative = "two.sided", 
                                    conf.int = TRUE, conf.level = 0.95) %>% 
                         tidy() %>% 
                         transmute(dataname = category_name_gakurekisimple, category = d, 
                                   comp = "MOJ vs A2", 
                                   OR = estimate, OR_lwr = conf.low, OR_upr = conf.high, 
                                   p.value, sig = p.asterisk2(p.value), method, 
                                   func = "exact2x2::fisher.exact")
                     }
              )
            fisherCT_Census = 
              map_df(factors_gakurekisimple_eng, 
                     function(e){
                       # e = factors_gakurekisimple_eng[1] # test
                       temp_dataset = temp_dataset %>% 
                         filter(Group %in% c("CT", "Census")) %>% 
                         mutate(Group = fct_rev(fct_drop(Group)), 
                                category = if_else(category == e, e, "Else"))
                       fisher.exact(temp_dataset$category, temp_dataset$Group, alternative = "two.sided", 
                                    conf.int = TRUE, conf.level = 0.95) %>% 
                         tidy() %>% 
                         transmute(dataname = category_name_gakurekisimple, category = e, 
                                   comp = "Census2020 vs CT", 
                                   OR = estimate, OR_lwr = conf.low, OR_upr = conf.high, 
                                   p.value, sig = p.asterisk2(p.value), method, 
                                   func = "exact2x2::fisher.exact")
                     }
              )
            
            full_join(fisherMOJ_A2, fisherCT_Census, 
                      by = join_by(dataname, category, comp, 
                                   OR, OR_lwr, OR_upr, p.value, sig, method, func))
          }) %>% 
  select(dataname, everything())
data_gakurekisimple = select(data_gakurekisimple, -Gender_Group)
freq_gakurekisimple = select(freq_gakurekisimple, -Gender_Group)

## plot
color_gakurekisimple_fill = set_names(c("#FF8082", "#FFCABF", "#FFFF99"), factors_gakurekisimple_eng)
color_gakurekisimple_color = set_names(c("#ECF0F1", "#0D0D0D", "#0D0D0D"), factors_gakurekisimple_eng)

factors_recode_temp = factors_groups_MOJ_recode
names(factors_recode_temp) = paste0(names(factors_groups_MOJ_recode), "<br>Women, N = ", n_gakurekisimpleF$N_Group)
plot_gakurekisimpleF = 
  myfunc_stackedplot(filter(data_gakurekisimple, Gender == "F"), factors_gakurekisimple_eng, 
                     groups_recode = factors_recode_temp, 
                     fill = color_gakurekisimple_fill, color = color_gakurekisimple_color, 
                     category_name = category_name_gakurekisimple)

factors_recode_temp = factors_groups_MOJ_recode
names(factors_recode_temp) = paste0(names(factors_groups_MOJ_recode), "<br>Men, N = ", n_gakurekisimpleM$N_Group)
plot_gakurekisimpleM = 
  myfunc_stackedplot(filter(data_gakurekisimple, Gender == "M"), factors_gakurekisimple_eng, 
                     groups_recode = factors_recode_temp, 
                     fill = color_gakurekisimple_fill, color = color_gakurekisimple_color, 
                     category_name = category_name_gakurekisimple)
# plot_gakurekisimpleF / plot_gakurekisimpleM +
#   plot_layout(guides = "collect", axis = "collect")


### Census 2020 gakurekisimple
# data_gakureki2020 = read_csv(file = "Data_original/FEH_00200521_250304151116.csv", skip = 1, 
#                              show_col_types = FALSE) %>% 
#   transmute(Gender = "男女", Age = "年齢", 
#             JHS = `（卒業者）小学校` + `（卒業者）中学校` + `未就学者`, 
#             HS = `（卒業者）高校・旧中`, 
#             College = `（卒業者）短大・高専` + `（卒業者）大学`, 
#             GradS = `（卒業者）大学院`, 
#             School = `在学者`, 
#             Unknown = `（卒業者）不詳` + `在学か否かの別「不詳」`)


## age & education bar plot ###################################################
if(settings_A3 == "CTA3vsA2") setting_compare = "CTA3vsA2"
if(settings_A3 == "CTvsA2") setting_compare = "CTvsA2"

source("Scripts/A2A3data_import.R")
CTA2_AgeEdu = fulldata %>% 
  filter(Group != "A3") %>% 
  mutate(Group = factor(if_else(Group == "CTA3", "CT", Group), levels = factors_group2)) %>% 
  select(ID, Gender, Group, age_subject, education) %>% 
  mutate(Gender = fct_recode(Gender, "Women" = "F", "Men" = "M"), 
         Group = fct_recode(fct_drop(Group), !!!factors_groups_short_recode))

# x_col = "age_subject"; y_lab = "Age of participants"; y_intercept = NULL; jitter_h = 0 # test
# scale_y = scale_y_continuous(expand = c(0, 0), limits = c(15, 65), breaks = seq(10, 70, by = 10)) # test
# x_col = "education"; y_lab = "Years of post-compulsory education"; y_intercept = 0; jitter_h = .2 # test
# scale_y = scale_y_continuous(expand = c(0, 0), limits = c(-.5, 9.5), breaks = seq(0, 9, by = 2)) # test
myfunc_plot = 
  function(x_col, y_lab = "Y label", y_intercept = NULL, jitter_h = 0, scale_y = scale_y_continuous()){
    
    df_jitter = CTA2_AgeEdu %>% 
      rename(value = all_of(x_col))
    
    df_summary = df_jitter %>% 
      group_by(Gender, Group) %>% 
      summarise(.groups = "drop", 
                N = sum(!is.na(value), na.rm = TRUE), 
                Min = min(value, na.rm = TRUE), Max = max(value, na.rm = TRUE), 
                SD = sqrt(var(value, na.rm = TRUE)*((N - 1)/N)), sd = sd(value, na.rm = TRUE), SEM = sd(value, na.rm = TRUE)/sqrt(N), 
                M = mean(value, na.rm = TRUE), Median = median(value, na.rm = TRUE), 
                Kurtosis = moments::kurtosis(value, na.rm = TRUE), # 尖度 Kurtosis 
                Skewness = moments::skewness(value, na.rm = TRUE)) %>% # 歪度 Skewness
      ungroup() %>% 
      mutate(variable = y_lab, 
             t95 = qt(p = 0.05/2, df = N - 1, lower.tail = F) %>% suppressWarnings(), 
             CI = t95 * SEM, M_lwr = M - CI, M_upr = M + CI) %>% # 伝えるための心理統計 (2012) p.125; same as one-sample "t.test()"
      select(variable, everything())
    
    temp_plot = ggplot() + 
      facet_wrap(~ Gender, 
                 nrow = 1, 
                 scales = "free", 
                 strip.position = "top") + 
      {if(!is.null(y_intercept)) geom_hline(yintercept = y_intercept, color = "#1A1A1A", linewidth = .4)} + 
      geom_jitter(data = df_jitter, 
                  aes(x = Group, y = value, fill = Group, color = Group, alpha = Group),
                  width = .3, height = jitter_h, 
                  shape = 21, size = .9, stroke = .6) +
      geom_errorbar(data = df_summary, 
                    aes(x = Group, ymin = M_lwr, ymax = M_upr, color = Group), show.legend = FALSE, 
                    width = .1, linewidth = .5, alpha = 1) + 
      geom_point(data = df_summary, 
                 aes(x = Group, y = M, color = Group), show.legend = FALSE, 
                 size = 2.5, shape = 18, alpha = 1) + 
      scale_y + 
      scale_fill_manual(values = c("#FFFFFF", "#C9ACE6")) + 
      scale_color_manual(values = c("#795548", "#990099")) + 
      scale_alpha_manual(values = c(.5, .25)) + 
      labs(y = y_lab) + 
      theme_classic() + 
      theme(text = element_text(family = "Arial", color = "#0D0D0D"),
            line = element_line(color = "#1A1A1A", linewidth = .3),
            rect = element_rect(color = "#1A1A1A", linewidth = .3), 
            # legend & strip
            legend.position = "none", 
            strip.text = element_text(color = "#0D0D0D", face = "bold", size = 10, 
                                      margin = margin(t = 0, b = 1, l = 0, r = 0, unit = "mm")), 
            strip.background = element_blank(), 
            # axis
            axis.line.y = element_line(color = "#1A1A1A", linewidth = .4),
            axis.line.x = element_blank(), 
            axis.ticks.y = element_line(color = "#1A1A1A", linewidth = .4),
            axis.ticks.x = element_blank(), 
            axis.text.x = element_text(color = "#0D0D0D", face = "bold", size = 10, 
                                       margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm")), 
            axis.text.y = element_text(color = "#0D0D0D", size = 9, 
                                       margin = margin(t = 0, b = 0, l = 2, r = 1, unit = "mm")), 
            axis.title.x = element_blank(), 
            axis.title.y = element_text(color = "#0D0D0D", face = "bold", size = 10, 
                                        margin = margin(t = 0, b = 0, l = 1, r = 0, unit = "mm")),
            # panel
            panel.background = element_rect(color = NA, fill = "#FFFFFF"), 
            panel.grid.major.y = element_line(color = "#BDC3C7", linewidth = .2), 
            panel.grid.minor.y = element_line(color = "#BDC3C7", linewidth = .1), 
            panel.grid.major.x = element_blank(), 
            
            # plot
            plot.margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm"))
    
    list(summary = df_summary, plot = temp_plot)
  }

plot_Age = myfunc_plot(x_col = "age_subject", y_lab = "Age of participants", y_intercept = NULL, jitter_h = 0, 
                       scale_y = scale_y_continuous(expand = c(0, 0), limits = c(18, 54), breaks = seq(10, 70, by = 10)))
plot_Edu = myfunc_plot(x_col = "education", y_lab = "Years of post-compulsory education", y_intercept = 0, jitter_h = .025, 
                       scale_y = scale_y_continuous(expand = c(0, 0), limits = c(-.5, 9.5), breaks = seq(0, 9, by = 2)))
# plot_Age$summary
# plot_Edu$summary
plot_Age$plot
plot_Edu$plot



# export ####################################################
# ls(pattern = "^data_|^freq_|^res_|^plot_")
rm(list = ls(pattern = "^data_(CT|A2)|^data_age_child_"))
# rm(list = ls(pattern = "^plot_age_child_main$"))

## save as .Rdata
namelist_save = ls(pattern = "^data_|^freq_|^res_|^plot_")
save(list = namelist_save, file = paste0("Reports/A2A3_supp_", settings_A3, ".Rdata"))

## save as .txt
sink(paste0("Reports/", temp_file_name, ".txt"))
age_child_Levenetest
age_child_Welchtest
age_child_ttest
age_child_KStest
sessionInfo()
sink()

## save as .xlsx
order_dataname = c("age_child", "gyakutai", "kagaisya", "setaisimple", "syogai",
                   "CSLC", "keizaisimple2", "gakurekisimple", "oyafuzai")
namelist_freq = paste0("freq_", order_dataname)
namelist_res = paste0("res_", order_dataname[!(order_dataname %in% c("age_child", "CSLC"))])

freq_supp = map_df(namelist_freq, get) %>%
  mutate(Gender = if_else(is.na(Gender), "BOTH", Gender), 
         FileCreated = Sys.time()) %>%
  select(dataname, Group, Gender, everything())
res_supp = map_df(namelist_res, get) %>%
  mutate(Gender = if_else(is.na(Gender), "BOTH", Gender), 
         FileCreated = Sys.time()) %>%
  select(dataname, Gender, everything())

writexl::write_xlsx(list("freq" = freq_supp, "fisher" = res_supp), format_headers = F,
                    path = paste0("Reports/", temp_file_name, ".xlsx"))

## save as .pdf
mytheme_tags = theme(plot.tag = 
                       element_text(family = "Arial", face = "bold", color = "#0D0D0D", size = 14, 
                                    margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm")), 
                     plot.tag.location = "margin")

### Fig S1a ####
plotS1A = plot_gyakutai + 
  labs(tag = "A") + 
  mytheme_tags + 
  theme(plot.margin = margin(t = 10, b = 2.5, l = 5, r = 5, unit = "mm"))
plotS1B = plot_age_child_main + 
  labs(tag = "B") + 
  mytheme_tags + 
  theme(plot.margin = margin(t = 2.5, b = 2.5, l = 10, r = 10, unit = "mm"))
plotS1C = plot_kagaisya + 
  labs(tag = "C") + 
  mytheme_tags + 
  theme(plot.margin = margin(t = 2.5, b = 2.5, l = 5, r = 5, unit = "mm"))
plotS1D = plot_setaisimple + 
  labs(tag = "D") + 
  mytheme_tags + 
  theme(plot.margin = margin(t = 2.5, b = 10, l = 5, r = 5, unit = "mm"))

height_S1a = c(3, 6, 3, 4, 9)
sum(height_S1a)
quartz(type = "pdf", file = paste0("Figures/A2A3_figS1a_", 
                                   settings_A3, "_", str_remove_all(date_original, "\\-"), ".pdf"),
       family = "Arial", width = A4w, height = A4h)
plotS1A + free(plotS1B, side = "lr") + plotS1C + plotS1D + plot_spacer() + 
  plot_layout(ncol = 1, heights = height_S1a)
dev.off()


### Fig S1b ####
plotS1E1 = plotlist_syogai[[1]] + 
  labs(tag = "E") + 
  mytheme_tags + 
  mytheme_legend + 
  theme(axis.title.x = element_blank(), 
        plot.margin = margin(t = 10, b = 0, l = 10, r = 10, unit = "mm"))
plotS1E2 = plotlist_syogai[[2]] + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        plot.margin = margin(t = 0, b = 0, l = 10, r = 10, unit = "mm"))
plotS1E3 = plotlist_syogai[[3]] + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        plot.margin = margin(t = 0, b = 0, l = 10, r = 10, unit = "mm"))
plotS1E4 = plotlist_syogai[[4]] + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none", 
        plot.margin = margin(t = 0, b = 0, l = 10, r = 10, unit = "mm"))
plotS1E5 = plotlist_syogai[[5]] + 
  theme(plot.margin = margin(t = 0, b = 2, l = 10, r = 10, unit = "mm"))

plotS1F1 = plot_CSLC + 
  labs(tag = "F") + 
  mytheme_tags + 
  theme(plot.margin = margin(t = 2, b = 1, l = 10, r = 10, unit = "mm"))
plotS1F2 = plot_keizaisimple2 + 
  theme(plot.margin = margin(t = 1, b = 2, l = 10, r = 10, unit = "mm"))

plotS1G1 = plot_gakurekisimpleF + 
  labs(tag = "G") + 
  mytheme_tags + 
  mytheme_legend + 
  theme(axis.line.x.top = element_line(color = "#1A1A1A", linewidth = .3), 
        axis.line.x.bottom = element_blank(), 
        axis.ticks.x.top = element_line(color = "#1A1A1A", linewidth = .3),
        axis.ticks.x.bottom = element_blank(), 
        axis.text.x.top = element_text(color = "#0D0D0D", size = 9),
        axis.text.x.bottom = element_blank(),
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 10, r = 10, unit = "mm"))
plotS1G2 = plot_gakurekisimpleM + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 1, b = 10, l = 10, r = 10, unit = "mm"))

height_S1b = c(13, 2, 4, 4, 4)
sum(height_S1b)
quartz(type = "pdf", file = paste0("Figures/A2A3_figS1b_", 
                                   settings_A3, "_", str_remove_all(date_original, "\\-"), ".pdf"),
       family = "Arial", width = A4w, height = A4h)
free(plotS1E1 / plotS1E2 / plotS1E3 / plotS1E4 / plotS1E5, type = "label", side = "lr") + 
  plotS1F1 + 
  plotS1F2 + 
  plotS1G1 + 
  plotS1G2 + 
  plot_layout(ncol = 1, heights = height_S1b)
dev.off()


### Fig S2 ####
# mytheme_tags_title = 
#   theme(plot.title = 
#           element_text(family = "Arial", face = "bold", color = "#0D0D0D", size = 14, 
#                        margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "mm")), 
#         plot.title.position = "plot")
# plotS2A = plot_Age$plot + 
#   labs(title = "A") + 
#   mytheme_tags_title + 
#   theme(plot.margin = margin(t = 10, b = 5, l = 10, r = 0, unit = "mm"))
# plotS2B = plot_Edu$plot + 
#   labs(title = "B") + 
#   mytheme_tags_title + 
#   theme(plot.margin = margin(t = 10, b = 5, l = 10, r = 10, unit = "mm"))
# plotS2C1 = plot_oyafuzaiF + 
#   labs(title = "C") + 
#   theme(plot.title = 
#           element_text(family = "Arial", face = "bold", color = "#0D0D0D", size = 14, 
#                        margin = margin(t = 0, b = 2, l = 0, r = 0, unit = "mm")), 
#         plot.title.position = "plot") + 
#   mytheme_legend + 
#   theme(axis.line.x.top = element_line(color = "#1A1A1A", linewidth = .3), 
#         axis.line.x.bottom = element_blank(), 
#         axis.ticks.x.top = element_line(color = "#1A1A1A", linewidth = .3),
#         axis.ticks.x.bottom = element_blank(), 
#         axis.text.x.top = element_text(color = "#0D0D0D", size = 9),
#         axis.text.x.bottom = element_blank(),
#         axis.title.x = element_blank(), 
#         plot.margin = margin(t = 2.5, b = 1, l = 10, r = 10, unit = "mm"))
# plotS2C2 = plot_oyafuzaiM + 
#   theme(legend.position = "none", 
#         plot.margin = margin(t = 1, b = 10, l = 10, r = 10, unit = "mm"))
# 
# height_S2 = c(8, 2, 2, 13)
# sum(height_S2)
# quartz(type = "pdf", file = paste0("Figures/A2A3_figS2_", 
#                                    settings_A3, "_", str_remove_all(date_original, "\\-"), ".pdf"),
#        family = "Arial", width = A4w, height = A4h)
# (free(plotS2A | plotS2B, side = "lr")) / 
#   plotS2C1 / plotS2C2 / plot_spacer() + 
#   plot_layout(ncol = 1, heights = height_S2)
# dev.off()

plotS2C1 = plot_oyafuzaiF + 
  mytheme_legend + 
  theme(axis.line.x.top = element_line(color = "#1A1A1A", linewidth = .3), 
        axis.line.x.bottom = element_blank(), 
        axis.ticks.x.top = element_line(color = "#1A1A1A", linewidth = .3),
        axis.ticks.x.bottom = element_blank(), 
        axis.text.x.top = element_text(color = "#0D0D0D", size = 9),
        axis.text.x.bottom = element_blank(),
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 2.5, b = 1, l = 10, r = 10, unit = "mm"))
plotS2C2 = plot_oyafuzaiM + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 1, b = 2.5, l = 10, r = 10, unit = "mm"))

quartz(type = "pdf", file = paste0("Figures/A2A3_figS2_", 
                                   settings_A3, "_", str_remove_all(date_original, "\\-"), ".pdf"),
       family = "Arial", width = A4w, height = A4h/5)
plotS2C1 / plotS2C2 + 
  plot_layout(ncol = 1)
dev.off()

writeLines(txt_sessioninfo, paste0("SessionInfo/", temp_file_name, ".txt"), sep = "\n")

warnings()


# ## test ####
# x = matrix(c(10,20,30,10), nrow = 2, byrow = T)
# x
# fisher.exact(x)
# # fisher.test(x)
# 
# y = matrix(c(10,20,30,10)*100, nrow = 2, byrow = T)
# y
# fisher.exact(y)
# # fisher.test(y)
# 
# z = matrix(c(10,20,30,10)/10, nrow = 2, byrow = T)
# z
# fisher.exact(z)
# # fisher.test(z)

