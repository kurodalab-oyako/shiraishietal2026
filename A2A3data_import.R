# data import (update: 2021.12.17) ####
# update 2025.03.17: remove F0545 (grandmother in A3)
Sys.setenv(LANGUAGE = "en_US.UTF-8")
library(tidyverse)

## settings ####
date_today = str_remove_all(Sys.Date(), "-")

if(!exists("setting_variables")) setting_variables = "MainVars"
if(!exists("setting_language")) setting_language = "jpn"
if(!exists("setting_compare")) setting_compare = "CTvsA2"
if(!exists("setting_subset")) setting_subset = ""


## import prepared dataset ####
load("Data_latest/A2A3analysis_maindata.Rdata")
if(grepl("FamilyVars", setting_variables)){
  load("Data_latest/A2A3analysis_familydata.Rdata")
  fulldata = filter(fulldata_fam, ID != "SD", !grepl("Mean", ID)) %>% 
    mutate(Gender = fct_relevel(Gender, c("F", "M")) %>% fct_drop())
}else{
  fulldata = fulldata_main
}
# names(fulldata)

if(setting_compare == "CTA3vsA2"){
  fulldata = mutate(fulldata, 
                    Group = factor(if_else(Group == "A2", "A2", "CTA3"), 
                                   levels = c("CTA3", "A2"))
                    )
}

if(setting_subset == "bio"){
  fulldata = filter(fulldata, 
                    I4_03_自分orパートナーと非血縁の子 == 0)
}


## original data info ####
# nchar("A2A3newdata_all_")
file_original = fulldata[[1, "OriginalFile"]]
date_original = fulldata[[1, "OriginalDataDate"]]


## mean of all groups ####
age_subject_meanAll = mean(fulldata$age_subject, na.rm = T) # 全群の平均値
education_meanAll = mean(fulldata$education, na.rm = T) # 全群の平均値
age_1st_child_meanAll = mean(fulldata$age_1st_child, na.rm = T) # 全群の平均値


## mean of CT & A2 groups ####
age_subject_meanCTA2 = mean(filter(fulldata, Group != "A3")$age_subject, na.rm = T) # CT & A2群の平均値
education_meanCTA2 = mean(filter(fulldata, Group != "A3")$education, na.rm = T) # CT & A2群の平均値
age_1st_child_meanCTA2 = mean(filter(fulldata, Group != "A3")$age_1st_child, na.rm = T) # CT & A2群の平均値


## mean of A2 group ####
age_subject_meanA2 = mean(fulldata$age_subject[fulldata$Group == "A3"], na.rm = T) # A2群の平均値
education_meanA2 = mean(fulldata$education[fulldata$Group == "A3"], na.rm = T) # A2群の平均値
age_1st_child_meanA2 = mean(fulldata$age_1st_child[fulldata$Group == "A3"], na.rm = T) # A2群の平均値


## dataset for summary ####
factors_gender3 = c("MIX", "F", "M")
if(!exists("factors_group")) factors_group = c("ALL", "CT", "A3", "A2")
fulldata_for_summary = bind_rows(fulldata, mutate(fulldata, Gender = "MIX", Group = "ALL"), 
                                 mutate(fulldata, Gender = "MIX"), mutate(fulldata, Group = "ALL")) %>% 
  mutate(Gender = fct_relevel(Gender, factors_gender3), 
         Group = fct_relevel(Group, factors_group) %>% suppressWarnings())


## data summary ####
summary_both = fulldata %>%
  group_by(Group, Gender) %>%
  summarise(N = length(ID), .groups = "drop")

summary_Gender = summary_both %>%
  group_by(Gender) %>%
  summarise(across(N, ~ sum(.)), .groups = "drop") %>%
  mutate(Group = "ALL")

summary_Group = summary_both %>%
  group_by(Group) %>%
  summarise(across(N, ~ sum(.)), .groups = "drop") %>%
  mutate(Gender = "MIX")

summary_all = bind_rows(summary_both, summary_Gender) %>%
  pivot_wider(names_from = "Gender", values_from = "N") %>%
  rowwise() %>%
  mutate(MIX = sum(F, M))

summary_jpn = summary_all %>%
  mutate(Group = case_when(Group ==  "CT" ~ "一般群", Group == "A3" ~ "プログラム群", Group == "A2" ~ "事件群",
                           Group == "ALL" ~ "全群計")) %>%
  rename(群 = "Group", 女性 = "F", 男性 = "M", 男女計 = "MIX")


summary_eng = summary_all %>%
  mutate(Group = case_when(Group ==  "CT" ~ "Control", Group == "A3" ~ "Program", Group == "A2" ~ "Case",
                           Group == "ALL" ~ "Sum")) %>%
  rename(Female = "F", Male = "M", Sum = "MIX")

summary_N = bind_cols(summary_eng, summary_jpn) %>% 
  mutate(FileCreated = Sys.time())
