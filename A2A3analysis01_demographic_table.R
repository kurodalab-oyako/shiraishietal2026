# Demographic table (Table 1) and control-subgroup table (Supplementary Table S3) ##############################
# Based on A2A3analysis01_demographic.R
#
# Output: Results/A2A3_demographic_table_<date>.xlsx
#   Sheet "Table 1"  : Case group (women, men) vs Control group (women, men; control = CT + A3)
#   Sheet "Table S3" : Control group split into community-recruited (CT) and parenting program participants (A3)
#   Sheet "summary"  : long-format numbers behind both tables (N, mean, SD, CI, counts, proportions)
#
# Notes
#   - SD uses the sample SD (n - 1 in the denominator), consistent with the 95% CI (t distribution).
#     The previous script (A2A3analysis01) used n in the denominator (setting_sd_denominator = "n" reproduces it).
#   - Percentages use all participants in each column as the denominator; missing values are shown as "Unknown".
#   - Type of crime convicted is taken from Data_latest/A2details.xlsx and shown for the case group only (Table 1).
#
# Before starting, clear all objects from workspace & restart R!
rm(list = ls())
Sys.setenv(LANGUAGE = "en_US.UTF-8")

library(tidyverse)
library(openxlsx)


## settings ####################################################################################################
setting_sd_denominator = "n-1"     # "n-1" (sample SD; recommended) / "n" (as in A2A3analysis01)
setting_digits = 1                 # decimals for mean, SD, CI, and percentages
setting_drop_empty_unknown = TRUE  # drop an "Unknown" row when it is 0 in every column of the table
setting_font = "Arial"
dir_out = "Results"

# data import settings (same as A2A3analysis05_cumrisk_all.R: keep A3 as its own group, all participants)
setting_subset = "all"
group_type     = "CTA3"


## data ########################################################################################################
source("Scripts/A2A3functions.R")       # date_original_chr, etc.
source("Scripts/A2A3data_import.R")     # fulldata

if(!all(c("A2", "CT", "A3") %in% unique(as.character(fulldata$Group)))){
  stop("fulldata must contain the groups A2, CT, and A3 separately. Check A2A3data_import.R settings.")
}

### English labels and recoding ####
levels_relation = c("Biological parent", "Step or adoptive parent", "Cohabitant of the caregiver", "Unknown")
levels_marital  = c("Unmarried", "Legally married", "Married (common-law)", "Married (not specified)",
                    "Separated", "Divorced or widowed", "Unknown")
levels_household = c("Living with both biological parents", "Single parent", "Living with a non-kin caregiver",
                     "Other", "Unknown")
levels_education = c("Junior-high school graduate & dropout",
                     "High school / vocational school (post-junior high school) graduate",
                     "University / junior college / vocational school (post-high school) graduate",
                     "Master's degree or higher", "Unknown")
levels_childgender = c("Female", "Male", "Male & female twin", "Unknown")
levels_crime = c("Homicide", "Injury / Injury Causing Death",
                 "Unlawful Capture or Confinement Causing Death or Injury",
                 "Abandonment by a Person Responsible for Their Care / Abandonment Causing Death or Injury",
                 "Other", "Unknown")

data_demo = fulldata %>%
  transmute(
    ID,
    Gender = factor(as.character(Gender), levels = c("F", "M")),
    Group  = as.character(Group),

    age_subject, education,

    relation = case_when(
      A3_01_対象児との続柄 %in% c("実母", "実父")     ~ "Biological parent",
      A3_01_対象児との続柄 %in% c("義継養父", "義継養母") ~ "Step or adoptive parent",
      A3_01_対象児との続柄 %in% c("MB", "その他")     ~ "Cohabitant of the caregiver",
      TRUE ~ "Unknown"),

    marital = case_when(
      A4_02_回答者の現在の婚姻状況 == "未婚"                    ~ "Unmarried",
      A4_02_回答者の現在の婚姻状況 == "結婚中"                  ~ "Legally married",
      A4_02_回答者の現在の婚姻状況 == "事実婚中"                ~ "Married (common-law)",
      A4_02_回答者の現在の婚姻状況 == "結婚または事実婚中"      ~ "Married (not specified)",
      A4_02_回答者の現在の婚姻状況 %in% c("別居中", "離婚準備中") ~ "Separated",
      A4_02_回答者の現在の婚姻状況 %in% c("離婚", "事実婚解消") ~ "Divorced or widowed",
      TRUE ~ "Unknown"),

    household = case_when(
      A4_01_養育者の世帯構成 %in% c("実父母", "実父母 (特記事項)") ~ "Living with both biological parents",
      A4_01_養育者の世帯構成 %in% c("一人親 (未婚)", "一人親 (離婚)", "一人親 (別居)",
                                    "一人親 (死別)", "一人親 (その他)")  ~ "Single parent",
      A4_01_養育者の世帯構成 %in% c("非血縁者と同居 (再婚)", "非血縁者と同居 (内縁関係)",
                                    "非血縁者と同居 (養父母)", "非血縁者と同居 (実親の同居人)",
                                    "非血縁者と同居 (その他)")           ~ "Living with a non-kin caregiver",
      A4_01_養育者の世帯構成 %in% c("実親不在で親族と同居", "その他", "不明") ~ "Other",
      TRUE ~ "Unknown"),

    education_cat = case_when(
      A4_04_回答者の最終学歴 %in% c("0.00 中退", "0.00 中退卒不明", "0.00 中卒", "1.00 専修退", "1.50 高退")
        ~ "Junior-high school graduate & dropout",
      A4_04_回答者の最終学歴 %in% c("1.50 専修退卒不明", "2.00 専修卒", "2.25 高退卒不明",
                                    "2.50 専修卒 or 高卒", "3.00 高卒", "4.00 短専退", "5.00 大退")
        ~ "High school / vocational school (post-junior high school) graduate",
      A4_04_回答者の最終学歴 %in% c("4.50 短専退卒不明", "5.00 短専卒", "6.00 大退卒不明", "7.00 大卒", "8.00 院退")
        ~ "University / junior college / vocational school (post-high school) graduate",
      A4_04_回答者の最終学歴 %in% c("8.50 院退卒不明", "9.00 院卒")
        ~ "Master's degree or higher",
      TRUE ~ "Unknown"),

    child_gender = case_when(
      A3_03_対象児の性別JPN == "女児"       ~ "Female",
      A3_03_対象児の性別JPN == "男児"       ~ "Male",
      A3_03_対象児の性別JPN == "男女の双子" ~ "Male & female twin",
      TRUE ~ "Unknown")
  ) %>%
  mutate(relation      = factor(relation,      levels = levels_relation),
         marital       = factor(marital,       levels = levels_marital),
         household     = factor(household,     levels = levels_household),
         education_cat = factor(education_cat, levels = levels_education),
         child_gender  = factor(child_gender,  levels = levels_childgender))

# Unrecoded raw values would silently become "Unknown": list them so they can be checked
check_unknown = data_demo %>%
  select(ID, Group, relation, marital, household, education_cat, child_gender) %>%
  pivot_longer(-c(ID, Group), names_to = "variable", values_to = "value") %>%
  filter(value == "Unknown") %>%
  count(Group, variable, name = "n_unknown")
if(nrow(check_unknown) > 0){
  message("Rows recoded as 'Unknown' (check that no category was missed):")
  print(check_unknown)
}

### Type of crime (case group only) ####
data_crime = readxl::read_excel("Data_latest/A2details.xlsx", sheet = "A2 data", na = c("", "NA")) %>%
  transmute(Gender = factor(case_when(Gender1 == 0 ~ "F", Gender1 == 1 ~ "M"), levels = c("F", "M")),
            crime = case_when(Type_cr == 1 ~ "Homicide",
                              Type_cr == 2 ~ "Injury / Injury Causing Death",
                              Type_cr == 3 ~ "Unlawful Capture or Confinement Causing Death or Injury",
                              Type_cr == 4 ~ "Abandonment by a Person Responsible for Their Care / Abandonment Causing Death or Injury",
                              Type_cr == 5 ~ "Other",
                              TRUE ~ "Unknown") %>%
              factor(levels = levels_crime),
            col = "Case")

n_crime = count(data_crime, Gender)
n_case  = count(filter(data_demo, Group == "A2"), Gender)
if(!isTRUE(all.equal(n_crime$n, n_case$n[match(n_crime$Gender, n_case$Gender)]))){
  warning("The number of case participants in A2details.xlsx differs from fulldata. Check the crime-type rows.")
}


## column definitions ##########################################################################################
# col: the column group used in each table (Case / Control / Community / Program)
data_cols = bind_rows(
  data_demo %>% filter(Group == "A2") %>% mutate(col = "Case"),
  data_demo %>% filter(Group %in% c("CT", "A3")) %>% mutate(col = "Control"),
  data_demo %>% filter(Group == "CT") %>% mutate(col = "Community"),
  data_demo %>% filter(Group == "A3") %>% mutate(col = "Program")
) %>%
  mutate(col = factor(col, levels = c("Case", "Control", "Community", "Program")))

N_cols = data_cols %>% count(col, Gender, name = "N_col", .drop = FALSE)


## summary functions ###########################################################################################
fmt = function(x) formatC(x, format = "f", digits = setting_digits)

summ_num = function(v, label){
  data_cols %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(col, Gender, .drop = FALSE) %>%
    summarise(.groups = "drop",
              n    = n(),
              mean = mean(.data[[v]]),
              sd_n1 = sd(.data[[v]]),
              sd_n  = sqrt(stats::var(.data[[v]]) * (n - 1) / n),
              min  = min(.data[[v]]),
              max  = max(.data[[v]])) %>%
    mutate(sd  = if(setting_sd_denominator == "n") sd_n else sd_n1,
           se  = sd_n1 / sqrt(n),
           t95 = qt(.975, df = n - 1),
           lwr = mean - t95 * se,
           upr = mean + t95 * se,
           section = label, type = "numeric") %>%
    left_join(N_cols, by = c("col", "Gender"))
}

summ_fct = function(df, v, label){
  df %>%
    mutate(value = .data[[v]]) %>%
    count(col, Gender, value, name = "n", .drop = FALSE) %>%
    left_join(N_cols, by = c("col", "Gender")) %>%
    mutate(prop = n / N_col, section = label, type = "factor",
           value = as.character(value))
}

# numeric: three rows (Mean ± SD, Range, 95%CI)
rows_num = function(s){
  s %>%
    transmute(section, col, Gender,
              `Mean ± SD` = paste0(fmt(mean), " ± ", fmt(sd)),
              Range       = paste0(formatC(min, format = "f", digits = 0), "–",
                                   formatC(max, format = "f", digits = 0)),
              `95%CI`     = paste0("[", fmt(lwr), ", ", fmt(upr), "]")) %>%
    pivot_longer(c(`Mean ± SD`, Range, `95%CI`), names_to = "row", values_to = "cell") %>%
    mutate(row = factor(row, levels = c("Mean ± SD", "Range", "95%CI")))
}

# factor: one row per category, "n (x.x%)"
rows_fct = function(s, levels){
  s %>%
    mutate(pct  = if_else(N_col == 0, NA_real_, 100 * prop),
           pct_txt = case_when(is.na(pct) ~ "",
                               pct == 0   ~ "0%",
                               pct == 100 ~ "100%",
                               TRUE       ~ paste0(fmt(pct), "%")),
           cell = paste0(n, " (", pct_txt, ")"),
           row  = factor(value, levels = levels)) %>%
    select(section, col, Gender, row, cell, n)
}


## build tables ################################################################################################
sections = list(
  list(var = "age_subject",   label = "Age (at the time of the survey)",                  type = "num"),
  list(var = "relation",      label = "Relation to the target child",                     type = "fct", levels = levels_relation),
  list(var = "marital",       label = "Marital status (at the time of the survey)",       type = "fct", levels = levels_marital),
  list(var = "household",     label = "Household of the target child (during the target period)", type = "fct", levels = levels_household),
  list(var = "education_cat", label = "Educational background",                           type = "fct", levels = levels_education),
  list(var = "education",     label = "Years of post-compulsory education",               type = "num"),
  list(var = "child_gender",  label = "Gender of the target child",                       type = "fct", levels = levels_childgender)
)

summary_list = list()
rows_list = list()
for(s in sections){
  if(s$type == "num"){
    sm = summ_num(s$var, s$label)
    rows_list[[s$label]] = rows_num(sm)
  }else{
    sm = summ_fct(data_cols, s$var, s$label)
    rows_list[[s$label]] = rows_fct(sm, s$levels)
  }
  summary_list[[s$label]] = mutate(sm, variable = s$var)
}

# crime type (case only)
label_crime = "Type of crime convicted"
sm_crime = data_crime %>%
  mutate(col = factor(col, levels = levels(data_cols$col))) %>%
  summ_fct("crime", label_crime) %>%
  filter(col == "Case")
summary_list[[label_crime]] = mutate(sm_crime, variable = "crime")
rows_list[[label_crime]] = rows_fct(sm_crime, levels_crime)

summary_all = bind_rows(summary_list) %>%
  relocate(section, variable, type, col, Gender, N_col)

# wide table for a set of columns
make_table = function(cols, include_crime){
  keys = expand_grid(col = cols, Gender = c("F", "M")) %>% mutate(key = paste(col, Gender, sep = "_"))
  secs = names(rows_list)
  if(!include_crime) secs = setdiff(secs, label_crime)

  body = map_dfr(secs, function(sec){
    r = rows_list[[sec]] %>%
      filter(col %in% cols) %>%
      mutate(key = paste(col, Gender, sep = "_"))

    # drop "Unknown" rows that are 0 in every column of this table
    if(setting_drop_empty_unknown && "n" %in% names(r)){
      empty_unknown = r %>% group_by(row) %>% summarise(all0 = all(n == 0), .groups = "drop") %>%
        filter(row == "Unknown", all0) %>% pull(row)
      r = filter(r, !row %in% empty_unknown)
    }

    w = r %>%
      select(row, key, cell) %>%
      pivot_wider(names_from = key, values_from = cell) %>%
      arrange(row) %>%
      mutate(row = as.character(row))

    # crime type: control columns are not applicable
    for(k in keys$key) if(!k %in% names(w)) w[[k]] = ".."
    bind_rows(tibble(row = sec, .is_section = TRUE),
              mutate(w, .is_section = FALSE))
  })

  body %>%
    mutate(across(all_of(keys$key), ~ replace_na(., ""))) %>%
    select(row, .is_section, all_of(keys$key))
}

table1  = make_table(c("Case", "Control"), include_crime = TRUE)
tableS3 = make_table(c("Community", "Program"), include_crime = FALSE)


## export (formatted xlsx) #####################################################################################
col_labels = c(Case = "Case group", Control = "Control group",
               Community = "Community-recruited participants", Program = "Parenting program participants")

st_title   = createStyle(fontName = setting_font, fontSize = 11, textDecoration = "bold")
st_head1   = createStyle(fontName = setting_font, fontSize = 10, textDecoration = "bold", halign = "center",
                         border = "Bottom", borderColour = "#404040")
st_head2   = createStyle(fontName = setting_font, fontSize = 10, textDecoration = "bold", halign = "center")
st_head3   = createStyle(fontName = setting_font, fontSize = 10, halign = "center",
                         border = "Bottom", borderColour = "#404040")
st_section = createStyle(fontName = setting_font, fontSize = 10, textDecoration = "bold")
st_row     = createStyle(fontName = setting_font, fontSize = 10, indent = 1, wrapText = TRUE, valign = "center")
st_cell    = createStyle(fontName = setting_font, fontSize = 10, halign = "center", valign = "center")
st_last    = createStyle(border = "Bottom", borderColour = "#404040")
st_note    = createStyle(fontName = setting_font, fontSize = 9, wrapText = TRUE, valign = "top")

write_table_sheet = function(wb, sheet, tbl, cols, title, notes){
  addWorksheet(wb, sheet, gridLines = FALSE)
  keys = as.vector(t(outer(cols, c("F", "M"), paste, sep = "_")))
  nc = 1 + length(keys)

  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet, st_title, rows = 1, cols = 1)

  # header row 2: column groups; row 3: Women / Men; row 4: n
  for(i in seq_along(cols)){
    c1 = 2 + 2 * (i - 1)
    writeData(wb, sheet, col_labels[[cols[i]]], startRow = 2, startCol = c1)
    mergeCells(wb, sheet, cols = c1:(c1 + 1), rows = 2)
    writeData(wb, sheet, t(c("Women", "Men")), startRow = 3, startCol = c1, colNames = FALSE)
    nn = N_cols %>% filter(col == cols[i]) %>% arrange(Gender) %>% pull(N_col)
    writeData(wb, sheet, t(paste0("(n = ", nn, ")")), startRow = 4, startCol = c1, colNames = FALSE)
  }
  addStyle(wb, sheet, st_head1, rows = 2, cols = 2:nc, gridExpand = TRUE)
  addStyle(wb, sheet, st_head2, rows = 3, cols = 2:nc, gridExpand = TRUE)
  addStyle(wb, sheet, st_head3, rows = 4, cols = 1:nc, gridExpand = TRUE)

  # body
  r0 = 5
  writeData(wb, sheet, select(tbl, row, all_of(keys)), startRow = r0, startCol = 1, colNames = FALSE)
  rr = r0 + seq_len(nrow(tbl)) - 1
  sec_rows = rr[tbl$.is_section]
  item_rows = rr[!tbl$.is_section]
  addStyle(wb, sheet, st_section, rows = sec_rows, cols = 1, gridExpand = TRUE)
  addStyle(wb, sheet, st_row, rows = item_rows, cols = 1, gridExpand = TRUE)
  addStyle(wb, sheet, st_cell, rows = rr, cols = 2:nc, gridExpand = TRUE)
  addStyle(wb, sheet, st_last, rows = max(rr), cols = 1:nc, gridExpand = TRUE, stack = TRUE)

  # notes
  nr = max(rr) + 2
  for(k in seq_along(notes)){
    writeData(wb, sheet, notes[k], startRow = nr + k - 1, startCol = 1)
    mergeCells(wb, sheet, cols = 1:nc, rows = nr + k - 1)
    addStyle(wb, sheet, st_note, rows = nr + k - 1, cols = 1:nc, gridExpand = TRUE)
    setRowHeights(wb, sheet, rows = nr + k - 1, heights = 14 * ceiling(nchar(notes[k]) / 120) + 2)
  }

  setColWidths(wb, sheet, cols = 1, widths = 62)
  setColWidths(wb, sheet, cols = 2:nc, widths = 17)
  freezePane(wb, sheet, firstActiveRow = r0, firstActiveCol = 2)
}

sd_note = if(setting_sd_denominator == "n") "SD was calculated with n in the denominator." else NULL

notes_table1 = c(
  paste("Age at the time of the survey and years of post-compulsory education are presented as mean ± SD,",
        "range, and 95% CI; all other characteristics are presented as n (%)."),
  paste("The target period is the period preceding the maltreatment incident for the case group and",
        "the corresponding child-rearing period for the control group."),
  "Type of crime convicted is applicable only to the case group (..).",
  "The control group includes parenting program participants; the two subgroups are shown in Supplementary Table S3.",
  sd_note)

notes_tableS3 = c(
  paste("Characteristics as in Table 1, shown separately for community-recruited participants and",
        "parenting program participants of the control group."),
  paste("Age at the time of the survey and years of post-compulsory education are presented as mean ± SD,",
        "range, and 95% CI; all other characteristics are presented as n (%)."),
  sd_note)

wb = createWorkbook()
write_table_sheet(wb, "Table 1", table1, c("Case", "Control"),
                  "Table 1. Characteristics of the participants and the target child", notes_table1)
write_table_sheet(wb, "Table S3", tableS3, c("Community", "Program"),
                  "Supplementary Table S3. Characteristics of the participants in the two subgroups of the control group",
                  notes_tableS3)
addWorksheet(wb, "summary")
writeData(wb, "summary", mutate(summary_all, FileCreated = format(Sys.time())))

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
file_out = file.path(dir_out, paste0("A2A3_demographic_table_", date_original_chr, ".xlsx"))
saveWorkbook(wb, file_out, overwrite = TRUE)
message("Saved: ", normalizePath(file_out))

writeLines(capture.output(sessionInfo()),
           file.path(dir_out, paste0("A2A3_demographic_table_", date_original_chr, "_sessioninfo.txt")))
