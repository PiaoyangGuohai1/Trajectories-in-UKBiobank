library(dplyr)
library(readr)
library(data.table)

data_base_info <- fread("fields/result/base_info.csv")
data_biochemical <- fread("fields/result/biochemical.csv")
data_blood_pressure <- fread("fields/result/blood_pressure.csv")
data_lifestyle <- fread("fields/result/lifestyle.csv")

data <- Reduce(function(x,y) merge(x, y, by = "participant.eid", all = TRUE), list(data_base_info, data_blood_pressure, data_lifestyle, data_biochemical))

fields_name <- readxl::read_excel("fields/总数据库文件/all_fields.xlsx")
fields_name <- fields_name %>% 
  select(ID, Title)

head(fields_name)

data <- data %>%
  rename_with(~ fields_name$Title[match(., fields_name$ID)], .cols = everything())

library(naniar)
missing <- data %>% miss_var_summary()

library(skimr)
check <- data %>% skim()

fwrite(check, "result/data/01_personal_info/UKB_base_info_check.csv")
fwrite(data, "result/data/01_personal_info/UKB_base_info.csv")


if(T){
  death_date <- fread("fields/result/death.csv")
  table(death_date$death.ins_index)
  death_date <- death_date[!which(duplicated(death_date$death.eid)), c("death.eid","death.date_of_death")]
  setnames(death_date, c("death.eid", "death.date_of_death"), c("eid","date_of_death"))
  head(death_date)
}


if(T){
  death_cause <- fread("fields/result/death_cause.csv")
  
  death_cause <- death_cause[
    death_cause.ins_index == 1 | 
      (death_cause.ins_index == 0 & !death_cause.eid %in% death_cause[death_cause.ins_index == 1, unique(death_cause.eid)]), 
  ]
  
  death_cause <- dcast(death_cause, 
                       death_cause.eid ~ death_cause.level, 
                       value.var = "death_cause.cause_icd10", 
                       fun.aggregate = function(x) paste(x, collapse = ",")
  )
  
  setnames(death_cause, c("death_cause.eid", "1", "2"), c("eid","cause_1", "cause_2"))
  death_cause$cause_2 <- strsplit(as.character(death_cause$cause_2), ",")
  convert_to_3digit <- function(codes) {substr(codes, 1, 3)}
  
  death_cause$cause_1 <- convert_to_3digit(death_cause$cause_1)
  death_cause$cause_2 <- lapply(death_cause$cause_2, convert_to_3digit)
  
  head(death_cause)
  rm(convert_to_3digit)
}


setdiff(death_date$eid, death_cause$eid)
setdiff(death_cause$eid, death_date$eid)

if(T){
  death <- left_join(death_date, death_cause)
  rm(death_date, death_cause)
  saveRDS(death, "result/data/01_personal_info/death_info.rds")
}


fields_name <- readxl::read_excel("fields/总数据库文件/all_fields.xlsx")
fields_name <- fields_name %>% 
  select(ID, Title)

library(stringr)
mapped_data <- fields_name %>%
  filter(str_detect(Title, "^Date")) %>%
  mutate(
    abbreviation = str_extract(Title, "(?<=Date )[A-Za-z0-9]+"),
    disease_name = str_extract(Title, "(?<=first reported \\().*(?=\\))")
  ) %>%
  select(abbreviation, disease_name)

mapped_data <- mapped_data %>%
  mutate(group = substr(abbreviation, 1, 1))

fwrite(mapped_data, "result/data/01_personal_info/ICD10_abbreviation_name.csv")
