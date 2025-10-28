library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)

file_list <- list.files("fields/result/", pattern = "^C1712", full.names = TRUE)

file_list <- file_list[!grepl("source\\.csv$", file_list)]


data_list <- lapply(file_list, function(x) {
  fread(x)
})


data <- Reduce(function(x, y) merge(x, y, by = "participant.eid", all = TRUE), data_list)
rm(file_list, data_list)

fields_name <- readxl::read_excel("fields/总数据库文件/all_fields.xlsx")
fields_name <- fields_name %>% 
  select(ID, Title)
  
head(fields_name)

data <- data %>%
  rename_with(~ fields_name$Title[match(., fields_name$ID)], .cols = everything()) %>%
  rename(eid = `Participant ID`)

colnames(data) <- colnames(data) %>%
  map_chr(~ ifelse(str_starts(., "Date"), 
                   str_split(., " ")[[1]][2],
                   .))

data_base_info <- fread("result/data/01_personal_info/UKB_base_info.csv")

data_base_info <- data_base_info %>%
  rename(eid = `Participant ID`,
         Birth_year = `Year of birth`,
         Age_i0 = `Age when attended assessment centre | Instance 0`,
         BMI = `Body mass index (BMI) | Instance 0`) %>%
  select(eid, Birth_year, Age_i0,Sex, BMI)


data <- left_join(data, data_base_info)
rm(data_base_info)

data_long <- melt(data,
                  id.vars = c("eid", "Birth_year", "Age_i0", "Sex", "BMI"),
                  variable.name = "disease",
                  value.name = "diagnosis_date",
                  na.rm = TRUE)



data_long <- data_long[, .SD[order(diagnosis_date)], by = eid]
head(data_long)

fwrite(data_long, file = "result/data/01_personal_info/ICD10_first_reported_date_long_for_all_diseases_515w_row.csv")


file_list <- list.files("fields/result/", pattern = "source.csv$", full.names = TRUE)
data_list <- lapply(file_list, function(x) {
  fread(x)
})
data <- Reduce(function(x, y) merge(x, y, by = "participant.eid", all = TRUE), data_list)
rm(file_list, data_list)

fields_name <- readxl::read_excel("fields/总数据库文件/all_fields.xlsx")
fields_name <- fields_name %>% 
  select(ID, Title)

head(fields_name)

data <- data %>%
  rename_with(~ fields_name$Title[match(., fields_name$ID)], .cols = everything()) %>%
  rename(eid = `Participant ID`)
data <- tibble(data)
colnames(data) <- colnames(data) %>%
  map_chr(~ ifelse(str_starts(., "Source of report of "), 
                   str_split(., " ")[[1]][5],
                   .))


data_long <- data %>%
  pivot_longer(
    cols = -eid,
    names_to = "disease",
    values_to = "diagnosis_source",
    values_drop_na = TRUE
  )

head(data_long)
fwrite(data_long, file = "result/data/01_personal_info/ICD10_first_reported_source_long_for_all_diseases_162w_row.csv")
