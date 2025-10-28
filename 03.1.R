rm(list = ls())
library(dplyr)
library(tidyverse)
library(lubridate)
library(parallel)
library(data.table)
library(ggplot2)

data_long <- fread("result/data/01_personal_info/ICD10_first_reported_date_long_for_all_diseases_515w_row.csv")
head(data_long)

data_long <- data_long[grep("^[EIN]", disease)]

n_disease_stats <- data_long[,.N, by = disease]

data_source <- fread("result/data/01_personal_info/ICD10_first_reported_source_long_for_all_diseases_162w_row.csv")
head(data_source)

data <- left_join(data_long, data_source, by = join_by(eid, disease))
rm(data_long, data_source)
table(data$diagnosis_source)

length(unique(data$eid))


problematic_dates <- c("1900-01-01", "1901-01-01", "1902-02-02", "1903-03-03", "1909-09-09", "2037-07-07")
problematic_eids <- data[diagnosis_date %in% problematic_dates, ]

data <- data[!(diagnosis_date %in% problematic_dates), ]
rm(problematic_dates,problematic_eids)

data <- data %>%
  mutate(diagnosis_source1 = recode_factor(diagnosis_source,
                                          "20" = "Death register only",
                                          "21" = "Death register and other source(s)",
                                          "30" = "Primary care only",
                                          "31" = "Primary care and other source(s)",
                                          "40" = "Hospital admissions data only",
                                          "41" = "Hospital admissions data and other source(s)",
                                          "50" = "Self-report only",
                                          "51" = "Self-report and other source(s)"
  )) %>%
  mutate(diagnosis_source2 = recode_factor(diagnosis_source,
                                          "20" = "Death register",
                                          "21" = "Death register",
                                          "30" = "Primary care",
                                          "31" = "Primary care",
                                          "40" = "Hospital admissions data",
                                          "41" = "Hospital admissions data",
                                          "50" = "Self-report",
                                          "51" = "Self-report"
  )) %>%
  mutate(Single_source = recode_factor(diagnosis_source,
                                          "20" = "Yes",
                                          "21" = "No",
                                          "30" = "Yes",
                                          "31" = "No",
                                          "40" = "Yes",
                                          "41" = "No",
                                          "50" = "Yes",
                                          "51" = "No"))

table(data$diagnosis_source1, useNA = "always")
table(data$diagnosis_source2, useNA = "always")
table(data$Single_source, useNA = "always")

data$diagnosis_date <- as.Date(data$diagnosis_date)

data[, age_at_diagnosis := year(diagnosis_date) - Birth_year]
table(data$age_at_diagnosis)

data <- data %>%
  mutate(disease_category = substr(disease, 1, 1)) %>%
  mutate(disease_category = case_when(
    disease_category == "E" ~ "E: Metabolic",
    disease_category == "I" ~ "I: Circulatory",
    disease_category == "N" ~ "N: Genitourinary",
    TRUE ~ "Other"
  ))

table(data$disease_category)

data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Female", "Male"))

head(data)


data_filtered <- data[diagnosis_source1 != "Self-report only",]
nrow(data) - nrow(data_filtered)


check_18 <- data_filtered[age_at_diagnosis < 18,]
table(check_18$diagnosis_source1)

data_filtered <- data_filtered[age_at_diagnosis >= 18,]



disease_counts <- data_filtered %>%
  group_by(disease, disease_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  filter(count >= 20)

fwrite(disease_counts, "result/data/01_personal_info/ICD10_check.csv")


ICD_unkown <- fread("result/data/01_personal_info/ICD10_check_finished.csv")

nrow_before <- nrow(data_filtered)
data_filtered <- data_filtered %>%
  filter(disease %in% ICD_keep$disease)
nrow_after <- nrow(data_filtered)
nrow_before-nrow_after

library(lubridate)

processed_data <- data_filtered[
  order(eid, diagnosis_date),
  {
    dia_types <- disease
    has_E10_E11 <- any(dia_types %chin% c("E10", "E11"))
    has_E10_E13 <- any(dia_types %chin% c("E10", "E11", "E12", "E13"))
    
    first_main <- which(dia_types %chin% c("E10", "E11"))[1]
    
    keep <- 
      (dia_types %chin% c("E10", "E11") & seq_along(dia_types) == first_main) | 
      (dia_types %chin% c("E12", "E13") & !has_E10_E11) | 
      (dia_types == "E14" & !has_E10_E13) | 
      (!dia_types %chin% c("E10", "E11", "E12", "E13", "E14"))
    
    .SD[keep]
  }, 
  by = eid]

all(colnames(data_filtered) %in% colnames(processed_data))

gender <- processed_data[,c("eid", "Sex")]
gender <- gender[!duplicated(gender$eid),]
table(gender$Sex)

length(unique(processed_data$eid))
nrow(processed_data)

table(processed_data$diagnosis_source1, useNA = "always")
table(processed_data$diagnosis_source2, useNA = "always")
table(data$Single_source, useNA = "always")

fwrite(processed_data, "result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
