rm(list = ls())
library(readr)
library(data.table)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
source("R/fun/00_cal_weight.R", echo=TRUE)

trajectories <- readRDS("result/data/04_trajectories_cluster/trajectories_choose.rds")

trajectories_split <- trajectories %>%
  mutate(trajectory_parts = strsplit(as.character(trajectory_name), "_")) %>%
  mutate(trajectory_pairs = map(trajectory_parts, ~ purrr::map2(.x[-length(.x)], .x[-1], paste, sep = "_"))) %>%
  unnest(cols = c(trajectory_pairs)) %>%
  mutate(
    D1 = sapply(trajectory_pairs, function(x) strsplit(x, "_")[[1]][1]),
    D2 = sapply(trajectory_pairs, function(x) strsplit(x, "_")[[1]][2])
  ) %>%
  select(D1, D2) %>%
  distinct()

data_long <- fread("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
data_long <- setDT(data_long)
head(data_long)

death <- fread("fields/result/death.csv")
death <- death %>%
  select(death.eid, death.date_of_death) %>%
  mutate(disease = "death") %>%
  rename(eid = death.eid, diagnosis_date = death.date_of_death)
head(death)

death[,diagnosis_date:=diagnosis_date+1]

data_long <- rbind(data_long, death, fill=TRUE)
rm(death)

data_long[, diagnosis_date := as.Date(diagnosis_date)]
setorder(data_long, eid, diagnosis_date)

data_long <- data_long[disease %in% c(unique(c(trajectories_split$D1, trajectories_split$D2)), "death"),
                       .(eid, Birth_year, Sex, disease, diagnosis_date)]
head(data_long)

setorder(data_long, eid, diagnosis_date, disease)

data_long[, time_order := rleid(diagnosis_date), by = eid]

data_long[, next_diagnosis_date := as.Date(sapply(1:.N, function(i) {
  current_order <- time_order[i]
  next_date <- diagnosis_date[time_order == current_order + 1]
  next_date[1]
}), origin = "1970-01-01"), by = eid]

data_long[, next_diagnosis := sapply(1:.N, function(i) {
  current_order <- time_order[i]
  next_diseases <- disease[time_order == current_order + 1]
  if (length(next_diseases) > 0) toString(next_diseases) else NA_character_
}), by = eid]


data_long[, last_diagnosis_date := as.Date("2021-09-30")]
data_long[grepl("death", next_diagnosis), last_diagnosis_date := as.Date(next_diagnosis_date)]

data_long[, follow_up_days := as.numeric(last_diagnosis_date - diagnosis_date, units = "days")]

data_long[, time_interval_days := as.numeric(next_diagnosis_date - diagnosis_date, units = "days")]

if(T){
  death_rows <- tibble(
    D1 = unique(c(trajectories_split$D1, trajectories_split$D2)),
    D2 = "death"
  )
  
  valid_transitions <- bind_rows(trajectories_split, death_rows)
}

transition_matrix <- calculate_transition_matrix(data_long, valid_transitions, years = 5)

hist(transition_matrix$avg_time_interval) 
if(F){
  mean_weight <- mean(transition_matrix$weight, na.rm = TRUE)
  std_weight <- sd(transition_matrix$weight, na.rm = TRUE)
  
  transition_matrix[, standardized_weight := (weight - mean_weight) / std_weight]
}

head(transition_matrix)
if(!dir.exists("result/data/05_trans_prob/")){
  dir.create("result/data/05_trans_prob/")
}
fwrite(transition_matrix, file = "result/data/05_trans_prob/transition_probs.csv")
