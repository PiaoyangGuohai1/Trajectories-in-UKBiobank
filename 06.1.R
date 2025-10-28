rm(list = ls())
library(readr)
library(data.table)
library(stringr)
library(parallel)
library(dplyr)
source("./R/fun/00_fun_延长轨迹.R", echo=TRUE)

data_long <- read_csv("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
data_long <- data.table(data_long)
head(data_long)

direction <- read_csv("result/data/02_RR/UKB_cal_relativte_risk_with_direction.csv")
direction <- data.table(direction)
head(direction)

nrow(direction)

significant_pairs <- direction[direction_significant == 1]
rm(direction)

if(file.exists("result/data/03_trajectories_result/eid_rep/trajectories_1_result.RData")){
  load("result/data/03_trajectories_result/eid_rep/trajectories_1_result.RData")
} else {
  head(data_long)
  
  trajectories_1 <- data.frame(trajectory_name = unique(significant_pairs$D1),
                               D1 = unique(significant_pairs$D1))
  
  disease_eid <- data_long %>%
    select(disease, eid) %>%
    distinct()
  
  disease_summary <- disease_eid %>%
    group_by(disease) %>%
    summarise(D1_number = n(),
              D1_eid = paste(eid, collapse = ","))
  
  trajectories_1 <- trajectories_1 %>%
    left_join(disease_summary, by = c("D1" = "disease"))
  
  save(trajectories_1, file= "result/data/03_trajectories_result/eid_rep/trajectories_1_result.Rds", compress = FALSE)
}



max_level <- 7
all_traj <- run_all_trajectories(
  max_level = max_level,
  data_long = data_long,
  trajectories_1_result = trajectories_1,
  significant_pairs = significant_pairs,
  only_once = FALSE,
  ncore = 40,
  result_dir = "result/data/03_trajectories_result/eid_rep"
)
