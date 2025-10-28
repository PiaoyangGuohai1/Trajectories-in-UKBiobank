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

significant_pairs <- direction[direction_significant == 1]
rm(direction)

if(file.exists("result/data/03_trajectories_result/eid_only/trajectories_1_result.rds")){
  trajectories_1 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_1_result.rds")
} else {
  trajectories_1 <- data.frame(trajectory_name = unique(significant_pairs$D1),
                               D1 = unique(significant_pairs$D1))
  
  disease_list <- unique(significant_pairs$D1)
  
  remaining_eids <- unique(data_long$eid)
  
  results <- data.frame(
    disease = character(),
    count   = integer(),
    eids    = I(list())
  )
  
  while (length(disease_list) > 0 && length(remaining_eids) > 0) {
    message("Remaining eids: ",length(remaining_eids))
    counts <- sapply(disease_list, function(d) {
      sum(data_long$disease == d & data_long$eid %in% remaining_eids)
    })
    
    max_idx <- which.max(counts)
    max_disease <- disease_list[max_idx]
    max_count   <- counts[max_idx]
    
    if (max_count == 0) {
      break
    }
    
    eids_with_max <- unique(data_long$eid[data_long$disease == max_disease &
                                            data_long$eid %in% remaining_eids])
    
    results <- rbind(
      results,
      data.frame(
        disease = max_disease,
        count   = max_count,
        eids    = paste(eids_with_max, collapse = ",")
      )
    )
    
    remaining_eids <- setdiff(remaining_eids, eids_with_max)
    
    disease_list <- disease_list[-max_idx]
  }
  trajectories_1 <- data.table(
    trajectory_name = results$disease,
    D1 = results$disease,
    D1_number = results$count,
    D1_eid = results$eids
  )
  saveRDS(trajectories_1, file = "result/data/03_trajectories_result/eid_only/trajectories_1_result.rds")
}


library(dplyr)

max_level <- 7
all_traj <- run_all_trajectories(
  max_level = max_level,
  data_long = data_long,
  trajectories_1_result = trajectories_1,
  significant_pairs = significant_pairs,
  only_once = TRUE,
  ncore = 40,
  result_dir = "result/data/03_trajectories_result/eid_only"
)
