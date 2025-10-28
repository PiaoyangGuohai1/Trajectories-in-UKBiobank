rm(list = ls())
library(readr)
library(data.table)
library(stringr)
library(parallel)
source("./R/fun/00_fun_延长轨迹.R", echo=TRUE)

data_long <- read_csv("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
data_long <- data.table(data_long)
head(data_long)

result_df <- read_csv("result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv")
result_df <- data.table(result_df)
result_df[P_value < (0.05 / nrow(result_df)), .N]
result_df <- result_df[P_value < (0.05 / nrow(result_df)), .(D1, D2)]


ncore = 40

significant_pairs <- result_df
rm(result_df)

only_once =  TRUE
if(file.exists("result/data/03_trajectories_result_re/trajectories_1_result.rds")){
  trajectories_1_result <- readRDS("result/data/03_trajectories_result_re/trajectories_1_result.rds")
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
        eids    = I(list(eids_with_max))
      )
    )
    
    remaining_eids <- setdiff(remaining_eids, eids_with_max)
    
    disease_list <- disease_list[-max_idx]
  }
  trajectories_1_result <- data.table(
    trajectory_name = results$disease,
    D1 = results$disease,
    D1_number = results$count,
    D1_eid = results$eids
  )
  saveRDS(trajectories_1_result, file = "result/data/03_trajectories_result_re/trajectories_1_result.rds")
}


only_onece = TRUE
if(file.exists("result/data/03_trajectories_result_re/trajectories_2_result.rds")){
  trajectories_2_result <- readRDS("result/data/03_trajectories_result_re/trajectories_2_result.rds")
} else {
  trajectories_2 <- mclapply(1:nrow(trajectories_1_result), function(i){
    message("Processing: ", i)
    add_trajectory(data = data_long, 
                   trajectory = trajectories_1_result[i,], 
                   significant_pairs=significant_pairs,
                   only = only_onece)
    
  }, mc.cores = ncore)
  
  trajectories_2_result <- do.call(rbind, trajectories_2)
  sum(trajectories_2_result$D2_number > 20)
  saveRDS(trajectories_2_result, "result/data/03_trajectories_result_re/trajectories_2_result.rds")
  trajectories_2_table <- trajectories_2_result %>%
    dplyr::select(-matches("_eid$"))
  fwrite(trajectories_2_table, "result/data/03_trajectories_result_re/trajectories_2_result.csv")
}


if(file.exists("result/data/03_trajectories_result_re/trajectories_3_result.rds")){
  trajectories_3_result <- readRDS("result/data/03_trajectories_result_re/trajectories_3_result.rds")
} else {
  trajectories_3 <- mclapply(1:nrow(trajectories_2_result), function(i){
    message("Processing: ", i)
    add_trajectory(data = data_long, 
                   trajectory = trajectories_2_result[i,], 
                   significant_pairs=significant_pairs,
                   only = only_onece)
    
  }, mc.cores = ncore)
  
  trajectories_3_result <- do.call(rbind, trajectories_3)
  sum(trajectories_3_result$D3_number > 20)
  saveRDS(trajectories_3_result, "result/data/03_trajectories_result_re/trajectories_3_result.rds")
  trajectories_3_table <- trajectories_3_result %>%
    dplyr::select(-matches("_eid$"))
  fwrite(trajectories_3_table, "result/data/03_trajectories_result_re/trajectories_3_result.csv")
}


if(file.exists("result/data/03_trajectories_result_re/trajectories_4_result.rds")){
  trajectories_4_result <- readRDS("result/data/03_trajectories_result_re/trajectories_4_result.rds")
} else {
  trajectories_4 <- mclapply(1:nrow(trajectories_3_result), function(i){
    message("Processing: ", i)
    add_trajectory(data = data_long, 
                   trajectory = trajectories_3_result[i,], 
                   significant_pairs=significant_pairs,
                   only = only_onece)
    
  }, mc.cores = ncore)
  
  trajectories_4_result <- do.call(rbind, trajectories_4)
  sum(trajectories_4_result$D4_number > 20)
  saveRDS(trajectories_4_result, "result/data/03_trajectories_result_re/trajectories_4_result.rds")
  trajectories_4_table <- trajectories_4_result %>%
    dplyr::select(-matches("_eid$"))
  fwrite(trajectories_4_table, "result/data/03_trajectories_result_re/trajectories_4_result.csv")
}

if(file.exists("result/data/03_trajectories_result_re/trajectories_5_result.rds")){
  trajectories_5_result <- readRDS("result/data/03_trajectories_result_re/trajectories_5_result.rds")
} else {
  trajectories_5 <- mclapply(1:nrow(trajectories_4_result), function(i){
    message("Processing: ", i)
    result <-  add_trajectory(data = data_long, 
                              trajectory = trajectories_4_result[i,], 
                              significant_pairs=significant_pairs,
                              only = only_onece)
    return(result)
    
  }, mc.cores = ncore)
  
  trajectories_5_result <- do.call(rbind, trajectories_5)
  sum(trajectories_5_result$D5_number > 20)
  saveRDS(trajectories_5_result, "result/data/03_trajectories_result_re/trajectories_5_result.rds")
  trajectories_5_table <- trajectories_5_result %>%
    dplyr::select(-matches("_eid$"))
  fwrite(trajectories_5_table, "result/data/03_trajectories_result_re/trajectories_5_result.csv")
}




if(file.exists("result/data/03_trajectories_result_re/trajectories_6_result.rds")){
  trajectories_6_result <- readRDS("result/data/03_trajectories_result_re/trajectories_6_result.rds")
} else {
  trajectories_6 <- mclapply(1:nrow(trajectories_5_result), function(i){
    message("Processing: ", i)
    add_trajectory(data = data_long, 
                   trajectory = trajectories_5_result[i,], 
                   significant_pairs=significant_pairs,
                   only = only_onece)
    
  }, mc.cores = ncore)
  
  trajectories_6_result <- do.call(rbind, trajectories_6)
  sum(trajectories_6_result$D6_number > 20)
  saveRDS(trajectories_6_result, "result/data/03_trajectories_result_re/trajectories_6_result.rds")
  trajectories_6_table <- trajectories_6_result %>%
    dplyr::select(-matches("_eid$"))
  fwrite(trajectories_6_table, "result/data/03_trajectories_result_re/trajectories_6_result.csv")
}
