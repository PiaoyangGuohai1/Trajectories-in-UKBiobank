check_number <- function(data, d_next, last_disease = NULL){
  if(is.null(last_disease)){
    setDT(data)
    res_D <- data[disease == d_next, .(eid, Birth_year, diagnosis_date)]
    avg_age_at_diagnosis <- res_D[, mean(year(diagnosis_date) - Birth_year, na.rm = TRUE)]
    
    res_D <- list(disease = d_next,
                  eid = res_D[, eid],
                  number = res_D[,.N],
                  mean_age = avg_age_at_diagnosis)
    
    
  } else {
    filtered_data_d <- data[disease %in% c(d_next, last_disease)]
    
    valid_eids <- filtered_data_d[
      , .(has_D_after = any(disease == d_next & diagnosis_date >= diagnosis_date[disease == last_disease])),
      by = eid
    ][
      has_D_after == TRUE, eid
    ]
    
    avg_age_at_diagnosis <- filtered_data_d[
      eid %in% valid_eids & disease == d_next,
      mean(year(diagnosis_date) - Birth_year, na.rm = TRUE)
    ]
    
    res_D <- list(disease = d_next,
                  eid = valid_eids,
                  number = length(valid_eids),
                  mean_age = avg_age_at_diagnosis)
  }
  
  return(res_D)
}


add_trajectory <- function(data, trajectory, significant_pairs, only = FALSE){
  
  trajectory_name = trajectory$trajectory_name
  trajectory_diseases <- unlist(strsplit(trajectory$trajectory_name, "_"))
  trajectory_n <- length(trajectory_diseases)
  message("Currently searching level ",trajectory_n," trajectory: ", trajectory_name, "...\n" )
  
  last_disease <- trajectory_diseases[length(trajectory_diseases)]
  
  current_eids <- trajectory[[paste0("D", trajectory_n, "_eid")]][[1]]
  if (length(current_eids) == 1){
    current_eids <- str_split(trajectory[[paste0("D", trajectory_n, "_eid")]], ",")[[1]]
  }
  
  new_trajectories <- data.frame()
  
  
  if(only == FALSE){
    possible_next <- significant_pairs[D1 == last_disease, D2]
    
    if (length(possible_next) > 0) {
      
      data_filtered <- data_long[eid %in% current_eids,]
      data_filtered <- data_filtered[order(eid, diagnosis_date)]
      
      for (d_next in possible_next) {
        t_next <- paste(c(trajectory_diseases, d_next), collapse = "_")
        message("  Searching new trajectory: ", t_next)
        d_next_eid <- check_number(data = data_filtered, 
                                   d_next = d_next,
                                   last_disease = last_disease)
        
        message("  Found: ", d_next_eid$number, " individuals\n")
        
        if(length(d_next_eid$eid) > 0){
          trajectory_new <- trajectory
          trajectory_new$trajectory_name = t_next
          trajectory_new[[paste0("D", trajectory_n+1)]] <- d_next_eid$disease
          trajectory_new[[paste0("D", trajectory_n+1, "_eid")]] <- paste(d_next_eid$eid, collapse = ",")
          trajectory_new[[paste0("D", trajectory_n+1, "_number")]] <- d_next_eid$number
          trajectory_new[[paste0("D", trajectory_n+1, "_mean_age")]] <- d_next_eid$mean_age
          
          new_trajectories <- rbind(new_trajectories, trajectory_new)
        }
      }
    }
  } else {
    while (TRUE) {
      
      data_filtered <- data_long[eid %in% current_eids,]
      data_filtered <- data_filtered[order(eid, diagnosis_date)]
      
      if (nrow(data_filtered) == 0){
        break
      }
      
      pair_counts <- data_filtered[
        order(eid, diagnosis_date),
        .(
          D1 = unlist(lapply(1:(.N-1), function(i) rep(disease[i], .N - i))),
          D2 = unlist(lapply(1:(.N-1), function(i) disease[(i+1):.N]))
        ),
        by = eid
      ][, .N, by = .(D1, D2)][
        D1 == last_disease,
      ] 
      
      
      pair_counts_filtered <- merge(
        pair_counts,
        significant_pairs,
        by = c("D1", "D2")
      )
      pair_counts_filtered <- pair_counts_filtered[order(-N)]
      
      if(nrow(pair_counts_filtered) > 0){
        d_next <- pair_counts_filtered$D2[1]
      } else {
        break
      }
      
      t_next <- paste(c(trajectory_diseases, d_next), collapse = "_")
      message("  Searching new trajectory: ", t_next)
      d_next_eid <- check_number(data = data_filtered, 
                                 d_next = d_next,
                                 last_disease = last_disease)
      
      message("  Found: ", d_next_eid$number, " individuals\n")
      
      if(length(d_next_eid$eid) > 0){
        trajectory_new <- trajectory
        trajectory_new$trajectory_name = t_next
        trajectory_new[[paste0("D", trajectory_n+1)]] <- d_next_eid$disease
        trajectory_new[[paste0("D", trajectory_n+1, "_eid")]] <- paste(d_next_eid$eid, collapse = ",")
        trajectory_new[[paste0("D", trajectory_n+1, "_number")]] <- d_next_eid$number
        trajectory_new[[paste0("D", trajectory_n+1, "_mean_age")]] <- d_next_eid$mean_age
        
        new_trajectories <- rbind(new_trajectories, trajectory_new)
        current_eids <- setdiff(current_eids, d_next_eid$eid)
      }
    }
  }
  
  if (length(new_trajectories) > 0) {
    return(new_trajectories)
  }
  
  return(NULL)
}

library(data.table)
library(dplyr)
library(parallel)

process_trajectory_level <- function(level,
                                     prev_result,
                                     data,
                                     significant_pairs,
                                     only_once,
                                     ncore,
                                     result_dir = "result/data/03_trajectories_result/eid_rep") {
  rds_file <- file.path(result_dir, paste0("trajectories_", level, "_result.rds"))
  csv_file <- file.path(result_dir, paste0("trajectories_", level, "_result.csv"))
  
  if (file.exists(rds_file)) {
    message(sprintf("Loading existing trajectories_%d_result", level))
    return(readRDS(rds_file))
  }
  
  trajectories <- mclapply(
    seq_len(nrow(prev_result)),
    function(i) {
      message(sprintf("Processing level %d, trajectory %d/%d", level, i, nrow(prev_result)))
      add_trajectory(
        data = data,
        trajectory = prev_result[i, ],
        significant_pairs = significant_pairs,
        only = only_once
      )
    },
    mc.cores = ncore
  )
  
  trajectories_df <- do.call(rbind, trajectories)
  cnt_over20 <- sum(trajectories_df[[paste0("D", level, "_number")]] > 20)
  message(sprintf("Level %d: %d trajectories with D%d_number > 20", level, cnt_over20, level))
  
  saveRDS(trajectories_df, rds_file)
  trajectories_df %>%
    select(-matches("_eid$")) %>%
    fwrite(csv_file)
  
  return(trajectories_df)
}

run_all_trajectories <- function(max_level = 6,
                                 data_long,
                                 trajectories_1,
                                 significant_pairs,
                                 only_once,
                                 ncore = 1,
                                 result_dir) {
  results <- vector("list", max_level)
  results[[1]] <- trajectories_1
  for (lvl in 2:max_level) {
    results[[lvl]] <- process_trajectory_level(
      level = lvl,
      prev_result = results[[lvl - 1]],
      data = data_long,
      significant_pairs = significant_pairs,
      only_once = only_once,
      ncore = ncore,
      result_dir = result_dir
    )
  }
  return(results)
}
