library(dplyr)
library(tidyr)
library(purrr)

expand_trajectories <- function(trajectories) {
  expanded_data <- list()
  
  for (i in 1:nrow(trajectories)) {
    trajectory <- trajectories$trajectory_name[i]
    diseases <- strsplit(trajectory, "_")[[1]]
    n_levels <- length(diseases)
    
    if (n_levels > 6) {
      stop("This function only supports trajectories up to 6 levels.")
    }
    
    for (j in 1:n_levels) {
      sub_trajectory <- paste(diseases[1:j], collapse = "_")
      current_disease <- diseases[j]
      
      if (j == 1) {
        eid <- trajectories$D1_eid[i]
        number <- trajectories$D1_number[i]
        mean_age <- trajectories$D1_mean_age[i]
      } else if (j == 2) {
        eid <- trajectories$D2_eid[i]
        number <- trajectories$D2_number[i]
        mean_age <- trajectories$D2_mean_age[i]
      } else if (j == 3) {
        eid <- trajectories$D3_eid[i]
        number <- trajectories$D3_number[i]
        mean_age <- trajectories$D3_mean_age[i]
      } else if (j == 4) {
        eid <- trajectories$D4_eid[i]
        number <- trajectories$D4_number[i]
        mean_age <- trajectories$D4_mean_age[i]
      } else if (j == 5) {
        eid <- trajectories$D5_eid[i]
        number <- trajectories$D5_number[i]
        mean_age <- trajectories$D5_mean_age[i]
      } else if (j == 6) {
        eid <- trajectories$D6_eid[i]
        number <- trajectories$D6_number[i]
        mean_age <- trajectories$D6_mean_age[i]
      }
      
      expanded_data[[length(expanded_data) + 1]] <- data.frame(
        sub_trajectory = sub_trajectory,
        disease = current_disease,
        eid = eid,
        number = number,
        mean_age = mean_age,
        stringsAsFactors = FALSE
      )
    }
  }
  
  expanded_df <- bind_rows(expanded_data)
  
  expanded_df <- expanded_df %>% distinct(sub_trajectory, .keep_all = TRUE)
  
  return(expanded_df)
}
