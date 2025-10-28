set_identify <- function(key_diag, trajectory_name){
  
  trajectories_with_key <- trajectories[grepl(key_diag, trajectory_name)]
  
  preceding_set <- character()
  succeeding_set <- character()
  
  for (trajectory in trajectories_with_key$trajectory_name) {
    diseases <- unlist(strsplit(trajectory, "_"))
    key_indices <- which(diseases == key_diag)
    
    for (key_index in key_indices) {
      if (key_index > 1) {
        preceding_diseases <- diseases[1:(key_index - 1)]
        preceding_set <- unique(c(preceding_set, preceding_diseases))
      }
      if (key_index < length(diseases)) {
        succeeding_diseases <- diseases[(key_index + 1):length(diseases)]
        succeeding_set <- unique(c(succeeding_set, succeeding_diseases))
      }
    }
  }
  
  return(list(preceding_set, succeeding_set))
}


match_controls <- function(iteration, cases, control_candidates) {
  set.seed(123 + iteration)
  
  controls_pool = control_candidates
  matched_pairs <- data.table()
  
  for (i in 1:nrow(cases)) {
    case <- cases[i]
    potential_controls <- controls_pool[Sex == case$Sex & age_at_d1 == case$age_at_d1]
    
    if (nrow(potential_controls) == 0) next
    
    selected_control <- potential_controls[sample(.N, 1)]
    
    matched_pairs <- rbind(matched_pairs, data.table(
      case_eid = case$eid,
      control_eid = selected_control$eid,
      interval_days = case$interval_days
    ))
    
    controls_pool <- controls_pool[eid != selected_control$eid]
  }
  
  if (nrow(matched_pairs) == 0) return(NA)
  
  matched_controls <- merge(control_candidates, matched_pairs[,.(control_eid, case_eid, interval_days)], by.x = "eid", by.y = "control_eid", all.y = T)

  return(matched_controls)
}


D2_status <- function(data_long, data, key_diagnoses) {
  data_d2 <- data_long[disease %in% key_diagnoses, ]
  data <- merge(data, data_d2[, .(eid, d2_date = diagnosis_date)], by = "eid", all.x = T)
  data <- data[, d2_status := d2_date >= first_d1_date & d2_date <= first_d3_date]
  data[is.na(d2_status), d2_status := FALSE]
  return(data)
}


cal_RR <- function(cases, controls){
  
  cases_rate = sum(cases$d2_status)/nrow(cases)
  
  controls_rate = sum(controls$d2_status)/nrow(controls)
  
  RR <- cases_rate / controls_rate
  
  return(RR)
}


one_iteration <- function(iter, data = data_long_choose, sample_size = 1000) {
  message("Iteration: ",iter,"...")
  sampled_cases <- cases[sample(nrow(cases), sample_size), ]
  
  matched_controls <- match_controls(iter, sampled_cases, control_candidates)
  setDT(matched_controls)
  matched_controls[, first_d3_date := first_d1_date + as.integer(interval_days)]
  
  matched_controls <- D2_status(data, matched_controls, D2)
  
  RR <- cal_RR(sampled_cases, matched_controls)
  
  return(RR)
}
