get_last_code_regex <- function(traj_str, sep = "_") {
  ifelse(is.na(traj_str), NA, 
         sub(paste0(".*", sep, "([[:alnum:]]+)$"), "\\1", traj_str))
}

calculate_death <- function(traj, data_death, data_long, trajectories_eid, data_person) {
  last_diesease <- get_last_code_regex(traj)
  
  eid_list <- as.integer(unlist(strsplit(trajectories_eid[trajectory_name == traj, eid], ",")))
  
  death_traj <- data_death[eid %in% eid_list]
  
  last_dates <- data_long[disease == last_diesease & eid %in% eid_list, .(eid, last_disease_date = diagnosis_date)]
  
  traj_data <- merge(last_dates, death_traj, by = "eid", all.x = TRUE)
  
  traj_data <- merge(traj_data, data_person[, .(eid, birth_date, gender)], by = "eid")
  
  cutoff_date <- as.IDate("2021-11-30")
  
  traj_data[, survival_time := pmin(ifelse(is.na(date_of_death), 
                                          as.numeric(as.Date(cutoff_date) - as.Date(last_disease_date)), 
                                          as.numeric(as.Date(date_of_death) - as.Date(last_disease_date))), 
                                   15 * 365.25)]
  
  traj_data[, survival_time_years := survival_time / 365.25]
  
  traj_data[, death_event := ifelse(!is.na(date_of_death) & survival_time_years <= 15, 1, 0)]
  
  traj_data[, age_at_last_disease := as.numeric(as.Date(last_disease_date) - as.Date(birth_date)) / 365.25]
  
  death_count <- sum(traj_data$death_event)
  
  death_eids <- list(traj_data[death_event == 1, eid])
  
  total_person_years <- sum(traj_data$survival_time_years)
  
  death_rate <- (death_count / total_person_years) * 1000
  
  traj_data[, death_within_5_years := ifelse(death_event == 1 & survival_time_years <= 5, 1, 0)]
  death_5_years_count <- sum(traj_data$death_within_5_years)
  death_5_years_prob <- death_5_years_count / nrow(traj_data)
  
  traj_data[, traj := traj]
  
  return(list(
    trajectory_name = traj,
    death_count = death_count,
    total_survival_person_years = total_person_years,
    death_rate_per_1000_person_years = death_rate,
    death_5_years_count = death_5_years_count,
    death_5_years_prob = death_5_years_prob,
    traj_data = traj_data
  ))
}

survif <- function(results, traj){
  
  traj_data <- results[[traj]]$traj_data
  
  fit <- survfit(Surv(survival_time_years, death_event) ~ 1, data = traj_data)
  
  km_plot <- ggsurvplot(fit, data = traj_data,
                        title = paste("Survival Curve for Trajectory:", traj_data$traj[1]),
                        xlab = "Years since last disease diagnosis",
                        ylab = "Survival Probability",
                        risk.table = TRUE,
                        conf.int = TRUE)
  print(km_plot)
  return(fit)
}

survif_compare <- function(results, traj1, traj2){
  traj_data1 <- results[[traj1]]$traj_data
  traj_data2 <- results[[traj2]]$traj_data
  
  
  traj_data1[, group := traj1]
  traj_data2[, group := traj2]
  
  combined_data <- rbind(traj_data1, traj_data2)
  
  fit_combined <- survfit(Surv(survival_time_years, death_event) ~ group, data = combined_data)
  
  logrank_test <- survdiff(Surv(survival_time_years, death_event) ~ group, data = combined_data)
  
  print(logrank_test)
  
  comparison_plot <- ggsurvplot(fit_combined, data = combined_data,
                                title = "Comparison of Survival Curves",
                                xlab = "Years since last disease diagnosis",
                                ylab = "Survival Probability",
                                risk.table = TRUE,
                                conf.int = TRUE,
                                pval = TRUE)
  
  comparison_plot
}

calculate_disease_risk <- function(traj, data_long, trajectories_eid, choose_disease) {
  eid_list <- as.integer(unlist(strsplit(trajectories_eid[trajectory_name == traj, eid], ",")))
  
  data_long_traj <- data_long[eid %in% eid_list]
  
  last_disease = get_last_code_regex(traj)
  dates <- data_long_traj[disease == last_disease, 
                          .(last_disease_date = min(diagnosis_date)), by = eid]
  
  disease_dates <- data_long_traj[disease == choose_disease, 
                                  .(choose_disease_date = min(diagnosis_date)), by = eid]
  
  dates <- merge(dates, disease_dates, by = "eid", all.x = TRUE)
  
  
  dates[, end_date := max(data_long$diagnosis_date)]
  
  dates[, observation_end := pmin(choose_disease_date, end_date, na.rm = TRUE)]
  
  dates[, person_years := as.numeric(difftime(observation_end, last_disease_date, units = "days")) / 365.25]
  
  dates[, choose_disease_occurred := !is.na(choose_disease_date)]
  
  choose_disease_count <- sum(dates$choose_disease_occurred)
  
  choose_disease_eids <- paste(dates[choose_disease_occurred == TRUE, eid], collapse = ",")
  
  total_person_years <- sum(dates$person_years)
  
  incidence_rate <- choose_disease_count / total_person_years
  
  return(list(trajectory_name = traj,
              disease_count = choose_disease_count, 
              disease_eids = choose_disease_eids, 
              total_person_years = total_person_years,
              incidence_rate = incidence_rate))
}


clean_elements <- function(lst) {
  lapply(lst, function(x) {
    x$traj_data <- NULL
    x
  })
}
