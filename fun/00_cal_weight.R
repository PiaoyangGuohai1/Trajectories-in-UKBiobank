calculate_transition_matrix <- function(data_long, valid_transitions, years = 5) {
  years_days <- years * 365.25
  
  transition_matrix <- data.table(D1 = character(0), D2 = character(0), count = integer(0),
                                  total = integer(0), probability = numeric(0), 
                                  avg_time_interval = numeric(0), weight = numeric(0))
  for (i in 1:nrow(valid_transitions)) {
    D1 <- valid_transitions$D1[i]
    D2 <- valid_transitions$D2[i]
    
    if (D2 == "death"){
      patients_D1 <- data_long[disease == D1]
      total <- uniqueN(patients_D1$eid)
    } else {
      patients_D1 <- data_long[disease == D1 & (follow_up_days >= years_days)]
      total <- uniqueN(patients_D1$eid)
    }
    
    count <- patients_D1[
      disease == D1 & grepl(D2, next_diagnosis) & time_interval_days <= years_days, 
      uniqueN(eid)
    ]
    
    probability <- count / total
    
    
    if(T){
      time_intervals <- data_long[
        disease == D1 & next_diagnosis == D2 & time_interval_days <= years_days, 
        as.numeric(time_interval_days) / 365.25
      ]
      
      avg_time_interval <- mean(time_intervals, na.rm = TRUE)
      
      if (is.na(avg_time_interval) || avg_time_interval == 0) {
        weight <- 0
      } else {
        weight <- probability / avg_time_interval
      }
    }
    
    transition_matrix <- rbind(transition_matrix, data.table(
      D1 = D1, D2 = D2, 
      count = count,
      total = total,
      probability = probability,
      avg_time_interval = avg_time_interval,
      weight = weight
    ))
  }
  
  return(transition_matrix)
}
