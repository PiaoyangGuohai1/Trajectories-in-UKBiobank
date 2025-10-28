cal_direction <- function(data, D1, D2){
  message("Processing: ", D1 ," and ", D2)
  data_choose <- data[disease %in% c(D1, D2),]
  
  first_diagnosis <- data_choose[, .(
    D1_date = diagnosis_date[disease == D1],
    D2_date = diagnosis_date[disease == D2]
  ), by = eid]
  
  first_diagnosis[, direction := fifelse(
    D1_date < D2_date, "D1_before_D2",
    fifelse(D2_date < D1_date, "D2_before_D1",
            fifelse(!is.na(D1_date) & D1_date == D2_date, "Same_time", NA_character_)
    )
  )]
  
  ND1 <- nrow(first_diagnosis[direction == "D1_before_D2"])
  ND2 <- nrow(first_diagnosis[direction == "D2_before_D1"])
  Nsame <- nrow(first_diagnosis[direction == "Same_time"])
  
  binom_result <- binom.test(ND1, ND1 + ND2 + Nsame, p = 0.5)
  
  result <- data.table(D1 = D1, D2 = D2, 
                       ND1 = ND1, 
                       ND2 = ND2,
                       Nsame = Nsame, 
                       p_direction = binom_result$p.value,
                       estimate = binom_result$estimate)
  return(result)
}
