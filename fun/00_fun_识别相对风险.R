find_exposed <- function(data, D1, D2,
                         follow_up_limit = as.Date("2017-04-01")){
  
  if (D1 %in% colnames(data) & D2 %in% colnames(data) ){
    print("Both D1 and D2 in data")
  }
  
  exposed_group <- data[disease == D1 & diagnosis_date < follow_up_limit]
  exposed_group[, Age_begin := year(diagnosis_date) - Birth_year]
  exposed_group <- merge(exposed_group, data[disease == D2,  .(eid, disease, diagnosis_date)], by = "eid", all.x = TRUE)
  exposed_group <- exposed_group[is.na(diagnosis_date.y) | diagnosis_date.y > diagnosis_date.x]
  exposed_group[, within_5_years := ifelse(!is.na(diagnosis_date.y) & (diagnosis_date.y - diagnosis_date.x <= years(5)), 1, 0)] 
  
  return(exposed_group)
}

find_contorl <- function(data, D1, D2,
                         follow_up_limit = as.Date("2017-04-01")){
  
  if (D1 %in% colnames(data) & D2 %in% colnames(data) ){
    print("Both D1 and D2 in data")
  }
  
  D1_eid <- data[disease == D1, unique(eid)]
  control_group <- data[!eid %in% D1_eid]
  
  control_group <- control_group[diagnosis_date < follow_up_limit, ]
  D2_diagnosis <- data[disease == D2, .(eid, disease, diagnosis_date)]
  control_group[, Age_begin := year(diagnosis_date) - Birth_year]
  
  setkey(control_group, eid)
  setkey(D2_diagnosis, eid)
  control_group <- merge(control_group, D2_diagnosis, by = "eid", all.x = TRUE)
  control_group <- control_group[is.na(diagnosis_date.y) | diagnosis_date.x < diagnosis_date.y]
  control_group[, within_5_years := ifelse(!is.na(diagnosis_date.y) & (diagnosis_date.y - diagnosis_date.x <= years(5)), 1, 0)]
  table(control_group$within_5_years)
  control_group <- unique(control_group, by = c("eid", "diagnosis_date.x"))
  return(control_group)
}


match_controls <- function(exposed_group, control_group, nsample=1, binomial=FALSE) {
  
  dt_exposed <- exposed_group[
    , `:=`(
      diagnosis_date_start = diagnosis_date.x - 7,
      diagnosis_date_end = diagnosis_date.x + 7
    )
  ][
    , .(exposed_eid = eid, Sex, Age_begin, exposed_begin = diagnosis_date.x, 
        diagnosis_date_start, diagnosis_date_end)
  ]
  
  dt_control <- control_group[
    , .(control_eid = eid, Sex, Age_begin, control_begin = diagnosis_date.x)
  ]
  
  possible_matches <- dt_exposed[
    dt_control,
    on = .(
      Sex,
      Age_begin,
      diagnosis_date_start <= control_begin,
      diagnosis_date_end >= control_begin
    ),
    nomatch = 0,
    allow.cartesian = TRUE
  ]
  
  if(binomial) {
    possible_matches <- possible_matches[, .(
      exposed_eid,
      exposed_begin,
      control_eid,
      control_begin = diagnosis_date_start
    )]
    
    control_matches <- control_group[
      possible_matches, 
      on = .(eid = control_eid, diagnosis_date.x = control_begin),
      nomatch = 0
    ]
    
    return(control_matches)
    
  } else {
    possible_matches <- possible_matches[, .(
      exposed_eid,
      exposed_begin,
      control_eid,
      control_begin = diagnosis_date_start
    )]
    
    unique_matches <- possible_matches[, {
      sampled_rows <- .SD[sample(.N, nsample, replace = TRUE)]
      sampled_rows[, sampling_iteration := seq_len(.N)]
      sampled_rows
    }, by = exposed_eid]
    
    control_matches <- control_group[
      unique_matches, 
      on = .(eid = control_eid, diagnosis_date.x = control_begin),
      nomatch = 0
    ]
    
    return(control_matches)
  }
}

cal_relativte_risk <- function(data = data, D1 = "I20", D2 = "I25", nsample=1, binomial=FALSE){
  message("Processing ", D1, " and ", D2,  "....")
  exposed_group <- find_exposed(data = data_long, D1 = D1, D2 = D2)
  message("Exposed group contains ", nrow(exposed_group), " individuals.")
  
  control_group <- find_contorl(data = data_long, D1 = D1, D2 = D2)
  
  
  message("Matching controls for exposed group...")
  mathed_group <- match_controls(exposed_group, control_group, binomial = binomial, nsample = nsample)
  
  
  if(!binomial){
    exposed_group <- exposed_group[eid %in% mathed_group$exposed_eid,]
    message("Performed ", nsample, " matching iterations.")
    message(nrow(exposed_group), " exposed individuals were matched with controls.")
    
    C_exposed <- sum(exposed_group$within_5_years == 1)
    C_control <- mathed_group[, .(C_control = sum(within_5_years == 1)), by = sampling_iteration]
    mean_C_control <- mean(C_control$C_control)
    
    RR <- C_exposed / mean_C_control
    
    P_value <- sum(C_control$C_control >= C_exposed) / nrow(C_control)
    C_control <- mean_C_control
    
  } else {
    message("Total ", nrow(mathed_group), " control matches (each admission counts as one match!)")
    
    prob_list <- mathed_group[, .(P_i = sum(within_5_years == 1) / .N), by = exposed_eid]
    
    P_avg <- mean(prob_list$P_i)
    
    C_exposed <- sum(exposed_group$within_5_years == 1)
    
    P_value <- pbinom(q = C_exposed, size = nrow(exposed_group), prob = P_avg, lower.tail = FALSE)
    
    P_exposed <- C_exposed / nrow(exposed_group)
    C_control <- sum(mathed_group$within_5_years == 1)
    P_control <- C_control/ nrow(mathed_group)
    
    RR <- P_exposed / P_control
  }
  
  message(D1, " to ", D2, " RR: ", RR)
  message(D1, " to ", D2, " P-value: ", P_value)
  
  result <- data.table(D1 = D1, D2 = D2, 
                       n_exposed = length(unique(mathed_group$exposed_eid)),
                       c_exposed = C_exposed, 
                       n_control = length(unique(mathed_group$eid)),
                       c_control = C_control,
                       RR = RR, P_value = P_value, 
                       binomial = binomial)
  return(result)
}
