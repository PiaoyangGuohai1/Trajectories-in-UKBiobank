library(readr)
library(data.table)
rm(list = ls())
source("./R/fun/00_fun_识别方向性.R", echo=TRUE)

data_long <- read_csv("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
data_long <- data.table(data_long)
head(data_long)

result_df <- read_csv("result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv")
result_df <- data.table(result_df)


result_df[P_value < (0.05 / nrow(result_df)), .N]

filtered_result <- result_df[P_value < (0.05 / nrow(result_df)), .(D1, D2)]

if(!file.exists("result/data/02_RR/UKB_cal_relativte_risk_with_direction.csv")){
  library(parallel)
  results_list <- mclapply(1:nrow(filtered_result), function(i) {
    message("Processing: ", i)
    D1 <- filtered_result[i, D1]
    D2 <- filtered_result[i, D2]
    result <- cal_direction(data_long, D1, D2)
    return(result)
  }, mc.cores = 60)
  
  
  final_result <- rbindlist(results_list)
  
  setkey(result_df,D1, D2)
  setkey(final_result,D1, D2)
  result_df <- merge(result_df, final_result)
  
  result_df[, direction_significant := ifelse(p_direction < (0.05/nrow(result_df)) & estimate > 0.5, 1, 0)]
  
  table(result_df$direction_significant)
  
  write_csv(result_df, "result/data/02_RR/UKB_cal_relativte_risk_with_direction.csv")
} else {
  result_df <- fread("result/data/02_RR/UKB_cal_relativte_risk_with_direction.csv")
  table(result_df$direction_significant)
}

if(F){
  rm(list = ls())
  data_binomial <- fread("result/data/02_RR/UKB_cal_relativte_risk_result_binomial.csv") %>%
    select(D1, D2, n_exposed, c_exposed, n_control, c_control, RR, P_value) %>%
    rename(`RR (bimomial)` = RR,
           `P value (bimomial)` = P_value)
  head(data_binomial)
  
  data_sample <- fread("result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv") %>%
    select(D1, D2, RR, P_value) %>%
    rename(`RR (sample)` = RR,
           `P value (sample)` = P_value)
  head(data_sample)
  
  data_direction <- fread("result/data/02_RR/UKB_cal_relativte_risk_with_direction.csv") %>%
    select(D1, D2, p_direction, estimate, direction_significant) %>%
    rename(`P value (direction)` = p_direction,
           Estimate = estimate,
           Significant = direction_significant)
  head(data_direction)
  
  data_st3 <- data_binomial %>%
    merge(data_sample) %>%
    merge(data_direction)
  
  fwrite(data_st3, "result/data/02_RR/supplementa_table_2.csv")
}
