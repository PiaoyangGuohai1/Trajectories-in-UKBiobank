library(readr)
library(data.table)
library(lubridate)
rm(list = ls())
source("./R/fun/00_fun_识别相对风险.R", echo=TRUE)

data_long <- fread("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
head(data_long)

if(F){
  eid <- data_long[,.N, by = eid]
  disease <- data_long[,.N, by = disease]
  
  disease_combinations <- expand.grid(D1 = disease$disease, D2 = disease$disease)
  disease_combinations <- subset(disease_combinations, D1 != D2)
  
  if(F){
    disease_combinations$D1 <- as.character(disease_combinations$D1)
    disease_combinations$D2 <- as.character(disease_combinations$D2)
    nrow(disease_combinations)
    
    check_number <- function(data, D1, D2){
      eids_D1 <- data[disease == D1, unique(eid)]
      eids_D2 <- data[disease == D2, unique(eid)]
      
      num_both <- length(intersect(eids_D1, eids_D2))
      return(num_both)
    }
    
    number <- lapply(1:nrow(disease_combinations), function (x){
      D1 <- disease_combinations[x, "D1"]
      D2 <- disease_combinations[x, "D2"]
      message("check the disease pairs: ", D1, "和", D2, "(n=", x, ")")
      num <- check_number(data_long, D1, D2)
      return(num)
    })
    
    
    disease_combinations$number <- unlist(number)
    sum(disease_combinations$number >= 20)
    
    disease_combinations <- disease_combinations[number >= 20, ]
    rm(number)
  }
  
  fwrite(disease_combinations, "result/data/02_RR/disease_combinations.csv")
}
disease_combinations <- fread("result/data/02_RR/disease_combinations.csv")


ncore = 40
if (file.exists("result/data/02_RR/UKB_cal_relativte_risk_result_binomial.csv")) {
  result_df <- fread("result/data/02_RR/UKB_cal_relativte_risk_result_binomial.csv")
} else {
  nrow(disease_combinations)
  
  library(parallel)
  start = Sys.time()
  results_list <- mclapply(1:nrow(disease_combinations), function(i) {
    print(paste0("Processing: ", i))
    D1 <- disease_combinations$D1[i]
    D2 <- disease_combinations$D2[i]
    result <- cal_relativte_risk(data = data_long, D1 = D1, D2 = D2, binomial = TRUE)
    return(result)
  }, mc.cores = ncore)
  end = Sys.time()
  print(end - start)
  
  result_df <- do.call(rbind, results_list)
  
  fwrite(result_df, "result/data/02_RR/UKB_cal_relativte_risk_result_binomial.csv")
}


if(T){
  result_df[P_value < 0.05, .N]
  result_df[P_value < 0.001, .N]
  result_df[P_value < (0.001/nrow(result_df)), .N]
  result_df[
    P_value < (0.001/nrow(result_df)) & 
      c_exposed >= 20 &
      !is.na(RR),.N
  ]
  
  result_df_filtered <- result_df[
    P_value < 0.05 & 
      c_exposed >= 20 &
      !is.na(RR)
  ]
  
  fwrite(result_df_filtered, "result/data/02_RR/UKB_cal_relativte_risk_result_binomial_sig.csv")
  
  disease_combinations_filtered <- data.frame(D1 = result_df_filtered[,D1], 
                                              D2 = result_df_filtered[,D2])
  
  print(nrow(disease_combinations_filtered))
}



do_sample = TRUE

while(do_sample){
  if (file.exists("result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv")) {
    sample_df <- fread("result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv")
    
    if (nrow(sample_df) < nrow(disease_combinations_filtered)){
      disease_combinations_filtered <- disease_combinations_filtered[!(
        paste0(disease_combinations_filtered$D1, "_", disease_combinations_filtered$D2) %in% 
          paste0(sample_df$D1, "_", sample_df$D2)
      ), ]
      sample_df_raw <- sample_df
      sample_df <- NULL
      print(paste0("Previous calculation incomplete, remaining: ",nrow(disease_combinations_filtered)," pairs..."))
      
    } else {
      do_sample = FALSE
      print("All calculations completed!")
    }
  }
  
  if(do_sample){
    ncore = 20
    library(parallel)
    start = Sys.time()
    sample_list <- mclapply(1:nrow(disease_combinations_filtered), function(i) {
      print(paste0("Processing: ", i))
      D1 <- disease_combinations_filtered$D1[i]
      D2 <- disease_combinations_filtered$D2[i]
      result <- cal_relativte_risk(data = data_long, D1 = D1, D2 = D2, binomial = FALSE, nsample = 10000)
      return(result)
    }, mc.cores = ncore)
    
    end = Sys.time()
    print(end - start)
    
    sample_df <- do.call(rbind, sample_list)
    if(exists("sample_df_raw")){
      sample_df <- rbind(sample_df_raw, sample_df)
    }
    write_csv(sample_df, "result/data/02_RR/UKB_cal_relativte_risk_result_sample_10k.csv")
  }
}
