rm(list = ls())
library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(survival)
library(parallel)
source("R/fun/00_cal_central_diagnoses.R", echo=TRUE)

trajectories <- readRDS("result/data/04_trajectories_cluster/trajectories_choose.rds")
setDT(trajectories)


disease <- unique(unlist(strsplit(trajectories$trajectory_name, "_")))


data_long <- fread("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
setDT(data_long)
data_long[, diagnosis_date := as.IDate(diagnosis_date)]
setorder(data_long, eid, diagnosis_date)


all_result <- data.frame()
for (D2 in disease) {

  message(paste0("Processing ", D2, "..."))
  D1 <- set_identify(D2, trajectory_name)[[1]]
  D3 <- set_identify(D2, trajectory_name)[[2]]
  
  
  if (length(D1) != 0 & length(D3) != 0){
    data_long_choose <- data_long[disease %in% c(D2, D1, D3),]
    setorder(data_long_choose, eid, diagnosis_date)
    
    data_d1 <- data_long_choose[disease %in% D1, ][
      , .(Birth_year, Sex, first_d1_date = min(diagnosis_date)), by = eid
    ][
      ,age_at_d1 := year(first_d1_date) - Birth_year
    ]
    
    data_d1 <- unique(data_d1, by = "eid")
    head(data_d1)
    
    data_d3 <- data_long_choose[disease %in% D3, ][
      , .(first_d3_date = min(diagnosis_date)), by = eid
    ]
    
    data_d1_d3 <- merge(data_d1, data_d3, by = "eid", all.x = TRUE)
    rm(data_d1, data_d3)
    data_d1_d3 <- data_d1_d3[is.na(first_d3_date) | first_d3_date > first_d1_date]
    
    cases <- data_d1_d3[!is.na(first_d3_date), ][
      , interval_days := as.numeric(first_d3_date - first_d1_date)
    ]
    
    cases <- D2_status(data_long_choose, cases, D2)
    cases[,.N, by=d2_status]
    
    control_candidates <- data_d1_d3[is.na(first_d3_date), ]
    
    n_cores <- 40
    options(mc.cores = n_cores)
    
    n_iterations <- 10000
    
    RR_values <- numeric(n_iterations)
    
    set.seed(123)
    RR_values_list <- mclapply(1:n_iterations, one_iteration, mc.cores = n_cores)
    
    RR_values <- unlist(RR_values_list)
    
    log_RR_values <- log(RR_values)
    
    mean_log_RR <- mean(log_RR_values)
    se_log_RR <- sd(log_RR_values)
    
    Z_value <- mean_log_RR / se_log_RR
    
    P_value <- 2 * (1 - pnorm(abs(Z_value)))
    
    CI_log_lower <- mean_log_RR - 1.96 * se_log_RR
    CI_log_upper <- mean_log_RR + 1.96 * se_log_RR
    
    RR_mean <- exp(mean_log_RR)
    RR_CI_lower <- exp(CI_log_lower)
    RR_CI_upper <- exp(CI_log_upper)
    
    result_dt <- data.table(
      key_diagnoses = D2,
      RR_mean = RR_mean,
      RR_CI_lower = RR_CI_lower,
      RR_CI_upper = RR_CI_upper,
      P_value = P_value
    )
    
    result_dt[, `:=`(
      mean_log_RR = mean_log_RR,
      se_log_RR = se_log_RR,
      Z_value = Z_value
    )]
    
    print(result_dt)
    all_result <- rbind(all_result, result_dt)
  } else {
    message("No preceding/subsequent diagnosis set for this disease!")
  }
}



fwrite(all_result, "result/data/07_center_diagnosis/key_diagnoses_of_RR_results.csv")


df <- fread("result/data/07_center_diagnosis/key_diagnoses_of_RR_results_10.csv")

df <- df[complete.cases(df),]
head(df)
library(tidyverse)
library(ggh4x)

head(df)
dff <- df %>% 
  arrange(mean_log_RR) %>%
  mutate(
    RR_mean_fmt = formatC(RR_mean, format = "f", digits = 3, width = 5),
    RR_CI_lower_fmt = formatC(RR_CI_lower, format = "f", digits = 3, width = 5),
    RR_CI_upper_fmt = formatC(RR_CI_upper, format = "f", digits = 3, width = 5),
    RR_lab = paste0(RR_mean_fmt, " (", RR_CI_lower_fmt, "-", RR_CI_upper_fmt, ")")
  ) %>%
  mutate(id = row_number())

head(dff)


library(ggplot2)
library(forcats)

y_length <- nrow(df)
ggthemr("flat")


dff %>% 
  mutate(P_value = formatC(P_value, format = "e", digits = 3)) %>%
  ggplot(aes(y = fct_rev(key_diagnoses))) +
  geom_errorbarh(aes(xmin = RR_CI_lower, xmax = RR_CI_upper), height = 0.2, size = 1, color  = "#1f78b4") +
  geom_point(aes(x = RR_mean), size = 2, shape = 23, fill = "#e31a1c", show.legend = FALSE) +
  labs(x = "Relative Risk") +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    labels = seq(0, 10, by = 1),
    guide  = "axis_truncated",
    expand = expansion(add = c(1, 0))
  ) +
  coord_cartesian(ylim = c(1, y_length+1), xlim = c(-1.5, 12), clip = "off") +
  geom_text(aes(x = -1.5, y = key_diagnoses, label = RR_lab), size = 3.8) +
  geom_text(aes(x = 11, y = key_diagnoses, label = P_value), size = 3.8) +
  annotate(geom = "segment", x = 1, xend = 1, y = 0.5, yend = y_length+0.5, linetype = 2) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold",
                                   margin = margin(l = 1, r = 1, unit = "cm")),
        axis.title.y = element_blank(),
        axis.text.x = element_text(),
        panel.background = element_blank()) +
  annotate(geom = "text", x = -1.5, y = y_length+1, fontface = "bold",
           vjust = 0.5, size = 4, label = "Relative Risk\n   (95% CI)") +
  annotate(geom = "text", x = -3.5, y = y_length+1, vjust = 0.5, size = 4, 
           label = "Diagnoses", fontface = "bold") +
  annotate(geom = "text", x = 11, y = y_length+1, vjust = 0.5, size = 4, 
           label = "P\ value", fontface = "bold")


ggsave("result/data/07_center_diagnosis/verify_central_diagnosis.pdf", height = 4.5,width = 10)
