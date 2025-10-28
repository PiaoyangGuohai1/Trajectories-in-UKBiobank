rm(list = ls())
library(readr)
library(data.table)
library(plotly)
library(data.table)
library(paletteer)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggthemr)

data_long <- read_csv("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
data_long <- data.table(data_long)
head(data_long)

run_bootstrap_validation <- function(file_path, data_long, num_bootstrap = 10000) {
  
  trajectories <- readRDS(file_path)
  
  if (is.null(trajectories) || (is.data.frame(trajectories) && nrow(trajectories) == 0)) {
    message("  Skipping ", basename(file_path), ": no trajectories to process.")
      return(NULL)
    }
    
  level <- as.integer(str_extract(basename(file_path),
                                  "(?<=trajectories_)\\d+(?=_result\\.rds)"))
  eid_col <- paste0("D", level, "_eid")
  
  if (!eid_col %in% colnames(trajectories)) {
    message("  Skipping ", basename(file_path),
            ": missing column ", eid_col)
      return(NULL)
    }
    
  Npop <- length(unique(data_long$eid))
  bootstrap_support <- sapply(trajectories[[eid_col]], function(eids_str) {
    if (is.na(eids_str) || eids_str == "") return(NA_real_)
    eids <- unlist(str_split(eids_str, ","))
    N <- length(eids)
    P <- N / Npop
    samples <- rbinom(n = num_bootstrap, size = Npop, prob = P)
    mean(samples >= 20)
  })
  
  trajectories %>%
    mutate(bootstrap_support = bootstrap_support) %>%
    select(trajectory_name, bootstrap_support)
}

process_folder <- function(folder = c("eid_only", "eid_rep"),
                           data_long, num_bootstrap = 10000) {
  folder <- match.arg(folder)
  input_dir  <- file.path("result/data/03_trajectories_result", folder)
  output_csv <- file.path(input_dir, "trajectories_bootstrap_result.csv")
  
  if (file.exists(output_csv)) {
    message("Skipping: already exists ", output_csv)
    return(invisible(NULL))
  }
  
  files <- list.files(input_dir,
                      pattern    = "^trajectories_\\d+_result\\.rds$",
                      full.names = TRUE)
  if (length(files) == 0) {
    stop("No trajectories_*_result.rds files in ", input_dir)
  }
  
  results_list <- lapply(files, function(fp) {
    message("Processing ", basename(fp))
    run_bootstrap_validation(fp, data_long, num_bootstrap)
  })
  results_list <- Filter(Negate(is.null), results_list)
  
  if (length(results_list) == 0) {
    stop("No valid trajectories processed in ", input_dir)
  }
  
  final_result <- bind_rows(results_list)
  fwrite(final_result, output_csv)
  message("Generated combined results: ", output_csv)
}

process_folder("eid_only", data_long)
process_folder("eid_rep", data_long)
