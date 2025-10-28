rm(list = ls())
library(readr)
library(data.table)
library(purrr)
library(stringr)
library(dplyr)

if(F){
  trajectories_2 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_2_result.rds")
  trajectories_3 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_3_result.rds")
  trajectories_4 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_4_result.rds")
  trajectories_5 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_5_result.rds")
  trajectories_6 <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_6_result.rds")
  
  trajectories_2[, traj_length := 2]
  trajectories_3[, traj_length := 3]
  trajectories_4[, traj_length := 4]
  trajectories_5[, traj_length := 5]
  trajectories_6[, traj_length := 6]
  
  all_trajectories <- rbindlist(list(trajectories_2, trajectories_3, trajectories_4, trajectories_5, trajectories_6), fill = TRUE)
  boostrap <- fread("result/data/03_trajectories_result/eid_only/trajectories_bootstrap_result.csv")
  
  all_trajectories <- all_trajectories %>%
    filter(trajectory_name %in% boostrap[ bootstrap_support > 0.99, trajectory_name])
  
  saveRDS(all_trajectories, file = "result/data/03_trajectories_result/eid_only/trajectories_combined.rds")
}

if(F){
  path  <- "result/data/03_trajectories_result/eid_only/"
  files <- list.files(path,
                      pattern    = "^trajectories_\\d+_result\\.rds$",
                      full.names = TRUE)
  
  traj_list <- map(files, function(f) {
    lvl  <- as.integer(str_extract(basename(f),
                                   "(?<=trajectories_)\\d+(?=_result\\.rds)"))
    traj <- readRDS(f)
    
    if (is.null(traj) || !is.data.frame(traj) || nrow(traj) == 0) {
      message("Skipping ", basename(f), ": no data")
      return(NULL)
    }
    
    dis_col <- paste0("D", lvl)
    num_col <- paste0("D", lvl, "_number")
    eid_col <- paste0("D", lvl, "_eid")
    
    miss <- setdiff(c(dis_col, num_col, eid_col), names(traj))
    if (length(miss) > 0) {
      message("Skipping ", basename(f),
              ": missing column(s) ", paste(miss, collapse = ", "))
      return(NULL)
    }
    
    traj %>%
      rename(
        disease = !!sym(dis_col),
        number  = !!sym(num_col),
        eid     = !!sym(eid_col)
      ) %>%
      select(trajectory_name, disease, number, eid) %>%
      mutate(
        last_traj = if (lvl == 1) "TOP"
        else sub("_[^_]+$", "", trajectory_name),
        level     = lvl
      )
  })
  
  all_trajectories <- traj_list %>%
    compact() %>%
    bind_rows()
  
  bootstrap <- fread(file.path(path, "trajectories_bootstrap_result.csv"))
  filtered  <- all_trajectories %>%
    semi_join(
      bootstrap %>% filter(bootstrap_support > 0.99) %>% select(trajectory_name),
      by = "trajectory_name"
    )
  
  saveRDS(filtered, file.path(path, "trajectories_combined_long_data.rds"))
  message("Saved combined trajectories to trajectories_combined_long_data.rds")
}


rm(list = ls())
library(plotly)
library(data.table)
library(RColorBrewer)
library(jsonlite)
library(dplyr)
library(stringi)
library(stringr)


all_trajectories <- readRDS("result/data/03_trajectories_result/eid_only/trajectories_combined_long_data.rds")
colnames(all_trajectories)


all_trajectories[, eid := strsplit(eid, ",")]

setorder(all_trajectories, -level)
for (i in 1:nrow(all_trajectories)) {
  current_traj <- all_trajectories$trajectory_name[i]
  sub_trajs <- all_trajectories[last_traj == current_traj, trajectory_name]
  if (length(sub_trajs) > 0) {
    sub_eids <- unlist(all_trajectories[trajectory_name %in% sub_trajs, eid])
    all_trajectories[i, eid := list(setdiff(eid[[1]], sub_eids))]
  }
}
all_trajectories[, number := lengths(eid)]
saveRDS(all_trajectories, "result/data/03_trajectories_result/eid_only/trajectories_combined_clean_eid.rds")

nodes_master <- readRDS("result/data/03_trajectories_result/para_for_sanky_plot.rds")

all_diseases_person <- unique(unlist(strsplit(all_trajectories$trajectory_name, split = "_")))

nodes_master <- nodes_master[name %in% all_diseases_person,]

  nodes_master[, id := .I - 1]
  
links <- data.table(source = integer(), target = integer(), value = numeric())
for (i in 1:nrow(all_trajectories)) {
  traj <- all_trajectories$trajectory_name[i]
  diseases <- strsplit(traj, "_")[[1]]
  if (length(diseases) > 1) {
    for (j in 1:(length(diseases) - 1)) {
      source_disease <- diseases[j]
      target_disease <- diseases[j + 1]
      source_id <- nodes_master[name == source_disease, id]
      target_id <- nodes_master[name == target_disease, id]
      value <- all_trajectories[i, number]
      links <- rbind(links, data.table(source = source_id, target = target_id, value = value))
    }
  }
}

aggr_links <- links[, .(value = sum(value)), by = .(source, target)]

aggr_links$color <- plotly::toRGB(
  nodes_master$color[ aggr_links$source + 1 ], 
  alpha = 0.5
)

p <- plot_ly(
  type = "sankey",
  domain = list(x = c(0,1), y = c(0,1)),
  node = list(
    label     = nodes_master$name,
    color     = nodes_master$color,
    pad       = 15,
    thickness = 20,
    line      = list(color = "black", width = 0.5)
  ),
  link = list(
    source = aggr_links$source,
    target = aggr_links$target,
    value  = aggr_links$value,
    color  = aggr_links$color
  )
)



p

htmlwidgets::saveWidget(p, "./result/data/03_trajectories_result/sankey_plot_person_200.html")
