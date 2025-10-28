rm(list = ls())
library(readr)
library(data.table)
library(igraph)
library(parallel)
library(dplyr)

if(T){
  rm(list = ls())
  trajectories_2 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_2_result.rds")
  trajectories_3 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_3_result.rds")
  trajectories_4 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_4_result.rds")
  trajectories_5 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_5_result.rds")
  trajectories_6 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_6_result.rds")
  boostrap <- fread("result/data/03_trajectories_result/eid_rep/trajectories_bootstrap_result.csv")
  table(boostrap$bootstrap_support > 0.99)

  
  
  
  trajectories <- rbindlist(
    list(trajectories_2, trajectories_3, trajectories_4, trajectories_5, trajectories_6),
    fill = TRUE
  ) %>%
    select(
      trajectory_name,
      matches("^D[0-9]+$")
    )
  
  trajectories <- setDT(trajectories)
  trajectories <- trajectories[trajectory_name %in% boostrap[bootstrap_support > 0.99, trajectory_name] ,]
  saveRDS(trajectories, "result/data/03_trajectories_result/eid_rep/trajectories_for_cluster.rds")
  
  rm(trajectories_2, trajectories_3, trajectories_4, trajectories_5, trajectories_6, boostrap)
}


rm(list = ls())
trajectories <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_for_cluster.rds")

if(T){
  all_diagnoses <- unique(unlist(strsplit(trajectories$trajectory_name, "_")))
  jaccard_matrix <- matrix(0, nrow = length(all_diagnoses), ncol = length(all_diagnoses), dimnames = list(all_diagnoses, all_diagnoses))
  
  dx_cols <- grep("^D\\d+$", names(trajectories), value = TRUE)
  diagnosis_matrix <- sapply(all_diagnoses, function(diag) {
    mat <- trajectories[, ..dx_cols] == diag
    rowSums(mat, na.rm = TRUE) > 0
  })
  
  colnames(diagnosis_matrix) <- all_diagnoses
  
  co_occurrence <- t(diagnosis_matrix) %*% diagnosis_matrix
  
  diag_counts <- diag(co_occurrence)
  
  jaccard_matrix <- co_occurrence / (outer(diag_counts, diag_counts, "+") - co_occurrence)
  
  dt_jaccard <- as.data.frame(jaccard_matrix)
  dt_jaccard$diagnosis <- rownames(jaccard_matrix)
  
  dt_jaccard <- dt_jaccard[, c("diagnosis", setdiff(names(dt_jaccard), "diagnosis"))]
  
  write_csv(dt_jaccard, "result/data/04_trajectories_cluster/dt_jaccard.csv")
  
}

if(T){
  library(stats)
  library(ggplot2)
  
  distance_matrix <- as.dist(1 - jaccard_matrix)
  
  hclust_result <- hclust(distance_matrix, method = "ward.D2")
  
  pdf("result/data/04_trajectories_cluster/hcluster_for_trajectories_cluster_4.pdf", width = 16, height = 6)
  plot(hclust_result, main = "Hierarchical Clustering of Diagnoses based on Jaccard Index", 
       xlab = "Diagnoses", sub = "", cex = 0.9)
  dev.off()
}

pdf("result/data/04_trajectories_cluster/diagnosis_cluster_of_MCL_1.25.pdf")
reticulate::source_python('./R/fun/00_cal_mcl.py')
dev.off()

pdf("result/data/04_trajectories_cluster/diagnosis_cluster_of_louvain.pdf")
reticulate::source_python('./R/fun/00_cal_louvain.py')
dev.off()


if(T){
  library(aricode)
  library(clue)
  
  convert_dict_to_vector <- function(cluster_dict) {
    cluster_vector <- c()
    labels_vector <- c()
    
    for (cluster_id in names(cluster_dict)) {
      cluster_elements <- cluster_dict[[cluster_id]]
      cluster_vector <- c(cluster_vector, rep(cluster_id, length(cluster_elements)))
      labels_vector <- c(labels_vector, cluster_elements)
    }
    
    return(factor(cluster_vector[order(labels_vector)]))
  }
  
  mcl_clusters <- convert_dict_to_vector(cluster_dict_mcl)
  louvain_clusters <- convert_dict_to_vector(cluster_dict_louvain)
  
  nmi_result <- NMI(mcl_clusters, louvain_clusters)
  print(paste("NMI:", nmi_result))
  
  rand_index <- cl_agreement(as.cl_partition(mcl_clusters), as.cl_partition(louvain_clusters), method = "Rand")
  print(paste("Rand Index:", rand_index))
  
}

idx1 <- which(sapply(cluster_dict_mcl, function(x) "I10" %in% x))
disease_choose1 <- cluster_dict_mcl[[idx1]]

idx2 <- which(sapply(cluster_dict_louvain, function(x) "I10" %in% x))
disease_choose2 <- cluster_dict_louvain[[idx2]]

disease_choose1
disease_choose2



if(T){
  library(VennDiagram)
  library(grid)
  
  venn_plot <- venn.diagram(
    x = list(
      MCL     = disease_choose1,
      louvain = disease_choose2
    ),
    filename    = NULL,
    fill        = c("#1f78b4", "#e31a1c"),
    alpha       = 0.6,
    cat.col     = c("#1f78b4", "#e31a1c"),
    cat.cex     = 1.2,
    cat.fontface= "bold",
    cat.dist    = c(0.03, 0.03),
    cat.pos     = c(-20, 20),
    margin      = 0.05,
    lwd         = 2,
    cex         = 1.1,
    fontface    = "bold"
  )
  
  grid.newpage()
  grid.draw(venn_plot)

}
disease_choose1
disease_choose2
intersect(disease_choose1 , disease_choose2)
union(disease_choose1 , disease_choose2)
(disease_choose <- intersect(disease_choose1 , disease_choose2))
saveRDS(disease_choose, "result/data/04_trajectories_cluster/disease_choose.rds")


trajectories[, included_in_disease_choose := {
  diseases <- unlist(strsplit(trajectory_name, "_"))
  if (all(diseases %in% disease_choose)) 1 else 0
}, by = trajectory_name]



trajectories[, .N, by = included_in_disease_choose]


trajectories_choose <- trajectories[included_in_disease_choose == 1, .SD, .SDcols = !("included_in_disease_choose")]
saveRDS(trajectories_choose, "result/data/04_trajectories_cluster/trajectories_choose.rds")
