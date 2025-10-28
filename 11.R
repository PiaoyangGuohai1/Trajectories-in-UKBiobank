rm(list = ls())
library(ggplot2)
library(readr)
library(data.table)
library(dplyr)
library(visNetwork)
library(showtext)
library(ggrepel)

trajectories <- readRDS("result/data/04_trajectories_cluster/trajectories_choose.rds")
trajectories <- trajectories[!is.na(D3),]
setDT(trajectories)

weight <- read_csv("result/data/05_trans_prob/transition_probs.csv")
setDT(weight)
head(weight)
weight <- weight[, .(from = D1, to = D2, prob = probability)]

weight

death_weight <- weight[to=="death",]

color <- readRDS("result/data/03_trajectories_result/para_for_sanky_plot.rds")
head(color)
source("./R/fun/00_fun_绘制轨迹图.R", echo=TRUE)
threshold = 0

network <- plot_trajectory_network(
  trajectories = trajectories,
  weight = weight,
  icd_color = color,
  death_weight = death_weight,
  threshold_quantile = threshold,
  include_legend = F
)


network$plot
htmlwidgets::saveWidget(network$plot, "result/plot/trajectory_network_核心子模块.html", selfcontained = TRUE)

if(T){
  library(igraph)
  
  g <- graph_from_data_frame(d = network$edges[, .(from, to, weight = prob)], directed = TRUE)
  
  if(any(E(g)$weight <= 0)) stop("All edge weights must be positive for weighted centrality calculations")
  
  degree_weighted <- strength(g, mode = "all", weights = E(g)$weight)
  
  betweenness_weighted <- betweenness(g, directed = TRUE, weights = 1/E(g)$weight)
  
  closeness_weighted <- closeness(g, mode = "all", weights = 1/E(g)$weight, normalized = TRUE)
  
  eigenvector_weighted <- eigen_centrality(g, weights = E(g)$weight)$vector
  
  nodes_df <- data.frame(
    id = V(g)$name,
    degree = degree_weighted,
    betweenness = betweenness_weighted,
    closeness = closeness_weighted,
    eigen = eigenvector_weighted
  )
  n <- vcount(g)
  nodes_df$betweenness_std <- nodes_df$betweenness / ((n-1)*(n-2))
  nodes_df <- nodes_df %>% left_join(network$nodes[,c("id", "disease_name")])
  
  fwrite(nodes_df, file = paste0("result/data/06_netplot/Network_center_", threshold, ".csv"))
  
}

nodes_df

library(ggthemr)

ggthemr_reset()
ggthemr("flat")

common_theme <- theme(
  text            = element_text(size = 12, color = "black"),
  axis.title      = element_text(size = 12, color = "black"),
  axis.text       = element_text(size = 12, color = "black"),
  strip.text      = element_text(size = 12, color = "black"),
  plot.title      = element_text(size = 16, hjust = 0, color = "black"),
  legend.box      = "horizontal",
  legend.title    = element_text(size = 16, color = "black"),
  legend.text     = element_text(size = 12, color = "black")
)


ggplot(nodes_df, aes(x = closeness, y = degree, size = eigen, color = betweenness)) +
  geom_point(alpha = 1) +
  geom_text_repel(
    aes(label = id),
    size = 4,
    color = "black",
    nudge_y = 0.006,
    nudge_x = 0.0005,
    segment.size = 0.2,
    segment.alpha = 0.5
  ) +
  scale_color_gradient(low = "#1f78b4", high = "#e31a1c", name = "Betweenness Centrality") +
  scale_size_continuous(range = c(3, 10), name = "Eigenvector Centrality") +
  labs(
    x = "Closeness Centrality",
    y = "Degree Centrality"
  ) +
  common_theme +
  theme(
    legend.position  = "right",
    legend.direction = "vertical",
    legend.box       = "vertical",
    legend.title       = element_text(size = 12),
    legend.spacing.y   = unit(1.5, "lines"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )


ggsave(paste0("result/data/06_netplot/network_character_",threshold,".pdf"), width = 8, height = 6)
