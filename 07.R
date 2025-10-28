rm(list = ls())
library(readr)
library(data.table)
library(stringr)
library(dplyr)

if(T){
  rm(list = ls())
  trajectories_2 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_2_result.rds")
  trajectories_3 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_3_result.rds")
  trajectories_4 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_4_result.rds")
  trajectories_5 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_5_result.rds")
  trajectories_6 <- readRDS("result/data/03_trajectories_result/eid_rep/trajectories_6_result.rds")
  trajectories_2[, traj_length := 2]
  trajectories_3[, traj_length := 3]
  trajectories_4[, traj_length := 4]
  trajectories_5[, traj_length := 5]
  trajectories_6[, traj_length := 6]
  
  all_trajectories <- rbindlist(list(trajectories_2, trajectories_3, trajectories_4, trajectories_5, trajectories_6), fill = TRUE)
  boostrap <- fread("result/data/03_trajectories_result/eid_rep/trajectories_bootstrap_result.csv")
  
  all_trajectories <- all_trajectories %>%
    filter(trajectory_name %in% boostrap[ bootstrap_support > 0.99, trajectory_name])
  
  fwrite(all_trajectories, file = "result/data/03_trajectories_result/eid_rep/trajectories_combined.csv")
}


library(plotly)
library(data.table)
library(paletteer)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggthemr)

df <- fread("result/data/03_trajectories_result/eid_rep/trajectories_combined.csv", na.strings = "")

if(T){
  head(df)
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  icd_color <- readRDS("result/data/03_trajectories_result/para_for_sanky_plot.rds")
  
  
  plot_top3_traj <- function(df, traj_len, color= icd_color, base_size = 12) {
    df_sub <- df %>% filter(traj_length %in% traj_len)
    traj_len_max = max(traj_len)
    get_top3 <- function(x) {
      tab <- prop.table(table(x))
      top <- sort(tab, decreasing = TRUE)[1:3]
      data.frame(
        code = names(top),
        prop = as.numeric(top),
        rank = seq_along(top),
        stringsAsFactors = FALSE
      )
    }
    
    stages <- paste0('D', seq_len(traj_len_max))
    total_pos <- 4 * traj_len_max - 1
    df_list <- list()
    for (i in seq_along(stages)) {
      pos_start <- (i - 1) * 4 + 1
      df_stage <- get_top3(df_sub[[ stages[i] ]]) %>%
        mutate(
          stage = stages[i],
          pos = pos_start:(pos_start + 2)
        )
      df_list[[i]] <- df_stage
    }

    df_plot <- bind_rows(df_list) %>%
      left_join(color, by = c("code" = "name"))
    
    axis_labels <- rep('', total_pos)
    axis_labels[ df_plot$pos ] <- df_plot$code
    
    ggthemr_reset()
    ggthemr("flat")
    
    common_theme <- theme(
      text            = element_text(size = 14, color = "black"),
      axis.title      = element_text(size = 14, color = "black"),
      axis.text       = element_text(size = 12, color = "black"),
      strip.text      = element_text(size = 14, color = "black"),
      plot.title      = element_text(hjust = 0),
      legend.box      = "horizontal",
      legend.title    = element_text(size = 16, color = "black"),
      legend.text     = element_text(size = 12, color = "black")
    )
    
    ggplot(df_plot, aes(x = factor(pos), y = prop, fill = code)) +
      geom_col(width = 1) +
      geom_text(aes(label = percent(prop, accuracy = 0.1)),
                vjust = -0.8, size = base_size * 0.3, angle = 45, hjust = 0.1) +
      scale_fill_manual(
        values = setNames(icd_color$color, icd_color$name),
        guide  = "none"
      ) +
      scale_x_discrete(
        limits = as.character(seq_len(total_pos)),
        labels = axis_labels,
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        x = NULL,
        y = 'Proportion',
        title = paste('Top 3 Diagnoses for All Trajectories')
      ) +
      common_theme +
      theme(
        axis.text.x = element_text(vjust = 0.5, size = base_size),
        axis.text.y = element_text(size = base_size),
        axis.title.y = element_text(size = base_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      ) + theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.background= element_rect(fill = "transparent", color = NA)
      )
  }
  
  plot_top3_traj(df, traj_len = c(2,3,4,5,6))
  
  ggsave(filename = "result/plot/top3_diagnosis_for_all_trajectories.pdf", height = 5, width = 12)
  
  p1 <- plot_top3_traj(df, traj_len = c(3)) + labs(title = "Top Diagnosis for Stage 3 Trajectories")
  p2 <- plot_top3_traj(df, traj_len = c(4)) + labs(title = "Top Diagnosis for Stage 4 Trajectories")
  p3 <- plot_top3_traj(df, traj_len = c(5)) + labs(title = "Top Diagnosis for Stage 5 Trajectories")
  library(patchwork)
  p1/p2/p3
  ggsave(filename = "result/plot/top3_diagnosis_for_all_trajectories_combined_supplement.pdf", height = 12, width = 12)
  
}




df <- df %>%
  select( -traj_length)

if(F){
  d1_counts <- table(df$D1)
  total_traj <- sum(d1_counts)
  i10_percent <- d1_counts["I10"] / total_traj * 100
  other_top <- sort(d1_counts, decreasing = TRUE)[2:3]/total_traj * 100
  
  observed <- c(d1_counts["I10"], total_traj - d1_counts["I10"])
  expected_prop <- c(1/length(unique(df$D1)), 1 - 1/length(unique(df$D1)))
  chi_test <- chisq.test(observed, p = expected_prop)
  
  cat("=== Starting disease distribution analysis ===\n")
  cat(sprintf("Hypertension(I10) as starting node: %.1f%% (%d/%d)\n",
              i10_percent, d1_counts["I10"], total_traj))
  cat(sprintf("Second most common starting diseases: %s (%.1f%%), %s (%.1f%%)\n\n",
              names(other_top)[1], other_top[1]/total_traj*100,
              names(other_top)[2], other_top[2]/total_traj*100))
  
  cat("=== Chi-square test results ===\n")
  cat(sprintf("Chi-square = %.1f\ndf = %d\nP-value = %.2e",
              chi_test$statistic, 
              chi_test$parameter,
              chi_test$p.value))
}


colnames(df)
head(df)

df <- df %>%
  filter(grepl("N18", trajectory_name))

df_long <- df %>%
  select(D1, D2, D3, D4, D5, D6) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("D"),
    names_to = "step",
    values_to = "diagnosis",
    values_drop_na = TRUE
  ) %>%
  arrange(row_id, step) %>%
  group_by(row_id) %>%
  mutate(next_diagnosis = lead(diagnosis)) %>%
  ungroup()


if (TRUE) {
  library(data.table)
  library(paletteer)
  
  all_diseases_count <- sort(
    table(c(df_long$diagnosis, df_long$next_diagnosis)),
    decreasing = TRUE
  )
  all_diseases <- names(all_diseases_count)
  n_diseases  <- length(all_diseases)
  
  nodes_master <- data.table(name = all_diseases)
  nodes_master[, id := .I - 1]
  
  d <- palettes_d_names
  (base <- paletteer_d("ggthemes::Tableau_20"))
  
  palette_base <- colorRampPalette(base)(n_diseases)
  
  big_n   <- 30
  small_n <- n_diseases - big_n
  
  big_colors   <- palette_base[round(seq(1, n_diseases, length.out = big_n))]
  small_colors <- palette_base[-round(seq(1, n_diseases, length.out = big_n))][seq_len(small_n)]
  final_colors <- c(big_colors, small_colors)
  
  color_table <- data.table(
    name  = all_diseases,
    color = final_colors
  )
  nodes_master <- merge(
    nodes_master,
    color_table,
    by = "name",
    all.x = TRUE
  )
  
  swap_node_colors <- function(nodes_dt, node_a, node_b) {
    tmp_color <- nodes_dt[name == node_a, color]
    nodes_dt[name == node_a, color := nodes_dt[name == node_b, color]]
    nodes_dt[name == node_b, color := tmp_color]
    return(nodes_dt)
  }
  
  nodes_master <- swap_node_colors(nodes_master, "I10", "E78")
  nodes_master <- swap_node_colors(nodes_master, "I73", "I10")
  nodes_master <- swap_node_colors(nodes_master, "E10", "N03")
  saveRDS(
    nodes_master,
    file = "result/data/03_trajectories_result/para_for_sanky_plot.rds"
  )
  
}


links <- df_long %>%
  filter(!is.na(next_diagnosis)) %>%
  group_by(diagnosis, next_diagnosis) %>%
  summarise(value = n(), .groups = "drop") %>%
  mutate(
    source = match(diagnosis, nodes_master$name) - 1,
    target = match(next_diagnosis, nodes_master$name) - 1
  )
head(links)
setDT(links)

links$color <- plotly::toRGB(
  nodes_master$color[ links$source + 1 ], 
  alpha = 0.5
)


p <- plot_ly(
  type = "sankey",
  domain = list(x = c(0, 1), y = c(0, 1)),
  node = list(
    label     = nodes_master$name,
    color     = nodes_master$color,
    pad       = 20,
    thickness = 20,
    line      = list(color = "black", width = 0.5)
  ),
  link = list(
    source = links$source,
    target = links$target,
    value  = links$value,
    color  = links$color
  )
)


p

htmlwidgets::saveWidget(p, file = "result/data/03_trajectories_result/sankey_plot_traj_10.html")
