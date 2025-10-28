plot_trajectory_network_metaNet <- function(trajectories, 
                                            weight, 
                                            death_weight = NULL, 
                                            threshold_quantile = 0.75, 
                                            fields_file = "fields/总数据库文件/all_fields.xlsx",
                                            include_legend = TRUE) {
  
  library(data.table)
  library(dplyr)
  library(MetaNet)
  library(visNetwork)
  library(htmltools)
  library(readxl)
  library(stringr)
  
  trajectories_split <- trajectories[, .(trajectory_name, nodes = strsplit(trajectory_name, "_")), 
                                     by = 1:nrow(trajectories)]
  
  edges_list <- lapply(1:nrow(trajectories_split), function(i) {
    nodes <- trajectories_split$nodes[[i]]
    if (length(nodes) > 1) {
      lapply(1:(length(nodes) - 1), function(j) {
        data.frame(from = nodes[j], to = nodes[j + 1])
      })
    }
  }) %>% unlist(recursive = FALSE)
  
  edges_df <- rbindlist(edges_list) %>% 
    .[, .N, by = .(from, to)] %>% 
    setnames("N", "value")
  
  edges_df <- merge(edges_df, weight[, .(from, to, prob)], 
                    by = c("from", "to"), all.x = TRUE)
  
  threshold <- quantile(edges_df$prob, threshold_quantile, na.rm = TRUE)
  edges_df_filtered <- edges_df[prob >= threshold]
  
  nodes <- unique(c(edges_df_filtered$from, edges_df_filtered$to))
  nodes_df <- data.frame(
    id = nodes,
    label = nodes,
    color = ifelse(substr(nodes, 1, 1) == "E", "#0077b6",
                   ifelse(substr(nodes, 1, 1) == "I", "#dc2f02",
                          ifelse(substr(nodes, 1, 1) == "N", "#ffb703", "gray")))
  )
  
  fields_name <- read_excel(fields_file)
  mapped_data <- fields_name %>%
    select(ID, Title) %>%
    filter(str_detect(Title, "^Date")) %>%
    mutate(
      abbreviation = str_extract(Title, "(?<=Date )[A-Za-z0-9]+"),
      disease_name = str_extract(Title, "(?<=first reported \\().*(?=\\))")
    ) %>%
    filter(abbreviation %in% nodes)
  
  nodes_df <- merge(nodes_df, mapped_data[, c("abbreviation", "disease_name")], 
                    by.x = "id", by.y = "abbreviation", all.x = TRUE)
  
  if (!is.null(death_weight)) {
    nodes_df <- merge(nodes_df, death_weight[, .(from, prob)], 
                      by.x = 'id', by.y = 'from', all.x = TRUE)
    nodes_df$label <- ifelse(is.na(nodes_df$prob),
                             nodes_df$label,
                             paste0(nodes_df$label, "\nProb: ", round(nodes_df$prob, 3)))
    nodes_df$title <- ifelse(is.na(nodes_df$prob),
                             paste0("Node: ", nodes_df$label, "<br>Disease: ", nodes_df$disease_name),
                             paste0("Node: ", nodes_df$label, "<br>Disease: ", nodes_df$disease_name, "<br>Death probability: ", round(nodes_df$prob, 3)))
  } else {
    nodes_df$title <- paste0("Node: ", nodes_df$label, "<br>Disease: ", nodes_df$disease_name)
  }
  
  net <- c_net_from_edgelist(edges_df_filtered[, c("from", "to", "prob")], direct = TRUE)
  
  class(net)
  get_v(net)
  get_e(net)
  
  vis_net <- visIgraph(net)
  
  vis_net <- vis_net %>%
    visNodes(
      color = list(background = nodes_df$color),
      title = nodes_df$title
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      title = paste("From", edges_df_filtered$from, "to", edges_df_filtered$to, "<br>Prob:", round(edges_df_filtered$prob, 3))
    ) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_kk")
  
  legend_html <- NULL
  if (include_legend) {
    legend_html <- tags$div(
      style = "max-width: 800px; padding: 10px; border: 1px solid black; margin-bottom: 15px;",
      tags$h4("Legend: Disease Abbreviations"),
      tags$ul(lapply(1:nrow(mapped_data), function(i) {
        tags$li(style = "margin-bottom: 5px;",
                paste(mapped_data$abbreviation[i], ": ", mapped_data$disease_name[i]))
      }))
    )
  }
  
  if (!is.null(legend_html)) {
    plot_output <- browsable(tagList(legend_html, vis_net))
  } else {
    plot_output <- vis_net
  }
  
  return(list(plot = plot_output,
              nodes = nodes_df,
              edges = edges_df_filtered))
}
