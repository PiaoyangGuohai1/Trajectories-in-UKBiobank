plot_trajectory_network <- function(trajectories,
                                    weight,
                                    icd_color,
                                    death_weight = NULL,
                                    threshold_quantile = 0.75,
                                    fields_file = "fields/总数据库文件/all_fields.xlsx",
                                    include_legend = TRUE) {
  library(data.table)
  library(dplyr)
  library(visNetwork)
  library(htmltools)
  library(readxl)
  library(stringr)
  
  trajectories_split <- trajectories[, .(trajectory_name,
                                         nodes = strsplit(trajectory_name, "_")),
                                     by = 1:nrow(trajectories)]
  
  edges_list <- lapply(1:nrow(trajectories_split), function(i) {
    nodes <- trajectories_split$nodes[[i]]
    if (length(nodes) > 1) {
      lapply(1:(length(nodes) - 1), function(j) {
        data.frame(from = nodes[j], to = nodes[j + 1], stringsAsFactors = FALSE)
      })
    }
  }) %>% unlist(recursive = FALSE)
  
  edges_df <- rbindlist(edges_list) %>%
    .[, .N, by = .(from, to)] %>%
    setnames("N", "value")
  
  edges_df <- merge(edges_df,
                    weight[, .(from, to, prob)],
                    by = c("from", "to"), all.x = TRUE)
  
  nodes <- unique(c(edges_df$from, edges_df$to))
  nodes_df <- data.frame(
    id    = nodes,
    label = nodes,
    stringsAsFactors = FALSE
  )
  
  nodes_df <- nodes_df %>%
    left_join(
      icd_color %>% select(name, color) %>% rename(id = name),
      by = "id"
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
  
  nodes_df <- merge(
    nodes_df,
    mapped_data[, c("abbreviation", "disease_name")],
    by.x = "id", by.y = "abbreviation", all.x = TRUE
  )
  nodes_df$title <- ifelse(
    is.na(nodes_df$disease_name),
    nodes_df$label,
    paste0("Node: ", nodes_df$label, "<br>Disease: ", nodes_df$disease_name)
  )
  
  if (!is.null(death_weight)) {
    nodes_df <- merge(
      nodes_df,
      death_weight[, .(from, prob)],
      by.x = 'id', by.y = 'from', all.x = TRUE
    )
    nodes_df$label <- ifelse(
      is.na(nodes_df$prob),
      nodes_df$label,
      paste0(nodes_df$label, "\nProb: ", round(nodes_df$prob, 3))
    )
    nodes_df$title <- ifelse(
      is.na(nodes_df$prob),
      nodes_df$title,
      paste0(nodes_df$title, "<br>Death probability: ", round(nodes_df$prob, 3))
    )
  }
  
  edges_df[, title := paste("From", from, "→", to, "<br>Prob:", round(prob, 3))]
  threshold <- quantile(edges_df$prob, threshold_quantile, na.rm = TRUE)
  edges_df_filtered <- edges_df[prob >= threshold]
  edges_df_filtered[, value := prob / max(prob, na.rm = TRUE)]
  
  nodes_in_edges <- unique(c(edges_df_filtered$from, edges_df_filtered$to))
  nodes_df_filtered <- nodes_df[nodes_df$id %in% nodes_in_edges, ]
  
  legend_html <- NULL
  if (include_legend) {
    legend_html <- tags$div(
      style = "max-width: 800px; padding: 10px; border: 1px solid #ccc; margin-bottom: 15px;",
      tags$h4("Legend: Disease Abbreviations"),
      tags$ul(lapply(1:nrow(mapped_data), function(i) {
        tags$li(
          style = "margin-bottom: 5px;",
          paste0(mapped_data$abbreviation[i], ": ", mapped_data$disease_name[i])
        )
      }))
    )
  }
  
  if (!is.null(death_weight)) {
    nodes_df_filtered <- nodes_df_filtered %>%
      mutate(
        size = scales::rescale(prob, to = c(10, 30), from = range(prob, na.rm = TRUE))
      )
  } else {
    nodes_df_filtered$size <- 20
  }
  
  network <- visNetwork(nodes_df_filtered, edges_df_filtered,
                        height = "600px", width = "100%") %>%
    visNodes(
      shape = "dot",
      size  = nodes_df_filtered$size,
      color = list(
        background = nodes_df_filtered$color,
        border     = "#2B7CE9"
      ),
      font  = list(size = 18, color = "black"),
      title = nodes_df_filtered$title
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      scaling = list(min = 1, max = 10),
      color   = list(color = "#A9A9A9"),
      font    = list(size = 12, color = "gray40")
    ) %>%
    visOptions(
      highlightNearest  = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visIgraphLayout(layout = "layout_with_kk")
  
  
  if (include_legend) {
    size_legend <- tags$div(
      style = "margin-top:10px;",
      tags$p("Node size represents five-year mortality probability (scaled)."),
      tags$ul(
        tags$li("Small (10) = Lowest probability"),
        tags$li("Medium (20) = Median probability"),
        tags$li("Large (30) = Highest probability")
      )
    )
    legend_html <- tagList(legend_html, size_legend)
  }
  
  if (!is.null(legend_html)) {
    return(list(
      plot  = browsable(tagList(legend_html, network)),
      nodes = nodes_df_filtered,
      edges = edges_df_filtered
    ))
  } else {
    return(list(
      plot  = network,
      nodes = nodes_df_filtered,
      edges = edges_df_filtered
    ))
  }
}
