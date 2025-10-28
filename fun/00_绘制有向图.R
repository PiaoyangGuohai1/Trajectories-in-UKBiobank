library(igraph)
library(readr)
library(data.table)
library(visNetwork)

result_df <- read_csv("data/RR/UKB_cal_relativte_risk_with_direction.csv")
result_df <- data.table(result_df)
head(result_df)

table(result_df$direction_significant)
filtered_df <- subset(result_df, direction_significant == 1)

edges <- data.frame(
  from = filtered_df$D1,
  to = filtered_df$D2,
  weight = filtered_df$RR
)



g <- graph_from_data_frame(d = edges, directed = TRUE)

nodes <- data.frame(id = V(g)$name, 
                    label = V(g)$name, 
                    color.background = "#FF7F0E", 
                    color.border = "black",
                    shape = "circle") 

edges$width <- scales::rescale(log(edges$weight + 1), to = c(1, 5))

network <- visNetwork(nodes, edges) %>%
  visEdges(arrows = "to", color = list(color = "darkblue")) %>%
  visNodes(font = list(color = "black", size = 20, align = "center")) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123) %>%
  visPhysics(enabled = FALSE)
visSave(network, file = "network_plot.html")

annotation <- paste("<b>Node abbreviations and full names:</b><br>",
                    paste0(names(disease), ": ", disease, collapse = "<br>"))

html_output <- tags$div(
  tags$div(network, style = "display: inline-block; vertical-align: top;"),
  tags$div(HTML(annotation), style = "display: inline-block; margin-left: 20px; vertical-align: top; 
           padding: 10px; border: 1px solid black; background-color: #f9f9f9;")
)

save_html(html_output, "network_with_annotation.html")
