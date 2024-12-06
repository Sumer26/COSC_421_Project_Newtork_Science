
library(igraph)
library(dplyr)
nodes <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Data_Nodes.csv", header = TRUE, stringsAsFactors = FALSE)
edges <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Data_Edges_2.csv", header = TRUE, stringsAsFactors = FALSE)

graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

adj_matrix <- as_adjacency_matrix(graph, attr = "weight", sparse = FALSE)
distance_matrix <- as.dist(1 / (adj_matrix + 1))
hc <- hclust(distance_matrix, method = "ward.D2")


clusters <- cutree(hc, k = 4)

V(graph)$cluster <- clusters

for (i in unique(clusters)) {
  cluster_nodes <- nodes %>% filter(Station %in% V(graph)$name[clusters == i])
  
  cluster_edges <- edges %>%
    filter(from %in% cluster_nodes$Station & to %in% cluster_nodes$Station)

  write.csv(cluster_nodes, paste0("/Users/sumermann/Desktop/421/Project_Latest/Cluster_", i, "_Nodes.csv"), row.names = FALSE, quote = FALSE)
  write.csv(cluster_edges, paste0("/Users/sumermann/Desktop/421/Project_Latest/Cluster_", i, "_Edges.csv"), row.names = FALSE, quote = FALSE)
}
