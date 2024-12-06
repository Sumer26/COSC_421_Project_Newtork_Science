library(igraph)
library(RColorBrewer)
library(dendextend)
library(ggplot2)

avg_heights <- c()
community_labels <- c()
num_nodes <- c()

cluster_path <- "/Users/sumermann/Desktop/421/Project_Latest/"

for (i in 1:4) {
  nodes <- read.csv(paste0(cluster_path, "Cluster_", i, "_Nodes.csv"), header = TRUE, stringsAsFactors = FALSE)
  edges <- read.csv(paste0(cluster_path, "Cluster_", i, "_Edges.csv"), header = TRUE, stringsAsFactors = FALSE)
  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
  
  cluster_colors <- brewer.pal(n = 4, name = "Dark2")
  V(graph)$color <- cluster_colors[i]
  layout <- layout_with_fr(graph, niter = 1000)
  
  plot(graph, layout = layout, main = paste("Community", i, "Graph"),
       vertex.size = 13, vertex.label = V(graph)$name, vertex.label.cex = 0.6,
       vertex.label.color = "black", edge.width = E(graph)$weight / max(E(graph)$weight) * 2,
       edge.color = "grey30", edge.arrow.size = 0.1, edge.arrow.width = 1, edge.arrow.mode = 1)
  
  adj_matrix <- as_adjacency_matrix(graph, attr = "weight", sparse = FALSE)
  distance_matrix <- as.dist(1 / (adj_matrix + 1))
  hc_community <- hclust(distance_matrix, method = "ward.D2")  # Use distinct hc_community variable
  dend <- as.dendrogram(hc_community)
  dend <- color_branches(dend, k = 4, col = cluster_colors[i])
  dend <- set(dend, "labels_cex", 0.3)
  plot(dend, main = paste("Dendrogram of Community", i, "Stations"), xlab = "Stations", ylab = "Height",
       sub = paste("Dendrogram with Colored Branches for Community", i), cex.main = 1.2)
  
  avg_cluster_height_community <- mean(hc_community$height)
  print(paste("Average Cluster Height for Community", i, ":", round(avg_cluster_height_community, 3)))
  
  avg_heights <- c(avg_heights, avg_cluster_height_community)
  community_labels <- c(community_labels, paste("Community", i))
  num_nodes <- c(num_nodes, vcount(graph))
}

full_data_nodes <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Data_Nodes.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_edges <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Data_Edges_2.csv", header = TRUE, stringsAsFactors = FALSE)
full_graph <- graph_from_data_frame(d = full_data_edges, vertices = full_data_nodes, directed = TRUE)

adj_matrix <- as_adjacency_matrix(full_graph, attr = "weight", sparse = FALSE)
distance_matrix <- as.dist(1 / (adj_matrix + 1))
hc <- hclust(distance_matrix, method = "ward.D2")
clusters <- cutree(hc, k = 4)
names(clusters) <- V(full_graph)$name
V(full_graph)$cluster <- clusters

cluster_colors <- brewer.pal(n = 4, name = "Dark2")
V(full_graph)$color <- cluster_colors[V(full_graph)$cluster]

full_graph_layout <- layout_with_fr(full_graph, niter = 5000)
plot(full_graph, layout = full_graph_layout, main = "Collective Network Graph of All Communities",
     vertex.size = 5, vertex.label = NA, vertex.color = V(full_graph)$color,
     edge.width = E(full_graph)$weight / max(E(full_graph)$weight) * 1.5,
     edge.color = "grey30", edge.arrow.size = 0.05, edge.arrow.width = 1, edge.arrow.mode = 1)

dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 4, col = cluster_colors)
dend <- set(dend, "labels_cex", 0.3)
plot(dend, main = "Collective Dendrogram of Major Stations", xlab = "Stations",
     ylab = "Height", sub = "Collective Dendrogram with Colored Branches Representing Clusters", cex.main = 1.2)

avg_cluster_height <- mean(hc$height)
print(paste("Average Cluster Height for the Complete Graph:", round(avg_cluster_height, 3)))

avg_heights <- c(avg_heights, avg_cluster_height)
community_labels <- c(community_labels, "Complete Graph")
num_nodes <- c(num_nodes, vcount(full_graph))

plot_data <- data.frame(Community = community_labels, AvgHeight = avg_heights, NumNodes = num_nodes)

ggplot(plot_data, aes(x = Community, y = AvgHeight, fill = Community)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Cluster Height of Each Community and Complete Graph",
       x = "Community",
       y = "Average Cluster Height") +
  scale_fill_brewer(palette = "Dark2")

