edges <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Data.csv", header = TRUE, stringsAsFactors = FALSE)

all_stations <- unique(c(edges$Departure.station, edges$Return.station))
all_stations_numeric <- sub("^([0-9]{4}).*", "\\1", all_stations)
all_stations_numeric <- all_stations_numeric[all_stations_numeric != "" & all_stations_numeric != "0000"]
all_stations_numeric <- sprintf("%04s", all_stations_numeric)

nodes_df <- data.frame(Station = all_stations_numeric, stringsAsFactors = FALSE)
write.csv(nodes_df, "/Users/sumermann/Desktop/421/Project_Latest/Nodes.csv", row.names = FALSE, quote = FALSE)

edges <- read.csv("/Users/sumermann/Desktop/421/Data.csv", header = TRUE, stringsAsFactors = FALSE)
edges_refined <- edges[, c("Departure.station", "Return.station")]
colnames(edges_refined) <- c("from", "to")

edges_refined$from <- sub("^([0-9]{4}).*", "\\1", edges_refined$from)
edges_refined$to <- sub("^([0-9]{4}).*", "\\1", edges_refined$to)
edges_refined <- edges_refined[!(edges_refined$from == "" | edges_refined$from == "0000" | edges_refined$from == "0"), ]
edges_refined$from <- sprintf("%04s", edges_refined$from)
edges_refined$to <- sprintf("%04s", edges_refined$to)

write.csv(edges_refined, "/Users/sumermann/Desktop/421/Project_Latest/Final_Edges.csv", row.names = FALSE, quote = FALSE)

edges <- read.csv("/Users/sumermann/Desktop/421/Project_Latest/Final_Edges.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(edges) <- c("from", "to")

edge_counts <- as.data.frame(table(paste(edges$from, edges$to)))
colnames(edge_counts) <- c("edge", "weight")

edge_split <- do.call(rbind, strsplit(as.character(edge_counts$edge), " "))
edges_weighted <- data.frame(from = edge_split[, 1], to = edge_split[, 2], weight = edge_counts$weight)

edges_weighted <- edges_weighted[!(edges_weighted$from == "0000" | edges_weighted$from == "0"), ]
edges_weighted <- edges_weighted[!(edges_weighted$to == "0000" | edges_weighted$to == "0"), ]
write.csv(edges_weighted, "/Users/sumermann/Desktop/421/Project_Latest/Data_Edges_2.csv", row.names = FALSE, quote = FALSE)
