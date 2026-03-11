rm(list = ls())

# Student: Luca Chinello
# Student ID: 57340A
# University of Milan - DAPS&Co 2025/2026

# Libraries loading, data import and visualisation -----------------------------
# Load necessary libraries
library(tidyverse)
library(igraph)  
library(ggraph) 
library(tidygraph)
library(patchwork)
library(sf)
library(ggplot2)
library(dplyr)

# Load the graph object
bologna_graph <- readRDS("02-data/bologna_graph.rds")

# Check the structure of the graph
class(bologna_graph)
bologna_graph # Undirected, Named, Weighted, Not bipartite - 8056 nodes, 11000 edges

vertex_attr_names(bologna_graph)
edge_attr_names(bologna_graph)

# Some useful node attributes to check:
head(V(bologna_graph)$area_statistica) # each node belongs to one of the 90 statistical areas of Bologna
head(V(bologna_graph)$quartiere) # each node belongs to one of the 6 quartieri of Bologna

head(V(bologna_graph)$reddito.mediano.per.area.statistica_Reddito.imponibile.mediano.dei.contribuenti..residenti)
V(bologna_graph)$reddito <- V(bologna_graph)$reddito.mediano.per.area.statistica_Reddito.imponibile.mediano.dei.contribuenti..residenti
head(V(bologna_graph)$reddito) # vertex name simplified, each vertex is associated with the median income of one of 
                               # the 90 statistical areas of Bologna

# Some useful edge attributes to check:
head(E(bologna_graph)$name)
head(E(bologna_graph)$weight) # each edge has a weight attribute, which is the length of the street segment in meters

# Crating nodes and edges tables
# Nodes
V(bologna_graph)$address <- V(bologna_graph)$name # assign the vertex name to a new attribute called "address"
V(bologna_graph)$name <- seq_len(vcount(bologna_graph)) # assign a new ID to each vertex to create a data frame
nodes_tab <- as_data_frame(bologna_graph, what = "vertices")

nodes_tab <- nodes_tab |> 
  relocate(name, address, area_statistica, quartiere, reddito, fid_2)
summary(nodes_tab)

# Edges
edges_tab <- as_data_frame(bologna_graph, what = "edges")
summary(edges_tab)

# Quick plot!
# Retrieving geom data from the original QGIS file
nodes_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "nodes")
edges_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "edges")

ggplot() +
  geom_sf(data = edges_sf, color = "black", size = 0.2) +
  geom_sf(data = nodes_sf, color = "darkred", size = 0.5) +
  theme_void()

# Analysis preparation -----------------------------
# H0 - comparing the communities detected by the algorithms with the existing neighbourhoods of Bologna (quartieri)

# Small fixes to the vertices
is.numeric(V(bologna_graph)$reddito) # check if the income variable is numeric, it is))
V(bologna_graph)$reddito <- as.numeric(V(bologna_graph)$reddito)

# Fixing the weight column in the edges attribute to make it a proximity measure (helps the algorithms)
# The Louvain algorithm supports weighted networks, however, the greater the weight, the stronger the connection between 
# two nodes. In our case, the weight is the length of the street segment, so we need to invert it to make it a 
# proximity measure (the shorter the street segment, the stronger the connection between two nodes).
E(bologna_graph)$proximity <- 1 / E(bologna_graph)$weight

# 1. Louvain algorithm (NO backbone) -----------------------------
set.seed(123)
louvain_full <- cluster_louvain(bologna_graph, weights = E(bologna_graph)$proximity) # using proximity to find stronger communities
V(bologna_graph)$louvain_full <- louvain_full$membership

modularity(louvain_full)
length(louvain_full) # 245 clusters

net <- readRDS("02-data/bologna_net.rds")
net <- net |> 
  activate("nodes") |> 
  mutate(community_full = V(bologna_graph)$louvain_full)

nodes_sf_plot <- net |> 
  activate("nodes") |>  
  as_tibble() |>  
  st_as_sf()

ggplot() +
  geom_sf(data = edges_sf, color = "grey90", size = 0.5) +
  geom_sf(data = nodes_sf_plot, aes(color = as.factor(community_full)), size = 0.6) +
  scale_color_viridis_d(option = "turbo") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Bologna: Louvain's algorithm (NO backbone)")

# 2. Weighted backbone -----------------------------
# Creating a weighted backbone. The Louvain algorithm will be tested again, along with other algorithms 
# that require a backbone to work properly (e.g., Girvan-Newman, Infomap, Leiden).

library(backbone)

# DA RIVEDERE E CORREGGEREEEEEEEEEE
# After several backbone attempts (see script 03-backbone_attempts.R), I opted for a morphological backbone instead of a
# statistical one. The main reason is that the latter was too fragmented, with a giant component of only 8 nodes, which
# is too little for a city. The morphological backbone considers the betweenness centrality of the edges, 
# which is a measure of how many shortest paths pass through an edge.
# 'Pù è corta la strada, più è probabile che ci passino i percorsi minimi'
E(bologna_graph)$eb <- edge_betweenness(bologna_graph, weights = E(bologna_graph)$weight)

# Keeping only top 20% of the edges with the highest betweenness centrality 
threshold_eb <- quantile(E(bologna_graph)$eb, 0.80)

# Creating the actaul backbone by deleting the edges with betweenness centrality below the threshold
bologna_backbone <- delete_edges(bologna_graph, E(bologna_graph)[eb < threshold_eb])

# Removing isolate nodes that would add noise
bologna_backbone <- delete_vertices(bologna_backbone, V(bologna_backbone)[degree(bologna_backbone) == 0])

# Final check
comp <- components(bologna_backbone)
cat("Archi rimasti:", ecount(bologna_backbone), "\n")
cat("Dimensione Giant Component:", max(comp$csize), "nodi\n")

# Backbone plot
# Plotting the sfnetwork object to keep the geometry attributes. I will filter this net according to the backbone 
# edges, then I will plot the original edges in grey and the backbone edges in red.
backbone_net <- net |>
  activate("edges") |>
  mutate(eb = edge_betweenness(as.igraph(net), weights = weight)) |>
  filter(eb >= threshold_eb)

backbone_edges_sf <- backbone_net |> 
  activate("edges") |> 
  as_tibble() |> 
  st_as_sf()

ggplot() +
  geom_sf(data = edges_sf, color = "grey50", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "firebrick", size = 0.6) +
  theme_void() +
  labs(title = "Morphological Backbone of Bologna")

# 3. Louvain algorithm on the backbone -----------------------------
set.seed(123)
LV <-  cluster_louvain(bologna_backbone, weights = E(bologna_backbone)$proximity)

V(bologna_backbone)$LV <- as.factor(membership(LV))
modularity(LV) #0.98
length(LV) # 138 clusters

nodes_backbone_sf <- net |> 
  activate("nodes") |> 
  mutate(node_id = row_number()) |> 
  mutate(community_LV = V(bologna_backbone)$LV[match(node_id, V(bologna_backbone)$name)]) |> 
  as_tibble() |> 
  filter(!is.na(community_LV)) |> 
  st_as_sf()

plot_LV <- ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "grey50", size = 0.3) +
  geom_sf(data = nodes_backbone_sf, aes(color = community_LV), size = 0.7, alpha = 0.7) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: Louvain Communities on Backbone",
    subtitle = "138 clusters" 
  )

print(plot_LV)

# 4. Girvan-Newman algorithm on the backbone -----------------------------
set.seed(123)
GN <- cluster_edge_betweenness(bologna_backbone, weights = E(bologna_backbone)$weight) # GN algorithm is based on weight
V(bologna_backbone)$GN <- as.factor(membership(GN))

modularity(GN) # 0.92
length(GN) # 74 clusters

nodes_backbone_sf <- nodes_backbone_sf |> 
  mutate(community_GN = V(bologna_backbone)$GN[match(node_id, V(bologna_backbone)$name)]) |> 
  as_tibble() |> 
  filter(!is.na(community_GN)) |> 
  st_as_sf()

plot_GN <- ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "grey50", size = 0.3) +
  geom_sf(data = nodes_backbone_sf, aes(color = community_GN), size = 0.7, alpha = 0.7) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: Girvan-Newman Communities on Backbone",
    subtitle = "74 clusters"
  )

print(plot_GN)

# 5. Label-Propagation algorithm on the backbone -----------------------------
set.seed(123)
LP <- cluster_label_prop(bologna_backbone, weights = E(bologna_backbone)$proximity) # LP algorithm is based on proximity
V(bologna_backbone)$LP <- as.factor(membership(LP))

modularity(LP) # 0.89
length(LP) # 682 clusters

nodes_backbone_sf <- nodes_backbone_sf |> 
  mutate(community_LP = V(bologna_backbone)$LP[match(node_id, V(bologna_backbone)$name)]) |> 
  as_tibble() |> 
  filter(!is.na(community_LP)) |> 
  st_as_sf()

ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "grey50", size = 0.3) +
  geom_sf(data = nodes_backbone_sf, aes(color = community_LP), size = 0.7, alpha = 0.7) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: Label Propagation Communities on Backbone",
    subtitle = "682 clusters"
  )

# GN and LV comparison -----------------------------
plot_GN + plot_LV

# Checks on GN
# Creating a table of the number of nodes in each cluster
GN_counts <- as.data.frame(table(V(bologna_backbone)$GN))
colnames(GN_counts) <- c("cluster_id", "node_count")

GN_counts$node_count <- as.numeric(as.character(GN_counts$node_count))

isolate_nodes <- sum(GN_counts$node_count == 1)
valid_clusters <- sum(GN_counts$node_count > 1)

# Summary
cat("--- GIRVAN-NEWMAN CHECK ---\n")
cat("Total clusters:      ", nrow(GN_counts), "\n")
cat("Isolated clusters (1): ", isolate_nodes, "\n") # 0
cat("Real clusters (>1):  ", valid_clusters, "\n") # 74 clusters
cat("Nodes per cluster mean:  ", mean(GN_counts$node_count), "\n") # 29 nodes, good!
cat("Nodes per cluster median:  ", median(GN_counts$node_count), "\n") # 37 nodes, good!


# PLOT: Girvan-Newman clusters over original neighbourhoods
nodes_clean <- nodes_backbone_sf |> 
  select(node_id, quartiere)

neighbourhoods <- nodes_clean |> 
  group_by(quartiere) |>  
  summarise(geometry = st_union(st_geometry(nodes_clean)[cur_group_rows()])) |> 
  st_convex_hull()

ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = nodes_backbone_sf, aes(color = community_GN), size = 1, alpha = 0.7) +
  geom_sf(data = neighbourhoods, fill = NA, color = "black", size = 0.4, linetype = "solid") +
  geom_sf_text(data = neighbourhoods, aes(label = quartiere), 
               size = 3, fontface = "bold", check_overlap = TRUE) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Bologna: Cluster GN vs Official neighbourhoods",
       subtitle = "Black lines refer to existing neighbourhoods")

