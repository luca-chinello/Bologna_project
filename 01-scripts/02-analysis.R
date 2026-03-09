# Student: Luca Chinello
# Student ID: 57340A
# University of Milan - DAPS&Co 2025/2026

###############################################################################
###             IMPORTING AND VISUALIZING NETWORK DATA                      ###
###############################################################################

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

## H0: testing several network-clustering algorithms

# Small fixes to the vertices
is.numeric(V(bologna_graph)$reddito) # check if the income variable is numeric, it is))
V(bologna_graph)$reddito <- as.numeric(V(bologna_graph)$reddito)

# Fixing the weight column in the edges attribute to make it a proximity measure (helps the algorithms)
# The Louvain algorithm supports weighted networks, however, the greater the weight, the stronger the connection between 
# two nodes. In our case, the weight is the length of the street segment, so we need to invert it to make it a 
# proximity measure (the shorter the street segment, the stronger the connection between two nodes).
E(bologna_graph)$proximity <- 1 / E(bologna_graph)$weight

# 1. Louvain algorithm (NO backbone)
set.seed(123)
louvain_full <- cluster_louvain(bologna_graph, weights = E(bologna_graph)$proximity)
V(bologna_graph)$louvain_full <- louvain_full$membership

modularity(louvain_full)
length(louvain_full) # 245 clusters

net <- readRDS("02-data/bologna_net.rds")
net <- net |> 
  activate("nodes") |> 
  mutate(community_full = V(bologna_graph)$louvain_full)

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), color = "grey90", size = 0.5) +
  geom_sf(data = st_as_sf(net, "nodes"), aes(color = as.factor(community_full)), size = 0.6) +
  scale_color_viridis_d(option = "turbo") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Bologna: Louvain's algorithm (NO backbone)")


# 2. Creating a weighted backbone. The Louvain algorithm will be tested again, along with other algorithms 
# that require a backbone to work properly (e.g., Girvan-Newman, Infomap, Leiden).

library(backbone)

# 1. Calcoliamo la centralità di ogni arco (Betweenness)
# Usiamo 'weight' (la lunghezza originale) come costo del percorso
# 'Pù è corta la strada, più è probabile che ci passino i percorsi minimi'
E(bologna_graph)$eb <- edge_betweenness(bologna_graph, weights = E(bologna_graph)$weight)

# 2. Identifichiamo la soglia per tenere il top 20% degli archi più centrali
threshold_eb <- quantile(E(bologna_graph)$eb, 0.80)

# 3. Creiamo la backbone morfologica
bologna_backbone <- delete_edges(bologna_graph, E(bologna_graph)[eb < threshold_eb])

# 4. Rimuoviamo i nodi isolati
bologna_backbone <- delete_vertices(bologna_backbone, V(bologna_backbone)[degree(bologna_backbone) == 0])

# 5. CONTROLLO FINALE (Quello che conta davvero)
comp <- components(bologna_backbone)
cat("Archi rimasti:", ecount(bologna_backbone), "\n")
cat("Dimensione Giant Component:", max(comp$csize), "nodi\n")

# Backbone plot
# 1. Calcoliamo la Betweenness e filtriamo DIRETTAMENTE sulla net
# Usiamo activate("edges") per dire a R che vogliamo lavorare sulle strade
backbone_net <- net |>
  activate("edges") |>
  mutate(eb = edge_betweenness(as.igraph(net), weights = weight)) |>
  filter(eb >= threshold_eb)

# 2. Ora st_as_sf() FUNZIONA al volo perché l'oggetto è ancora "spaziale"
backbone_edges_sf <- backbone_net |> 
  activate("edges") |> 
  as_tibble() |> 
  st_as_sf()

ggplot() +
  geom_sf(data = edges_sf, color = "grey50", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "firebrick", size = 0.6) +
  theme_void() +
  labs(title = "Morphological Backbone of Bologna")
