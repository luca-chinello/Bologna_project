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

V(bologna_graph)$reddito <- V(bologna_graph)$reddito.mediano.per.area.statistica_Reddito.imponibile.mediano.dei.contribuenti..residenti
V(bologna_graph)$reddito <- as.numeric(V(bologna_graph)$reddito)

E(bologna_graph)$proximity <- 1 / E(bologna_graph)$weight

library(backbone)

# Base backbone extraction, fragmented
backbone_1 <- backbone_from_weighted(bologna_graph, model = "disparity", alpha = 0.05)

cat("Archi originali:", ecount(bologna_graph), "\n")
cat("Archi backbone:", ecount(backbone_1), "\n")
cat("Percentuale mantenuta:", (ecount(backbone_1) / ecount(bologna_graph)) * 100, "%\n")

comp <- components(backbone_1)
cat("Numero di isole:", comp$no, "\n")
cat("Dimensione della componente principale:", max(comp$csize), "nodi\n")

# Backbone with Serrano (2009) values calculated manually.
# Before applying the backbone function, I need to prepare the data. The backbone function supports igraph objects, 
# however, I need to instruct the algorithm to use proximity instead of weight as the measure of connection strength.
# Also, I want to keep all the attributes of the original graph, hence the choice not to use a matrix as input. 
# The backbone package crashes without prior specification about edges weights and nodes alpha, therefore, I will
# manually calculate these values as explained by Serrano et al (2009).

# Still too fragmented. The main component only has 8 nodes!

V(bologna_graph)$force <- strength(bologna_graph, weights = E(bologna_graph)$proximity)
V(bologna_graph)$degree <- degree(bologna_graph)

# Calculating the alpha value for each node
# Formula: (1 - weight/strength)^(degree - 1)
E(bologna_graph)$p_value <- sapply(1:ecount(bologna_graph), function(i) {
  nodes <- ends(bologna_graph, i)
  w <- E(bologna_graph)$proximity[i]
  
  # The formula consoiders two nodes and keeps all cases in which at least one node is relevant
  # Node 1
  s1 <- V(bologna_graph)$force[match(nodes[1], V(bologna_graph)$name)]
  k1 <- V(bologna_graph)$degree[match(nodes[1], V(bologna_graph)$name)]
  alpha1 <- if(k1 > 1) (1 - (w / s1))^(k1 - 1) else 0
  
  # Node 2
  s2 <- V(bologna_graph)$force[match(nodes[2], V(bologna_graph)$name)]
  k2 <- V(bologna_graph)$degree[match(nodes[2], V(bologna_graph)$name)]
  alpha2 <- if(k2 > 1) (1 - (w / s2))^(k2 - 1) else 0
  
  return(min(alpha1, alpha2))
})

# Backbone with p-value threshold of 0.05 (Serrano et al, 2009)
backbone_2 <- delete_edges(bologna_graph, E(bologna_graph)[p_value > 0.05])

# Cleaning isolate vertices
bologna_backbone <- delete_vertices(backbone_2, V(backbone_2)[degree(backbone_2) == 0])

cat("Archi originali:", ecount(bologna_graph), "\n")
cat("Archi backbone:", ecount(backbone_2), "\n")
cat("Percentuale mantenuta:", (ecount(backbone_2) / ecount(bologna_graph)) * 100, "%\n")

comp <- components(backbone_2)
cat("Numero di isole:", comp$no, "\n")
cat("Dimensione della componente principale:", max(comp$csize), "nodi\n")
