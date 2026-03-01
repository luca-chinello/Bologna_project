library(sf)
library(sfnetworks)
library(dplyr)

# 1. Loading the QGIS file, layer = "edges"
edges_sf <- st_read("02-data/Bologna_Rete2.gpkg", layer = "edges")

# 2. Creating the net from the existing edges
net <- as_sfnetwork(edges_sf, directed = FALSE)

# 3. Assigning IDs to nodes
nodes_ID <- net %>% 
  activate("nodes") %>% 
  st_as_sf()

# 4. Creating "from"and "to" columns for edges
arches <- net %>% 
  activate("edges") %>% 
  st_as_sf()

# 5. Exporting the dataset, CSV and gpkg formats
# CSV
write.csv(st_drop_geometry(nodes_ID), "02-data/Bologna_Nodes_igraph.csv", row.names = FALSE)
write.csv(st_drop_geometry(arches), "02-data/Bologna_Edges_igraph.csv", row.names = FALSE)

# GeoPackage:
st_write(nodes_ID, "02-data/Dataset_Bologna_Network.gpkg", layer = "nodes", append = FALSE)
st_write(arches, "02-data/Dataset_Bologna_Network.gpkg", layer = "edges", append = FALSE)
