rm(list = ls())

library(sf)
library(sfnetworks)
library(dplyr)
library(igraph)

# 1. Loading the QGIS file
nodes_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "nodes")
edges_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "edges")

names(nodes_sf)
nodes_sf$qgis_id <- nodes_sf$node_id
nodes_sf <- nodes_sf |> 
  relocate(node_id) |> 
  select(where(~!all(is.na(.))))

# 2. Creating the net from the existing edges
net <- as_sfnetwork(edges_sf, directed = FALSE)

zone_urbanistiche <- st_read("02-data/zone_urbanistiche.geojson")
zone_urbanistiche <- st_transform(zone_urbanistiche, 32632)
head(zone_urbanistiche$zona_prossimita)

net <- net |> 
  activate("nodes") |> 
  st_join(nodes_sf, join = st_nearest_feature) |> 
  st_join(zone_urbanistiche |>  select(zona_prossimita), join = st_intersects) |> 
  mutate(name = as.character(qgis_id))

vertex_attr_names(net)
head(V(net)$name)

saveRDS(net, file = "02-data/bologna_net.rds")

# 3. Activating nodes and edges, dropping the geometry to let igraph work better
nodes_for_igraph <- net %>% 
  activate("nodes") %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>%
  mutate(node_id = row_number()) %>%
  relocate(node_id) %>%
  distinct(node_id, .keep_all = TRUE) %>%
  select(where(~!all(is.na(.))))

edges_for_igraph <- net %>% 
  activate("edges") %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  select(where(~!all(is.na(.))))

# 5. Crating the graph
bologna_graph <- graph_from_data_frame(
  d = edges_for_igraph, 
  vertices = nodes_for_igraph, 
  directed = FALSE
)

vertex_attr_names(net)
saveRDS(bologna_graph, file = "02-data/bologna_graph.rds")

# -----------

# Steps to read the data in the final script

library(igraph)
library(dplyr)

bologna_graph <- readRDS("02-data/bologna_graph.rds")
vertex_attr_names(grafo_bologna)
