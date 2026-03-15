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

mean(degree(bologna_graph)) # 2.73
edge_density(bologna_graph) # 0.0003

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

vertex_attr_names(bologna_graph)
head(V(bologna_graph)$original_id)

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

# PLOTS!
# Bologna plot
# Retrieving geom data from the original QGIS file
nodes_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "nodes")
edges_sf <- st_read("02-data/Bologna_Rete_Finale.gpkg", layer = "edges")

ggplot() +
  geom_sf(data = edges_sf, color = "black", size = 0.2) +
  geom_sf(data = nodes_sf, color = "darkred", size = 0.5) +
  theme_void()

# Weight distribution plot
edges_sf2 <- edges_sf |> 
  as_tibble()

ggplot(edges_sf2, aes(x = weight)) +
  geom_histogram(fill = "firebrick", color = "white", bins = 50, alpha = 0.7) +
  scale_x_log10() + 
  theme_minimal() +
  labs(title = "Weight distribution (edges length)",
       x = "Edge weight (Log Scale)",
       y = "Frequency"
  )

# Abstract connections distribution plot
net <- readRDS("02-data/bologna_net.rds")
net_edges <- net |> 
  activate("edges")

ggraph(net_edges, layout = 'fr') +
  geom_edge_link(alpha = 0.5, color = "grey40", width = 0.3) +
  theme_graph() +
  labs(title = "Bologna: Abstract connections")


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

# 1. Louvain algorithm -----------------------------
set.seed(123)
LV <- cluster_louvain(bologna_graph, weights = E(bologna_graph)$proximity) # using proximity to find stronger communities
V(bologna_graph)$community_LV <- as.factor(membership(LV))

modularity(LV) # 0.98
length(LV) # 245 clusters

net <- net |> 
  activate("nodes") |> 
  mutate(community_LV = V(bologna_graph)$community_LV)

nodes_sf_plot <- net |> 
  activate("nodes") |>  
  as_tibble() |>  
  st_as_sf()

plot_LV <- ggplot() +
  geom_sf(data = edges_sf, color = "grey90", size = 0.5) +
  geom_sf(data = nodes_sf_plot, aes(color = as.factor(community_LV)), size = 0.6, alpha = 0.6) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Bologna: Louvain's algorithm")

print(plot_LV)


# 2. Label-Propagation algorithm on the backbone -----------------------------
set.seed(123)
LP <- cluster_label_prop(bologna_graph, weights = E(bologna_graph)$proximity) # LP algorithm is based on proximity
V(bologna_graph)$community_LP <- as.factor(membership(LP))

modularity(LP) # 0.82
length(LP) # 2350 clusters

net <- net |> 
  activate("nodes") |> 
  mutate(community_LP = V(bologna_graph)$community_LP)

nodes_sf_plot <- net |> 
  activate("nodes") |>  
  as_tibble() |>  
  st_as_sf()

LP_plot <- ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = nodes_sf_plot, aes(color = as.factor(community_LP)), size = 0.6, alpha = 0.6) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: Label Propagation Communities on Backbone",
    subtitle = "682 clusters"
  )

print(LP_plot)


# 4. Girvan-Newman algorithm -----------------------------
set.seed(123)
GN <- readRDS("02-data/GN.rds")
# GN <- cluster_edge_betweenness(bologna_graph, weights = E(bologna_graph)$weight) # GN algorithm is based on weight
# saveRDS(GN, file = "02-data/GN.rds")
V(bologna_graph)$community_GN <- as.factor(membership(GN))

modularity(GN) # 0.93
length(GN) # 133 clusters

net <- net |> 
  activate("nodes") |> 
  mutate(community_GN = V(bologna_graph)$community_GN)

nodes_sf_plot <- net |> 
  activate("nodes") |>  
  as_tibble() |>  
  st_as_sf()

plot_GN <- ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = nodes_sf_plot, aes(color = community_GN), size = 0.6, alpha = 0.6) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: Girvan-Newman Communities",
    subtitle = "133 clusters"
  )

print(plot_GN)


# 4. Girvan-Newman BINARY algorithm -----------------------------
set.seed(123)
GN_bin <- readRDS("02-data/GN_bin.rds")
# GN_bin <- cluster_edge_betweenness(bologna_graph, weights = NULL)
# saveRDS(GN_bin, file = "02-data/GN_bin.rds")
V(bologna_graph)$community_GN_bin <- as.factor(membership(GN_bin))

modularity(GN_bin) # 0.93
length(GN_bin) # 133 clusters

net <- net |> 
  activate("nodes") |> 
  mutate(community_GN_bin = V(bologna_graph)$community_GN_bin)

nodes_sf_plot <- net |> 
  activate("nodes") |>  
  as_tibble() |>  
  st_as_sf()

plot_GN_bin <- ggplot() +
  geom_sf(data = edges_sf, color = "grey80", size = 0.1) +
  geom_sf(data = nodes_sf_plot, aes(color = community_GN_bin), size = 0.6, alpha = 0.6) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Bologna: BINARY Girvan-Newman Communities",
    subtitle = "133 clusters"
  )

print(plot_GN_bin)


# 4.1 Girvan-Newman Main component
cat("Lowest components count possible in GN:", components(bologna_graph)$no, "\n")
comp <- components(bologna_graph)
giant_id <- which.max(comp$csize)
bologna_giant <- induced_subgraph(bologna_graph, V(bologna_graph)[comp$membership == giant_id])

cat("Number of nodes in cleaned net:", vcount(bologna_giant), 
    "(", vcount(bologna_graph) - vcount(bologna_giant), "isolate nodes were removed)\n")

V(bologna_graph)$is_giant <- V(bologna_graph)$name %in% V(bologna_giant)$name

nodes_sf <- nodes_sf |> 
  mutate(is_giant = V(bologna_graph)$is_giant[match(node_id, V(bologna_graph)$name)])

ggplot() +
  geom_sf(data = edges_sf, color = "grey90", size = 0.1) +
  geom_sf(data = nodes_sf, aes(color = is_giant), size = 0.4, alpha = 0.8) +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "red"), 
                     name = "Is it included in the main component?") +
  theme_void() +
  labs(title = "Bologna: Main component",
       subtitle = "Red points are not included in the main component")

# The main component is still very big and representative, we can definitely run 
# the GN algorithm on it without risking of conducting a partial analysis.

set.seed(123)
# GN_bin_giant <- cluster_edge_betweenness(bologna_giant, weights = NULL)
# saveRDS(GN_bin_giant, file = "02-data/GN_bin_giant.rds")
GN_bin_giant <- readRDS("02-data/GN_bin_giant.rds")
V(bologna_giant)$community_GN_giant <- as.factor(membership(GN_bin_giant))

modularity(GN_bin_giant) # 0.93
length(GN_bin_giant) # 41 clusters

# PLOT: Girvan-Newman Main component clusters over original neighbourhoods
df_GN_giant <- igraph::as_data_frame(bologna_giant, what = "vertices") |> 
  select(name, community_GN_giant)

df_GN_giant$name <- as.character(df_GN_giant$name)

net <- net |> 
  activate("nodes") |> 
  left_join(df_GN_giant, by = "name")

nodes_sf_plot <- net |> 
  activate("nodes") |> 
  as_tibble() |> 
  st_as_sf() |> 
  filter(!is.na(community_GN_giant))

nodes_clean <- nodes_sf |> 
  select(node_id, quartiere)

neighbourhoods <- nodes_clean |> 
  group_by(quartiere) |>  
  summarise(geometry = st_union(st_geometry(nodes_clean)[cur_group_rows()])) |> 
  st_convex_hull()

ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = nodes_sf_plot, aes(color = community_GN_giant), size = 1, alpha = 0.7) +
  geom_sf(data = neighbourhoods, fill = NA, color = "black", size = 0.4, linetype = "solid") +
  geom_sf_text(data = neighbourhoods, aes(label = quartiere), 
               size = 3, fontface = "bold", check_overlap = TRUE) +
  scale_color_discrete() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Bologna: Cluster GN vs Official neighbourhoods",
       subtitle = "Black lines refer to existing neighbourhoods")

# 4.2 Girvan-Newman 90, 25 and 6 clusters
V(bologna_giant)$cluster_6  <- as.factor(cut_at(GN_bin_giant, 6))
V(bologna_giant)$cluster_25 <- as.factor(cut_at(GN_bin_giant, 25))
V(bologna_giant)$cluster_90 <- as.factor(cut_at(GN_bin_giant, 90))

# PLOT: comparison 90 vs 25 vs 6 vs 41
final_map <- data.frame(
  node_id = V(bologna_giant)$name,
  cluster_6 = V(bologna_giant)$cluster_6,
  cluster_25 = V(bologna_giant)$cluster_25,
  cluster_90 = V(bologna_giant)$cluster_90,
  community_GN_giant = V(bologna_giant)$community_GN_giant
)

final_map <- final_map |> 
  mutate(node_id = as.character(node_id))

nodes_sf2 <- nodes_sf %>%
  mutate(node_id = as.character(node_id))

nodes_sf2 <- nodes_sf2 %>%
  left_join(final_map, by = "node_id")

print(head(nodes_sf2$node_id))
print(head(final_map$node_id))

# Clusters plot 
# Long dataframe
nodes_sf_long <- nodes_sf2 |> 
  select(node_id, cluster_6, cluster_25, cluster_90, community_GN_giant) %>%
  pivot_longer(
    cols = c(cluster_6, cluster_25, cluster_90, community_GN_giant),
    names_to = "scale",
    values_to = "cluster_id"
  ) |> 
  mutate(scale = factor(scale, levels = c("cluster_6", "cluster_25", "cluster_90", "community_GN_giant"),
                        labels = c("6 Cluster (Macro)", "25 Cluster (Meso)", "90 Cluster (Micro)", "GN Main Component (41)")))

# Facet_wrap
ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = nodes_sf_long, aes(color = as.factor(cluster_id)), size = 0.4) +
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~ scale) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(2, "lines")
  ) +
  labs(
    title = "Bologna: Girvan-Newman manual cluster division",
    subtitle = "6, 25, 90  and 41 clusters"
  )


# ANOVA test ------------------
# Anova on GN 6 clusters & 6 Bologna's Neighbourhoods
data_anova <- igraph::as_data_frame(bologna_giant, what = "vertices") |> 
  filter(!is.na(reddito))

model_quartieri <- lm(reddito ~ quartiere, data = data_anova)
model_GN6 <- lm(reddito ~ cluster_6, data = data_anova)

model_zone <- lm(reddito ~ zona_prossimita, data = data_anova)
model_GN25 <- lm(reddito ~ cluster_25, data = data_anova)

model_aree <- lm(reddito ~ area_statistica, data = data_anova)
model_GN90 <- lm(reddito ~ cluster_90, data = data_anova)

model_GN_giant <- lm(reddito ~ community_GN_giant, data = data_anova)

# Results
anova_results <- data.frame(
  model_anova = c("Quartieri (6)", "GN_6", "Zone di Prossimità (25)", "GN_25", 
                  "Area Statistica (90)", "GN_90", "GN Main Component"),
  R_quadrato_Adj = c(
    summary(model_quartieri)$adj.r.squared,
    summary(model_GN6)$adj.r.squared,
    summary(model_zone)$adj.r.squared,
    summary(model_GN25)$adj.r.squared,
    summary(model_aree)$adj.r.squared,
    summary(model_GN90)$adj.r.squared,
    summary(model_GN_giant)$adj.r.squared
  )
)

print(anova_results)

# Plots ANOVA (Zone prossimità, GN_25 e GN Main Component)
boxplot_zone <- ggplot(data_anova, aes(x = reorder(zona_prossimita, reddito, median), y = reddito)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income distribution - administrative 25 proximity zones",
       x = "Official Zones", y = "Median income")

print(boxplot_zone)


boxplot_GN_25 <- ggplot(data_anova, aes(x = reorder(cluster_25, reddito, median), y = reddito)) +
  geom_boxplot(fill = "firebrick", alpha = 0.7, outlier.size = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income distribution - cluster GN_25",
       x = "Morphological clusters (GN)", y = "Median income")

print(boxplot_GN_25)


boxplot_GN_giant <- ggplot(data_anova, aes(x = reorder(community_GN_giant, reddito, median), y = reddito)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7, outlier.size = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income distribution - GN Main Component",
       x = "Morphological clusters (GN Main)", y = "Median income")

print(boxplot_GN_giant)


cluster_income <- data_anova |> 
  group_by(community_GN_giant) |> 
  summarise(median_income_cluster = median(reddito, na.rm = TRUE))


nodes_sf_plot <- net |> 
  activate("nodes") |> 
  as_tibble() |>
  st_as_sf() |> 
  left_join(cluster_income, by = "community_GN_giant") |> 
  filter(!is.na(community_GN_giant))

cluster_stats <- nodes_sf_plot |>
  st_drop_geometry() |>
  group_by(community_GN_giant) |>
  mutate(reddito = as.numeric(as.character(reddito.mediano.per.area.statistica_Reddito.imponibile.mediano.dei.contribuenti..residenti))) |>
  summarise(
    reddito_mediano = median(reddito, na.rm = TRUE),
    n_nodi = n()) |> 
  filter(!is.na(reddito_mediano))

net <- net |> 
  activate("nodes") |> 
  left_join(cluster_stats, by = "community_GN_giant")

ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = net |> activate("nodes") |> as_tibble() |> st_as_sf(), 
          aes(color = reddito_mediano), size = 0.8) +
  geom_sf(data = neighbourhoods, fill = NA, color = "black", size = 0.5) +
  scale_color_viridis_c(option = "magma", name = "Reddito Mediano (€)") +
  theme_void() +
  labs(
    title = "Morfologia Stradale vs Reddito Mediano",
    subtitle = "Colorazione basata sulla mediana di ogni cluster (Girvan-Newman)",
    caption = "Confrontabile con la distribuzione delle 25 zone ufficiali"
  )






# Supponendo che il tuo modello si chiami res_anova
# res_anova <- aov(reddito ~ community_GN_giant, data = nodes_sf_plot)

par(mfrow = c(2, 2)) # Divide lo schermo in 4
plot(model_GN_giant)      # Genera i 4 grafici diagnostici




mean(degree(bologna_graph))
edge_density(bologna_graph)
