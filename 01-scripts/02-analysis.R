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
library(magick)

# Load the graph object
bologna_graph <- readRDS("02-data/bologna_graph.rds")

# Check the structure of the graph
class(bologna_graph)
bologna_graph # Undirected, Named, Weighted, Not bipartite - 8056 nodes, 11000 edges

vertex_attr_names(bologna_graph)
edge_attr_names(bologna_graph)

mean(degree(bologna_graph)) # 2.73
min(degree(bologna_graph)) #1
max(degree(bologna_graph)) # 6
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

set.seed(123)
layout.mat.fr <- layout_(graph=bologna_graph, layout=with_fr())
layout.mat.fr
plot(bologna_graph, layout=layout.mat.fr, edge.arrow.size=0.5)


# Weight distribution plot
edges_sf2 <- edges_sf |> 
  as_tibble()

weight_plot <- ggplot(edges_sf2, aes(x = weight)) +
  geom_histogram(fill = "firebrick", color = "white", bins = 50, alpha = 0.7) +
  scale_x_log10() + 
  theme_minimal() +
  labs(title = "Weight distribution (edges length)",
       x = "Edge weight (Log Scale)",
       y = "Frequency"
  )
print(weight_plot)
# ggsave("03-outputs/weight_plot.png", plot = weight_plot, width = 8, height = 6)

# Abstract connections distribution plot
net <- readRDS("02-data/bologna_net.rds")
net_edges <- net |> 
  activate("edges")

#abstract_plot <- ggraph(net_edges, layout = 'stress') +
#  geom_edge_link(alpha = 0.5, color = "black", width = 0.3) +
#  theme_graph() +
#  labs(title = "Bologna: Abstract connections")

# ggsave("03-outputs/abstract_plot.png", plot = abstract_plot, width = 8, height = 6)
abstract_plot <- image_read("03-outputs/abstract_plot.png")
print(abstract_plot)


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
# ggsave("03-outputs/plot_LV.png", plot = plot_LV, width = 8, height = 6)

# 2. Label-Propagation algorithm -----------------------------
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
# ggsave("03-outputs/LP_plot.png", plot = LP_plot, width = 8, height = 6)

# 3 Girvan-Newman algorithm -----------------------------
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
# ggsave("03-outputs/plot_GN.png", plot = plot_GN, width = 8, height = 6)

# 4 Girvan-Newman BINARY algorithm -----------------------------
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
# ggsave("03-outputs/plot_GN_bin.png", plot = plot_GN_bin, width = 8, height = 6)

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

main_comp <- ggplot() +
  geom_sf(data = edges_sf, color = "grey90", size = 0.1) +
  geom_sf(data = nodes_sf, aes(color = is_giant), size = 0.4, alpha = 0.8) +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "red"), 
                     name = "Is it included in the main component?") +
  theme_void() +
  labs(title = "Bologna: Main component",
       subtitle = "Red points are not included in the main component,
       7791 nodes left out of 8056, 265 isolate nodes were removed")

print(main_comp)
# ggsave("03-outputs/main_comp.png", plot = main_comp, width = 8, height = 6)

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

GN_giant_plot <- ggplot() +
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

print(GN_giant_plot)
# ggsave("03-outputs/GN_giant_plot.png", plot = GN_giant_plot, width = 8, height = 6)

# 4.2 Girvan-Newman 90, 25 and 6 clusters
V(bologna_giant)$cluster_6  <- as.factor(cut_at(GN_bin_giant, 6))
V(bologna_giant)$cluster_25 <- as.factor(cut_at(GN_bin_giant, 25))
V(bologna_giant)$cluster_90 <- as.factor(cut_at(GN_bin_giant, 90))

# TABLE: crosstab between GN_giant (41 clusters) and existing neighbourhoods
library(janitor)

data_cross <- igraph::as_data_frame(bologna_giant, what = "vertices")

crosstab_tabyl <- data_cross |> 
  tabyl(quartiere, community_GN_giant) |> 
  adorn_percentages("col") |>   
  adorn_pct_formatting(digits = 1) |>   
  adorn_ns()
print(crosstab_tabyl)

t_plot <- data_cross |> 
  tabyl(quartiere, community_GN_giant, show_na = FALSE) |> 
  adorn_percentages("col")

# df_long for plotting
df_long <- t_plot |> 
  pivot_longer(
    cols = -quartiere,  
    names_to = "Cluster",       
    values_to = "Proporzione"
  )

cluster_order <- df_long |> 
  group_by(Cluster) |>
  slice_max(order_by = Proporzione, n = 1, with_ties = FALSE) |>
  arrange(quartiere, Cluster) |> 
  pull(Cluster)

df_long <- df_long |> 
  mutate(Cluster = factor(Cluster, levels = cluster_order)) |>
  mutate(Proporzione_long = ifelse(Proporzione <= 0, NA, Proporzione))

# TABLE plot
crosstab_plot <-  ggplot(df_long, aes(x = Cluster, y = quartiere, fill = Proporzione_long)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "cividis", 
    labels = scales::percent,
    na.value = "white",
    begin = 0.9,
    end = 0,  
    limits = c(0.001, 1)
  ) +
  theme_minimal() +
  labs(
    title = "Morphological matching of Neighbourhoods and Clusters (GN Main Component)",
    x = "41 Clusters (Girvan-Newman)",
    y = "6 Neighbourhoods",
    fill = "Matching %"
  ) +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank()
        )

print(crosstab_plot)
# ggsave("03-outputs/crosstab_plot.png", plot = crosstab_plot, width = 8, height = 6)


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
clusters_facet <- ggplot() +
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

print(clusters_facet)
# ggsave("03-outputs/clusters_facet.png", plot = clusters_facet, width = 12, height = 10)

# ANOVA test ------------------
# Anova on GN 6 clusters & 6 Bologna's Neighbourhoods
data_anova <- igraph::as_data_frame(bologna_giant, what = "vertices") |> 
  filter(!is.na(reddito))

model_quartieri <- lm(reddito ~ quartiere, data = data_anova)
model_GN6 <- lm(reddito ~ cluster_6, data = data_anova)
aov_quartieri <- aov(reddito ~ quartiere, data = data_anova)
aov_GN6 <- aov(reddito ~ cluster_6, data = data_anova)

model_zone <- lm(reddito ~ zona_prossimita, data = data_anova)
model_GN25 <- lm(reddito ~ cluster_25, data = data_anova)
aov_zone <- aov(reddito ~ zona_prossimita, data = data_anova)
aov_GN25 <- aov(reddito ~ cluster_25, data = data_anova)

model_aree <- lm(reddito ~ area_statistica, data = data_anova)
model_GN90 <- lm(reddito ~ cluster_90, data = data_anova)
aov_aree <- aov(reddito ~ area_statistica, data = data_anova)
aov_GN90 <- aov(reddito ~ cluster_90, data = data_anova)

model_GN_giant <- lm(reddito ~ community_GN_giant, data = data_anova)
aov_GN_giant <- aov(reddito ~ community_GN_giant, data = data_anova)

summary(model_quartieri)
summary(model_GN6)
summary(aov_quartieri)
summary(aov_GN6)

summary(model_zone)
summary(model_GN25)
summary(aov_zone)
summary(aov_GN25)

summary(model_aree)
summary(model_GN90)
summary(aov_aree)
summary(aov_GN90)
summary(model_GN_giant)

summary(aov_GN_giant)

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
data_anova <- data_anova |> 
  filter(!is.na(zona_prossimita))

boxplot_zone <- ggplot(data_anova, aes(x = reorder(zona_prossimita, reddito, median), y = reddito)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.5, na.rm = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income dist. - 25 proximity zones",
       x = "Official Zones", y = "Median income")

print(boxplot_zone)
# ggsave("03-outputs/boxplot_zone.png", plot = boxplot_zone, width = 8, height = 6)


boxplot_GN_25 <- ggplot(data_anova, aes(x = reorder(cluster_25, reddito, median), y = reddito)) +
  geom_boxplot(fill = "firebrick", alpha = 0.7, outlier.size = 0.5, na.rm = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income dist. - GN_25",
       x = "Morphological clusters (GN)", y = "Median income")

print(boxplot_GN_25)
# ggsave("03-outputs/boxplot_GN_25.png", plot = boxplot_GN_25, width = 8, height = 6)

boxplot_25_comp <- boxplot_zone + boxplot_GN_25 + plot_layout(ncol = 2)
print(boxplot_25_comp)
# ggsave("03-outputs/boxplot_25_comp.png", plot = boxplot_25_comp, width = 12, height = 6)

boxplot_GN_giant <- ggplot(data_anova, aes(x = reorder(community_GN_giant, reddito, median), y = reddito)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7, outlier.size = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Median income distribution - GN Main Component",
       x = "Morphological clusters (GN Main)", y = "Median income")

print(boxplot_GN_giant)
# ggsave("03-outputs/boxplot_GN_giant.png", plot = boxplot_GN_giant, width = 8, height = 6)


# PLOT median income by cluster
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

income_per_clusters <- ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = net |> activate("nodes") |> as_tibble() |> st_as_sf(), 
          aes(color = reddito_mediano), size = 0.8) +
  geom_sf(data = neighbourhoods, fill = NA, color = "black", size = 0.5) +
  scale_color_viridis_c(option = "magma", name = "Median Income (€)") +
  theme_void() +
  labs(
    title = "Bologna Morphology vs Median Income",
    subtitle = "Median income distribution across the GN main component clusters (41)"
  )

print(income_per_clusters)
# ggsave("03-outputs/income_per_clusters.png", plot = income_per_clusters, width = 8, height = 6)


# PLOT node degree by cluster
V(bologna_giant)$degree <- as.numeric(degree(bologna_giant))

df_degree <- data.frame(
  name = as.character(V(bologna_giant)$name),
  degree = as.numeric(degree(bologna_giant))
)

net <- net |> 
  activate("nodes") |> 
  mutate(name = as.character(name)) |>
  left_join(df_degree, by = "name")

nodes_sf_plot2 <- net |> 
  activate("nodes") |> 
  as_tibble() |>
  st_as_sf() |> 
  filter(!is.na(degree))

degree_per_clusters <- ggplot() +
  geom_sf(data = edges_sf, color = "grey70", size = 0.1) +
  geom_sf(data = nodes_sf_plot2, aes(color = degree), size = 0.8) +
  geom_sf(data = neighbourhoods, fill = NA, color = "black", size = 0.5) +
  scale_color_viridis_c(option = "viridis", name = "Node Degree") +
  theme_void() +
  labs(
    title = "Bologna: Node Connectivity (Degree)",
  )

# ggsave("03-outputs/degree_per_clusters.png", plot = degree_per_clusters, width = 8, height = 6)
print(degree_per_clusters)


# PLOT: avg node degree in 25 proximity zones
node_data <- as_tibble(igraph::as_data_frame(bologna_giant, what = "vertices"))

zone_stats <- node_data |>
  filter(!is.na(reddito)) |>
  filter(!is.na(zona_prossimita)) |> 
  group_by(zona_prossimita) |>
  summarise(
    avg_degree = mean(degree, na.rm = TRUE),
    median_income = mean(reddito, na.rm = TRUE),
    n_nodes = n()
  )

node_25_scatter <- ggplot(zone_stats, aes(x = avg_degree, y = median_income)) +
  geom_point(aes(size = n_nodes, color = n_nodes), alpha = 0.7) +
  geom_text(aes(label = zona_prossimita), check_overlap = TRUE, vjust = -1, size = 3) +
  theme_minimal() +
  labs(
    title = "Connectivity vs. Median Income",
    subtitle = "Average Node Degree vs. Median Income (25 Proximity Zones)",
    x = "Mean Node Degree",
    y = "Median Income (€)",
    size = "Number of nodes",
    color = "Number of nodes"
  ) +
  scale_y_continuous(labels = scales::dollar_format(suffix = " €", prefix = ""))

print(node_25_scatter)
# ggsave("03-outputs/node_25_scatter.png", plot = node_25_scatter, width = 8, height = 6)


# ERGM implementation -----------------------------
library(ergm)
library(intergraph)
library(network)
library(stargazer)

# Main component cleaning
nodes_to_remove <- which(is.na(V(bologna_giant)$reddito))
bologna_giant_clean <- delete_vertices(bologna_giant, nodes_to_remove) 

bologna_giant_clean <- simplify(bologna_giant_clean, remove.multiple = TRUE, remove.loops = TRUE)

cat("Nodes removed:", length(nodes_to_remove), "\n")
cat("Nodes left:", vcount(bologna_giant_clean), "\n")


# Creation of a clean net for the ERGM implementation
v_attr <- igraph::as_data_frame(bologna_giant_clean, what = "vertices") |> 
  select(name, reddito) |> 
  mutate(reddito = as.numeric(reddito))

e_df <- igraph::as_data_frame(bologna_giant_clean, what = "edges") |> 
  select(1:2)

bologna_lite <- graph_from_data_frame(
  d = e_df,
  vertices = v_attr,
  directed = FALSE
)

bologna_net_ergm <- asNetwork(bologna_lite)
bologna_net_ergm
network::get.vertex.attribute(bologna_net_ergm, "reddito") |> head()

set.seed(123)
ergm_model <- ergm(bologna_net_ergm ~ edges +
                     absdiff("reddito")+
                     nodecov("reddito"),
                        control = control.ergm(
                          MCMLE.maxit = 15, 
                          parallel = 6
                        ))

summary(ergm_model)


# Transitivity check
# Global transitivity
global_trans <- transitivity(bologna_giant, type = "global")
print(paste("Global transitivity:", global_trans))

# Local transitivity
local_trans <- data.frame(
  name = as.character(V(bologna_giant)$name),
  transit = transitivity(bologna_giant, type = "local")
)

net <- net |> 
  activate("nodes") |> 
  mutate(reddito = as.character(reddito.mediano.per.area.statistica_Reddito.imponibile.mediano.dei.contribuenti..residenti))

nodes_sf_plot2 <- net |> 
  left_join(local_trans, by = "name") |> 
  mutate(transit = ifelse(is.na(transit), 0, transit))

nodes_sf_plot2 <- nodes_sf_plot2 |> 
  select(node_id, quartiere, reddito, degree, transit)

df_scatter <- nodes_sf_plot2 |> 
  st_drop_geometry() |> 
  as.data.frame() |> 
  filter(!is.na(reddito)) |> 
  mutate(decile = ntile(reddito, 10)) |> 
  group_by(decile) |> 
  summarise(
    avg_income = mean(as.numeric(reddito), na.rm = TRUE),
    avg_trans = mean(transit, na.rm = TRUE),
    avg_degree = mean(as.numeric(degree), na.rm = TRUE)
  )

# PLOT: transitivity & income
plot_transit <- ggplot(df_scatter, aes(x = avg_income, y = avg_trans)) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = TRUE) + 
  geom_line(color = "grey80", linewidth = 0.5) +
  geom_point(aes(color = avg_degree, size = avg_degree)) +
  scale_color_viridis_c(option = "plasma", name = "Average Degree") +
  theme_minimal() +
  labs(
    title = "Income & Transitivity",
    subtitle = "Analysis of the relationship between Average Decile Income and Average Transitivity (Clustering)",
    x = "Average Income in each Decile (€)",
    y = "Average Transitivity (Clustering)",
    size = "Average Degree"
  ) +
  scale_x_continuous(labels = function(x) paste0(format(x, big.mark = ","), " €"))

print(plot_transit)
# ggsave("03-outputs/plot_transit.png", plot = plot_transit, width = 8, height = 6)


# Nodal Density check
zone_urbanistiche <- st_read("02-data/zone_urbanistiche.geojson") |> 
  st_transform(32632) |> 
  filter(!is.na(zona_prossimita)) |> 
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1000000)

# 2. AGGREGAZIONE: Qui prendiamo SIA il numero di nodi SIA il reddito medio
# Assumiamo che il tuo dataframe con i nodi si chiami 'node_data'
# e che la colonna del reddito si chiami 'reddito'
node_stats <- node_data |>
  filter(!is.na(zona_prossimita)) |> 
  group_by(zona_prossimita) |> 
  summarise(
    n_nodes = n(),
    avg_income = mean(reddito, na.rm = TRUE) # <--- QUESTO È IL PEZZO MANCANTE!
  )

# 3. UNISCI I DATI (Join tra Poligoni e Statistiche dei Nodi)
density_stats <- zone_urbanistiche |> 
  left_join(node_stats, by = "zona_prossimita") |> 
  mutate(node_density = n_nodes / area_km2)

# 4. RISULTATO FINALE: Ora includiamo anche 'avg_income'
density_final <- density_stats |> 
  st_drop_geometry() |> # Rimuoviamo la geometria per visualizzare meglio la tabella
  select(zona_prossimita, n_nodes, area_km2, node_density, avg_income) |> 
  arrange(desc(node_density))

# Controlla se ora avg_income compare
summary(density_final)

# PLOT
library(ggrepel)
library(ggplot2)


node_dens <- ggplot(density_final, aes(x = node_density, y = avg_income)) +
  # Aggiungiamo i punti con dimensione basata sul numero di nodi
  geom_point(aes(size = n_nodes), color = "lightblue", alpha = 0.8) +
  # Etichette per le zone (solo le più estreme per non affollare)
  geom_text_repel(aes(label = zona_prossimita), size = 3, max.overlaps = 10) +
  # Scala colori professionale
  scale_color_viridis_c(option = "mako", direction = -1) +
  theme_minimal() +
  labs(
    title = "Density vs. Median Income",
    subtitle = "Inverse correlation between urban permeability (nodes/km²) and median income",
    x = "Nodal Density (Intersections per km²)",
    y = "Median Income (€)",
    size = "Nodes number"
  )

print(node_dens)
# ggsave("03-outputs/node_density.png", plot = node_dens, width = 8, height = 6)

mean(degree(bologna_graph))
edge_density(bologna_graph)
