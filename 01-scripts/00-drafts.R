###################
# PROVA MAPPA GN PER REDDITO
###############

# 1. Calcoliamo il reddito medio per cluster (GN)
redditi_cluster <- df_analisi %>%
  group_by(community_gn) %>%
  summarise(reddito_medio_cluster = mean(reddito, na.rm = TRUE))

# 2. Uniamo questo dato alla nostra sf per il plot
nodes_reddito_sf <- nodes_backbone_sf %>%
  left_join(redditi_cluster, by = c("community" = "community_gn"))

# 3. Plot "Socio-Morfologico"
ggplot() +
  geom_sf(data = edges_sf, color = "grey95", size = 0.1) +
  geom_sf(data = backbone_edges_sf, color = "grey80", size = 0.3) +
  geom_sf(data = nodes_reddito_sf, aes(color = reddito_medio_cluster), size = 1.2) +
  # Scala dal rosso (povero) al verde/blu (ricco)
  scale_color_viridis_c(option = "magma", name = "Reddito Medio (€)") +
  theme_void() +
  labs(title = "Bologna: Segregazione Economica sulla Backbone (GN)",
       subtitle = "I cluster sono stati colorati in base al reddito medio dei residenti")