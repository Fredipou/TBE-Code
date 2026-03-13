## Trophic network of Ptoid seen at Mt. Megantic 2025 ----

source("Data_cleaning.R")

# Cleaning Df and Transforming in long form

## Lepidop and plants

Lep_long <- Lepidop_host %>%
  pivot_longer(
    cols = -1,
    names_to = "Plant",
    values_to = "Interaction"
  ) %>%
  filter(!is.na(Interaction) & Interaction > 0) %>%
  rename(Lepidoptere = 1)

edges_pl_lep <- Lep_long %>%
  select(from = Plant, to = Lepidoptere)
g_pl_lep <- graph_from_data_frame(edges_pl_lep, directed = TRUE)
g_tbl_pl_lep <- as_tbl_graph(g_pl_lep) %>%
  mutate(
    trophic_level = ifelse(name %in% edges_pl_lep$from, "Plant", "Herbivore"),
    level = ifelse(trophic_level == "Plant", 1, 2)
  )
layout_pl_lep <- create_layout(g_tbl_pl_lep, layout = "sugiyama")
ggraph(layout_pl_lep) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = trophic_level), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) +
  scale_color_manual(values = c(Plant = "forestgreen", Herbivore = "orange")) +
  ggtitle("Plants → Herbivores") +
  scale_y_reverse() +
  theme_void() +
  theme(legend.position = "bottom")

# Ptoid and Lepidop

Ptoid_long <- Ptoid_host %>%
  pivot_longer(
    cols = -1,
    names_to = "Parasitoid",
    values_to = "Interaction"
  ) %>%
  filter(!is.na(Interaction) & Interaction > 0) %>%
  rename(Lepidoptere = 1)

edges_lep_ptoid <- Ptoid_long %>%
  select(from = Lepidoptere, to = Parasitoid)

g_lep_ptoid <- graph_from_data_frame(edges_lep_ptoid, directed = TRUE)
g_tbl_lep_ptoid <- as_tbl_graph(g_lep_ptoid) %>%
  mutate(
    trophic_level = ifelse(name %in% edges_lep_ptoid$from, "Herbivore", "Parasitoid"),
    level = ifelse(trophic_level == "Herbivore", 1, 2)
  )
layout_lep_ptoid <- create_layout(g_tbl_lep_ptoid, layout = "sugiyama")
ggraph(layout_lep_ptoid) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(color = trophic_level), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) +
  scale_color_manual(values = c(Herbivore = "orange", Parasitoid = "red")) +
  ggtitle("Herbivores → Parasitoids") +
  scale_y_reverse() +
  theme_void() +
  theme(legend.position = "bottom")

### Grouping plants by traits

feuillus <- c("Acer saccharum", "Acer saccharinum", "Acer spicatum", 
              "Acer rubrum", "Acer negundo", "Salix sp",
              "Ulmus americana", "Ulmus rubra", "Tilia americana",
              "Fagus grandifolia", "Populus alba", "Populus nigra",
              "Populus grandidentata", "Populus deltoides",
              "Populus tremuloides", "Populus balsamifera",
              "Quercus alba", "Quercus rubra", "Quercus nigra",
              "Quercus serrata", "Quercus garryana", "Quercus macrocarpa",
              "Quercus coccinea", "Fraxinus americana",
              "Fraxinus nigra", "Fraxinus pennsylvanica",
              "Carya sp.", "Carya ovata", "Carya cordiformis",
              "Ostrya virginiana", "Juglans nigra",
              "Malus pumila", "Malus sylvestris",
              "Alnus sp.", "Alnus incana", "Alnus rubra",
              "Arbutus menziesii", "Asimina triloba",
              "Castanea dentata", "Prunus virginiana",
              "Prunus nigra", "Prunus americana", "Prunus persica",
              "Prunus serotina", "Prunus pensylvanica",
              "Betula populifolia", "Betula papyrifera",
              "Betula alleghaniensis", "Betula nigra")

coniferes <- c("Pinus alba", "Pinus strobus", "Pinus virginiana",
               "Pinus rigida", "Pinus sylvestris", "Pinus resinosa",
               "Pinus contorta", "Pinus ponderosa", "Pinus banksiana",
               "Picea abies", "Picea glauca", "Picea mariana",
               "Picea rubens", "Picea pungens", "Picea sitchensis",
               "Larix occidentalis", "Larix decidua", "Larix laricina",
               "Abies balsamea", "Abies concolor", "Abies grandis",
               "Abies amabilis", "Abies alba", "Abies lasiocarpa",
               "Thuja plicata", "Thuja occidentalis",
               "Tsuga heterophylla", "Tsuga canadensis",
               "Pseudotsuga menziesii", "Menziesii glauca")

arbustes <- c("Amelanchier alnifolia", "Amelanchier canadensis",
              "Corylus americana", "Myrica gale",
              "Comptonia peregrina", "Ilex decidua",
              "Kalmia sp.", "Rhododendron groenlandicum",
              "Sorbus sp.", "Rosa sp.", "Rubus idaeus",
              "Toxicodendron pubescens", "Toxicodendron vernis",
              "Rhus coriaria", "Rhus copallina",
              "Spiraea alba", "Sambucus nigra",
              "Cornus alternifolia")

herbacees <- c("Aralia nudicaulis", "Solidago sp.",
               "Urtica dioica", "Sanguisorba officinalis",
               "Aster novae-angliae", "Silene vulgaris")
