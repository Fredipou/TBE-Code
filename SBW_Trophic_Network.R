## Trophic network of Ptoid seen at Mt. Megantic 2025 ----

source("Data_cleaning.R")

# Cleaning Df and Transforming in long form ----

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

### Grouping plants by traits ----

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

## Creating Shiny with tree host grouped by tree type (coniferous or deciduous) ----

SBW_Trophic_Grouped <- fluidPage(
  titlePanel("Sous-réseau Parasitoïdes → Hôtes → Plantes"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "parasitoids",
        "Sélectionner un ou plusieurs parasitoïdes :",
        choices = sort(unique(edges_lep_ptoid$to)),
        multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("network_plot", height = "750px")
    )
  )
)


server <- function(input, output, session) {
  
  output$network_plot <- renderPlot({
    
    req(input$parasitoids)
    
    edges_lep_ptoid_sub <- edges_lep_ptoid %>%
      filter(to %in% input$parasitoids) %>%
      mutate(weight = 1)
    
    herbivores_sel <- unique(edges_lep_ptoid_sub$from)
    
    edges_pl_lep_sub <- edges_pl_lep %>%
      filter(to %in% herbivores_sel) %>%
      mutate(
        plant_group = case_when(
          from %in% feuillus   ~ "Feuillus",
          from %in% coniferes  ~ "Conifères",
          from %in% arbustes   ~ "Arbustes",
          from %in% herbacees  ~ "Herbacées",
          TRUE ~ "Autres plantes"
        )
      ) %>%
      group_by(plant_group, to) %>%
      summarise(weight = n(), .groups = "drop") %>%
      rename(from = plant_group)
    
    # Combine
    edges_sub <- bind_rows(edges_pl_lep_sub, edges_lep_ptoid_sub)
    
    #  Graph
    g_sub <- graph_from_data_frame(edges_sub, directed = TRUE)
    
    g_tbl_sub <- as_tbl_graph(g_sub) %>%
      mutate(
        trophic_level = case_when(
          name %in% edges_pl_lep_sub$from ~ "Plant",
          name %in% edges_pl_lep_sub$to ~ "Herbivore",
          TRUE ~ "Parasitoid"
        )
      )
    
    layout_sub <- create_layout(g_tbl_sub, layout = "sugiyama")
    
    #  Plot
    
    ggraph(layout_sub) +
      geom_edge_link(aes(width = weight), alpha = 0.4) +
      scale_edge_width(range = c(0.5, 3)) +
      geom_node_point(aes(color = trophic_level), size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) +
      scale_color_manual(values = c(
        Plant = "forestgreen",
        Herbivore = "orange",
        Parasitoid = "red"
      )) +
      guides(edge_width = guide_legend(title = "Nb espèces végétales")) +
      scale_y_reverse() +
      ggtitle(paste("Sous-réseau pour :", paste(input$parasitoids, collapse = ", "))) +
      theme_void() +
      theme(legend.position = "bottom")
    
  })
}

shinyApp(SBW_Trophic_Grouped, server)

## Same thing but with un-grouped tree species ----

SBW_Trophic_by_species <- fluidPage(
  titlePanel("Sous-réseau Parasitoïdes → Hôtes → Plantes"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "parasitoids",
        "Sélectionner un ou plusieurs parasitoïdes :",
        choices = sort(unique(edges_lep_ptoid$to)),
        multiple = TRUE
      )
    ),
    mainPanel(
      plotOutput("network_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  output$network_plot <- renderPlot({
    
    req(input$parasitoids)  # ne rien afficher si rien sélectionné
    
    # Filtrer interactions Lepidoptères → Parasitoïdes sélectionnés
    edges_lep_ptoid_sub <- edges_lep_ptoid %>%
      filter(to %in% input$parasitoids)
    
    
    # Hôtes sélectionnés
    herbivores_sel <- unique(edges_lep_ptoid_sub$from)
    
    # Filtrer interactions Plantes → Lépidoptères pour ces hôtes
    edges_pl_lep_sub <- edges_pl_lep %>%
      filter(to %in% herbivores_sel)
    
    # Combiner les arêtes
    edges_sub <- bind_rows(edges_pl_lep_sub, edges_lep_ptoid_sub)
    
    # Créer le graphe
    g_sub <- graph_from_data_frame(edges_sub, directed = TRUE)
    
    # Ajouter trophic level
    g_tbl_sub <- as_tbl_graph(g_sub) %>%
      mutate(
        trophic_level = case_when(
          name %in% edges_pl_lep_sub$from ~ "Plant",
          name %in% edges_pl_lep_sub$to ~ "Herbivore",
          TRUE ~ "Parasitoid"
        ),
        level = case_when(
          trophic_level == "Plant" ~ 1,
          trophic_level == "Herbivore" ~ 2,
          trophic_level == "Parasitoid" ~ 3
        )
      )
    
    # Layout
    layout_sub <- create_layout(g_tbl_sub, layout = "sugiyama")
    
    # Graphe
    ggraph(layout_sub) +
      geom_edge_link(alpha = 0.3) +
      geom_node_point(aes(color = trophic_level), size = 4) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 200) +
      scale_color_manual(values = c(Plant = "forestgreen", Herbivore = "orange", Parasitoid = "red")) +
      scale_y_reverse() +
      ggtitle(paste("Sous-réseau pour :", paste(input$parasitoids, collapse = ", "))) +
      theme_void() +
      theme(legend.position = "bottom")
  })
}

shinyApp(SBW_Trophic_by_species, server)

