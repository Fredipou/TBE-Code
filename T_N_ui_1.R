source("Data_cleaning.R")
source("Trophic_Network_separated.R")

ui <- fluidPage(
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

shinyApp(ui, server)
