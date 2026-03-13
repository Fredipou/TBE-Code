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

shinyApp(ui, server)
