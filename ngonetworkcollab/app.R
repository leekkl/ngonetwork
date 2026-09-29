
library(haven)
library(tidyverse)
library(writexl)
library(readxl)
library(dplyr)
library(igraph)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggiraph) 
library(patchwork) 
library(tidyr) 
library(sf) 
library(plotly)
library(htmlwidgets)
library(shiny)
library(RColorBrewer)
library(viridis)
library(visNetwork)
library(tibble)
library(GGally)
library(network)
library(sna)
library(rsconnect)


rsconnect::writeManifest()

full_ngo_wish_collab_revised <- read_excel("full_ngo_wish_collab.xlsx")
Survey_data_paper1_cleaned <- read_excel("Survey_data_paper1_cleaned.xlsx")

full_ngo_wish_collab_revised1 <- full_ngo_wish_collab_revised %>% pivot_longer(-ngoid, names_to = "partners") %>% filter(!is.na(value)) %>%
  filter(partners != "ASEAN" & partners != "International" & partners != "European Union" & partners != "Latin America" & partners != "Regional" & partners != "Asia/Pacific"
         & partners != "Caribbean" & partners != "Africa" & partners != "Asia" & partners != "Europe" & partners != "Pacific") 

full_ngo_wish_collab_revised2 <- full_ngo_wish_collab_revised1 %>% pivot_wider(names_from = "partners", values_from = "value")

full_ngo_wish_collab_revised3 <- full_ngo_wish_collab_revised %>% select(ngoid) %>% left_join(full_ngo_wish_collab_revised2, by = "ngoid")

full_ngo_wish_collab_revised3[is.na(full_ngo_wish_collab_revised3)] <- 0

full_ngo_wish_collab_revised_mat <- as.data.frame(full_ngo_wish_collab_revised3)
rownames(full_ngo_wish_collab_revised_mat) <- full_ngo_wish_collab_revised3$ngoid
full_ngo_wish_collab_revised_mat1 <- as.matrix(full_ngo_wish_collab_revised_mat[,-1])

#graphing
full_ngo_wish_collab_revised_g <- graph_from_biadjacency_matrix(full_ngo_wish_collab_revised_mat1) #graph a prerequisite for bipartite
projngo_full_ngo_wish_collab_revised_g1 <- bipartite_projection(full_ngo_wish_collab_revised_g)
projngo_full_ngo_wish_collab_revised_g1
#by ngoid
full_ngo_wish_collab_revised_country_mat1 <- as_adjacency_matrix(projngo_full_ngo_wish_collab_revised_g1$proj2, sparse=F, attr="weight") 

edges_revised_wish_bipartite <- as.data.frame(as_edgelist(full_ngo_wish_collab_revised_g))
colnames(edges_revised_wish_bipartite)<-c("from","to")

## one-mode (countries)  
full_ngo_wish_collab_revised_country_mat2 <- graph_from_adjacency_matrix(full_ngo_wish_collab_revised_country_mat1, mode = "undirected", diag=F)

edges_full_ngo_wish_collab_revised_country <- data.frame(as_edgelist(full_ngo_wish_collab_revised_country_mat2))
colnames(edges_full_ngo_wish_collab_revised_country)<-c("from","to")


#Nodes that are not connected are those NGOs said they wanted to collaborate but within only one country

nodes_full_ngo_wish_collab_revised_country <- data.frame(id = as.character(V(full_ngo_wish_collab_revised_country_mat2)))
nodes_full_ngo_wish_collab_revised_country$font.size<-20
nodes_full_ngo_wish_collab_revised_country$id <- as.character(rownames(full_ngo_wish_collab_revised_country_mat1))

edges_full_ngo_wish_collab_revised_country <- data.frame(as_edgelist(full_ngo_wish_collab_revised_country_mat2))
colnames(edges_full_ngo_wish_collab_revised_country)<-c("from","to")


#World data
world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- world %>% select(geounit, su_a3, geometry)

world_joined <- left_join(Survey_data_paper1_cleaned, world1, by = c("country_code" = "su_a3"))
world_joined1 <- world_joined %>% filter(!is.na(region_cole_geist)) %>% select(country_code, geometry, region_cole_geist, lib_democracy)

world_joined2 <- world_joined1 %>% group_by(region_cole_geist, geometry) %>% count(country_code) #by count
world_joined3 <- world_joined2 %>% filter(!is.na(region_cole_geist))

Survey_data_paper1_cleaned2 <- Survey_data_paper1_cleaned %>% mutate(country_code1 = country_code)
Survey_data_paper1_cleaned2$country_code1 <- ifelse(Survey_data_paper1_cleaned2$country_code1 == "XKX", "KOS", Survey_data_paper1_cleaned2$country_code1)
Survey_data_paper1_cleaned2$country_code1 <- ifelse(Survey_data_paper1_cleaned2$country_code1 == "SLC", "LCA", Survey_data_paper1_cleaned2$country_code1)
Survey_data_paper1_cleaned2$country_code1 <- ifelse(Survey_data_paper1_cleaned2$country_code1 == "SAM", "WSM", Survey_data_paper1_cleaned2$country_code1)
Survey_data_paper1_cleaned2$country_code1 <- ifelse(Survey_data_paper1_cleaned2$country_code1 == "NCP", "CYN", Survey_data_paper1_cleaned2$country_code1)
Survey_data_paper1_cleaned2$country_code1 <- ifelse(Survey_data_paper1_cleaned2$country_code1 == "GRN", "GRD", Survey_data_paper1_cleaned2$country_code1)
Survey_data_paper1_cleaned2$region_cole_geist <- ifelse(Survey_data_paper1_cleaned2$region_cole_geist == ".", NA, Survey_data_paper1_cleaned2$region_cole_geist)#world_joined_r1.1 should be used but need to update the region 

world_joined_r1.1 <- left_join(world, Survey_data_paper1_cleaned2, by = c("su_a3" = "country_code1")) %>% select(admin, su_a3, geometry, region_cole_geist, v2x_egal, v2x_partipdem) %>% filter(admin != "Antartica") %>% group_by(admin, region_cole_geist,  v2x_egal, v2x_partipdem) %>% rename(Social_equality = v2x_egal, Democratic_participation = v2x_partipdem) %>% count(admin) %>% select(-n) %>%
  mutate(admin = recode(admin, 
                        "United States of America" = "United States"))

world_joined_r1.2 <- world_joined_r1.1 %>%
  mutate(region_cole_geist = case_when(
    admin == "United States" ~ "Western Protestant",
    admin == "Brunei" ~ "Other Islamic",
    admin == "Laos" ~ "East Asia",
    admin == "Lebanon" ~ "Islamic Middle East and North Africa",
    admin == "Ecuador" ~ "Latin America and the Philippines", 
    admin == "Ghana" ~ "Sub-Saharan Africa",
    admin == "Nepal" ~ "Hindu",
    TRUE ~ region_cole_geist
  ) 
  )

world_joined_r1.2 <- world_joined_r1.2 %>% filter(!is.na(region_cole_geist))

#only countries (no region/global/NA)
world_joined_r1.3 <- world_joined_r1.2 %>% filter(region_cole_geist != "Global") %>% 
  filter(region_cole_geist != "Regional")

world_joined_r1.2.1 <- world_joined_r1.2 %>% group_by(region_cole_geist, Social_equality, Democratic_participation) %>% count(admin) %>%
  mutate(admin = recode(admin, 
                        "United States of America" = "United States"))


#Network
nodes_full_ngo_wish_collab_revised_country1 <- left_join(nodes_full_ngo_wish_collab_revised_country, world_joined_r1.2.1, 
                                                         by = c("id" = "admin")) %>% select(id, font.size, region_cole_geist,
                                                                                            Social_equality,
                                                                                            Democratic_participation) %>%
  rename(group = region_cole_geist) %>% 
  group_by(group) %>%
  mutate(across(where(is.numeric), ~coalesce(., mean(., na.rm = TRUE)))) %>%
  ungroup() %>%
  group_by(id, group) %>% mutate(Social_equality = mean(Social_equality),
                                 Democratic_participation = mean(Democratic_participation)) %>%
  distinct()

nodes_full_ngo_wish_collab_revised_country2 <- nodes_full_ngo_wish_collab_revised_country1 %>% select(-font.size) %>% mutate(font.size = 30)

ui <- fluidPage(
  titlePanel("Exploring desired national partnerships and indices"),
  fluidRow(
    column(6, 
           h3("Network of desired partnerships among countries"),
           visNetworkOutput("network_plot", height = "400px")
    ),
    column(6, 
           h3("Scatterplot of social equality and democratic participation indices"),
           girafeOutput("girafe_plot", height = "400px")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  # Reactive value to track the globally selected node/item
  selected_node <- reactiveVal(NULL)
  
  # 1. Sync visNetwork selection to the reactive value
  observeEvent(input$network_clicked_nodes, {
    req(input$network_clicked_nodes)
    # visNetwork click event returns a list; extract the first entry
    node_id <- input$network_clicked_nodes[[1]]
    if (length(node_id) > 0) {
      selected_node(as.numeric(node_id))
    }
  })
  
  # 2. Sync ggiraph selection to the reactive value
  observeEvent(input$girafe_plot_selected, {
    # ggiraph returns data_id value of the selected element
    if (!is.null(input$girafe_plot_selected)) {
      selected_node(as.numeric(input$girafe_plot_selected))
    }
  })
  
  # 3. Render visNetwork
  output$network_plot <- renderVisNetwork({
    visNetwork(nodes_full_ngo_wish_collab_revised_country2, edges_full_ngo_wish_collab_revised_country) %>%
      visIgraphLayout(randomSeed = 120) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE, main = "Select by country",
                                                                  style = 'width: 180px; height: 20px;
                                 color: black;
                                 outline:none;'),
                 selectedBy = list(variable="group", main = "Select by region", style = 'width: 180px; height: 20px;
                                 color: black;
                                 outline:none;')) %>%
      # Custom JS Event: Writes back to input$network_clicked_nodes
      visEvents(click = "function(properties) {
        Shiny.onInputChange('network_clicked_nodes', properties.nodes);
      }")
  })
  
  # 4. Push updates to visNetwork when ggiraph triggers a selection
  observe({
    req(selected_node())
    visNetworkProxy("network_plot") %>%
      visSelectNodes(id = selected_node())
  })
  
  # 5. Render ggiraph (Changes selection layout dynamically based on reactive variable)
  output$girafe_plot <- renderGirafe({
    current_sel <- selected_node()
    
    p <- ggplot(nodes_full_ngo_wish_collab_revised_country2, aes(x = Social_equality, y = Democratic_participation, color = group, data_id = id, tooltip = id)) +
      geom_text_repel_interactive(aes(label = id, color = group)) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 6, color = "grey40"), # Small x-axis labels
            axis.text.y = element_text(size = 6, color = "grey40"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      labs(x = "Social equality index", y = "Democratic participation index")
    
    girafe(
      ggobj = p,
      options = list(
        opts_selection(
          type = "single", 
          css = "stroke:red;stroke-width:3px;fill-opacity:0.9;",
          selected = as.character(current_sel) # Sets interactive state programmatically
        ),
        opts_hover(css = "fill-opacity:0.7;cursor:pointer;")
      )
    )
  })
}

shinyApp(ui, server)

