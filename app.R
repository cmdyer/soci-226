library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

#Reading in data universally
arcane_edges <- read.csv("arcane_edges.csv")
arcane_nodes <- read.csv("arcane_nodes.csv")

arcane_net <- tbl_graph(nodes = arcane_nodes, 
                        edges = arcane_edges,
                        directed = TRUE)

#Setting Colors up universally to be used across pages
color_scale <- c(Noxus = "darkred",
               Piltover = "royalblue", 
               Zaun = "limegreen", 
               Unknown = "goldenrod",
               M = "royalblue",
               `F` = "magenta3")



##### UI #####----------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(preset = "vapor"),
  page_navbar(title = "Arcane Season 1 Character References",
              nav_panel(
                "Introduction",
                card(
                  card_header("Introduction and Navigation"),
                  "This website contains network analysis of character-to-character references in the first season of the show Arcane.
                  On this page, you will find an overview of the data collection process and some key findings.
                  The tabs at the top of the page lead to focused analysis based on the named category.
                  The 'Full Network' page contains the most comprehensive information, including the network measures evaluated below.
                  "
                ),
                card(
                  card_header("Data Collection"),
                  "For the edges data, I manually collected the occurrences of each character mentioning another character.
                  This created a directed network, with ties weighted by how many times a mentioned occured. 
                  I decided to exclude uses of “you,” “we,” or the plural “they” to limit the data to only individual characters mentioning one other individual. 
                  I also chose to include mentions of characters by descriptions such as “some guy” when it was clear that the “source” character was referencing a specific, known person.
                  Only one mention of another character was counted per sentence.
                  Node attributes were assigned based on informaiton provided in the episodes. 
                  Further explanation of node attributes can be found under the 'By Character' tab, in order to be easily referenced when looking at individual characters' attributes."
                ),
                card(
                  card_header("Centrality Measures"),
                  "One important finding is that the characters with high betweenness centrality were not necessarily the characters with the highest degree centrality.
                  Notably, Vi is shown to have the highest degree centrality, followed closely by Jayce.
                  However, Jayce has a significantly higher betweenness centrality, over doubling that of Vi, who is the second highest character.
                  Initially, this is suprising considering Vi's more prominent role in the storyline of the show.
                  Yet upon reflection, Jayce's character does play the role of a broker- he is in a position of power, and converses with both the upperclass and lowerclass factions that emerge.
                  Another interesting finding was the distribution of places of residence in the central figures by both measures.
                  For degree centrality, there are more characters from Zaun among the highest-degree characters.
                  This reflects the storyline's primary focus on the lives of Zaun residents (i.e. Vi, Silco, and Jinx).
                  In looking at betweenness centrality, we again see this switch where Piltover residents (i.e. Jayce, Mel, and Caitlyn) have more central broker roles.
                  In the show, Piltover represents the more upperclass society, so this accurately depicts the social power these characters are able to hold.
                  These findings can be shown in the bar graph in the 'Full Network' tab.
                  "
                ),
                card(
                  card_header("Assortativity"),
                  "Assortativity was measured by the node attributes family and residence.
                  These attributes were chosen due to the prominent family relationships in the show and the story's focus on the interaction and conflict between the regions Zaun and Piltover.
                  Place of residence was shown to have the highest assortativity (.57), indicating that character's were moderately likely to have stronger connections with characters from the same place.
                  This is indicative of the polarized relationship between the two main regions, with travel between the two ares being infrequent.
                  However, given that the show follows the conflict between these areas, there is a lot of conversations between and about characters from both sides.
                  This explains why the assortativity value is not higher.
                  Looking at families, the assortativity value was initially extremely low (.08).
                  This is misleading, however, as the family attribute groups all characters whose family is unknown into one 'family'.
                  Once this unknown family is removed, the assortativity value increases to .2, which still indicates characters are only slightly more likely to have strong connections with their family members.
                  One reason this may be is the data's exclusion of family-like relationships, which noteably excludes father figures like Vander and Silco from being a part of the families of prominent characters Vi and Jinx.
                  These underlying connections may be lowering the famliy assortativity by appearing as strong ties to completely unrelated characters, which may not be the most accurate depiction of their relationship.
                  "
                )
              ),
              nav_panel(
                "Full Network",
                card(
                  card_header("Centrality Across the Season"),
                  "Choose a centrality measure to see the role of each character across the entire season.
                  By looking at degree, we can see what characters were talked about- and talking about others- the most.
                  However, by looking at betweenness centrality, we can see that not all the characters with a high degree were in broker positions.
                  A high degree may convey a sense of importance to the viewer, as they are often on screen talking about others and/or their name comes up in conversation frequently.
                  Yet the betweenness centrality reveals that within the world of the show, certain characters play more important connecting roles socially.
                  To limit the data to a reasonable scope, all characters with a degree of 1 were removed.
                  This removes all characters who were mentioned, or mentioned someone else, only one time.",
                  layout_sidebar(sidebar = list(
                    radioButtons(
                      "color2",
                      "Choose an Attribute",
                      choices = list(
                        "Gender" = "gender",
                        "Residence" = "residence"
                      ),
                      selected = "gender"
                    ),
                  radioButtons("measure1",
                                "Choose a centrality measure",
                               choices = c(
                                 "Degree Centrality" = "degree",
                                 "Betweenness Centrality" = "betweenness"),
                               selected = "degree"
                               )),
                  plotOutput("bar")
                    )),
                card(
                  card_header("Assortativity"),
                  "Assortativity measures the likelihood of nodes to be highly connected with other nodes that share a certain property.
                  For this network, the two attributes that would provide the most interesting look into assortativity were residence and family.
                  'Known Families' is a subset of the data, with all characters of unknown families removed.
                  ",
                  tableOutput("assort")
                ),
                card(
                  card_header("Full Network Visualization"),
                  "View the full season network, with nodes sized by the chose centrality measure.
                  Nodes are colored by place of residence.
                  Characters with a degree less than 1 were removed in order to improve the readability of the visualization.",
                  radioButtons(
                    "measure2",
                    "Choose a centrality measure",
                    choices = c(
                      "Degree Centrality" = "degree",
                      "Betweenness Centrality" = "betweenness"
                    ),
                    selected = "degree"
                  ),
                  plotOutput("full_network")
                )
                
              ),
              
              nav_panel(
                "By Character",
                card(
                  height = "350px",
                  card_header("Node Attributes"),
                  "Learn more about the attributes given to each character",
                  selectInput(
                    "nodes_select",
                    "Select a node attribute to learn more",
                    choices = list(
                      "Name" = "The names of each character, including descriptive information in parentheses when necessary. Ex. 'Vi (Jinx’s Hallucination)'",
                      "Gender" = "A binary variable of either 'F' for Female or 'M' for male.",
                      "Real" = "A binary variable of either TRUE/FALSE that indicates whether the character is a real person. This is only FALSE for characters that occurred as hallucinations or in one case, the voiceover of a letter being read. These characters indicated as FALSE are the same characters that required further description in the 'name' attribute.",
                      "Jinx" = "A binary variable of either TRUE/FALSE that is only TRUE for the characters Powder and Jinx. This is one character with two names, which have been recorded separately. This variable was created to easily combine or keep these characters separate in future analysis.",
                      "Residence" = "This attribute lists the current residence of each character. These locations included 'Piltover', 'Zaun', 'Noxus', and 'Unknown' (in the case the character’s residence can not be determined through context).",
                      "Family" = "This attribute indicates genetic familial relationships (i.e. not including adopted/found family). Since not all characters’ last names are known, these families were assigned a number. Not 'real' characters were included in their family but not listed below for clarity. Any characters without mentioned family were assigned the last number.
                              <br>1.	Vi, Powder, Jinx, Vi’s Mother, Vi’s Father
                              <br>2.	Caitlyn, Mrs. Kiramman, Mr. Kiramman
                              <br>3.	Jayce, Ximena, Jayce’s Father
                              <br>4.	Mel, Ambessa, Kino, Mel’s Father, Mel’s Grandfather
                              <br>5.	Amara, Rohan
                              <br>6.	Marcus, Ren
                              <br>7.	Renni, Renni’s Son
                              <br>8.	Sevika, Sevika’s Father
                              <br>9.	Singed, Singed’s Daughter
                              <br>10.	Woman, Woman’s Daughter
                              <br>11.	All other characters"
                    ),
                    selected = "The names of each character, including descriptive information in parentheses when necessary. Ex. 'Vi (Jinx’s Hallucination)'"
                  ),
                  uiOutput("nodes")
                ),
                
                card(
                  height = "300px",
                  card_header("Learn about each character"),
                  "",
                  selectInput(
                    "select",
                    "Select a character",
                    choices = arcane_nodes |> pull("name"),
                    selected = 1
                  ),
                  tableOutput("ourCharacter")
                )),
              
              nav_panel(
                "By Episode",
                #ggraph
                card(
                  card_header("Visualization by Episode"),
                  "",
                  layout_sidebar(sidebar = list( 
                  selectInput(
                    "ep",
                    "Choose an Episode",
                    choices = list(
                      "Episode 1" = 1,
                      "Episode 2" = 2,
                      "Episode 3" = 3,
                      "Episode 4" = 4,
                      "Episode 5" = 5,
                      "Episode 6" = 6,
                      "Episode 7" = 7,
                      "Episode 8" = 8,
                      "Episode 9" = 9
                    ),
                    selected = 1
                  ),
                  radioButtons(
                    "color",
                    "Choose an Attribute",
                    choices = list(
                      "Gender" = "gender",
                      "Residence" = "residence"
                    ),
                    selected = "gender"
                  ),
                  radioButtons(
                    "measure",
                    "Choose a Centrality Measure",
                    choices = c(
                      "Degree Centrality" = "degree",
                      "Betweenness Centrality" = "betweenness"
                    ),
                    selected = "degree"
                  )),
                  plotOutput("example_network"),
                  height = "600px")
                ),
                
                #interactive
                card(
                  card_header("Interactive Network"),
                  "Interact with the characters to see their connections",
                  selectInput(
                    "ep2",
                    "Choose an Episode",
                    choices = list(
                      "Episode 1" = 1,
                      "Episode 2" = 2,
                      "Episode 3" = 3,
                      "Episode 4" = 4,
                      "Episode 5" = 5,
                      "Episode 6" = 6,
                      "Episode 7" = 7,
                      "Episode 8" = 8,
                      "Episode 9" = 9
                    ),
                    selected = 1
                  ),
                  radioButtons(
                    "size_by",
                    "Centrality Measure",
                    choices = c("Degree" = "degree", "Betweenness Centrality" = "betweenness"),
                    selected = "degree"
                  ),
                  visNetworkOutput("int_network"),
                  height = "600px"
                )
              )
  ))


# Section two: make the stuff display

##### SERVER #####-----------------------------------------------------------------

server <- function(input, output) {

# Full Network ------------------------------------------------------------
  ##Bar graph
  #data
  bar <- reactive({
    arcane_bar <- arcane_net |> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(mode = "all"), 
        betweenness = centrality_betweenness()) |>
      filter(degree > 1)
    
    bar_data <- data.frame(name = V(arcane_bar)$name,
                           degree = V(arcane_bar)$degree,
                           betweenness = V(arcane_bar)$betweenness,
                           residence = V(arcane_bar)$residence,
                           gender = V(arcane_bar)$gender,
                           family = V(arcane_bar)$family)
    bar_data
  })
  
  #output
  output$bar <- renderPlot({
    arcane_bar <- bar()
    
    b <- ggplot(arcane_bar, aes(x = reorder(name, .data[[input$measure1]]), y = .data[[input$measure1]])) + 
      geom_col(aes(fill = .data[[input$color2]])) +
      scale_fill_manual(values = color_scale) +
      coord_flip()
    
    b
  })
  
  ##Assortativity
  #data
  
  output$assort <- renderTable({
    res_sort <- assortativity_nominal(arcane_net, as.integer(as.factor(V(arcane_net)$residence)))
    
    fam_sort <- assortativity_nominal(arcane_net, as.integer(as.factor(V(arcane_net)$family)))
    
    k_fam_sort <- arcane_net |>
      filter(family != 11)
    k_fam_sort <- assortativity_nominal(k_fam_sort, as.integer(as.factor(V(k_fam_sort)$family)))
    
    sort_table <- data.frame(
      Attribute = c("Residence", "Family", "Known Families"),
      Assortativity = c(res_sort, fam_sort, k_fam_sort)
    )
    
    sort_table
  })
  
  ##network
  #data
  full_network <- reactive({
    
    full_net <- arcane_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(mode = "all"), 
        betweenness = centrality_betweenness())|>
      filter(degree > 1)
    
    full_net
  })
  
  #visualizaiton
  output$full_network <- renderPlot({
    full_net <- full_network() 
    
    p1<- ggraph(full_net, layout = "auto") +
      geom_node_point(aes(size = .data[[input$measure2]], color = residence)) +
      scale_colour_manual(values = color_scale)+
      scale_size_continuous(range = c(1, 10)) +
      geom_edge_link(aes(width = weight), alpha = 0.5, color = "grey", arrow = arrow(length = unit(2, 'mm')), end_cap = circle(1, "mm")) +
      scale_edge_width(range = c(.1,2)) +
      labs(Nodes = input$measure2) + 
      geom_node_text(aes(label = name), color = "black", repel = TRUE, label.padding = .5) +
      theme_graph()
    
    p1
  })
  

# By Character ------------------------------------------------------------
  ##Node attributes
  output$nodes <- renderText({
    paste(input$nodes_select) })
  
  
  ##Character Info
  table <- reactive({
    chr_choice <- arcane_nodes |> filter(name == input$select)
    
    chr_choice
  })
  
  output$ourCharacter <- renderTable({
    chr_table <- table()
    
    chr_table
  })
  

# By Episode --------------------------------------------------------------
  ## By Episode Network
  #make network
  network <- reactive({
    
    arcane_net <- arcane_net |> activate(edges) |> filter(episode == input$ep) |>
      activate(nodes) |> mutate(degree = centrality_degree(mode = "all")) |>
      filter(degree > 0)
    
    arcane_net <- arcane_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(mode = "all"), 
        betweenness = centrality_betweenness())
    
    arcane_net
  })
  
  #make visualizaiton
  output$example_network <- renderPlot({
    arcane_net <- network() 
    
    p<- ggraph(arcane_net, layout = "auto") +
      geom_node_point(aes(size = .data[[input$measure]], color = .data[[input$color]])) +
      scale_colour_manual(values = color_scale) +
      scale_size_continuous(range = c(1, 10)) +
      geom_edge_link(aes(width = weight), alpha = 0.5, color = "grey", arrow = arrow(length = unit(2, 'mm')), end_cap = circle(1, "mm")) +
      scale_edge_width(range = c(.1,2)) +
      labs(Nodes = input$measure) + 
      geom_node_text(aes(label = name), color = "black", repel = TRUE) +
      theme_graph()
    
    p
  })
  
  ##INTERACTIVE
  #make network
  network2 <- reactive({
    arcane_net <- arcane_net |> activate(edges) |> filter(episode == input$ep2) |>
      activate(nodes) |> mutate(degree = centrality_degree(mode = "all")) |>
      filter(degree > 0)
    
    arcane_net <- arcane_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(mode = "all"), 
        betweenness = centrality_betweenness())
    
    #nodes
    nodes_df <- arcane_net |> 
      activate(nodes) |> 
      as_tibble() |> 
      rowid_to_column("id") |> 
      mutate(value = if (input$size_by == "degree") degree else betweenness) |>
      mutate(label = name)
    
    #edges
    edges_df <- arcane_net |> 
      activate(edges) |> 
      as_tibble() |> 
      rename(from = 1, to = 2)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  #visualize it
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    nodes <- net2$nodes
    edges <- net2$edges 
    
    visNetwork(nodes, net2$edges) |> 
      
      visNodes(borderWidth = 1, 
               color = list(
                 background = "pink", 
                 border = "red", 
                 highlight =  "violetred"),
               label = "name")|>
      
      visEdges(
        color = list(color = "purple", highlight = "navy")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE) |> 
      
      visPhysics(stabilization = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



