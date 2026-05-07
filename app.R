library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)

arcane_edges <- read.csv("arcane_edges.csv")
arcane_nodes <- read.csv("arcane_nodes.csv")

arcane_net <- tbl_graph(nodes = arcane_nodes, 
                        edges = arcane_edges,
                        directed = TRUE) 

ui <-fluidPage(
  
  titlePanel("Arcane Season 1 Character References"),
  
  page_sidebar(
    title = "Analysis of character-to-character reference in the first season of Arcane", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Introduction"), "For the edges data, I manually collected the occurrences of each character mentioning another character. 
      I decided to exclude uses of “you,” “we,” or the plural “they” to limit the data to only individual characters mentioning one other individual.
       I also chose to include mentions of characters by descriptions such as “some guy” when it was clear that the “source” character was referencing a specific, known person. 
      Only one mention of another character was counted per sentence."),
    
    #node attribute explanation
    card(
      card_header("Node Attributes"), "Learn more about the attributes given to each node",
      selectInput("nodes_select", 
                  "Select a node attribute to learn more", 
                  choices = list("Name" = "The names of each character, including descriptive information in parentheses when necessary. Ex. 'Vi (Jinx’s Hallucination)'",
                                 "Gender" = "A binary variable of either 'F' for Female or 'M' for male.",
                                 "Real" = "A binary variable of either TRUE/FALSE that indicates whether the character is a real person. This is only FALSE for characters that occurred as hallucinations or in one case, the voiceover of a letter being read. These characters indicated as FALSE are the same characters that required further description in the 'name' attribute.",
                                 "Jinx" = "A binary variable of either TRUE/FALSE that is only TRUE for the characters Powder and Jinx. This is one character with two names, which have been recorded separately. This variable was created to easily combine or keep these characters separate in future analysis.",
                                 "Residence" = "This attribute lists the current residence of each character. These locations included 'Piltover', 'Zaun', 'Noxus', and 'Unknown' (in the case the character’s residence can not be determined through context).",
                                 "Family" = "This attribute indicates genetic familial relationships (i.e. not including adopted/found family). Since not all characters’ last names are known, these families were assigned a number. Not 'real' characters were included in their family but not listed below for clarity. Any characters without mentioned family were assigned the last number."
                                 ),
                  selected = "A binary variable of either 'F' for Female or 'M' for male."), 
      textOutput("nodes")
    ),
    
    card(
      card_header("Learn about each character"), "",
      selectInput("select", 
                  "select an option", 
                  choices = arcane_nodes |> pull("name"),
                  selected = 1), 
      tableOutput("ourCharacter")
    ), 
    
    #ggraph
    card(card_header("Visualization by Episode"),
         selectInput("size",
                     "Choose an Episode", 
                     choices = list("Episode 1" = 1, 
                                    "Episode 2" = 2,
                                    "Episode 3" = 3,
                                    "Episode 4" = 4,
                                    "Episode 5" = 5,
                                    "Episode 6" = 6,
                                    "Episode 7" = 7,
                                    "Episode 8" = 8,
                                    "Episode 9" = 9
                                    ), 
                     selected = 1), 
         radioButtons("measure",
                     "Choose a centrality measure", 
                     choices = c("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"), 
                     selected = "degree"), 
         plotOutput("example_network"), height = "600px"),
    
    #interactive
    card(card_header("Interactive Network"), 
         "Interact with the characters to see their connections",
         selectInput("size2",
                     "Choose an Episode", 
                     choices = list("Episode 1" = 1, 
                                    "Episode 2" = 2,
                                    "Episode 3" = 3,
                                    "Episode 4" = 4,
                                    "Episode 5" = 5,
                                    "Episode 6" = 6,
                                    "Episode 7" = 7,
                                    "Episode 8" = 8,
                                    "Episode 9" = 9
                     ), 
                     selected = 1),
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                                  "Betweenness Centrality" = "betweenness"), 
                      selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
  )
)


# Section two: make the stuff display

server <- function(input, output) {
  
  #Card 1 for real this time
  output$nodes <- renderText({
    paste(input$nodes_select) })
  
  
  # CARD 1: Character Info
  table <- reactive({
    chr_choice <- arcane_nodes |> filter(name == input$select)
    
    chr_choice
  })
  
  output$ourCharacter <- renderTable({
    chr_table <- table()
    
    chr_table
  })
  
  # CARD 2: Simple Network
  
  #make network
  network <- reactive({
    
    arcane_net <- arcane_net |> activate(edges) |> filter(episode == input$size) |>
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
      geom_node_point(aes(size = .data[[input$measure]], color = residence)) +
      scale_colour_manual(values = c(Noxus = "darkred",
                                     Piltover = "royalblue", 
                                     Zaun = "limegreen", 
                                     Unknown = "goldenrod")) +
      scale_size_continuous(range = c(1, 10)) +
      geom_edge_link(aes(width = weight), alpha = 0.5, color = "black", arrow = arrow(length = unit(2, 'mm')), end_cap = circle(1, "mm")) +
      scale_edge_width(range = c(.1,2)) +
      labs(Nodes = input$measure) + 
      geom_node_text(aes(label = name), color = "black", repel = TRUE) +
      theme_graph()
    
    p
  })
  
  # CARD 3: INTERACTIVE
  
  #make network
  network2 <- reactive({
    arcane_net <- arcane_net |> activate(edges) |> filter(episode == input$size2) |>
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



