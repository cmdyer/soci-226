library(shiny)
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)


ui <-fluidPage(
  
  titlePanel("Arcane Season 1 Character Reference"),
  
  page_sidebar(
    title = "Analysis of character-to-character reference in the first season of Arcane", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Introduction"), "For the edges data, I manually collected the occurrences of each character mentioning another character. 
      I decided to exclude uses of “you,” “we,” or the plural “they” to limit the data to only individual characters mentioning one other individual.
       I also chose to include mentions of characters by descriptions such as “some guy” when it was clear that the “source” character was referencing a specific, known person. 
      Only one mention of another character was counted per sentence."),
    card(
      card_header("Dynamic Demo 1"), "you could put a caption like so",
      selectInput("select", 
                  "select an option", 
                  choices = list("Option A" = "A", 
                                 "Option B" = "B"),
                  selected = 1), 
      textOutput("ourVariable")
    ), 
    
    
    card(card_header("here's a network!"),
         selectInput("size",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"), 
                     selected = 1), 
         plotOutput("example_network"), height = "600px"),
    
    
    card(card_header("An interactive network?!"), 
         "we can use the package VisNetwork to make it happen", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                                  "Betweenness Centrality" = "betweenness"), 
                      selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
  )
)


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  
  # let's create a simple example network with 10 nodes and calculate the degree centrality
  
  # CARD 2 
  #reactive is needed for code that has to run but isn't displayed anywhere
  network <- reactive({
    ex_net <- play_gnp(n = 10, p = 0.5, directed = FALSE)
    
    ex_net <- ex_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(), 
        betweenness = centrality_betweenness())
    
    ex_net
  })
  
  # now let's get it visualized and reactive to our choice from above! 
  
  output$example_network <- renderPlot({
    ex_net <- network() 
    
    p<- ggraph(ex_net, layout = "auto") +
      geom_edge_link(alpha = 0.3, color = "grey80") + 
      geom_node_point(aes(size = .data[[input$size]]), #relies on the button
                      color = "pink") + 
      scale_size_continuous(range = c(.5, 10)) + 
      labs(Nodes = input$size) + 
      theme_graph()
    
    p
    #you need to call the graph or it won't display
  })
  
  # CARD 3 
  
  # we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 
  
  network2 <- reactive({
    arcane_edges <- read.csv("arcane_edges.csv")
    arcane_nodes <- read.csv("arcane_nodes.csv")
    
    arcane_net <- tbl_graph(nodes = arcane_nodes, 
                            edges = arcane_edges,
                            directed = TRUE) 
    
    ep9 <- arcane_net |> activate(edges) |> filter(episode == 9) |>
      activate(nodes) |> mutate(degree = centrality_degree(mode = "all")) |>
      filter(degree > 0)
    
    
    #visnetwork needs nodes and edges lists
    nodes_df <- ep9 |> 
      activate(nodes) |> 
      as_tibble() |> 
      rowid_to_column("id") |> #it needs an id
      mutate(value = if (input$size_by == "degree") degree else betweenness) |>
      mutate(label = name)
    # have to give size based on "value" for visNetwork
    #dynamic element needs to be in nodes table not in visnetwork
    
    edges_df <- ep9 |> 
      activate(edges) |> 
      as_tibble() |> 
      rename(from = 1, to = 2)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    nodes <- net2$nodes
    edges <- net2$edges 
    
    
    visNetwork(nodes, net2$edges) |> 
      
      visNodes(borderWidth = 1, 
               color = list(
                 background = "pink", 
                 border = "red", 
                 highlight =  "purple"),
               label = "name")|>
      
      visEdges(
        color = list(color = "purple", highlight = "black")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), #highlights connected nodes, important
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE) |> #makes it cool
      
      visPhysics(stabilization = TRUE) #this is basically layout, but sta. should prob be true
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



