# TRUTH AND CONFORMITY ON NETWORKS
# << UI >>
# by Aydin Mohseni & Cole Williams


# Load the shiny GUI library
library(shiny)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Main title
  titlePanel("Truth and Conformity in Dynamic Networks"),
  
  # Load MathJax 
  withMathJax(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("Players",
                   "Number of Players:",
                   min = 2,
                   max = 50,
                   value = 20,
                   step = 1
                   ),
       
       selectInput("NetworkType", 
                   "Network Type",
                   c("Complete", "Circle", "Star", "Random"),
                   selected = "Complete"
                   ),
       
       # Only show this panel only if CUSTOM is selected in NETWORK TYPE
       conditionalPanel(
         
         condition = "input.NetworkType == 'Random'",
         
         align = "center",
         
         sliderInput("NetworkDensity", 
                     "NetworkDensity", 
                     min =1 , 
                     max = 20, 
                     value = 4,
                     width = '90%',
                     step = 1
                     )
       ),
       
       selectInput("InitialDeclarations", 
                   "Initial Declarations",
                   c("Consensus on Truth", "Mixed", "Consensus on Falsity"),
                   selected = "Consensus on Falsity"
       ),
       
       selectInput("TypeDistribution", 
                   "Type Distribution",
                   c("All Truth-Seeking", "Mixed", "All Conformist"),
                   selected = "Mixed"
       ),
       
       # Only show this panel only if MIXED is selected in TYPE DISTRIBUTION
       conditionalPanel(
         condition = "input.TypeDistribution == 'Mixed'",
         
         style='margin-bottom:100px;',
         align = "center",
         
         column(width = 6, 
                sliderInput("TypeBeta", 
                            "Conformist", 
                            min = 0 , 
                            max = 5, 
                            value = 1,
                            step = .2,
                            ticks = FALSE
                )
         ),
                
         column(width = 6, 
            sliderInput("TypeAlpha", 
                        "Truth-Seeking", 
                        min = 0 , 
                        max = 5, 
                        value = 1,
                        step = .2,
                        ticks = FALSE
            )
         ),
         
         plotOutput(outputId = "TypeDistributionPlot", width = "90%", height = "120px")
         
       ),

       tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),       
       
       sliderInput("Duration", 
                   "Full Rounds of Play:",
                   min = 1,
                   max = 10,
                   value = 3,
                   step = 1
                   ),
       
       p(actionButton("runSimulation", "Run Simulation"), align = "center")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      style='margin-bottom:180px;',
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Initial Network", 
                           plotOutput(outputId = "networkInit", 
                                      width = "100%")
                  ),
                  tabPanel("Network Game",
                           
                           style='padding: 20px;',
                           align = "center",
                           
                           sliderInput("SimulationStep", NULL, 
                                       min = 1, 
                                       max = 100,
                                       value = 1, 
                                       step = 1,
                                       animate = animationOptions(interval=300, 
                                                                  loop = F,
                                                                  playButton = T, 
                                                                  pauseButton = T))
                  ),                  
                  tabPanel("Evolution of Beliefs and Declarations",
                           column(width = 6, offset = 0, style='padding:20px;', 
                                  plotOutput(outputId = "evolutionPlot")
                           )
                  )
      )
    )
)))
