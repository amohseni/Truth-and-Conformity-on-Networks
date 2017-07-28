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
  titlePanel("Truth and Conformity on Networks"),
  
  # Load MathJax 
  withMathJax(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      width = 3,
      
       sliderInput("Players",
                   "Number of Players:",
                   min = 2,
                   max = 50,
                   value = 15,
                   step = 1
                   ),
       
       selectInput("NetworkType", 
                   "Network Type",
                   c("Complete", "Regular", "Circle", "Star", "Random"),
                   selected = "Complete"
                   ),
       
       # Only show this panel only if REGULAR is selected in NETWORK TYPE
       conditionalPanel(
         
         condition = "input.NetworkType == 'Regular'",
         
         align = "center",
         
         sliderInput("regDegree", 
                     "Regular Network Degree", 
                     min = 0, 
                     max = 20, 
                     value = 4,
                     step = 2,
                     width = '90%'
                     )
       ),
       
       # Only show this panel only if RANDOM is selected in NETWORK TYPE
       conditionalPanel(
         
         condition = "input.NetworkType == 'Random'",
         
         align = "center",
         
         sliderInput("NetworkDensity", 
                     "Network Density", 
                     min = 0, 
                     max = 1, 
                     value = .3,
                     step = .05,
                     width = '90%'
         )
       ),
       
       selectInput("InitialDeclarations", 
                   "Initial Declarations",
                   c("Consensus on True State", "Uniformly at Random", "Consensus on False State"),
                   selected = "Uniformly at Random"
       ),
       
       selectInput("TypeDistribution", 
                   "Type Distribution",
                   c("All Truth-Seeking", "Heterogeneous", "All Conformist"),
                   selected = "Heterogeneous"
       ),
       
       # # Only show this panel only if MIXED is selected in TYPE DISTRIBUTION
       # conditionalPanel(
       #   condition = "input.TypeDistribution == 'Heterogeneous'",
       #   
       #   style='margin-bottom:100px;',
       #   align = "center",
       #   
       #   column(width = 6, 
       #      sliderInput("TypeAlpha", 
       #                  "Truth-Seeking", 
       #                  min = 0 , 
       #                  max = 5, 
       #                  value = 3,
       #                  step = .2,
       #                  ticks = FALSE
       #      )
       #   ),
       #          
       #   column(width = 6, 
       #      sliderInput("TypeBeta", 
       #                  "Conformist", 
       #                  min = 0 , 
       #                  max = 5, 
       #                  value = 3,
       #                  step = .2,
       #                  ticks = FALSE
       #      )
       #   ),
       #   
       #   plotOutput(outputId = "TypeDistributionPlot", width = "90%", height = "120px")
       #   
       # ),

       tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),       
       
       sliderInput("Duration", 
                   "Full Rounds of Play:",
                   min = 1,
                   max = 10,
                   value = 3,
                   step = 1
                   ),
       
       p(actionButton("runSimulation", "Re-Run Simulation"), align = "center")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(type = "tabs", 
                  tabPanel("Network Animation", 
                           
                           fluidRow(
                             
                             style='margin-top: 20px;',
                             
                             plotOutput(outputId = "networkAnimation", 
                                        width = "100%")
                           ),
                           
                           fluidRow(
                             
                             style='margin-top: 140px; margin-left: 150px;',
                             
                             sliderInput("simulationStep", NULL, 
                                       min = 0, 
                                       max = 5,
                                       value = 0, 
                                       step = 1,
                                       animate = animationOptions(interval = 100, 
                                                                  loop = FALSE))
                           )
                           
                  ),
                  
                  tabPanel("Evolution of Beliefs and Declarations",
                           
                           style='margin-top: 20px;',

                           column(width = 6, offset = 0, style='padding:0px;',
                                  p(plotOutput(outputId = "evolutionPlot"), align = "center")
                           )
                  )
      )
    )
)))
