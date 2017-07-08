# TRUTH AND CONFORMITY ON NETWORKS
# << UI >>
# by Aydin Mohseni & Cole Williams


# Load the shiny GUI library
library(shiny)

# Set encoding for special characters
# Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for visual
  includeCSS("www/spacelab.css"),
  
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
                   value = 20
                   ),
       
       selectInput("NetworkType", 
                   "Network Type",
                   c("Complete", "Circle", "Star", "Random"),
                   selected = "Complete"
                   ),
       
       selectInput("InitialState", 
                   "Initial State",
                   c("Inaccurate Consensus", "Uniformly at Random", "Accurate Consensus"),
                   selected = "Random"
       ),
       
       sliderInput("Duration",
                   "Rounds of Play:",
                   min = 1,
                   max = 10,
                   value = 2
                   )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Network Animation", 
                           plotOutput(outputId = "networkGame", width = "100%")
                  ),
                  tabPanel("Evolution of Beliefs and Declarations"
                           
                  )
      )
    )
)))
