# TRUTH AND CONFORMITY ON NETWORKS
# << UI >>
# by Aydin Mohseni & Cole Williams

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for aesthetics
  includeCSS("styles.css"),
  
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
       
       sliderInput("Duration",
                   "Rounds of Play:",
                   min = 1,
                   max = 10,
                   value = 2
                   )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput(outputId = "networkGame", width = "100%")
    )
  )
))
