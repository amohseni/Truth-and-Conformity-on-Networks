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
  # Turn of the 'graying out' effect during rendering of plots
  tags$style(type = "text/css",
             ".recalculating {opacity: 1.0;}"), 
  
  # Main title
  titlePanel("Truth and Conformity on Networks"),
  
  fluidRow(
    style = "padding: 10px; margin-top: 10px", 
    column(
      width = 3,
      # Introduction text:
      p(
        tags$b("Description:"),
        "This model explores how social networks, in tandem with conformity bias, can influence the flow and reliability of information in inquiry."
      )
    ),
    column(
      width = 3,
      # Introduction text:
      p(
        "In the model, heterogeneous agents--motivated, to varying degrees,
        by the competing priorities of accuracy and of conformity to one’s peers--share public opinions."
      )
      ),
    column(
      width = 3,
      p(
        "Agents learn, via Bayesian conditionalization, both from private signals from nature,
        and from the public declarations of other agents."
      )
      ),
    column(
      width = 3,
      # Instructions text
      p(
        "Success in learning the truth can be effected by the distribution of priorities, social network structure, initial beliefs, 
        and initial declarations of the population."
      )
    )
    ),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    style = "background-color:#F2F2F2; padding: 20px 20px 0px 20px", 
    column(
      width = 3,
      offset = 0,
      p(
        tags$b("Instructions:"),
        "To run the simulation, choose your parameters, then click 'RUN SIMULATION bellow'"
      ),
      sliderInput(
        "Players",
        "Number of Players:",
        min = 2,
        max = 50,
        value = 15,
        step = 1
      )
    ),
    column(
      width = 3,
      offset = 0,
      selectInput(
        "NetworkType",
        "Network Type",
        c("Complete", "Regular", "Circle", "Star", "Random"),
        selected = "Regular"
      ),
      # Only show this panel only if REGULAR is selected in NETWORK TYPE
      conditionalPanel(
        condition = "input.NetworkType == 'Regular'",
        sliderInput(
          "regDegree",
          "Regular Network Degree",
          min = 0,
          max = 20,
          value = 14,
          step = 2        
          )
      ),
      # Only show this panel only if RANDOM is selected in NETWORK TYPE
      conditionalPanel(
        condition = "input.NetworkType == 'Random'",
        sliderInput(
          "NetworkDensity",
          "Random Network Density",
          min = 0,
          max = 1,
          value = .3,
          step = .05
        )
      )
    ),
    
    column(
      width = 3,
      offset = 0,
      selectInput(
        "InitialDeclarations",
        "Initial Declarations",
        c(
          "Consensus on True State",
          "Uniformly at Random",
          "Consensus on False State"
        ),
        selected = "Uniformly at Random"
      ),
      sliderInput(
        "InitialBelief",
        "Initial Beliefs",
        min = .05,
        max = .95,
        value = .5,
        step = .05
      )
    ),
    column(
      width = 3,
      offset = 0,
      selectInput(
        "TypeDistribution",
        "Type Distribution",
        c("All Truth-Seeking", "Heterogeneous", "All Conformist"),
        selected = "Heterogeneous"
      ),
      
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
      
      sliderInput(
        "Duration",
        "Rounds of Play",
        min = 1,
        max = 10,
        value = 3,
        step = 1
      )
    )
  ),
  
  fluidRow(
    style = 'background-color:#343434; padding-top: 15px; padding-bottom: 2px',
    p(actionButton("runSimulation", "Run Simulation"), align = "center")
    ), 
  
  # main (bottom) panel
  # Show the network animation & plot
  fluidRow(
    column(
      width = 6,
      offset = 0,
      fluidRow(style = 'margin-top: 0px; margin-left: 0px;',
               p(
                 plotOutput(outputId = "networkAnimation"), align = "left"
               )),
      fluidRow(
        style = 'margin-top: 120px;  margin-bottom: 20px; margin-left: 155px',
        sliderInput(
          "simulationStep",
          NULL,
          min = 0,
          max = 5,
          value = 0,
          step = 1,
          animate = animationOptions(interval = 100,
                                     loop = FALSE)
        )
      )
    ),
    column(
      width = 6,
      offset = 0,
      style = 'margin-top: 80px;; margin-bottom: 160px; padding:0px;',
      p(plotOutput(outputId = "evolutionPlot"), align = "right")
    )
  )
))
