ui <- pageWithSidebar(
  headerPanel("Reactive Conductor"),
  sidebarPanel(
    sliderInput("n", "nth number in Fibonacci sequence",
              min = 1, max = 100, value = 5)
  ),
  mainPanel(
    textOutput("nthValue"),
    textOutput("nthValueInv")
  )
)

fib <- function(n) ifelse(n<3, 1, fib(n-1)+fib(n-2))

server <- function(input, output) {
  
  # Calculate nth number in Fibonacci sequence
  currentFib         <- reactive({ 
    fib(as.numeric(input$n)) 
    if (n == 1) { return(1)}
    if (n == 2) { return(2)}
    })
  
  output$nthValue    <- renderText({ currentFib(1) })
  output$nthValueInv <- renderText({ 1 / currentFib(1) })
    
}

shinyApp(ui = ui, server = server)