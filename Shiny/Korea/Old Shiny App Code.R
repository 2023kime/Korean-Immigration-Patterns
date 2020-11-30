library(shiny)
ui <- fluidPage("Hello World",
                sliderInput(inputId = "num",
                            label = "Choose a number",
                            value = 25, min = 1, max = 100),
                plotOutput(outputId = "hist")
)

server <- function(input, output){
output$hist <- renderPlot({ 
  title <- "100 random normal values"
  hist(rnorm(100), main = title) 
  })
}

shinyApp(ui = ui, server = server)