library(shiny)
ui <- navbarPage("Modern Patterns in Korean Immigration",
  tabPanel("Graphs",
          fluidPage(
          titlePanel("Reasons why People Visit Korea"),
          sidebarLayout(
          sidebarPanel(
          selectInput(
            "line_plot",
            "Line Graph",
            c("Option A" = "a", "Option B" = "b")
            )),
         mainPanel(plotOutput("line_plot")))
  )),
  tabPanel("Analysis",
         titlePanel("Models of Korean Immigration and Economic Data"),
         p("I have several models.")),
  tabPanel("About",
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("Hello! My project looks at trends in the reasons why people have 
             applied to visit Korea in the two decades since 2000. The 'Hallyu,'
             or Korean Wave phenomenon, created enormous international interest
             in Korean cultural products and potential for the first time around
             the last years of the 1990s. Concurrently, government investment
             in Korean industries, the rise of chaebol conglomerates, and 
             modernization have accelerated the GDP and standard of living in 
             the country. A traditionally poorer 'sender' country that saw a net 
             exodus of emigrants for years after the Korean War (1950-53), has 
             the growth of wealth in Korea begun attracting people back to the
             country? Many in the Korean diaspora, particularly first- or 
             second-generation Korean-Americans, have actually sought to go to
             Korea and find success in the domestic market. Foreign countries 
             and travelers are also increasingly interested in the employment, 
             business, educational, and investment opportunities that Korea
             can offer. How does growth in GDP affect the different reasons
             that people outside of Korea have for coming to the country?")
)

server <- function(input, output){
  output$line_plot
}

shinyApp(ui = ui, server = server)
