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
  hist(rnorm(input$num), main = title) 
  })
}

shinyApp(ui = ui, server = server)



# My project looks at trends in the reasons why people have 
#applied to visit Korea in the two decades since 2000. The 'Hallyu,'
#or Korean Wave phenomenon, created enormous international interest
#in Korean cultural products and potential for the first time around
#the last years of the 1990s. Concurrently, government investment
#in Korean industries, the rise of chaebol conglomerates, and 
#modernization have accelerated the GDP and standard of living in 
#the country. A traditionally poorer 'sender' country that saw a net 
#exodus of emigrants for years after the Korean War (1950-53), has 
#the growth of wealth in Korea begun attracting people back to the
#country? Many in the Korean diaspora, particularly first- or 
#second-generation Korean-Americans, have actually sought to go to
#Korea and find success in the domestic market. Foreign countries 
#and travelers are also increasingly interested in the employment, 
#business, educational, and investment opportunities that Korea
#can offer. How does growth in GDP affect the different reasons
#that people outside of Korea have for coming to the country?


# Define UI for application that draws a histogram
ui <- navbarPage(
  "Final Project Title",
  tabPanel("Home", 
           titlePanel("Home"),
           h3("Welcome to my page!"),
           p("Hello, this is where I talk about my project."),
           img(src = "image.png", height = 200),
           h3(""),
           p(""),
           a("Google", href = "https://www.google.com")),
  
  tabPanel("Model",
           fluidPage(
             selectInput("x", "X variable", choices = names(d)),
             selectInput("y", "Y variable", choices = names(d)),
             selectInput("geom", "geom", c("point", "column", "jitter", "line")),
             plotOutput("plot")
           )),
  
  tabPanel("Discussion",
           titlePanel("Discussion Title"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
  
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p(""),
           h3("About Me"),
           p("My name is Esther Kim and I study Government and East Asian Studies. 
             You can reach me at eekim@college.harvard.edu."))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           column = geom_col(),
           #smooth = geom_smooth(se = TRUE, na.rm = TRUE),
           jitter = geom_jitter(),
           line = geom_line(),
    )
  })
  
  output$plot <- renderPlot({
    ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
      plot_geom()
  }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)



# OLD SHINY APP CODE! 
ui <- navbarPage("Rough Korea Data",
                 tabPanel("Home", 
                          titlePanel("Home"),
                          h3("Welcome to my page!"),
                          p("Hello. My final project for Gov 50: Data uses census and 
               immigration data in order to track and analyze the patterns of 
               immigration to Korea over the last few decades. I look at the total
               numbers of visitors, the types of visas (from business 
               to tourism to permanent residency) acquired, the sex breakdown, and
               the age group breakdown of incoming migrations. Taking economic
               data from the World Bank also gave me descriptions of the GDP and
               economic growth rates that I chart alongside these other variables.
               From here on, all reference to 'Korea' will indicate South Korea 
               unless otherwise noted. \n As Korea's standard of living has 
               risen in the decades of industrialization and modernization
               following the Korean War, there has been an increasing amount of 
               global interest in the country. How have patterns of migration
               to the country changed over the years, which sectors have grown
               the most, and how do significant domestic changes in the economy 
               (such as the 1997 IMF Crisis) or politics (such as 
               democratization in 1988) affect immigration?
               Overall why do people, both foreign and Korean 
               nationals, choose to go to Korea?"),
                          h3(""),
                          p(""),
                          img(scr = "peninsula.png")
                 ),
                 
                 
                 tabPanel("Model",
                          fluidPage(
                            selectInput("x", "X variable", choices = c("Total", "Male", "Female", "Reason", "year")),
                            selectInput("y", "Y variable", choices = c("Total", "Male", "Female", "Reason")),
                            selectInput("geom", "select", c("point", "column",
                                                            "smooth")),
                            plotOutput("plot") 
                          )),
                 
                 tabPanel("Graphs",
                          tabsetPanel(
                            tabPanel("Summary", dataTableOutput("all_reasons")),
                            tabPanel("Plot", plotOutput("plot1")),
                            tabPanel("Plot", plotOutput("plot2"))
                          )
                 ),
                 
                 tabPanel("Discussion",
                          fluidPage(
                            titlePanel("Discussion Title"),
                            p("Tour of the modeling choices you made and 
              an explanation of why you made them")
                          )),
                 
                 tabPanel("About", 
                          fluidPage(
                            titlePanel("About"),
                            h3("Project Background and Motivations"),
                            p("For this project, I aim to use immigration and census data from
               the US (roughly over the last 50 years) and South Korea in order
               to evaluate trends concerning the desirability of pursuing life
               in the US vs. staying or repatriating to Korea.
               Sources currently include the Yearbook of Immigration Statistics from the Department of Homeland
               Security for numbers of legal Korean immigrants by year, economic data from the Pew Research Center,
               and the CIA World Factbook. I still have to look into sources to obtain figures for the
               emigration of Korean Americans to Korea, which will probably entail digging into
               the Korean immigration ministry records."),
                            h3("About Me"),
                            p("My name is Esther Kim and I study Government and East Asian Studies. 
             You can reach me at eekim@college.harvard.edu."),
                            a("Github", href = "https://github.com/2023kime?tab=repositories")
                          ))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plot_geom <- reactive({
    switch(input$geom,
           point = geom_point(),
           smooth = geom_smooth(se = TRUE, na.rm = TRUE),
           column = geom_col(),
    )
  })
  
  output$plot <- renderPlot({
    ggplot(edited_korea2, aes(.data[[input$x]], .data[[input$y]]))+
      plot_geom()
  }, res = 96)
  
}


output$plot1 <- renderPlot({
  Plot1 <- Total
  Plot1
})
output$plot2 <- renderPlot({
  Plot2 <- all_reasons
  Plot2
})

# Run the application 
shinyApp(ui = ui, server = server)