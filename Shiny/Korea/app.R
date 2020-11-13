#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)
library(rvest)
library(rstanarm)



# Define UI for application that draws a histogram
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
