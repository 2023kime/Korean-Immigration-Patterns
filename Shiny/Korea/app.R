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

d <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_ev_probabilities_2020.csv",
              col_types = cols(cycle = col_double(),
                               branch = col_character(),
                               model = col_character(),
                               modeldate = col_character(),
                               candidate_inc = col_character(),
                               candidate_chal = col_character(),
                               candidate_3rd = col_logical(),
                               evprob_inc = col_double(),
                               evprob_chal = col_double(),
                               evprob_3rd = col_logical(),
                               total_ev = col_double(),
                               timestamp = col_character(),
                               simulations = col_double())) %>%
  select(total_ev, evprob_inc, evprob_chal)
View(d)


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

