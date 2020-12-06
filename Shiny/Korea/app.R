library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)
library(rvest)
library(rstanarm)
library(shinythemes)
library(gganimate)

# UI is defined for the app. Title is labeled.
EARFEIwithGDP <- read_csv("data/EARFEIwithGDP.csv")

ui <- navbarPage(
  fluidPage(titlePanel(title = "Modern Patterns in Korean Immigration"),
  
  # The first Tab shows my faceted graphs for different visa types.
  
                 # tabPanel(title = "Types of Visas",
                 # fluidPage(
                 # titlePanel("Reasons why People Visit Korea"),
                 # sidebarLayout(
                 # sidebarPanel(
                 # selectInput(
                 # radioButtons(
                 #   inputId = "reason",
                 #   label = "Choose the type of Visa",
                 #   choices = c("Academic" = `a`,
                 #             "Employment" = `b`,
                 #             "Entertainment" = `c`,
                 #             "Family" = `d`, 
                 #             "Investment" = `e`, 
                 #             "Religion" = `f`),
                 # selected = "Academic"), 
                 # mainPanel(plotOutput("lineplot"))))))),
 # The main Panel for this tab.
imageOutput("plot1")),


              tabPanel("Visa Permanency",
                       fluidPage(),
                       mainPanel(plotOutput("plot2"))),

 # The next tab shows my model of the ways different numbers of visas granted 
 # each year impacted Korean domestic economic growth. The second model is
 # a predictor of permanent v. temporary visas.
 
                 tabPanel("Model",
                   titlePanel("Models of Korean Immigration and Economic Data"),
                          p("My first model examines the influence different
                            types of visas granted since 2000 affect the 
                            domestic Korean economy. 
                            
                            My second model re-classified all visa applications
                            into two categories: temporary and permanent. The
                            increasing popularity of permanent visas in the
                            years since 2000 are tracked in the graph below.
                            It then predicts the number of permanent visa
                            applications that will be sought in 2030, based on
                            economic and visa trends in the existing data.
                            
m1 <- lm(Perm_sum ~ GDP + Year,
         data = joined)
summary(m1)
m1$coefficients[1] + m1$coefficients[2] * 28605.73 + m1$coefficients[3] * 2030")),
                 
                 tabPanel("About",
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
                          p("For much of the twentieth century, the 
                          Korean peninsula 
           experienced political, social, and economic ravagement: the 1900s 
           began with colonialism under Japan, the division of the country in 
           two after World War II, a devastatingly bloody Korean War, and 
           temporary occupation by foreign powers. As a result, Korea was one of
           the poorest countries in Asia, and many Koreans immigrated out of the
           country to pursue better lives for themselves and their families. 
           However, at the close of the century, Korea became renown for its 
           explosive economic recovery and growth, dubbed the 'miracle on the 
             Han River'; at the same time, the world reeled from 'Hallyu,' or 
             the Korean Wave phenomenon, in which Korean cultural products 
             (from music to food to media) skyrocketed to global popularity. 
             In this project, I take a look at how Korean soft power,
             measured through visas of entry granted each year, interacted and 
             predicted the changes in Korean 'hard' power, or economic status. 
             My data indicates patterns of foreign interest in traveling to 
             Korea from 2000 onwards."),
            h3("About Me"),
            p("My name is Esther Kim, and I am a sophomore at Harvard 
            studying Government and East Asian Studies.
            My contact is eekim@college.harvard.edu. 
            My Github account is https://github.com/2023kime."),
            
            p("A personal note--I myself am a second-generation Korean-American
             who is increasingly influenced by and curious about Korean culture.
             In stark contrast to my parents who left the country, I would like 
             to 'return' to Korea, literally and academically. Why others all 
             across the world are also enchanted by the opportunities of Korea 
             is an interesting question to me, and this information can be quite 
             useful for crafting Korean policy towards foreigners going forwards 
             as well as better understanding Korea's extraordinary modern 
              history.")
                 ))

# In the server, I dumped all of my data wrangling from the raw csv, the 
# summarizing and joining of different information about each visa type,
# and the way I combined this data with Korea's GDP data. I then plotted 
# this information as a reactive render plot of the input (type of visa selected
# by the radio button options).

server <- function(input, output){
outfile <- tempfile(fileext='.gif')
output$plot1  <- outfile
  
output$plot2 <- 
  
  # ggplot(EARFEIwithGDP, aes(Year, Percentages, color = Visa)) +
  #   geom_line() +
  #   facet_wrap(~ Visa) +
  #   geom_vline(xintercept = 2007, color = "darkgray") +
  #   geom_line(data = EARFEIwithGDP, aes(y = GDP_growth), color = "black", 
  #             lty = "dashed") +
  #   theme_bw() +
  #   labs(x = "Years", y = "Percentages",
  #        title = "Reasons why People Visit Korea",
  #        subtitle = "Types of Visas Granted from 2000 to 2020")
  
# withGDP <- full_join(join_pivot, kgrowth, by = "Year") %>%
#   rename(GDP_growth = 'GDP Growth') %>%
#   mutate(GDP_growth = (GDP_growth/5.8)*100) %>%
#   drop_na()
# join_pivot <- full_join(korea_GDP, Temp_Perm, by = "Year") %>%
#   select(Year, Temp, Perm) %>%
#   pivot_longer(cols = !Year, names_to = "Visa",
#                values_to = "Percentages")

# ouput$secondlineplot <- ggplot(withGDP, aes(Year, Percentages, color = Visa)) +
#   geom_line() +
#   geom_line(data = withGDP, 
#             aes(y = GDP_growth), 
#             color = "black", 
#             lty = "dashed") +
#   scale_color_manual(values = c("Temp" = "orange", "Perm" = "blue")) +
#   theme_linedraw() +
#   labs(x = "Years", y = "Percentages",
#        title = "Percentage of Visas for Permanent v. Temporary Stays in Korea",
#        subtitle = "2000 to 2020")
 # })
} 

shinyApp(ui = ui, server = server)


