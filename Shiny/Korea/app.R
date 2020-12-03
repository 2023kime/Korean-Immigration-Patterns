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

ui <- navbarPage(title = "Modern Patterns in Korean Immigration",
                 tabPanel(title = "Graphs",
                          fluidPage(titlePanel("Reasons why People Visit Korea"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(
                                          radioButtons(inputId = "p", 
                                                       label = "Choose the type of Visa",
                                                       choices = c("Academic" = `a`, 
                                                                   "Employment" = `b`, 
                                                                   "Entertainment" = `c`, 
                                                                   "Family" = `d`, 
                                                                   "Investment" = `e`, 
                                                                   "Religion" = `f`),
                                                       selected = "Academic")),
                                        mainPanel(plotOutput("linePlot")))))),
                 
                 tabPanel("Analysis",
                          titlePanel("Models of Korean Immigration and Economic Data"),
                          p("I have several models.")),
                 
                 tabPanel("About",
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
                          p("Hello! For much of the twentieth century, the Korean peninsula 
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
             Korea from 2000 onwards. 
             A personal note--I myself am a second-generation Korean-American
             who is increasingly influenced by and curious about Korean culture.
             In stark contrast to my parents who left the country, I would like 
             to 'return' to Korea, literally and academically. Why others all 
             across the world are also enchanted by the opportunities of Korea 
             is an interesting question to me, and this information can be quite 
             useful for crafting Korean policy towards foreigners going forwards 
             as well as better understanding Korea's extraordinary modern 
             history.")
                 ))

server <- function(input, output){
  library(ggplot2)
  library(tidyverse)
  
  # Data on all immigration reasons by M/F/T from 2000 to 2019
  all_reasons <- read_csv("edited_korea2.csv", col_types = cols(X1 = col_double(),
                                                                Reason = col_character(),
                                                                year = col_double(),
                                                                Age = col_character(),
                                                                Male = col_double(),
                                                                Female = col_double(),
                                                                Total = col_double())) %>%
    select(-X1) %>%
    filter(Age == "Total") %>%
    rename("Year" = "year")
  
  Categories <- all_reasons %>%
    group_by(Reason) %>%
    summarize(sum(Total), .groups = "drop")
  
  # This is total incoming visas from 2000 to 2019. Should calculate the other
  # variables as percentages of visas that year for graphing
  Total <- all_reasons %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("Sum" = "sum(Total)")
  totalsum <- Total$Sum
  
  
  # This has Job seeking by M/F/T for all ages from 2000 to 2020
  # Employment shows all job-related reasons for entering Korea
  Employment <- all_reasons %>%
    filter(Reason == "Job Seeking" | Reason == "Short-term Employment" | Reason == "Unskilled Employment" | Reason == "General Trainees") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Employment_Percentage = (sum/totalsum)*100)
  total_Employment <- Employment$sum
  
  # Academic shows all academic reasons for entering Korea (study, research)
  Academic <- all_reasons %>%
    filter(Reason == "study" | Reason == "Research")  %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Academic_Percentage = (sum/totalsum)*100)
  total_Academic <- Academic$sum
  
  # Religious activities
  Religious <- all_reasons %>%
    filter(Reason == "Religious Activities") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Religion_Percentage = (sum/totalsum)*100)
  total_Religious <- Religious$sum
  
  # Family-related reasons
  Family <- all_reasons %>%
    filter(Reason == "Visiting and Joining Family" | Reason == "Marriage Immigration") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Family_Percentage = (sum/totalsum)*100)
  total_Family <- Family$sum
  
  # Tourism and Entertainment
  Entertainment <- all_reasons %>%
    filter(Reason == "Sightseeing Pass" | Reason == "Art and Entertainment") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Entertainment_Percentage = (sum/totalsum)*100)
  total_Entertainment <- Entertainment$sum
  
  # Business/foreign investment
  Investment <- all_reasons %>%
    filter(Reason == "Investors" | Reason == "Short-term Business" | Reason == "Trade and Business") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("sum" = `sum(Total)`) %>%
    mutate(Investment_Percentage = (sum/totalsum)*100)
  total_Investment <- Investment$sum
  
  # This is the percentage of visa seekers who sought temporary v. permanent stays
  Temp <- all_reasons %>%
    filter(Reason == "Short-term Visitors") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("Temp_sum" = `sum(Total)`)
  Perm <- all_reasons %>%
    filter(Reason == "Permanent Residence") %>%
    group_by(Year) %>%
    summarize(sum(Total), .groups = "drop") %>%
    rename("Perm_sum" = `sum(Total)`)
  # percentage out of 100
  Temp_Perm <- full_join(Temp, Perm, by = "Year") %>%
    mutate(Total_sum = Temp_sum + Perm_sum) %>%
    mutate(Temp_percentage = (Temp_sum/Total_sum)*100) %>%
    mutate(Perm_percentage = (Perm_sum/Total_sum)*100) %>%
    rename(Temp = Temp_percentage, Perm = Perm_percentage)
  # calculate percentage of temp/perm from whole instead of each other 
  # (not add to 100, unlike Temp_Perm above)
  temp_v_perm <- full_join(Temp, Perm, by = "Year") %>%
    mutate(Temp_percentage = (Temp_sum/totalsum)*100) %>%
    mutate(Perm_percentage = (Perm_sum/totalsum)*100) %>%
    rename(Temp = Temp_percentage, Perm = Perm_percentage)
  
  # korea_GDP has GDP from 2000 to 2020
  korea_GDP <- read_excel("korea_data.xls", skip = 3) %>%
    select("Indicator Name", c("2000":"2020")) %>%
    rename("Indicator" = "Indicator Name") %>%
    filter( Indicator == "GDP per capita (constant 2010 US$)" ) %>%
    pivot_longer(cols = 2:22, names_to = "Year") %>%
    mutate(Year = as.double(.$Year))
  # GROWTH from the same korea economic data
  korea_growth <- read_excel("korea_data.xls", skip = 3) %>%
    select("Indicator Name", c("2000":"2020")) %>%
    rename("Indicator" = "Indicator Name") %>%
    filter( Indicator == "GDP growth (annual %)") %>%
    pivot_longer(cols = 2:22, names_to = "Year") %>%
    mutate(Year = as.double(.$Year))
  # For the graphs file
  kGDP <- korea_GDP %>%
    select(Year, value) %>%
    rename(GDP = value)
  kgrowth <- korea_growth %>%
    select(Year, value) %>%
    rename("GDP Growth" = value) %>%
    filter(Year != 2020)
  EA <- full_join(Employment, Academic, by = "Year")
  EAR <- full_join(EA, Religion, by = "Year") 
  EARF <- full_join(EAR, Family, by = "Year") 
  EARFE <- full_join(EARF, Entertainment, by = "Year") 
  EARFEI <- full_join(EARFE, Investment, by = "Year") %>%
    rename(Employment = Employment_Percentage,
           Academic = Academic_Percentage,
           Religion = Religion_Percentage,
           Family = Family_Percentage,
           Entertainment = Entertainment_Percentage,
           Investment = Investment_Percentage)
  join <- full_join(kgrowth, EARFEI, by = "Year") %>%
    select(Year, Employment, Academic, Religion, Family, 
           Entertainment, Investment) %>%
    pivot_longer(cols = !Year, names_to = "Visa",
                 values_to = "Percentages")
  EARFEIwithGDP <- full_join(join, kgrowth, by = "Year") %>%
    rename(GDP_growth = 'GDP Growth') %>%
    #  mutate(GDP_growth = (GDP_growth)) %>%
    drop_na()
  # data <- reactive({
  #   EARFEIwithGDP %>%
  #     filter(Visa == input$reason)})
  
  output$linePlot <- renderPlot({ 
    if(input$p == `a`){data <- EARFEIwithGDP %>% filter(Visa == "Academic")}
    if(input$p == `b`){data <- EARFEIwithGDP %>% filter(Visa == "Employment")}
    if(input$p == `c`){data <- EARFEIwithGDP %>% filter(Visa == "Entertainment")}
    if(input$p == `d`){data <- EARFEIwithGDP %>% filter(Visa == "Family")}
    if(input$p == `e`){data <- EARFEIwithGDP %>% filter(Visa == "Investment")}
    if(input$p == `f`){data <- EARFEIwithGDP %>% filter(Visa == "Religion")}
    ggplot(data, aes(Year, Percentages, color = Visa)) +
      geom_line() +
      #     facet_wrap(~ Visa) +
      geom_vline(xintercept = 2007, color = "darkgray") +
      geom_line(data = EARFEIwithGDP, aes(y = GDP_growth), color = "black", 
                lty = "dashed") +
      theme_bw() +
      labs(x = "Years", y = "Percentages",
           title = "Reasons why People Visit Korea",
           subtitle = "Types of Visas Granted from 2000 to 2020")
  })
} 
shinyApp(ui = ui, server = server)


