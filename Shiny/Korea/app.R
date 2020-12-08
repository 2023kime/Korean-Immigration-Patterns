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
library(gtsummary)

# Important datasets for my two graphs and regression tables are imported as csv.

EARFEIwithGDP <- read_csv("data/EARFEIwithGDP.csv")
withGDP <- read_csv("data/withGDP.csv")
withGDP$Visa <- withGDP$Visa %>% as.character
temp_perm <- read_csv("data/temp_perm.csv")

# UI is defined for the app. Title is labeled. First tab shows six categories
# of breakdown for Visa types.

ui <- navbarPage("Modern Patterns in Korean Immigration",
                 tabPanel("Visa Types",
                          fluidPage(theme = shinytheme("journal"),
                          titlePanel("Korean Growth and Travel Visas"),
                          h4("Historical Context"),
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
             Due to Hallyu as well as intensive government-backed investment
             in export industries, Korea has gained more attention from foreign
             countries."),
h4("Tracking Patterns through Visas"),
p("In this project, I take a look at how fluctuations in the Korean economy,
based on both domestic reforms and global financial crises, influenced
the appeal of visiting Korea for those outside the country. The visas of entry
into Korea granted each year since 2000 are separated into six popular 
categories: Academic, Employment, Entertainment, Family, Investment, and 
Religious-based reasons for traveling to Korea."),
mainPanel(plotOutput("plot1"), height = 650),
h3("Important Dates"),
          h4("1997"),
          p("The IMF crisis hit Korean growth heavily in 1997, and outstanding
          national loans were felt heavily as Korea entered the 21st century."),
          h4("2007"),
          p("Several important things happened in this year, demarcated on the 
          graphs by the gray line. Korea firstly went from a net sending nation 
          to a net receiving nation of immigrants."),
          p("However, the Global Financial Crisis caused a severe decline of 
            growth that would continue until 2009."),
          h4("2010"),
          p("Recovery entailed a sudden peak in growth in 2010. Government
          directed market reforms reorganized chaebol conglomerates and 
          liberalized the economy to help recovery. Going onwards, growth rate 
          has stabilized around the 3% mark.")
          )),

# Second tab is for showing length of stay comparison in visas.

              tabPanel("Visa Permanency",
                       fluidPage(
                         titlePanel("Length of Stay in Korea"),
                       mainPanel(h4("Permanent v. Temporary Visas"),
                                 p("The attractiveness of visiting Korea for 
                                   a short time v. deciding to settle in Korea
                                   to build a livelihood must be influenced
                                   by changed in Korea's wealth and living 
                                   standards. How did these broad 
                                   classifications of visas change as Korean
                                   GDP changed over time?"),
                                 p("In the interactive graph below, the black
                                   dashed line traces Korean GDP, scaled as a 
                                   percentage of of the GDP in 2007, the 
                                   significant year when Korea became a receiver
                                   nation. Temporary and Permanent Visas are 
                                   calculated as percentages as well, depending 
                                   on how many visas granted were for those 
                                   respective lengths of stay in a given year."),
                                sidebarLayout(
                                  sidebarPanel(
                                    p("Select Type of Visa"),
                                  selectInput(
                                    inputId = "visa",
                                    label = "Choose Length of Visa Stay",
                                    choices = c("Perm", "Temp"),
                                  )),
                                  mainPanel(
                                    h4("Visas of entry to Korea 2000-2019"),
                                  plotOutput("plot2", height = 500, width = 500))
                                ))
                       )),

 # The next tab shows my model of the ways different numbers of visas granted 
 # each year impacted Korean domestic economic growth. The second model is
 # a predictor of permanent v. temporary visas.
 
                 tabPanel("Model",
                   titlePanel("Models of Korean Immigration and Economic Data"),
                          p("My first model examines how the fluctuations in GDP
                          really affected the types of visas that were granted,
                          and by extension the appeal of traveling or staying in
                          Korea for foreigners. If GDP plays such an important
                            role in Korea's appeal to the world and helped cause 
                            it's switch from a sending to receiver nation in
                            2007, the correlation should be positive between
                            GDP and both temporary/permanent visas."),
                    mainPanel(gt_output("fit")),
                   p(""),
                       mainPanel(gt_output("fit2")),
                   h3("Regression of Temporary v. Permanent Visas"),
                   p("The first set of regressions looks at the regression of 
                   GDP on Temporary Visa applications. If GDP were to be 0, 
                   there would clearly be a very nonexistent demand for either 
                   temporary and/or permanent visas. For every unit increase in
                   GDP per year, however, demand for temporary visas increases
                   by 0.37 visas.
                   And demand for permanent visas increases at a more cautious
                   rate, at 0.02 visas per unit increase in GDP/year."),
                   p(""),
                   h3("Regression 2"),
                   p("My second model looks at how the other types of visas were
                   impacted by GDP growth."),
                   ),
              
# Last tab is for background on the project and me.

                 tabPanel("About",
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
            p("I myself am a second-generation Korean-American who is 
              increasingly influenced by and curious about Korean culture. 
              In stark contrast to my parents who left the country, I would like
              to 'return' to Korea, literally and academically. Why others all 
              across the world are also enchanted by the opportunities of Korea
              is an interesting question to me, and this information can be 
              quite useful for crafting Korean policy towards foreigners going 
              forwards as well as better understanding Korea's extraordinary 
              modern history."),
            h3("About Me"),
            p("My name is Esther Kim, and I am a sophomore at Harvard 
            studying Government and East Asian Studies."),
           p("My contact is eekim@college.harvard.edu. 
            My Github account is https://github.com/2023kime.")
                 ))

# In the server, I have four outputs, two for each graph and two for my 
# regression tables.

server <- function(input, output){

# Output for plot1 is simply importing my animated gif of the faceted graph
# in my first tab.
  
output$plot1  <- renderImage({
  filename <- file.path('animated.gif')
  list(src = filename,
       contentType = 'image/gif',
       alt = 'Animated')
}, deleteFile = FALSE)
  
# Output for plot2 is the reactive plot. Requires importing the final tibble,
# then looking at placing the reactive input element into the dataset.

output$plot2 <- renderPlot({

 filtered_withGDP <- withGDP %>%
     filter(Visa == input$visa)
ggplot(filtered_withGDP, mapping = aes(Year, Percentages, color = Visa)) +
    geom_line() +
    geom_line(withGDP, mapping = aes(y = GDP_growth), color = "black", lty = "dashed") +
    scale_color_manual(values = c("Temp" = "orange", "Perm" = "blue")) +
    theme_linedraw() +
    labs(x = "Years", y = "Percentages",
         title = "Percentage of Visas for Permanent v. Temporary Stays in Korea",
         subtitle = "2000 to 2020")})

# output for the first regression table

output$fit <- render_gt({
  f <- lm(formula = Temp_sum ~ GDP *Year,
            data = temp_perm)
  tbl_regression(f, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = md("**Regression of Temporary Visas**"),
               subtitle = "The Effect of GDP on Temporary Visas Sought") %>%
    tab_source_note(md("Source: Korean Statistical Information Service, 
                       *Immigration Data*")) %>%
                      tab_source_note(md("Worldbank, 
                       *GDP growth annual % - Korea, Rep.*"))})

# output for second regression table

output$fit2 <- render_gt({
  f2 <- lm(formula = Perm_sum ~ GDP *Year,
             data = joined)
  tbl_regression(f2, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = md("**Regression of Permanent Visas**"),
               subtitle = "The Effect of GDP on Permanent Visas Sought") %>%
    tab_source_note(md("Source: Korean Statistical Information Service, 
                       *Immigration Data*")) %>%
    tab_source_note(md("Worldbank, 
                       *GDP growth annual % - Korea, Rep.*"))
})
}

shinyApp(ui = ui, server = server)