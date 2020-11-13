#Since 1991 South Korea has experienced a large influx of foreign workers. 
#Approximately 10,000 Asian workers came to South Korea under a newly 
#established trainee program in 1992.
#Other important dates: Korea was net sender of immigrants until 2007
#In 2007 the UN declared South Korea an official receiving country. 
#The number of foreigners in South Korea grew from 390,000 in 1997 to 1 million in 2007. 

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
  summarize(sum(Total))


# This has Job seeking by M/F/T for all ages from 2000 to 2020
# Employment shows all job-related reasons for entering Korea
Employment <- all_reasons %>%
  filter(Reason == "Job Seeking" | Reason == "Short-term Employment" | Reason == "Unskilled Employment" | Reason == "General Trainees") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Employment_Percentage = (sum/totalsum)*100)

# Academic shows all academic reasons for entering Korea (study, research)
Academic <- all_reasons %>%
  filter(Reason == "study" | Reason == "Research")  %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Academic_Percentage = (sum/totalsum)*100)

# Religious activities
Religion <- all_reasons %>%
  filter(Reason == "Religious Activities") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Religion_Percentage = (sum/totalsum)*100)

# Family-related reasons
Family <- all_reasons %>%
  filter(Reason == "Visiting and Joining Family" | Reason == "Marriage Immigration") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Family_Percentage = (sum/totalsum)*100)

# Tourism and Entertainment
Entertainment <- all_reasons %>%
  filter(Reason == "Sightseeing Pass" | Reason == "Art and Entertainment") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Entertainment_Percentage = (sum/totalsum)*100)

# Business/foreign investment
Investment <- all_reasons %>%
  filter(Reason == "Investors" | Reason == "Short-term Business" | Reason == "Trade and Business") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("sum" = `sum(Total)`) %>%
  mutate(Investment_Percentage = (sum/totalsum)*100)

# This is the percentage of visa seekers who sought temporary v. permanent stays
Temp <- all_reasons %>%
  filter(Reason == "Short-term Visitors") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Temp_sum" = `sum(Total)`)
Perm <- all_reasons %>%
  filter(Reason == "Permanent Residence") %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Perm_sum" = `sum(Total)`)
Temp_Perm <- full_join(Temp, Perm, by = "Year") %>%
  mutate(Total_sum = Temp_sum + Perm_sum) %>%
  mutate(Temp_percentage = (Temp_sum/Total_sum)*100) %>%
  mutate(Perm_percentage = (Perm_sum/Total_sum)*100) 
# calculate percentage of temp/perm from whole instead of each other (not add to 100)
Rawer_join <- full_join(Temp, Perm, by = "Year") %>%
  mutate(Temp_percentage = (Temp_sum/totalsum)*100) %>%
  mutate(Perm_percentage = (Perm_sum/totalsum)*100)

# This is total incoming visas from 2000 to 2019. Should calculate the other
# variables as percentages of visas that year...? for graphing
Total <- all_reasons %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Sum" = "sum(Total)")
totalsum <- Total$Sum



# korea_GDP has GDP from 2000 to 2020
korea_GDP <- read_excel("korea_data.xls", skip = 3) %>%
  select("Indicator Name", c("2000":"2020")) %>%
  rename("Indicator" = "Indicator Name") %>%
  filter( Indicator == "GDP per capita (constant 2010 US$)" ) %>%
  pivot_longer(cols = 2:22, names_to = "Year") %>%
  mutate(Year = as.double(.$Year))
View(korea_GDP)

# This has Job Seeking by M/F/T and GDP values from 2000 to 2020
joined <- full_join(edited_korea3, korea_GDP, by = "Year")
View(joined)
stan_glm(formula = Total ~ value,
         data = joined,
         refresh = 0) %>%
  print(digits = 10, deatil = FALSE)

#Indicator == "GDP growth (annual %)" |
