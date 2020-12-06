#Since 1991 South Korea has experienced a large influx of foreign workers. 
#Approximately 10,000 Asian workers came to South Korea under a newly 
#established trainee program in 1992.
#Other important dates: Korea was net sender of immigrants until 2007
#In 2007 the UN declared South Korea an official receiving country. 
#The number of foreigners in South Korea grew from 390,000 in 1997 to 1 million in 2007. 

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
setwd("~/Desktop/Projects/Final-Project/Shiny/Korea")
write.csv(EARFEIwithGDP, "data/EARFEIwithGDP.csv")

