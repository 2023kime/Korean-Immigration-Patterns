#graphs


Total <- all_reasons %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Sum" = "sum(Total)")
totalsum <- Total$Sum

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

kGDP <- korea_GDP %>%
  select(Year, value) %>%
  rename(GDP = value)
kgrowth <- korea_growth %>%
  select(Year, value) %>%
  rename("GDP Growth" = value) %>%
filter(Year != 2020)

join_pivot <- full_join(korea_GDP, Temp_Perm, by = "Year") %>%
  select(Year, Temp, Perm) %>%
  pivot_longer(cols = !Year, names_to = "Visa",
               values_to = "Percentages")
 Perm_v_Temp <- ggplot(join_pivot, aes(Year, Percentages, color = Visa)) +
  geom_point() +
    scale_color_manual(values = c("navy", "darkred")) +
   theme_linedraw() +
   labs(x = "Years", y = "Percentages",
        title = "Percentage of Visas for Permanent v. Temporary Stays in Korea",
        subtitle = "2000 to 2020")


 # calculate GDP as a percentage of GDP in 2007, year when Korea became receiver nation
 withGDP <- full_join(join_pivot, kgrowth, by = "Year") %>%
   rename(GDP_growth = 'GDP Growth') %>%
   mutate(GDP_growth = (GDP_growth/5.8)*100) %>%
   drop_na()

 Perm_v_Temp_graph <- ggplot(withGDP, aes(Year, Percentages, color = Visa)) +
   geom_line() +
   geom_line(data = withGDP, aes(y = GDP_growth), color = "black", lty = "dashed") +
   scale_color_manual(values = c("Temp" = "orange", "Perm" = "blue")) +
   theme_linedraw() +
   labs(x = "Years", y = "Percentages",
        title = "Percentage of Visas for Permanent v. Temporary Stays in Korea",
        subtitle = "2000 to 2020")
 
 
 
 
 
 
 
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


plot <- ggplot(EARFEIwithGDP, aes(Year, Percentages, color = Visa)) +
  geom_line() +
  facet_wrap(~ Visa) +
  geom_vline(xintercept = 2007, color = "darkgray") +
  geom_line(data = EARFEIwithGDP, aes(y = GDP_growth), color = "black", 
            lty = "dashed") +
  theme_bw() +
  labs(x = "Years", y = "Percentages",
       title = "Reasons why People Visit Korea",
       subtitle = "Types of Visas Granted from 2000 to 2020")
# just figure out how to add on to legend to indicate gdp growth!

install.packages("gganimate")
library(gganimate)
animated <- plot +  
  geom_point(aes(group = seq_along(Year))) +
  transition_reveal(Year)
