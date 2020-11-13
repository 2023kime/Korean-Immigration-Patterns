#graphs

korea_GDP

Total <- all_reasons %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Sum" = "sum(Total)")
totalsum <- Total$Sum

# out of 100
Temp_Perm <- full_join(Temp, Perm, by = "Year") %>%
  mutate(Total_sum = Temp_sum + Perm_sum) %>%
  mutate(Temp_percentage = (Temp_sum/Total_sum)*100) %>%
  mutate(Perm_percentage = (Perm_sum/Total_sum)*100) %>%
  rename(Temp = Temp_percentage, Perm = Perm_percentage)
#not out of 100
temp_v_perm <- full_join(Temp, Perm, by = "Year") %>%
  mutate(Temp_percentage = (Temp_sum/totalsum)*100) %>%
  mutate(Perm_percentage = (Perm_sum/totalsum)*100) %>%
  rename(Temp = Temp_percentage, Perm = Perm_percentage)

kGDP <- korea_GDP %>%
  select(Year, value) %>%
  rename(GDP = value)

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
 withGDP <- full_join(join_pivot, kGDP, by = "Year") %>%
   mutate(GDP = (GDP/21191.25)*100) 
 ggplot(withGDP, aes(Year, Percentages, color = Visa)) +
   geom_point() +
   geom_line(data = withGDP, aes(y = GDP)) +
   scale_color_manual(values = c("Temp" = "navy", "Perm" = "darkred")) +
   theme_linedraw() +
   labs(x = "Years", y = "Percentages",
        title = "Percentage of Visas for Permanent v. Temporary Stays in Korea",
        subtitle = "2000 to 2020")
 