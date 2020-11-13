#graphs

korea_GDP

Total <- all_reasons %>%
  group_by(Year) %>%
  summarize(sum(Total)) %>%
  rename("Sum" = "sum(Total)")
totalsum <- Total$Sum

temp_v_perm <- full_join(Temp, Perm, by = "Year") %>%
  mutate(Temp_percentage = (Temp_sum/totalsum)*100) %>%
  mutate(Perm_percentage = (Perm_sum/totalsum)*100) %>%
  rename(Temp = Temp_percentage, Perm = Perm_percentage)

korea_GDP

join_pivot <- full_join(korea_GDP, temp_v_perm, by = "Year") %>%
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
