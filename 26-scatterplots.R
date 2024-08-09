vic_elec_day_type <- vic_elec |>
  filter(year(Time) == 2014) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday", 
    TRUE ~ "Weekend"))

vic_elec_day_type |> autoplot(Demand)

vic_elec_day_type |>
  ggplot(aes(x = Temperature, y = Demand, color = Day_Type)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Elecricity demand (GW)")

us_change |> GGally::ggpairs(columns = 2:6)
