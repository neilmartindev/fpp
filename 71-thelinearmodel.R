# Simple linear regression

us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")

us_change |> 
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quaterly % change)", 
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

# Multiple linear regression

us_change |> 
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

us_change |> 
  GGally::ggpairs(columns = 2:6)