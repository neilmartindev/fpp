eggs <- prices |>
  filter(!is.na(eggs)) |>
  select(eggs)
eggs |> autoplot() +
  labs(title = "Annual egg prices", y = "US$ (adjusted for inflation")

fit <- eggs |>
  model(RW(log(eggs) ~ drift ()))
fit

fc <- fit |>
  forecast(h = 50)
fc

fc |> autoplot(eggs) +
  labs(title = "Annual egg prices",
       y = "US% (adjusted for inflation")

fc |>
  autoplot(eggs, level = 80, point_forecast = lst(mean, median)) +
  labs(title = "Annual egg prices",
       y = "US$ (adjusted for inflation")
