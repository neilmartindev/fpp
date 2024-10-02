fit <- global_economy |>
  mutate(Pop = Population / 1e6) |>
  model(ets = ETS(Pop))
fit

fit |> forecast(h = 5)

aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips))

fit <- aus_holidays |>
  model(ets = ETS(Trips)) |>
  report()

components(fit) |> autoplot() + labs(title = "ETS(M,N,M) components")

residuals(fit)
