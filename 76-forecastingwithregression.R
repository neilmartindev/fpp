recent_production |> model (TSLM(Beer ~ trend() + season())) |>
  forecast() |> autoplot(recent_production)

fit_consBest <- us_change |>
  model(
    TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 1, Savings = 0.5, Unemployment = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |> autoplot(Consumption) +
  labs(y = "% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")