aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Population = Population/1e6)

aus_economy |>
  slice(-n()) |>
  stretch_tsibble(.init = 10) |>
  model(
    ETS(Population),
    ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, RMSE:MAPE)

aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy |> filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
train <- cement |> filter_index(. ~ "2007 Q4")

fit_arima <- train |> model(ARIMA(Cement))
report(fit_arima)
fit_arima |> gg_tsresiduals(lag_max = 16)

fit_ets <- train |> model(ETS(Cement))
report(fit_ets)
fit_ets |>
  gg_tsresiduals(lag_max = 16)