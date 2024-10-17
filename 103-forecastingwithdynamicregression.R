fit <- us_change |> model(ARIMA(Consumption ~ Income))
us_change_future <- new_data(us_change, 8) |>
  mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) |>
  autoplot(us_change) +
  labs(x = "Year", y = "Percentage change",
       title = "Forecasts from regression with ARIMA(1,0,2) erros")

  