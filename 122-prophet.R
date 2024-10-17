library(fable.prophet)
cement <- aus_production |>
  filter(year(Quarter) >= 1988)
train <- cement |>
  filter(year(Quarter) <= 2007)
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2,
                                      type = "multiplicative"))
  )

fc <- fit |> forecast(h = "2 years 6 months")
fc |> autoplot(cement)

fc |> accuracy(cement)

fit <- elec |>
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
              season(period = "day", order = 10) +
              season(period = "week", order = 5) +
              season(period = "year", order = 3))
  )
fit |>
  components() |>
  autoplot()

fit |> gg_tsresiduals()

fc <- fit |>
  forecast(new_data = elec_newdata)
fc |>
  autoplot(elec |> tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")