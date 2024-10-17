my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline |>
  model(stl_ets = my_dcmp_spec) |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")

gas_dhr <- us_gasoline |>
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

gas_dhr |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")