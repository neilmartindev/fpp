global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))

report(fit)

gg_tsresiduals(fit)

fit |> 
  forecast(h = 10) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

egypt <- global_economy |> filter(Code == "EGY")
egypt |> ACF(Exports) |> autoplot()
egypt |> PACF(Exports) |> autoplot()

global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(Exports, plot_type = "partial")


