insurance |>
  pivot_longer(Quotes:TVadverts) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

fit <- insurance |>
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |>
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts) +
                   lag(TVadverts, 2) + lag(TVadverts, 3))
  )

glance(fit)

fit_best <- insurance |>
  model(ARIMA(Quotes ~ pdq(d = 0) +
                TVadverts + lag(TVadverts)))
report(fit_best)

insurance_future <- new_data(insurance, 20) |>
  mutate(TVadverts = 8)
fit_best |>
  forecast(insurance_future) |>
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )