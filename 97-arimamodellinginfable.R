global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(title = "Central African Republic exports" , y = "% of GDP")

global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type = "partial")

caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = FALSE)
  )

caf_fit |> pivot_longer(!Country,
                        names_to = "Model name",
                        values_to = "Orders")

glance(caf_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

caf_fit |>
  select(search) |>
  gg_tsresiduals()

augment(caf_fit) |>
  filter(.model == "search") |>
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit |>
  forecast(h = 5) |>
  filter(.model == "search") |>
  autoplot(global_economy)
