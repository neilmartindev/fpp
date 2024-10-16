leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure |>
  gg_tsdisplay(difference(Employed, 12), plot_type = "partial", lag = 36) +
  labs(title = "Seasonally differenced", y="")

leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type = "partial", lag = 36) +
  labs(title = "Double differenced", y="")

fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )

fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

fit

glance(fit) |>
  arrange(AICc) |>
  select(.model:BIC)

fit |>
  select(auto) |>
  gg_tsresiduals(lag = 36)

augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag = 24, dof = 4)

forecast(fit, h = 36) |>
  filter(.model == "auto") |>
  autoplot(leisure) +
  labs(title = "US employment: leisure & hospitality", y = "People (millions)")

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |> autoplot(
  Cost
)

h02 |> autoplot(
  log(Cost) |> difference(12)
)

h02 |> gg_tsdisplay(difference(log(Cost), 12),
                    lag_max = 36, plot_type = "partial"
)

fit <- h02 |>
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2)))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)

fit <- h02 |> model(auto = ARIMA(log(Cost)))
report(fit)

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)

fit <- h02 |>
  model(best = ARIMA(log(Cost),
                     stepwise = FALSE, approximation = FALSE,
                     order_constraint = p + q + P + Q <= 9))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 9)

h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) |>
  forecast() |>
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")