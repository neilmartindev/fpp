ARIMA(y ~ x + pdq(1,1,0))

us_change |>
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

fit <- us_change |>
  model(ARIMA(Consumption ~ Income))
report(fit)

bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) |>
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "ARIMA residuals"))
  ) |>
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit |> gg_tsresiduals()

augment(fit) |>
  features(.innov, ljung_box, dof = 3, lag = 8)