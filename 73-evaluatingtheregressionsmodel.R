fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()
