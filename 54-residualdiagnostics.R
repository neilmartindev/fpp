fb_stock |> autoplot(Close)

fit <- fb_stock |> model(NAIVE(Close))
augment(fit) 

augment(fit) |>
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) |>
  filter(trading_day > 1100) |>
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) |>
  autoplot(.resid) +
  labs(y = "$US",
       title = "Residuals from naive method")

augment(fit) |>
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  labs(title = "Histogram of residuals")

augment(fit) |>
  ACF(.resid) |>
  autoplot() + labs(title = "ACF of residuals")

gg_tsresiduals(fit)

augment(fit) |>
  features(.resid, ljung_box, lag = 10)
