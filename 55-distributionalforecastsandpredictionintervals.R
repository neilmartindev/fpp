aus_production |>
  filter(!is.na(Bricks)) |>
  model(Season_naive = SNAIVE(Bricks)) |>
  forecast(h = "5 years") |>
  hilo(level = 95)