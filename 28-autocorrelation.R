recent_production |> ACF(Beer) |> autoplot()

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015) |>
  select(Date, Close)

google_2015 |> autoplot()
