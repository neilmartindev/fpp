google_2018 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018)

google_2018 |>
  autoplot(Close) +
  labs(y = "Closing stock price ($USD")

google_2018 |>
  ACF(Close) |>
  autoplot()

a10 <- PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost) / 1e6)

a10 |> autoplot()

a10 |> autoplot(
  log(Cost)
  )

a10 |> autoplot(
  log(Cost) |> difference(12)
)

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |> autoplot()

h02 |> autoplot(
  log(Cost)
)

h02 |> autoplot(
  log(Cost) |> difference(12) |> difference(1)
)
