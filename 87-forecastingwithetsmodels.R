h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)) 

h02 |> autoplot(Cost)

h02 |>
  model(ETS(Cost)) |>
  report()

h02 |>
  model(ETS(Cost ~ error("A") + trend("A") + season("A"))) |>
  report()

h02 |>
  model(ETS(Cost)) |>
  forecast() |>
  autoplot(h02)

h02 |>
  model(
    auto = ETS(Cost),
    AAA = ETS(Cost ~ error("A") + trend("A") + season("A"))
  ) |>
  accuracy()

