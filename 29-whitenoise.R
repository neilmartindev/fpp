set.seed(30)
wn <- tsibble(t = 1:50, y = rnorm(50), index = t)
wn |> autoplot(y)

wn |> ACF(y) |> autoplot()

pigs <- aus_livestock |>
  filter(State == "Victoria", Animal == 'Pigs', year(Month) >= 2014)

pigs |> autoplot(Count / 1e3) +
  labs(y = "Thousands", title = "Number of pigs slaughtered in Victoria")

pigs

pigs |> 
  ACF(Count) |>
  autoplot()
 