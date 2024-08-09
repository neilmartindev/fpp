a10 |>
  autoplot(Cost)

a10 |> gg_season(Cost, labels="both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

beer <- aus_production |>
  select(Quarter, Beer) |> filter(year(Quarter) >= 1992)

beer |> autoplot(Beer) + 
  labs(title = "Australian beer production", y = "Megalitres")

beer |> autoplot(Beer) + geom_point() +
  labs(title = "Australian beer production", y = "Megalitres")

beer |> gg_season(Beer, labels = "right")

vic_elec

vic_elec |> autoplot()

vic_elec  |> gg_season(Demand)

vic_elec |> gg_season(Demand, period = "week")
