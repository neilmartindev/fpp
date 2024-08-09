a10 |> 
  gg_subseries(Cost) + 
  labs( y = "$ million", 
        title = "Subseries plot: antidiabetic drug sales")

beer |> gg_subseries(Beer)

holiday <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holiday

holiday |> autoplot(Trips) +
  labs(y = "Thousands of trips", title = "Australian domestic holiday flights")

holiday |> gg_season(Trips) + 
  facet_wrap(vars(State), nrow = 2, scales = "free_y")+
  labs(y = "Thousands of trips", title = "Australian domestic holiday flights")

holiday |>
  gg_subseries(Trips) +
  labs(y = "Thousands of trips", title = "Australian domestic holiday flights")
