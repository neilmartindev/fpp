us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
us_retail_employment

dcmp <- us_retail_employment |>
  model(STL(Employed)) |>
  components() |>
  select(-.model)
dcmp

dcmp |> 
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp) +
  labs(title = "Naive forecasts of seasonally adjusted data")

us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  )) |>
  forecast() |>
  autoplot(us_retail_employment)