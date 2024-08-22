us_retail_employment |>
  model(STL(Employed ~ season(window = 9), robust = TRUE)) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")