marathon <- boston_marathon |>
  filter(Event == "Men's open division") |>
  select(-Event) |>
  mutate(Minutes = as.numeric(Time) / 60)

marathon |> autoplot(Minutes) + labs(y = "Winning times in minutes")

fit_trends <- marathon |>
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980)))
  )

fit_trends

fit_trends |>
  forecast(h = 10) |>
  autoplot(marathon)

fit_trends |>
  select(piecewise) |>
  gg_tsresiduals()