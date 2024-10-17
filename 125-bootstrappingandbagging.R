cement <- aus_production |>
  filter(year(Quarter) >= 1988) |>
  select(Quarter, Cement)
cement_stl <- cement |>
  model(stl = STL(Cement))
cement_stl |>
  components() |>
  autoplot()

cement_stl |>
  generate(new_data = cement, times = 10,
           bootstrap_block_size = 8) |>
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: Bootstrapped series",
       y="Tonnes ('000)")

sim <- cement_stl |>
  generate(new_data = cement, times = 100,
           bootstrap_block_size = 8) |>
  select(-.model, -Cement)

ets_forecasts <- sim |>
  model(ets = ETS(.sim)) |>
  forecast(h = 12)
ets_forecasts |>
  update_tsibble(key = .rep) |>
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: bootstrapped forecasts",
       y="Tonnes ('000)")

bagged <- ets_forecasts |>
  summarise(bagged_mean = mean(.mean))
cement |>
  model(ets = ETS(Cement)) |>
  forecast(h = 12) |>
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "Cement production in Australia",
       y="Tonnes ('000)")