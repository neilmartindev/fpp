recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

recent_production |>
  gg_lag(Beer, geom = "point") +
  labs( x = "lag(Beer, k")
