recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production |> model(TSLM(Beer ~ trend() + season()))
report(fit_beer) 

augment(fit_beer) |>
  ggplot(aes( x = Quarter)) +
  geom_line(aes(y = Beer, color = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = "Megalitres", title = "Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "orange"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted, colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values", title = "Quarterly beer production") +
  scale_colour_brewer(palette = "Dark2", name = "Quarter") +
  geom_abline(intercept = 0, slope = 1)

fit_beer |> gg_tsresiduals()

fit_beer |>
  forecast() |>
  autoplot(recent_production)

fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2))) 
report(fourier_beer)

aus_cafe <- aus_retail |>
  filter(Industry == "Cafes, restaurants and takeaway food services",
         year(Month) %in% 2004:2019) |>
  summarise(Turnover = sum(Turnover))
aus_cafe |> autoplot(Turnover)