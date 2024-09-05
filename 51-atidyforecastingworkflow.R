 gdppc <- global_economy |>
   mutate(GDP_per_capita = GDP / Population) |>
   select(Year, Country, GDP, Population, GDP_per_capita)
 
 gdppc
 
 gdppc |> filter(Country == "Sweden") |>
   autoplot(GDP_per_capita) +
   labs(title = 'GDP per capita for Sweden', y = "SUS")
 
 fit <- gdppc |>
   model(trend_model = TSLM(GDP_per_capita ~ trend()))
 
 fit
 
 fit |> forecast(h = "3 years")
 
 fit |>
   forecast(h = "3 years") |>
   filter(Country == "Sweden") |>
   autoplot(gdppc) + labs(title = "GDP per capita for Sweden", y ="SUS")
 