us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment |> 
  autoplot() +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")
  
dcmp <- us_retail_employment |> 
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) |> autoplot()

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "orange") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

components(dcmp) |> gg_subseries(Employed)

us_retail_employment |>
  autoplot(Employed, color="gray") +
  autolayer(components(dcmp), season_adjust, color = "darkblue") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")