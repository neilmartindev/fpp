algeria_economy <- global_economy |>
  filter(Country == "Algeria")
 
algeria_economy |> autoplot()
 
fit <- algeria_economy |>
  model(ANN =  ETS(Exports ~ error("A") + trend("N") + season("N")))
report(fit)
 
fit |> gg_tsresiduals

components(fit) |> autoplot()

components(fit) |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit |> 
  forecast(h = 5) |>
  autoplot(algeria_economy) +
  labs(y = "% of GDP", title = "Algerian Exports")