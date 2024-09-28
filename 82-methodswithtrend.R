aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6) 

fit <- aus_economy |>
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
report(fit)

components(fit) |> autoplot()

components(fit) |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit |>
  forecast(h = 10)|>
  autoplot(aus_economy) +
  labs(y = "Millions", title = "Population: Australia")

aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

www_usage <- as_tsibble(WWWusage)
www_usage |> autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

www_usage |>
  stretch_tsibble(.init = 10) |>
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) |>
  forecast(h = 1) |>
  accuracy(www_usage)

fit <- www_usage |>
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  )

tidy(fit)

fit |>
  forecast(h = 10) |>
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")