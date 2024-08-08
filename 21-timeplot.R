# Plotting the a10 data using the autoplot library

a10

a10 |> autoplot(Cost) + labs(y = "$ Millions",
                             title = "Australian Anti-Diabetic drug sales")

# Read in the Airlines tsibble
ansett

ansett |>
  autoplot()

# Filter by class and the aiports, identifying pilot strike and change of business classification of economy and business flights
melsyd_econclass <- ansett |>
  filter(Class == "Economy", Airports == "MEL-SYD") |>
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_econclass, Passengers) +
  labs(title = "Ansett Airlines economy class",
       subtitle = "Melbourne-Syndey",
       y = "Passengers ('000)")

melsyd_econclass


