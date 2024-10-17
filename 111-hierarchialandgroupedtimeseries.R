tourism <- tsibble::tourism |>
  mutate(State = recode(State,
                        `New South Wales` = "NSW",
                        `Northern Territory` = "NT",
                        `Queensland` = "QLD",
                        `South Australia` = "SA",
                        `Tasmania` = "TAS",
                        `Victoria` = "VIC",
                        `Western Australia` = "WA"
  ))

tourism_hts <- tourism |>
  aggregate_key(State / Region, Trips = sum(Trips))
tourism_hts

tourism_hts |>
  filter(is_aggregated(Region)) |>
  autoplot(Trips) +
  labs(y = "Trips ('000)",
       title = "Australian tourism: national and states") +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

tourism_hts |>
  filter(State == "NT" | State == "QLD" |
           State == "TAS" | State == "VIC", is_aggregated(Region)) |>
  select(-Region) |>
  mutate(State = factor(State, levels=c("QLD","VIC","NT","TAS"))) |>
  gg_season(Trips) +
  facet_wrap(vars(State), nrow = 2, scales = "free_y")+
  labs(y = "Trips ('000)")

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date)  |>
  as_tsibble(key = c(Gender, Legal, State, Indigenous),
             index = Quarter) |>
  relocate(Quarter)

prison_gts <- prison |>
  aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)

prison_gts |>
  filter(!is_aggregated(Gender), is_aggregated(Legal),
         is_aggregated(State)) |>
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")

prison_gts |>
  filter(!is_aggregated(Gender), !is_aggregated(Legal),
         !is_aggregated(State)) |>
  mutate(Gender = as.character(Gender)) |>
  ggplot(aes(x = Quarter, y = Count,
             group = Gender, colour=Gender)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and gender",
       y = "Number of prisoners ('000)") +
  facet_wrap(~ as.character(State),
             nrow = 1, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tourism_full <- tourism |>
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))