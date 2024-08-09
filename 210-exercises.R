library(fpp3)

# Bricks from aus_production

aus_production

bricks <- aus_production |>
  select(Quarter,Bricks)

bricks |> autoplot()

# Lynx from pelts

lynx <- pelt |>
  select(Year, Lynx)

lynx |> autoplot()

close <- gafa_stock |>
  select(Date, Symbol, Close)

# Demand from vic_elec

au_elec <- vic_elec |>
  select(Time,Demand)

au_elec |> autoplot()

# Added labels to last plot

close |> autoplot() +
  labs(y = "Closing Price per share ($)",
       title = "GAFA stock closing price")

# Peak closing price

peak_close <- close |>
  group_by(Symbol) |>
  filter(Close == max(Close)) |>
  select(Symbol, Date)

peak_close

# Ingesting tute1 file and creating tsibble

tute1 <- readr::read_csv("tute1.csv")

mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

mytimeseries |> 
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

# Exploring US gas dataset

head(USgas::us_total)

us_df <- USgas::us_total |>
  as_tsibble(key = state,
             index = year)

us_df

us_df |> filter(state %in% c('Maine', 'Vermont', 'New Hampshire', 'Massachusetts', 'Connecticut', 'Rhode Island')) |>
  ggplot(aes(x = year, y = y, color = state)) +
  geom_line() +
  facet_grid(state ~., scales = "free_y") +
  labs(y = "Consumption",
       title = "Annual natural gas consumption in New England")

trsm_df <- readxl::read_excel("tourism.xlsx") |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(key = c(Region, State, Purpose),
             index = Quarter)

reg_purp <- trsm_df |>
  group_by(Region, Purpose) |>
  mutate(Avg_Trips = mean(Trips)) |>
  ungroup() |>
  filter(Avg_Trips  == max(Avg_Trips)) |>
  distinct(Region, Purpose)
    
trsm_df |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  ungroup() -> tourism_by_state

tourism_by_state |> autoplot()
             
aus_arrivals |> autoplot()

gg_season(aus_arrivals)

gg_subseries(aus_arrivals)