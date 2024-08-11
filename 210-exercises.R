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

# Ingestion tourism.xlsx and creating a new tsibble
# As well as finding the average max overnight trips
# and the total trips by state

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

# Assessing the aus_arrivals tsibble and identifying
# patterns for each nation
             
aus_arrivals |> autoplot()

gg_season(aus_arrivals)

gg_subseries(aus_arrivals)

# Monthly Australian retail data

set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

myseries |> autoplot()

gg_season(myseries)

gg_subseries(myseries)

gg_lag(myseries)

myseries |> ACF(Turnover) |> autoplot()

# The seasonality shows that each year there is an increased
# turnover around Christmas and the summer months

# It is following an upward trend

# There is some indication around the late 1990s and late 2000s
# that there is decreased spending, likely due to recessions

# Total Private tsibble exploration

us_private <- us_employment |>
  filter(Title == "Total Private")

us_private

us_private |> autoplot(Employed)

gg_season(us_private)

gg_subseries(us_private)

gg_lag(us_private)

us_private |> ACF(Employed) |> autoplot()

# There is a clear upward trend here with employment
# with regular cycles were employment decreases due to
# economic recession. Seasonality is hard to tell due to the
# structure of the data but splitting each year into quarters
# would provide greater insight


bricks |> autoplot()

gg_season(bricks)

gg_subseries(bricks)

gg_lag(bricks)

bricks |> ACF(Bricks) |> autoplot()

# There is a clear seasonality of more bricks being
# produced around Q3 with there also being high production
# in Q2 and Q4 respectively where the graph then drops off
# as production slows due to the winter months

hare <- pelt |>
  select(Hare)

hare |> autoplot()

# gg_season(hare, Hare)

gg_subseries(hare)

gg_lag(hare)

hare |> ACF(Hare) |> autoplot()

# This tsibble shows a peak of hare pelts around the
# 1860s this could infer that more pelts were traded
# due to the need for fur during the American Civil War.
# Further, there appears to be general increased in pelt trading
# every 5 years and a drop that follows it. This could be due to population
# recovery. 
