library(fpp3)

# Import the PBS tsibble and filter out the ATC column to only return values with A10
# and then select Month, Concession, Type and Cost columns. 
# Summarise this and store the sum of the cost into a value called TotalC.
# Mutate the Cost into millions and put it back into the tsibble

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
    summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

a10  

# Examine the prison dataset and ingest it using readr.
# Mutate the Date column and turn this into quaters 
# Remove the Date column and then define the new keys and index in the new tsibble

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select (-Date) |>
  as_tibble(key = c(State, Gender, Legal, Indigenous),
            index = Quarter)

prison
