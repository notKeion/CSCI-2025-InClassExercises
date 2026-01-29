# Init Libraries
library(tidyverse)
library(nycflights13)

# Inspect the flights dataset
glimpse(flights)

flights <- flights |>
  mutate(dep_datetime = make_datetime(year = year, month = month, day = day, hour = hour, min = minute))

glimpse(flights)

flights |>
  filter(origin == "JFK", as_date(dep_datetime) == make_date(year = 2013, day = 20, month = 9),
  carrier %in% c("UA", "AA", "DL")) |>
  arrange(dep_datetime)|>
  ggplot(aes(x=dep_datetime, y=dep_delay, color = carrier))+
  geom_line()