library(tidyverse)
library(sf)
library(usmap)
library(tmap)
library(lubridate)

# 1. LOAD DATA
weather_raw <- read_csv("raw_weather_april_2024.csv") %>%
  mutate(valid = as_datetime(valid))

# 2. DEFINE CLOUD SCORING LOGIC
# Mapping METAR codes to numeric percentages
cloud_lookup <- c(
  "CLR" = 0, "SKC" = 0, 
  "FEW" = 25, 
  "SCT" = 50, 
  "BKN" = 75, 
  "OVC" = 100
)

# 3. AGGREGATE DATA BY PERIOD
weather_processed <- weather_raw %>%
  mutate(
    period = case_when(
      as.Date(valid) >= "2024-04-04" & as.Date(valid) <= "2024-04-06" ~ "before",
      as.Date(valid) >= "2024-04-07" & as.Date(valid) <= "2024-04-09" ~ "after_eclipse",
      TRUE ~ NA_character_
    ),
    cloud_pct = cloud_lookup[skyc1]
  ) %>%
  filter(!is.na(period) & !is.na(cloud_pct)) %>%
  group_by(state_abbr, period) %>%
  summarise(avg_cloud = mean(cloud_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_cloud)

# 4. PREPARE MAP DATA
map_states <- usmap::us_map(regions = "states") %>%
  rename(state_abbr = abbr) %>%
  left_join(weather_processed, by = "state_abbr")

# 5. PLOTS
# Map 1: Average Cloud Cover BEFORE (April 4-6)
plot_before <- tm_shape(map_states) +
  tm_polygons("before", 
              palette = "Blues", 
              title = "Avg Cloud Cover %",
              style = "cont") +
  tm_layout(main.title = "Weather: 3 Days Before (Apr 4-6)",
            main.title.size = 1,
            legend.outside = TRUE)

# Map 2: Average Cloud Cover AFTER/DURING (Apr 7-9)
plot_after <- tm_shape(map_states) +
  tm_polygons("after_eclipse", 
              palette = "Blues", 
              title = "Avg Cloud Cover %",
              style = "cont") +
  tm_layout(main.title = "Weather: Eclipse Period (Apr 7-9)",
            main.title.size = 1,
            legend.outside = TRUE)

# Map 3: The Delta (Did it get cloudier or clearer?)
map_states <- map_states %>% mutate(weather_diff = after_eclipse - before)

plot_diff <- tm_shape(map_states) +
  tm_polygons("weather_diff", 
              palette = "RdBu", 
              midpoint = 0,
              title = "Change in Clouds",
              style = "cont") +
  tm_layout(main.title = "Weather Shift (After minus Before)",
            main.title.size = 1,
            legend.outside = TRUE)

# 6. RENDER
print(plot_before)
print(plot_after)
print(plot_diff)