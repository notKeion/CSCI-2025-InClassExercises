library(tidyverse)
library(riem)
library(sf)
library(usmap)
library(tmap)
library(lubridate)

# 1. Dynamically get all US State ASOS Network Codes
# This avoids hardcoding; it finds every network ending in "_ASOS" for US states
us_networks <- riem_networks() %>%
  filter(str_detect(code, "^[A-Z]{2}_ASOS")) %>%
  filter(!code %in% c("AK_ASOS", "HI_ASOS")) # Optional: filter out non-contiguous if needed

glimpse(us_networks)

# 2. Function to get the first (primary) station and its weather for a network
# get_state_weather <- function(network_code) {
#   tryCatch({
#     # Get all stations in that state's network
#     stations <- riem_stations(network = network_code)
#     primary_station <- stations$id[1] # Take the first station in the list
    
#     # Pull measurements for April 8, 2024
#     res <- riem_measures(
#       station = primary_station, 
#       date_start = "2024-04-08", 
#       date_end = "2024-04-09"
#     ) %>%
#       # Focus on the peak eclipse window (approx 18:00 to 20:00 UTC)
#       filter(hour(valid) >= 18 & hour(valid) <= 20) %>%
#       summarise(
#         sky_condition = last(skyc1), # Get the cloud cover code
#         network = network_code
#       )
#     return(res)
#   }, error = function(e) return(NULL))
# }

# 3. Execute - This will loop through all state codes and fetch the data
# Note: This may take a minute as it makes ~50 web requests
all_state_weather <- map_df(us_networks$code, get_state_weather)

# 4. Clean and Merge
# Extract state abbreviation from network code (e.g., "TX_ASOS" -> "TX")
all_state_weather <- all_state_weather %>%
  mutate(abbr = str_extract(network, "^[A-Z]{2}")) %>%
  mutate(cloud_score = case_when(
    sky_condition %in% c("CLR", "SKC") ~ 0,
    sky_condition == "FEW" ~ 25,
    sky_condition == "SCT" ~ 50,
    sky_condition == "BKN" ~ 75,
    sky_condition == "OVC" ~ 100,
    TRUE ~ NA_real_
  ))

# 5. Map it using usmap
map_data_weather <- usmap::us_map(regions = "states") %>%
  left_join(all_state_weather, by = "abbr")

tm_shape(map_data_weather %>% st_as_sf(coords = c("x", "y"))) + # usmap uses x/y for geom
  tm_polygons("cloud_score", palette = "Blues", title = "Cloud Cover %") +
  tm_layout(main.title = "US Cloud Cover: April 8, 2024", legend.outside = TRUE)