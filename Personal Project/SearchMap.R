library(tidyverse)
library(tmap)
library(sf)
library(usmap) 
library(janitor) # Highly recommended for cleaning column names

# 1. Load and Clean Data
# We clean names to remove spaces/special chars (e.g., "Region" -> "region")
eyes_hurt_data <- read_csv('Personal Project/search_data/geoMap.csv') %>%
  janitor::clean_names() %>% 
  rename(state = region) %>%
  # Google Trends often puts the search term in the 2nd col; let's give it a generic name
  rename(search_volume = 2) 

# 2. Get Shapefile (The Clean Way)
# us_map(regions = "states") returns an sf object by default in recent versions.
# It includes a "full" column with proper Title Case state names (e.g., "New York").
usa_sf <- usmap::us_map(regions = "states") %>% 
  rename(state = full)

# 3. Join Data
# This is safer because both sources now use Title Case state names
map_data <- usa_sf %>%
  left_join(eyes_hurt_data, by = "state")

# 4. Plot
tm_shape(map_data) +
  tm_polygons(
    col = "search_volume",
    title = "Search Volume:\n'My Eyes Hurt'",
    palette = "OrRd",          # "OrRd" (Orange-Red) mimics the heat/pain better than ocean
    style = "jenks",           # "jenks" finds natural breaks in the data
    colorNA = "grey90",
    textNA = "No Data"
  ) +
  tm_layout(
    frame = FALSE,             # Removes the box around the map
    title.position = c("center", "top"),
    legend.position = c("right", "bottom"), 
    title = "\"My Eyes Hurt\" Search Volume by State (4/7/24 – 4/9/24)"
  )