library(tidyverse)
library(tmap)
library(sf)
library(usmap) 
library(janitor) # Highly recommended for cleaning column names

# 1. Load and Clean Data
# We clean names to remove spaces/special chars (e.g., "Region" -> "region")
eyes_hurt_data <- read_csv('Personal Project/search_data/geoMap.csv', skip = 0) %>%
  janitor::clean_names() %>% 
  rename(state = region) %>%
  # Google Trends often puts the search term in the 2nd col; let's give it a generic name
  rename(search_volume = 2) 


eyes_hurt_data_before <- read_csv('Personal Project/search_data/geoMap-daybefore.csv', skip = 2) %>%
  janitor::clean_names() %>% 
  rename(state = region) %>%
  # Change N/A values to 0
  mutate(across(starts_with("search_volume"), ~replace_na(., 0))) %>%
  # Google Trends often puts the search term in the 2nd col; let's give it a generic name
  rename(search_volume = 2) 

# 2. Get Shapefile (The Clean Way)
# us_map(regions = "states") returns an sf object by default in recent versions.
# It includes a "full" column with proper Title Case state names (e.g., "New York").
usa_sf <- usmap::us_map(regions = "state") %>% 
  rename(state = full) %>%
  st_transform(4326) # Adds flat profile to map, makes it better to see eclipse path

# 3. Join Data
# This is safer because both sources now use Title Case state names
map_data <- usa_sf %>%
  left_join(eyes_hurt_data, by = "state")

# 4. Plot with Eclipse Path

# --- Create the Eclipse Path Layers ---
eclipse_central_line <- read_csv('Personal Project/eclipse_path/eclipse_path.csv') %>% 
  filter(!is.na(C_Lon) & !is.na(C_Lat)) %>%
  st_as_sf(coords = c("C_Lon", "C_Lat"), crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  
  st_cast("LINESTRING") %>% 

  st_transform(st_crs(map_data))

eclipse_north_line <- read_csv('Personal Project/eclipse_path/eclipse_path.csv') %>% 
  filter(!is.na(N_Lon) & !is.na(N_Lat)) %>%
  st_as_sf(coords = c("N_Lon", "N_Lat"), crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(st_crs(map_data)) 

eclipse_south_line <- read_csv('Personal Project/eclipse_path/eclipse_path.csv') %>% 
  filter(!is.na(S_Lon) & !is.na(S_Lat)) %>%
  st_as_sf(coords = c("S_Lon", "S_Lat"), crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(st_crs(map_data)) 

# Final Plot with Eclipse Path
tm_shape(map_data) +
  tm_polygons(
    col = "search_volume", 
    palette = "OrRd", 
    style = "jenks",
    title = "Search Volume: 'Eyes Hurt'",
    colorNA = "grey90"
  ) +
  tm_shape(eclipse_central_line %>% st_cast("POINT")) + 
  tm_dots(col = "black", size = 0.2) + 
  tm_shape(eclipse_north_line) +
  tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
  tm_shape(eclipse_south_line) +
  tm_lines(col = "blue", lwd = 2, alpha = 0.7) +
  tm_layout(
    frame = TRUE,
    legend.position = c("right", "bottom")
  ) +
  tm_legend(legend.outside = TRUE)+
  tm_title(
    text="April 8, 2024 Eclipse Path (Blue) vs. 'My Eyes Hurt' Search Volume by State",
    fontface = "bold",
    frame=TRUE
  )


# 5. Correlation Analysis: Distance to Eclipse Central Line vs. Search Volume Change
metros_proj <- st_transform(map_data, 5070)         # USA Albers
eclipse_proj <- st_transform(eclipse_central_line, 5070)

# Calculate change in search volume
# eye_pain_change <- eyes_hurt_data %>%
#   select(state, search_volume) %>%
#   rename(search_volume_after = search_volume) %>%
#   left_join(
#     eyes_hurt_data_before %>%
#       select(state, search_volume) %>%
#       rename(search_volume_before = search_volume),
#     by = "state"
#   ) %>%
#   mutate(search_volume = search_volume_after - search_volume_before) %>%
#   select(state, search_volume)

# Calculate distance in kilometers
metros_proj$distance_km <- as.numeric(
  st_distance(metros_proj, eclipse_proj)
) / 1000

plot(metros_proj["distance_km"])

cor_test <- cor.test(
  metros_proj$distance_km,
  metros_proj$search_volume,
  method = "spearman",
  exact = FALSE)


# Interaction term significant. Goal
#



cor_test


plot(metros_proj["search_volume"])

filtered <- metros_proj |>
  filter(distance_km <= 800)

cor.test(
  filtered$distance_km,
  filtered$search_volume,
  method = "spearman",
  exact = FALSE
)

ggplot(filtered)+
  geom_point(aes(x=distance_km, y=search_volume)) +
  geom_smooth(aes(x=distance_km, y=search_volume), method="lm")

ggplot(metros_proj)+
  geom_point(aes(x=distance_km, y=search_volume)) +
  geom_smooth(aes(x=distance_km, y=search_volume), method="lm")



