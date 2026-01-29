library(tidyverse)
library(tmap)
library(sf)
library(usmap) 
library(janitor)


# Load Data 
eyes_hurt_data <- read_csv('Personal Project/search_data/geoMap-3daysafter.csv', skip = 2) %>%
  left_join(
    read_csv('Personal Project/search_data/geoMap-3daysbefore.csv', skip = 2) %>%
      rename(search_volume = 2) %>%
      select(Region, search_volume) %>%
      rename(sv_before = search_volume),
    by = "Region"
  ) %>%
  rename(state = Region) %>%
  rename(sv_after = 2) %>%
  mutate(across(starts_with("sv_"), ~replace_na(., 0))) %>%
  mutate(sv_diff = sv_after - sv_before)%>%
  mutate(sv_per = (sv_after - sv_before)/100) %>%
  mutate(sv_log_fold_change = log((sv_after + 1) / (sv_before + 1)))



map_data <- usmap::us_map(regions = "state") %>% 
  rename(state = full) %>%
  st_transform(4326) %>%
  left_join(eyes_hurt_data, by = "state")

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


#  Projections
flat_crs <- 6350
curve_crs <- 4326
original_crs <- 5070
state_proj <- st_transform(map_data, curve_crs) 
eclipse_proj <- st_transform(eclipse_central_line, curve_crs)

state_proj$distance_km <- as.numeric(
  st_distance(state_proj, eclipse_proj)
) / 1000
# Filters


# filtered <- state_proj |>
#    filter(sv_diff >= 0) 

filtered <- state_proj

# ===================
# CORRELATION TEST 1
# ===================
# TESTING WITH: FILTERED DATA
message("CORRELATION TEST 3: TESTING WITH: FILTERED DATA")
cor.test(
  filtered$distance_km,
  filtered$sv_diff,
  method = "spearman",
  exact = FALSE
)
ggplot(state_proj, aes(x = distance_km, y= sv_diff, color=sv_diff))+
  labs(
    title="Search Volume vs Distance from eclipse path",
    subtitle = "Recorded average search volume 72 hours (after - before) the eclipse",
    x = "Distance to Path (km)",
    y = "Search Volume of 'Eyes Hurt'",
    color = "% Change in Search Volume"

  )+
  geom_point(size=4)

# ===================
# CORRELATION TEST 2
# ===================
# TESTING WITH: UNFILTERED DATA
message("CORRELATION TEST 3: TESTING WITH: UNFILTERED DATA")
cor.test(
  state_proj$distance_km,
  state_proj$sv_diff,
  method = "spearman",
  exact = FALSE
)
# ===================
# CORRELATION TEST 3
# ===================
# TESTING WITH: UNFILTERED SEARCH VOLUME DAY AFTER
message("CORRELATION TEST 3: TESTING WITH: UNFILTERED SEARCH VOLUME DAY AFTER")
cor.test(
  state_proj$distance_km,
  state_proj$sv_after,
  method = "spearman",
  exact = FALSE
)

ggplot(state_proj, aes(x = distance_km, y= sv_after, color=sv_diff))+
  labs(
    title="Search Volume vs Distance from eclipse path",
    subtitle = "Recorded search volume 72 hours after the eclipse",
    x = "Distance to Path (km)",
    y = "Search Volume of 'Eyes Hurt'",
    color = "% Change in Search Volume"

  )+
  geom_point(size=4)

ggplot(filtered, aes(x = distance_km, y=sv_before))+
    labs(
    title="Search Volume vs Distance from eclipse path",
    subtitle = "Recorded search volume 72 hours Before the eclipse",
    x = "Distance to Path (km)",
    y = "Search Volume of 'Eyes Hurt'"
  )+
  geom_point()

# ===================
# CORRELATION TEST 4
# ===================
# TESTING WITH: UNFILTERED SEARCH VOLUME DAY AFTER
message("CORRELATION TEST 4: TESTING WITH: UNFILTERED LOG FOLD CHANGE")
cor.test(
  state_proj$distance_km,
  state_proj$sv_log_fold_change,
  method = "spearman",
  exact = FALSE
)
cor.test(
  state_proj$distance_km,
  state_proj$sv_before,
  method = "spearman",
  exact = FALSE
)


# =======
#  PLOTS
# =======


ggplot(filtered, aes(x = distance_km, sv_diff))+
  geom_point()

tm_shape(filtered) +
  tm_polygons(
    col = "sv_diff", 
    palette = "OrRd", 
    style = "jenks",
    title = "",
    colorNA = "grey90"
  ) +
  tm_shape(eclipse_proj %>% st_cast("POINT")) + 
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
  )+
  tm_title(
    text = "Difference in Search Volume: 'My Eyes Hurt' (72 hour average After - 72 hour average Before)",
    fontface = "bold",
    frame=TRUE
  )

tm_shape(filtered) +
  tm_polygons(
    col = "sv_after", 
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
  )+
  tm_title(
    text = "Search Volume 72 hours After Eclipse",
    fontface = "bold",
    frame=TRUE
  )

tm_shape(filtered) +
  tm_polygons(
    col = "sv_before", 
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
  )+
  tm_title(
    text = "Search Volume 72 hours Before Eclipse",
    fontface = "bold",
    frame=TRUE
  )

source("/Users/keionvergara/Documents/GitHub.nosync/CSCI-2025-InClassExercises/Personal Project/weather_data/analyze_weather_data.R", echo = TRUE)

# Search data with the weather data
final_analysis_data <- state_proj %>%
  left_join(weather_processed, by = c("abbr" = "state_abbr")) %>%
  mutate(
    # Create a 'Clear Sky' weight (0 to 1)
    clarity_weight = (100 - after_eclipse) / 100,
    # Adjust distance: If it's cloudy, it's as if they were 1000km away
    adj_distance = distance_km / (clarity_weight + 0.01),

    weighted_sv = sv_diff * clarity_weight
  )


# PROVING THE HYPOTHESIS: 
cor.test(
  final_analysis_data$adj_distance, 
  final_analysis_data$sv_diff, 
  method = "spearman"
)

ggplot(final_analysis_data, aes(x = distance_km, y = sv_diff)) +
  geom_point(aes(size = after_eclipse, color = after_eclipse)) +
  scale_color_gradient(low = "gold", high = "gray40") +
  # geom_text(aes(label = ifelse(sv_diff < 10 & distance_km < 200, state, "")), ) +
  labs(
    title = "Distance vs Search Volume (Weighted by Cloud Cover)",
    subtitle = "Gold points = Clear skies | Grey points = Cloudy skies",
    x = "Distance to Path (km)",
    y = "Increase in 'Eyes Hurt' Searches",
    color = "Cloud Cover %",
    size = "Cloud Cover %"
  ) +
  theme_minimal()
# FINAL MAP: CLOUD COVER AS CONTEXT, SEARCH VOLUME AS RESULT

tm_shape(final_analysis_data) +
  # Background: Cloud Cover (The "Context")

  tm_polygons(
    col = "after_eclipse", 
    palette = "Greys", 
    title = "Cloud Cover (%)",
    alpha = 0.6
  ) +
  # Foreground: The Search Volume (The "Result")
  # We use bubbles so the audience can see the state underneath
  tm_bubbles(
    size = "sv_diff", 
    col = "sv_diff",
    palette = "-RdYlBu", # Red for high search, Blue for low
    title.size = "Search Increase",
    title.col = "Search Intensity",
    scale = 1.5
  ) +
  tm_shape(eclipse_north_line) + tm_lines(col = "darkblue", lwd = 1, lty = "dashed") +
  tm_shape(eclipse_south_line) + tm_lines(col = "darkblue", lwd = 1, lty = "dashed") +    
  tm_layout(
      main.title =  "The 'Cloud Gap' Effect: Search Volume vs. Visibility",

    legend.outside = TRUE
  )

cor.test(
  rank(final_analysis_data$distance_km + rank(final_analysis_data$after_eclipse)), 
  final_analysis_data$sv_diff, 
  method = "spearman"
)