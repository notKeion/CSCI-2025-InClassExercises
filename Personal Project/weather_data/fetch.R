library(tidyverse)
library(riem)
library(furrr) # For parallel processing
library(lubridate)

# 1. SETUP PARALLEL PROCESSING FURRR- Network go burrrrrrr
# This tells R to use all but one of your CPU cores
plan(multisession, workers = parallel::detectCores() - 1)

# 2. Get the networks
us_networks <- riem_networks() %>%
  filter(str_detect(code, "^[A-Z]{2}_ASOS")) %>%
  filter(!code %in% c("AK_ASOS", "HI_ASOS"))

# 3. Optimized Fetch Function
fetch_state_history <- function(network_code) {
  tryCatch({
    stations <- riem_stations(network = network_code)
    primary_id <- stations$id[1]
    
    data <- riem_measures(
      station = primary_id,
      date_start = "2024-04-04",
      date_end = "2024-04-09"
    )
    
    if(nrow(data) > 0) {
      return(data %>% mutate(state_abbr = str_extract(network_code, "^[A-Z]{2}")))
    }
  }, error = function(e) return(NULL))
}

# 4. THE SPEED DEMON: future_map_dfr
# This replaces map_df and runs in parallel
message("Starting parallel fetch... this should be significantly faster.")

raw_weather_all_states <- us_networks$code %>%
  future_map_dfr(fetch_state_history, .progress = TRUE) 

# 5. Save and Close
write_csv(raw_weather_all_states, "./Personal Project/weather_data/raw_weather_april_2024.csv")
plan(sequential) # Shut down the parallel workers
message("Done! Data saved.")