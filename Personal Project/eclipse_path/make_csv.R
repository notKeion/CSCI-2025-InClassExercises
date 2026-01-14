# Description: This script reads a fixed width file containing eclipse path data from NASA,
#              parses the coordinates into decimal format, and saves the cleaned data as a CSV file.
library(tidyverse)

# 1. Read the Fixed Width File
col_widths <- c(9, 8, 10, 10, 10, 10, 10, 7, 4, 4, 5, 10)
col_names  <- c("Time", "N_Lat", "N_Lon", "S_Lat", "S_Lon", "C_Lat", "C_Lon",
                "Ratio", "Sun_Alt", "Sun_Azm", "Path_Width", "Duration")

eclipse <- read.fwf("Personal Project/eclipse_path/nasa.txt", 
                    widths = col_widths, 
                    skip = 8, 
                    col.names = col_names, 
                    stringsAsFactors = FALSE)

# 2. Parsing Function 
# This takes "157 11.2W", removes "W", converts to decimal, and makes it negative
parse_nasa_coords <- function(x) {
  # Remove leading/trailing whitespace
  x <- trimws(x)
  
  # Return NA if the string is empty or contains only non-coordinate characters
  if (x == "" || is.na(x)) return(NA_real_)
  
  # Identify direction and remove the letter
  is_negative <- grepl("[WS]", x)
  clean_x <- gsub("[NSEW]", "", x)
  
  # Split into Degrees and Minutes
  parts <- strsplit(trimws(clean_x), "\\s+")[[1]]
  
  # Basic math: Deg + (Min / 60)
  deg <- as.numeric(parts[1])
  min <- as.numeric(parts[2])
  res <- deg + (min / 60)
  
  # Apply the negative sign for West or South
  if (is_negative) res <- -res
  return(res)
}

# 3. Apply to Center, North, and South coordinates
eclipse_clean <- eclipse %>%
  # Rowwise is needed to apply the function to every individual cell
  rowwise() %>%
  mutate(
    across(c(N_Lat, N_Lon, S_Lat, S_Lon, C_Lat, C_Lon), 
           ~parse_nasa_coords(.x))
  ) %>%
  ungroup()

# 4. Write to CSV
write.csv(eclipse_clean, "Personal Project/eclipse_path/eclipse_path.csv", row.names = FALSE)