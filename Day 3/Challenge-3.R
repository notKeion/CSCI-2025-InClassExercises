# Challenge 3: Pokemon
# Data Source: https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-04-01/readme.md

# Init Libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2025-04-01')
pokemon_data <- tuesdata$pokemon

# Inspect the Data
glimpse(pokemon_data)

# Initialize the Columns
stat_columns <- c("attack", "defense", "hp", "special_attack", "special_defense", "speed")

# Reshape the Data in long format for plotting
 pokemon_stats_long <- pokemon_data |>
  pivot_longer(
    cols = all_of(stat_columns),
    names_to = "stat",
    values_to = "value"
  )

# Inspect the Reshaped Data to confirm it worked
glimpse(pokemon_stats_long)

# Create Base Plot 
base_plot <- ggplot(
  data = pokemon_stats_long,
  aes(
    x = value,
    y = type_1,
    color = generation_id
  )
)

# Jitter Points
stat_points <- geom_jitter(
  height = 0.20,
  alpha = 0.6,
  size = 1
)

# Add Facats
stat_facets <- facet_wrap(
  ~ stat,
  ncol = 3,
  scales = "free_x"
)

# Plot Labels & Theme Matching
plot_labels <- labs(
  title = "Pokemon stats by Type",
  x = "Value",
  y = "Type"
)

plot_theme <- theme_light() +
  theme(legend.position = "right")

# Combine all the components
pokemon_plot <-
  base_plot +
  stat_points +
  stat_facets +
  plot_labels +
  plot_theme

# Display the Plot
pokemon_plot
