# Initialize Libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(openintro)


# Load the Data
data <- duke_forest

# Inspect the Data
glimpse(data)

# Mutate Data
data <- data %>%
  mutate(decade = case_when(year_built >= 1990 ~ '1990 or after',
                            (year_built >= 1980 & year_built < 1990) ~ '1980',
                            (year_built >= 1970 & year_built < 1980) ~ '1970',
                            (year_built >= 1960 & year_built < 1970) ~ '1960',
                            (year_built >= 1950 & year_built < 1960) ~ '1950',
                            year_built < 1950                ~ "1940 or before"))
# Get Means
means <- data %>%
  group_by(decade) %>%
  summarise(mean_area = mean(area, na.rm = TRUE))

# Plot the Data
ggplot(means, aes(y = decade, x = mean_area)) +

  geom_segment(
    aes(yend = decade, x = 0, xend = mean_area),
    linewidth = 0.5,
    color = "black"
  ) +

  geom_point(size = 2.5, color = "black") +

  labs(
    title = "Mean area of houses in Duke Forest, by decade built",
    x = "Mean area (square feet)",
    y = "Decade Built"
  ) +
  theme_minimal()
