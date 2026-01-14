# Init Libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# Load the Data
data <- read_table('Day 4/26-01-06-15-18.data', col_names = FALSE)

# Inspect the Data
head(data)

# Filter Data, only want 40,000 and above
filtered_data <- data %>%
  filter(X1 < 40000)

head(filtered_data)

# Manipulate the data: Convert nanoseconds to microseconds
filtered_data <- filtered_data %>%
  mutate(X1 = X1 / 1000) # X1 is the decay time (t) in microseconds

head(filtered_data)

# Plot the count of x1 over time
ggplot(filtered_data, aes(x = X1, y = ..count..)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "black") 
  geom_smooth(method = "loess", color = "red", se = FALSE) + # Add a smooth line
  labs(title = "Distribution of Decay Time (X1)") # Add labels

# Regression, we want to fit an exponential decay model to the data
# The model is of the form: t = D_0*e^t+C


# nls_model <- nls(X1 ~ D0 * exp(-X2 / tau) + C,
#                  data = filtered_data,
#                  start = list(D0 = 10000, tau = 5000, C = min(filtered_data$X1)))
# summary(nls_model)

