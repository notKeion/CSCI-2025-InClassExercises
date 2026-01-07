# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

library(ggplot2)

# Load the TidyTuesday data for 2025 week 49
tuesdata <- tidytuesdayR::tt_load(2025, week = 49)

# Extract the qatarcars dataset
qatarcars <- tuesdata$qatarcars

# View the data
head(qatarcars)

# QUESTIONS TO ANSWER 

# Does the size of a trunk determine it's fuel economy? N
ggplot(qatarcars, aes(x = trunk, y = economy)) +
  geom_point() +
  labs(title = "Relationship between Trunk Volume and Fuel Economy",
       x = "Trunk Volume (liters)",
       y = "Fuel Economy (liters per 100 km)")

# The relationship between volume and mass
ggplot(qatarcars, aes(x = length*width*height, y = mass)) +
  geom_point() +
  labs(title = "Relationship between Volume and Mass",
       x = "Volume (meters^3)",
       y = "Mass (kg)")

# The relationship between price and car
ggplot(qatarcars, aes(x = log(price), y = performance, color = make)) +
  geom_point() +
  labs(title = "Performance vs Price by Car Make",
       x = "Log(Price) (QAR)",
       y = "Performance (0-100 km/h in seconds)")

# Performance-to-cost ratio vs Price by Car Brand
ggplot(qatarcars, aes(x = make, y = log(price / economy), color = make)) +
  geom_point() +
  labs(title = "Cost to (Fuel) performance ratio vs Price by Car Brand",
       x = "Vehicle Make",
       y = "Cost to performance ratio (log scale)") 

# Performance and price for each of the engine types (Electric, hybrid and petrol)
ggplot(qatarcars, aes(x = performance, y = log(price), color = enginetype)) +
  geom_point() +
  labs(title = "Performance vs Price by Engine Type",
       x = "Performance (0-100 km/h in seconds)",
       y = "Log(Price) (QAR)")