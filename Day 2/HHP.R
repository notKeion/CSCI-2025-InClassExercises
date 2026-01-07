# Init the Libraries
library(ggplot2)
library(readr)
library(dplyr)

# Load the Data
hhp_data <- read_csv("Day 2/TRAD_CLU_group.csv")

# Inspect the data
glimpse(hhp_data)
summary(hhp_data)

# Clean the Data: AI HELPER Ensure Time ordering: PRE then POST
hhp_data <- hhp_data |> 
     mutate(Time = factor(Time, levels = c("PRE", "POST")))

# Report the Data: Boxplot of Heart Rate by Traditional vs Cluster Training
ggplot(hhp_data, aes(x = Time, y = HR, color = Condition)) +
  geom_boxplot() +
  
  labs(title = "Heart Rate by Traditional vs Cluster Training",
       x = "Training Method",
       y = "Subject Heart Rate (BPM)") +
  theme_minimal()

# Additional Summary Statistics
aggregate(HR ~ Time + Condition, data = hhp_data, summary)





