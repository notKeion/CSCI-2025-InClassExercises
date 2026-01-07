# Init the Libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# Load the Data
hhp_data <- read_csv("Day 2/TRAD_CLU_group.csv")

# Inspect the data
glimpse(hhp_data)
summary(hhp_data)

# Clean the Data: AI HELPER Ensure Time ordering: PRE then POST
hhp_data <- hhp_data |> 
     mutate(Time = factor(Time, levels = c("PRE", "POST")))

# Report the Data: Part 1: Boxplot of Heart Rate by Traditional vs Cluster Training
ggplot(hhp_data, aes(x = Time, y = HR, color = Condition)) +
  geom_boxplot() +
  
  labs(title = "Heart Rate by Traditional vs Cluster Training",
       x = "Training Method",
       y = "Subject Heart Rate (BPM)") +
  theme_minimal()

# Report the Data: Part 2: The Difference in heartrate of PRE to POST by Traditional vs Cluster Training
hhp_data_diff <- hhp_data %>%
  pivot_wider(names_from = Time, values_from = HR) %>%
  mutate(Difference = POST - PRE)


ggplot(hhp_data_diff, aes(x = Condition, y = Difference, color = Condition)) +
  geom_boxplot(alpha=0.5,  outlier.shape = NA) +
  geom_jitter(width =0.23, height=0) +
  labs(title = "Difference in Heart Rate from PRE to POST by Training Method",
       x = "Training Method",
       y = "Difference in Heart Rate (BPM)") +
  theme_minimal()


# Additional Summary Statistics
aggregate(HR ~ Time + Condition, data = hhp_data, summary)





