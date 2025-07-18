# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example data (replace with your actual results)
data <- data.frame(
  Dataset = rep(c("Full Dataset", "California", "Texas", "New York"), each = 12),
  Feature = rep(c(
    "Stage", "Race", "Grade", "Chemo", "Income", "Radiation", "HER2", "PR", "Age Group",
    "Tumor Count", "Subtype", "ER"
  ), 4),
  Importance = c(
    # Full Dataset
    0.249966, 0.010270, 0.009505, 0.003053, 0.001662, 0.001448, 0.001402, 0.000581,
    0.000041, -0.000664, -0.002048, -0.002451,
    # California
    0.242461, -0.000944, 0.007440, 0.004908, 0.002551, -0.000378, 0.003067, -0.000284,
    -0.003488, 0.001370, -0.001431, 0.002808,
    # Texas
    0.225855, 0.005874, 0.013139, -0.001132, 0.000768, 0.000000, -0.003047, 0.004230,
    -0.000008, 0.001100, -0.003203, 0.004047,
    # New York
    0.247945, 0.009576, 0.004632, 0.015917, 0.004495, 0.002970, -0.000329, 0.002553,
    -0.002280, 0.002328, -0.009456, 0.002890
  )
)

# Optional: Reorder Feature factor based on average importance
data$Feature <- factor(data$Feature, levels = unique(data$Feature))

# Plot
ggplot(data, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Dataset, scales = "free_y") +
  labs(
    title = "Random Survival Forest Feature Importance by Dataset",
    x = "Feature",
    y = "Importance"
  ) +
  scale_fill_manual(values = c("red", "steelblue")) +
  theme_minimal()

# Remove Stage from the dataset
data_no_stage <- data %>%
  filter(Feature != "Stage")

# Plot without Stage
ggplot(data_no_stage, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ Dataset, scales = "free_y") +
  labs(
    title = "Random Survival Forest Feature Importance by Dataset (Excluding Stage)",
    x = "Feature",
    y = "Importance"
  ) +
  scale_fill_manual(values = c("red", "steelblue")) +
  theme_minimal()
