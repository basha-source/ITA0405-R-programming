# Load necessary libraries
library(reshape2)
library(ggplot2)
library(dplyr)

# Load the ChickWeight dataset
data("ChickWeight")

# Display first few rows
print("First few rows of ChickWeight dataset:")
print(head(ChickWeight))

### 4.(i) Order Data by Weight in Ascending Order, Grouped by Diet ###
ordered_data <- ChickWeight %>%
  arrange(Diet, weight)  # Arrange by Diet and then weight

# Extract the last 6 records
last_six_records <- tail(ordered_data, 6)
print("Last 6 records from ordered data frame:")
print(last_six_records)

### 4.(ii) (a) Perform Melting Function ###
melted_chick <- melt(ChickWeight, id.vars = c("Chick", "Time", "Diet"))
print("Melted ChickWeight dataset:")
print(head(melted_chick))

### 4.(ii) (b) Compute Mean Weight Grouped by Diet ###
mean_weight_diet <- dcast(melted_chick, Diet ~ variable, mean, na.rm = TRUE)
print("Mean weight grouped by Diet:")
print(mean_weight_diet)

### 4.(ii) (c) Compute Mode of Weight Grouped by Diet ###
# Function to compute mode
mode_function <- function(x) {
  uniq_values <- unique(x)
  freq <- tabulate(match(x, uniq_values))
  uniq_values[which.max(freq)]
}

# Compute mode of weight for each Diet
mode_weight_diet <- ChickWeight %>%
  group_by(Diet) %>%
  summarise(mode_weight = mode_function(weight))

print("Mode of weight grouped by Diet:")
print(mode_weight_diet)

### 5.(a) Create Boxplot for Weight Grouped by Diet ###
ggplot(ChickWeight, aes(x = as.factor(Diet), y = weight, fill = as.factor(Diet))) +
  geom_boxplot() +
  ggtitle("Box Plot of Weight Grouped by Diet") +
  xlab("Diet") +
  ylab("Weight") +
  theme_minimal()

### 5.(b) Create Histogram for Weight in Diet-1 ###
ggplot(ChickWeight %>% filter(Diet == 1), aes(x = weight)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Weight for Diet-1") +
  xlab("Weight") +
  ylab("Count") +
  theme_minimal()

### 5.(c) Create Scatter Plot for Weight vs Time Grouped by Diet ###
ggplot(ChickWeight, aes(x = Time, y = weight, color = as.factor(Diet))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines
  ggtitle("Scatter Plot: Weight vs Time Grouped by Diet") +
  xlab("Time") +
  ylab("Weight") +
  theme_minimal()
