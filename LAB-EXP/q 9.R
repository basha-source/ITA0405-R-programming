# Load necessary libraries
library(ggplot2)

# Load airquality dataset
data("airquality")

# Display initial summary of dataset
print("Summary of airquality dataset:")
print(summary(airquality))

### (i) Handling Missing Values ###
# Function to check missing values in each column
missing_values <- colSums(is.na(airquality))
print("Missing values in each column:")
print(missing_values)

# Define threshold for missing values (10%)
threshold <- 0.1 * nrow(airquality)

# Handle missing values
for (col in names(airquality)) {
  if (missing_values[col] > 0) {  # If column has missing values
    if (missing_values[col] < threshold) {
      # Drop rows where missing values are present
      airquality <- airquality[!is.na(airquality[[col]]), ]
    } else {
      # Replace missing values with column mean
      airquality[[col]][is.na(airquality[[col]])] <- mean(airquality[[col]], na.rm = TRUE)
    }
  }
}

print("Dataset after handling missing values:")
print(summary(airquality))

### (ii) Apply Linear Regression Using Least Squares Method ###
# Fit linear regression model (Ozone ~ Solar.R)
model <- lm(Ozone ~ Solar.R, data = airquality)

# Display model summary
print("Linear Regression Model Summary:")
print(summary(model))

### (iii) Scatter Plot with Regression Line ###
# Plot scatter plot with regression line
ggplot(airquality, aes(x = Solar.R, y = Ozone)) +
  geom_point(color = "blue") +  # Scatter points
  geom_smooth(method = "lm", col = "red", se = FALSE) +  # Regression line
  ggtitle("Scatter Plot: Ozone vs Solar Radiation") +
  xlab("Solar Radiation (Langley)") +
  ylab("Ozone (ppb)") +
  theme_minimal()
