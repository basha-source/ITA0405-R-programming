# Load required libraries
library(caret)  # For data splitting
library(e1071)  # For confusion matrix
library(ggplot2)  # For visualization

# Load the dataset
data(iris)

# Consider only Petal.Length and Petal.Width as features and Species as target
iris_data <- iris[, c("Petal.Length", "Petal.Width", "Species")]

# Convert Species into a binary classification (for logistic regression)
iris_data$Species <- as.factor(ifelse(iris_data$Species == "setosa", 0, 1))

# Split the dataset into training (80%) and testing (20%)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(iris_data$Species, p = 0.8, list = FALSE)
train_data <- iris_data[trainIndex, ]
test_data <- iris_data[-trainIndex, ]

# Create logistic regression model
model <- glm(Species ~ Petal.Length + Petal.Width, data = train_data, family = binomial)

# Predict probability on test data
predicted_probs <- predict(model, test_data, type = "response")

# Convert probability to class label using threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Create confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$Species)
print(conf_matrix)

