library(randomForest)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)

# Change the base directory here:
base_path <- "C:/Users/stdso/Documents/USTH/Spatial/Final Spatial"

file <- "obesity_classification.csv"
file_path <- file.path(base_path, file)
obesity_classification <- read.csv(file_path)

obesity_classification <- obesity_classification %>%
  mutate(Gender = factor(Gender),
         ObesityCategory = factor(ObesityCategory))

# Split data into features (X) and target (y)
X <- obesity_classification %>% select(-ObesityCategory)
y <- obesity_classification$ObesityCategory

# Split data into training and test sets
set.seed(123) 
trainIndex <- createDataPartition(y, p = .8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Train the Random Forest model
set.seed(123) 
rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 200)

# Predict on the test set
y_pred <- predict(rf_model, X_test)

# Evaluate the model
confusionMatrix <- confusionMatrix(y_pred, as.factor(y_test))
print(confusionMatrix)

# Convert to a table for plotting
cm_table <- as.table(confusionMatrix$table)

# Plotting
ggplot(data = as.data.frame(cm_table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%0.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(x = "True Label", y = "Predicted Label", fill = "Frequency",
       title = "Confusion Matrix for Obesity Category Prediction")

# Plotting
ggplot(obesity_classification, aes(x = BMI, fill = ObesityCategory)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "BMI Distribution by Obesity Category", x = "BMI", y = "Count") +
  theme_minimal()

# Data for prediction
individuals_data <- data.frame(
  Age = c(30, 30, 30), 
  Gender = factor(c('Male', 'Male', 'Male'), levels = levels(obesity_classification$Gender)),
  Height = c(179, 175, 180), 
  Weight = c(106, 98, 90),
  PhysicalActivityLevel = c(1, 1, 1) 
)

# Calculate BMI for each individual
individuals_data$BMI <- individuals_data$Weight / (individuals_data$Height / 100)^2

# Exclude columns that are not in the training dataset
individuals_data <- individuals_data %>% select(names(X_train))

# Predict ObesityCategory for each individual
predicted_categories <- predict(rf_model, individuals_data)

# Convert numeric predictions back to factor levels with descriptive labels
predicted_categories <- factor(predicted_categories, levels = levels(y_train), labels = levels(y_train))

# Assuming predicted_categories is your factor of predictions
individuals_data$PredictedCategory <- predicted_categories

# Displaying as a nicely formatted table
kable(individuals_data[, c("Age", "Gender", "Height", "Weight", "PhysicalActivityLevel", "BMI", "PredictedCategory")],
      caption = "Predicted Obesity Categories for Individuals")

# Adjusting the code to use "MeanDecreaseGini" for the feature importance
feature_importances <- importance(rf_model)
features <- rownames(feature_importances)
importance_score <- feature_importances[, "MeanDecreaseGini"]  # Using Mean Decrease Gini as the importance score
importance_df <- data.frame(Feature = features, Importance = importance_score)

# Ordering the data frame by importance score
importance_df <- importance_df[order(-importance_df$Importance),]

# Plotting the feature importances
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip coordinates to make it easier to read
  labs(title = "Feature Importance in Obesity Classification",
       x = "Feature", 
       y = "Mean Decrease Gini") +
  theme_minimal()

