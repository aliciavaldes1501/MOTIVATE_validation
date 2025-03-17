# First try of a random forest model to classify points into EUNIS 1
# keeping only  GPS points to train the model

# Load necessary libraries
library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(randomForestExplainer)

# Filter the data for training and testing sets
filtered_data <- db_resurv_RS_short %>%
  filter(!is.na(NDVI) & !is.na(NDMI) &
           EUNISa_1 %in% c("T", "R", "S", "Q", "N")) %>%
  # Ensure EUNISa_1 is a factor and NDVI, NDMI are numeric
  mutate(EUNISa_1 = as.factor(EUNISa_1),
         NDVI = as.numeric(NDVI),
         NDMI = as.numeric(NDMI))

train_data <- filtered_data %>%
  filter(`Location method` %in%
           c("Location with GPS", "Location with differential GPS"))

test_data <- filtered_data %>%
  filter(!(`Location method` %in%
             c("Location with GPS", "Location with differential GPS")))

# Train the random forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(EUNISa_1 ~ NDVI + NDMI, data = train_data,
                         importance = TRUE, ntree = 500, proximity = TRUE)

# Evaluate the model
predictions <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$EUNISa_1)

# Print the confusion matrix and model importance
print(conf_matrix)
print(importance(rf_model))

# Plot variable importance
varImpPlot(rf_model)

# Confusion matrix heatmat

# Convert confusion matrix to a data frame
conf_matrix_df <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_df) <- c("Prediction", "Reference", "Freq")

# Plot heatmap
ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

# ROC curves

# Convert predictions to a factor with the same levels as the actual data
predictions <- factor(predictions, levels = levels(test_data$EUNISa_1))

# Initialize a list to store ROC curves
roc_curves <- list()

# Calculate ROC curve for each class
for (class in levels(test_data$EUNISa_1)) {
  roc_curves[[class]] <- roc(test_data$EUNISa_1 == class, as.numeric(predictions == class))
}

# Plot ROC curves
plot(roc_curves[[1]], col = 1, main = "ROC Curves")
for (i in 2:length(roc_curves)) {
  plot(roc_curves[[i]], col = i, add = TRUE)
}
legend("bottomright", legend = levels(test_data$EUNISa_1), col = 1:length(roc_curves), lwd = 2)

# Plot error rate
plot(rf_model, main = "Error Rate vs. Number of Trees")
legend("topright", legend = colnames(rf_model$err.rate), col = 1:3, lwd = 2)


