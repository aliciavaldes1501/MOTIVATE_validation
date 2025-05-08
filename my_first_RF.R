# First try of a random forest model to classify points into EUNIS 1
# keeping only differential GPS / all GPS points to train the model

# Idea: keep only GPS points within the IQ range of each variable

# Load necessary libraries
library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(randomForestExplainer)
library(ggRandomForests)

######################### RF with all valid diff GPS points #########################

# Filter the data for training and testing sets
filtered_data <- diff_GPS_valid %>%
  filter(!is.na(NDVI_max) & !is.na(NDMI_max) &!is.na(NDWI_max) &
           !is.na(SAVI_max) & !is.na(EVI_max) & !is.na(NDVI_min) &
           !is.na(NDMI_min) & !is.na(NDWI_min) & !is.na(SAVI_min) &
           !is.na(EVI_min)) %>%
  mutate(EUNISa_1 = as.factor(EUNISa_1)) %>%
  filter(EVI_max <= 1 & EVI_min >= -1)

# Set seed (for reproducibility)
set.seed(123) 

# # Split GPS-located points into training and test sets
train_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[train_indices, ]
test_data <- filtered_data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(EUNISa_1 ~ NDVI_max + NDVI_min + NDMI_max + NDMI_min +
                           NDWI_max + NDWI_min + EVI_max + EVI_min +
                           SAVI_max + SAVI_min + canopy_height, 
                         # Could include SOS_DOY, Peak_DOY, EOS_DOY but
                         # this reduces a lot the number of observations in
                         # some categories
                         data = train_data, importance = TRUE, ntree = 500,
                         proximity = TRUE)
# Question - need to include plot somwhere?

# Evaluate the model
predictions <- predict(rf_model, test_data) # You get a predicted EUNISa_1 for each row
# When we improve this model, we could use this to predict EUNIS level 1 in all ReSurvey DB
# and establish which points are right / wrong
conf_matrix <- confusionMatrix(predictions, test_data$EUNISa_1) # Compare with EUNISa_1 in DB

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

# Plots
plot(gg_error(rf_model))
plot(gg_roc(rf_model))
plot(gg_vimp(rf_model))


######################### RF with all valid diff GPS points that are above p20 of NDVI_max and NDMI_min #########################

# Filter the data for training and testing sets
filtered_data <- diff_GPS_valid %>%
  filter(category_NDVI_max == "above_20th" &
           category_NDMI_min == "above_20th") %>%
  filter(!is.na(NDVI_max) & !is.na(NDMI_max) & !is.na(NDWI_max) &
           !is.na(SAVI_max) & !is.na(EVI_max) & !is.na(NDVI_min) &
           !is.na(NDMI_min) & !is.na(NDWI_min) & !is.na(SAVI_min) &
           !is.na(EVI_min)) %>%
  mutate(EUNISa_1 = as.factor(EUNISa_1)) %>%
  filter(EVI_max <= 1 & EVI_min >= -1)

# Set seed (for reproducibility)
set.seed(123) 

# # Split GPS-located points into training and test sets
train_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[train_indices, ]
test_data <- filtered_data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(EUNISa_1 ~ NDVI_max + NDVI_min + NDMI_max + NDMI_min +
                           NDWI_max + NDWI_min + EVI_max + EVI_min +
                           SAVI_max + SAVI_min + canopy_height, 
                         # Could include SOS_DOY, Peak_DOY, EOS_DOY but
                         # this reduces a lot the number of observations in
                         # some categories
                         data = train_data, importance = TRUE, ntree = 500,
                         proximity = TRUE)
# Question - need to include plot somwhere?

# Evaluate the model
predictions <- predict(rf_model, test_data) # You get a predicted EUNISa_1 for each row
# When we improve this model, we could use this to predict EUNIS level 1 in all ReSurvey DB
# and establish which points are right / wrong
conf_matrix <- confusionMatrix(predictions, test_data$EUNISa_1) # Compare with EUNISa_1 in DB

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

# Plots
plot(gg_error(rf_model))
plot(gg_roc(rf_model))
plot(gg_vimp(rf_model))

######################### RF with all valid GPS points #########################

# Filter the data for training and testing sets
filtered_data <- all_GPS_valid %>%
  filter(!is.na(NDVI_max) & !is.na(NDMI_max) &!is.na(NDWI_max) &
           !is.na(SAVI_max) & !is.na(EVI_max) & !is.na(NDVI_min) &
           !is.na(NDMI_min) & !is.na(NDWI_min) & !is.na(SAVI_min) &
           !is.na(EVI_min)) %>%
  mutate(EUNISa_1 = as.factor(EUNISa_1)) %>%
  filter(EVI_max <= 1 & EVI_min >= -1)

# Set seed (for reproducibility)
set.seed(123) 

# # Split GPS-located points into training and test sets
train_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[train_indices, ]
test_data <- filtered_data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(EUNISa_1 ~ NDVI_max + NDVI_min + NDMI_max + NDMI_min +
                           NDWI_max + NDWI_min + EVI_max + EVI_min +
                           SAVI_max + SAVI_min + canopy_height, 
                         # Could include SOS_DOY, Peak_DOY, EOS_DOY but
                         # this reduces a lot the number of observations in
                         # some categories
                         data = train_data, importance = TRUE, ntree = 500,
                         proximity = TRUE)
# Question - need to include plot somwhere?

# Evaluate the model
predictions <- predict(rf_model, test_data) # You get a predicted EUNISa_1 for each row
# When we improve this model, we could use this to predict EUNIS level 1 in all ReSurvey DB
# and establish which points are right / wrong
conf_matrix <- confusionMatrix(predictions, test_data$EUNISa_1) # Compare with EUNISa_1 in DB

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

# Plots
plot(gg_error(rf_model))
plot(gg_roc(rf_model))
plot(gg_vimp(rf_model))


######################### RF with all valid GPS points that are above p20 of NDVI_max and NDMI_min #########################

# Filter the data for training and testing sets
filtered_data <- all_GPS_valid %>%
  filter(category_NDVI_max == "above_20th" &
           category_NDMI_min == "above_20th") %>%
  filter(!is.na(NDVI_max) & !is.na(NDMI_max) & !is.na(NDWI_max) &
           !is.na(SAVI_max) & !is.na(EVI_max) & !is.na(NDVI_min) &
           !is.na(NDMI_min) & !is.na(NDWI_min) & !is.na(SAVI_min) &
           !is.na(EVI_min)) %>%
  mutate(EUNISa_1 = as.factor(EUNISa_1)) %>%
  filter(EVI_max <= 1 & EVI_min >= -1)

# Set seed (for reproducibility)
set.seed(123) 

# # Split GPS-located points into training and test sets
train_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[train_indices, ]
test_data <- filtered_data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(EUNISa_1 ~ NDVI_max + NDVI_min + NDMI_max + NDMI_min +
                           NDWI_max + NDWI_min + EVI_max + EVI_min +
                           SAVI_max + SAVI_min + canopy_height, 
                         # Could include SOS_DOY, Peak_DOY, EOS_DOY but
                         # this reduces a lot the number of observations in
                         # some categories
                         data = train_data, importance = TRUE, ntree = 500,
                         proximity = TRUE)
# Question - need to include plot somwhere?

# Evaluate the model
predictions <- predict(rf_model, test_data) # You get a predicted EUNISa_1 for each row
# When we improve this model, we could use this to predict EUNIS level 1 in all ReSurvey DB
# and establish which points are right / wrong
conf_matrix <- confusionMatrix(predictions, test_data$EUNISa_1) # Compare with EUNISa_1 in DB

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

# Plots
plot(gg_error(rf_model))
plot(gg_roc(rf_model))
plot(gg_vimp(rf_model))




