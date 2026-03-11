library(tidyverse)
library(here)
library(partykit)
library(party)
library(caret)
library(doParallel)
library(foreach)
library(parallel)
library(moreparty)
library(randomForest)
library(pROC)
library(rlang)
library(stringr)
library(beepr)
library(permimp)
library(purrr)
library(future.apply)

# Load functions
knitr::purl("functions_20260122.Rmd", output = "functions_20260122.R")
source("functions_20260122.R")

# Load model and prediction
load(here("objects", "RF_1500_202601", "rf_SatEmb_CH_model.Rdata"))
load(here("objects", "RF_1500_202601", "rf_SatEmb_CH_prediction.Rdata"))

# Read data
data_validation_SatEmb <- read_tsv(here(
  "data", "clean","final_RS_data_SatEmb_20251010.csv")) %>%
  rename

# Filter out manipulated observations
PlotObservationID_list <- read_csv(
  here("data", "clean","PlotObservationIDs_filtered_QRST_20251211.csv"))

data_validation_SatEmb <- data_validation_SatEmb %>%
  filter(PlotObservationID %in% PlotObservationID_list$PlotObservationID)

nrow(data_validation_SatEmb)

# Define list of predictor variables
vars_RF_SatEmb <- colnames(
  data_validation_SatEmb)[startsWith(colnames(data_validation_SatEmb), "A")]
vars_RF_SatEmb_CH <- c(vars_RF_SatEmb, "canopy_height")

# Filter data to use in RF models
filter_data_SatEmb <- function(data, variables) {
  data %>%
    mutate(EUNISa_1 = as.factor(EUNISa_1)) %>%
    # Remove rows with missing values in predictors
    dplyr::filter(if_all(all_of(variables), ~ !is.na(.))) %>%
    # Select only variables needed
    select(PlotObservationID, EUNISa_1, all_of(variables), 
           Lctnmth, valid_1) %>%
    # Select only GPS points
    dplyr::filter(Lctnmth == "Location with GPS" |
                    Lctnmth == "Location with differential GPS")
}

filtered_data_SatEmb_CH <- filter_data_SatEmb(
  data_validation_SatEmb %>%
    rough_validation_SatEmb(), # Rough validation based on CH
  vars_RF_SatEmb_CH)

# Split into training and test data sets
set.seed(123)
split_data_SatEmb_CH <- split_train_test(filtered_data_SatEmb_CH)

train_data_SatEmb_CH <- split_data_SatEmb_CH$train
test_data_SatEmb_CH <- split_data_SatEmb_CH$test

# Check overlap between training and testing datasets
sum(train_data_SatEmb_CH$PlotObservationID %in% test_data_SatEmb_CH$PlotObservationID)

# Confusion matrix
confusionMatrix(rf_SatEmb_CH_prediction, test_data_SatEmb_CH$EUNISa_1)

# k-fold cross-validation (4 folds)

set.seed(123)
ctrl <- trainControl(
  method = "cv",
  number = 4,
  allowParallel = FALSE,
  savePredictions = "final",
  classProbs = TRUE,     # set TRUE if you also want probabilities (for ROC)
  summaryFunction = multiClassSummary  # optional, for multi-class summaries
)

# caret has method "cforest". If fast engine is on your search path,
# caret will still call cforest; for fastcforest you usually wrap via custom model.
# Easiest path is to use cforest via caret:
cv_SatEmb_CH <- caret::train(
  x = filtered_data_SatEmb_CH[, vars_RF_SatEmb_CH],
  y = filtered_data_SatEmb_CH$EUNISa_1,
  # Using cforest, not fastcforest
  method = "cforest",
  trControl = ctrl,
  tuneGrid = expand.grid(
    mtry = round(sqrt(length(vars_RF_SatEmb_CH)))
  ),
  controls = party::cforest_control(
    ntree = 5
  )
)

cv_SatEmb_CH

# Extract predictions
pred <- cv_SatEmb_CH$pred

# Compute confusion matrix from pooled CV predictions
cm_all <- caret::confusionMatrix(
  data = pred$pred,
  reference = pred$obs
)

cm_all

# Confusion matrix per fold

cm_by_fold <- pred %>%
  group_by(Resample) %>%
  group_map(~ caret::confusionMatrix(
    data = .x$pred,
    reference = .x$obs
  ))

names(cm_by_fold) <- unique(pred$Resample)
cm_by_fold

# Pr-fold summary metrics

fold_metrics <- pred %>%
  group_by(Resample) %>%
  summarise(
    Accuracy = caret::confusionMatrix(pred, obs)$overall["Accuracy"],
    Kappa    = caret::confusionMatrix(pred, obs)$overall["Kappa"]
  )

fold_metrics

# Per-cass performance per fold

byclass_by_fold <- pred %>%
  group_by(Resample) %>%
  group_map(~ caret::confusionMatrix(
    data = .x$pred,
    reference = .x$obs
  )$byClass)

byclass_by_fold
