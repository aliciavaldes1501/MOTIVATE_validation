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


rf_SatEmb_CH_pred_probs_list <- predict(rf_SatEmb_CH$model, 
                                        newdata = test_data_SatEmb_CH, 
                                        type = "prob")

# Convertir lista → matriz
rf_SatEmb_CH_pred_probs <- do.call(rbind, rf_SatEmb_CH_pred_probs_list)

# Forzar estructura limpia
rf_SatEmb_CH_pred_probs <- as.data.frame(rf_SatEmb_CH_pred_probs)

colnames(rf_SatEmb_CH_pred_probs) <- sub("^EUNISa_1\\.", "", colnames(rf_SatEmb_CH_pred_probs))


# NEW


compute_multiclass_auc <- function(model, test_data, response_col = "EUNISa_1",
                                   class_levels = c("Q", "R", "S", "T"),
                                   return_pairwise = FALSE) {
  # Calcula AUC multicategoría Hand & Till con pROC::multiclass.roc
  # Pensada para modelos cforest (party/partykit) que devuelven lista de probabilidades.
  # Devuelve una lista con: auc (numérico), rocs (opcional), time (system.time)
  
  stopifnot(requireNamespace("pROC", quietly = TRUE))
  # Medir tiempo
  execution_time <- base::system.time({
    # 1) Predicción de probabilidades (cforest -> lista de vectores nombrados)
    probs_list <- predict(model, newdata = test_data, type = "prob")
    
    # 2) Convertir lista -> matriz/data.frame (filas = obs, columnas = clases)
    prob_df <- as.data.frame(do.call(rbind, probs_list))
    
    # 3) Normalizar nombres de columnas: a veces vienen como "EUNISa_1.Q"
    #    Dejamos solo "Q","R","S","T"
    colnames(prob_df) <- sub("^.*\\.", "", colnames(prob_df))
    
    # 4) Asegurar que estén todas las columnas de clases (si falta alguna, la creamos con 0)
    missing_classes <- setdiff(class_levels, colnames(prob_df))
    for (mc in missing_classes) prob_df[[mc]] <- 0
    
    # 5) Reordenar columnas exactamente como class_levels
    prob_df <- prob_df[, class_levels, drop = FALSE]
    
    # 6) Preparar respuesta real como factor con los mismos niveles
    actual <- factor(test_data[[response_col]], levels = class_levels)
    
    # 7) Calcular AUC multicategoría con pROC::multiclass.roc
    #    predictor puede ser data.frame/matrix con columnas por clase
    roc_multi <- pROC::multiclass.roc(response = actual,
                                      predictor = as.matrix(prob_df),
                                      quiet = TRUE)
  })
  
  # Armar salida
  out <- list(
    auc = as.numeric(roc_multi$auc),
    time = execution_time
  )
  
  if (isTRUE(return_pairwise)) {
    # roc_multi$rocs es una lista de listas con los ROC binarios por pares
    # Lo devolvemos tal cual para inspección detallada si quieres
    out$pairwise_rocs <- roc_multi$rocs
  }
  
  out
}


res <- compute_multiclass_auc(
  model = rf_SatEmb_CH$model,           # tu cforest
  test_data = test_data_SatEmb_CH,
  response_col = "EUNISa_1",
  class_levels = c("Q","R","S","T"),
  return_pairwise = TRUE
)

res$auc            # AUC multiclase (Hand & Till)
# res$pairwise_rocs  # Lista de ROC binarios por pares (opcional)
res$time           # tiempo de ejecución
