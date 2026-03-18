# Excluding EVI because many values outside of range -1, 1
vars_RF_months_Landsat_noCH <- c(
  # Min values of all indices
  "NDVI_min", "NDMI_min", "NDWI_min", "SAVI_min",
  # Max values of NDMI and NDWI
  "NDMI_max", "NDWI_max",
  # AUC of NDVI, and SAVI between march and oct
  "NDVI_auc_mar_oct", "SAVI_auc_mar_oct",
  # Values of NDVI and SAVI at march, pos and oct
  "NDVI_avg_value_03", "NDVI_pos_value", "NDVI_avg_value_10",
  "SAVI_avg_value_03", "SAVI_pos_value", "SAVI_avg_value_10",
  # Differences pos-march in value and doy
  "NDVI_diff_pos_march_value", "SAVI_diff_pos_march_value",
  "NDVI_diff_pos_march_doy", "SAVI_diff_pos_march_doy",
  # Differences pos-oct in value and doy
  "NDVI_diff_pos_oct_value", "SAVI_diff_pos_oct_value",
  "NDVI_diff_pos_oct_doy", "SAVI_diff_pos_oct_doy")


filtered_data_months_Landsat_noCH <- data_validation_Landsat %>%
  # Rough validation based on NDWI
  rough_validation_Landsat() %>% 
  # Join ReSurvey data to get Location method
  left_join(ReSurvey_data %>% select(PlotObservationID, Lctnmth)) %>%
  # Data filtering
  filter_data_Landsat(vars_RF_months_Landsat_noCH) %>% 
  # From GPS points, filter based on NDVI percentiles
  filter_by_group_percentiles("NDVI_pos_value")

# Months
set.seed(123)
split_data_months_Landsat_noCH <- split_train_test(filtered_data_months_Landsat_noCH)

train_data_months_Landsat_noCH <- split_data_months_Landsat_noCH$train
test_data_months_Landsat_noCH <- split_data_months_Landsat_noCH$test

# Check overlap between training and testing datasets
sum(train_data_months_Landsat_noCH$PlotObservationID %in% test_data_months_Landsat_noCHPlotObservationID)

rf_months_Landsat_noCH <- run_rf_parallel(vars_RF_months_Landsat_noCH, train_data_months_Landsat_noCH,
                                     "EUNIS1", ntree = 1500)

rf_months_Landsat_noCH_prediction <- predict(rf_months_Landsat_noCH$model,
                                             newdata = test_data_months_Landsat, 
                                             type = "response")

confusionMatrix(rf_months_Landsat_noCH_prediction,
                test_data_months_Landsat_noCH$EUNIS1)


# Refinement step 1

# Landsat, months, no CH
filtered_data_months_Landsat_noCH_with_probs <- bind_cols(
  filtered_data_months_Landsat_noCH %>% select(PlotObservationID, EUNIS1),
  as.data.frame(predict(rf_months_Landsat_noCH$model, 
                        # Predicting in all data (training + testing)
                        newdata = filtered_data_months_Landsat_noCH,
                        type = "response")) %>%
    rename(predicted_EUNIS = 1),
  do.call(rbind, predict(rf_months_Landsat_noCH$model, 
                         # Predicting in all data (training + testing)
                         newdata = filtered_data_months_Landsat_noCH,
                         type = "prob")) %>% as.data.frame()
) %>%
  rename(original_EUNIS = EUNIS1, prob_Q = EUNIS1.Q,
         prob_R = EUNIS1.R, prob_S = EUNIS1.S, prob_T = EUNIS1.T)

prob_cols_months_Landsat_noCH <- names(filtered_data_months_Landsat_noCH_with_probs)[grepl("^prob_", names(filtered_data_months_Landsat_noCH_with_probs))]

df_aux_months_Landsat_noCH <- calculate_df_aux(filtered_data_months_Landsat_noCH_with_probs,
                                          prob_cols_months_Landsat_noCH)

df_final_20_10_months_Landsat_noCH <- df_aux_months_Landsat_noCH %>%
  group_split(original_EUNIS) %>%
  map_dfr(filter_probs_neg20_pos10)

filtered_data_20_10_months_Landsat_probs_noCH <- filtered_data_months_Landsat_noCH %>%
  dplyr::filter(PlotObservationID %in% df_final_20_10_months_Landsat_noCH$PlotObservationID)

set.seed(123)
split_data_20_10_months_Landsat_noCH <- split_train_test(filtered_data_20_10_months_Landsat_probs_noCH)

train_data_20_10_months_Landsat_noCH <- split_data_20_10_months_Landsat_noCH$train
test_data_20_10_months_Landsat_noCH <- split_data_20_10_months_Landsat_noCH$test

sum(train_data_20_10_months_Landsat_nocH$PlotObservationID %in% test_data_20_10_months_Landsat$_nocHPlotObservationID)

rf_months_Landsat_refin_20_10_nocH <- run_rf_parallel(vars_RF_months_Landsat_noCH,
                                                 train_data_20_10_months_Landsat_noCH, 
                                                 "EUNIS1", ntree = 1500)



