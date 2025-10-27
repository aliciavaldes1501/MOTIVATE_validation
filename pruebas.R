
# ROC AUC
roc_obj <- roc(test_data_slope_S2$EUNISa_1, predict(rf_slope_S2$model,
                                                    newdata = test_data_slope_S2, 
                                                    type = "prob"))
auc_value <- auc(roc_obj)
auc_value