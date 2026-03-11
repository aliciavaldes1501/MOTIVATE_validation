## ---------------------------------------------------------------------------------------------------------------------------------------
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


## ---------------------------------------------------------------------------------------------------------------------------------------
printall <- function(tibble) {
  print(tibble, width = Inf)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
extract_info <- function(filename) {
  first_word <- strsplit(filename, "_")[[1]][1]
  biogeo <- str_extract(first_word,
                        "^(ALP|ANA|ARC|ATL|BLACKSEA|BOR|CON|MACARONESIA|MED|PANONIA|STEPPIC)")
  unit <- str_remove(first_word, biogeo)
  if (is.na(unit) || unit == "") unit <- NA_character_
  list(biogeo = biogeo, unit = unit)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_metrics_models <- function(df, index_cols = c("NDVI", "EVI", "SAVI")) {
  suppressPackageStartupMessages({
    library(mgcv)
    library(nlme)
  })
  
  plan(multisession)  # Set up parallel processing
  
  # Create a list of index-specific data frames
  index_dfs <- lapply(index_cols, function(index_col) {
    list(index_col = index_col, df = df %>%
           select(DOY, PlotObservationID, all_of(index_col)))
  })
  
  # Define the processing function for each index
  process_index <- function(index_data) {
    index_col <- index_data$index_col
    df_index <- index_data$df %>%
      filter(!is.na(.data[[index_col]])) %>%
      arrange(DOY)
    
    plot_id <- unique(df_index$PlotObservationID)
    
    if (nrow(df_index) < 10) {
      message("  Skipped: insufficient data (< 10 rows)")
      return(tibble(PlotObservationID = plot_id, index = index_col,
                    sos_slope = NA_real_, sos_threshold = NA_real_,
                    pos = NA_real_, eos_slope = NA_real_, 
                    eos_threshold = NA_real_, auc_slope = NA_real_,
                    auc_threshold = NA_real_, Vmax = NA_real_,
                    DOY = df_index$DOY, value = NA_real_))
    }
    
    # Replace early/late DOY values
    base_value_early <- mean(df_index %>% filter(DOY <= 50) %>% 
                               pull(index_col), na.rm = TRUE)
    base_value_late  <- mean(df_index %>% filter(DOY >= 315) %>% 
                               pull(index_col), na.rm = TRUE)
    
    df_index <- df_index %>%
      mutate(!!index_col := case_when(
        DOY <= 50 ~ base_value_early,
        DOY >= 315 ~ base_value_late,
        TRUE ~ .data[[index_col]]
      ))
    
    x <- df_index$DOY
    y <- df_index[[index_col]]
    weights <- rep(1, length(y))
    
    # GAM fit
    pred <- NULL
    for (i in 1:3) {
      gam_fit <- tryCatch({
        mgcv::bam(y ~ s(x, bs = "tp"),weights = weights)
      }, error = function(e) {
        message("  GAM fitting failed for ", plot_id, " - ", index_col, ": ", 
                e$message)
        return(NULL)
      })
      if (is.null(gam_fit)) {
        return(tibble(
          PlotObservationID = plot_id,
          index = index_col,
          sos_slope = NA_real_,
          sos_threshold = NA_real_,
          pos = NA_real_,
          eos_slope = NA_real_, 
          eos_threshold = NA_real_, 
          auc_slope = NA_real_,
          auc_threshold = NA_real_, 
          Vmin_pre = NA_real_, 
          Vmin_post = NA_real_,
          Vmax = NA_real_, 
          u_sos = NA_real_, 
          u_eos = NA_real_,
          DOY = df_index$DOY,
          value = NA_real_))
      }
      
      pred <- tryCatch({
        predict(gam_fit, newdata = tibble(x = x))
      }, error = function(e) {
        message("Prediction failed for ", plot_id, " - ", index_col, ": ",
                e$message)
        return(rep(NA_real_, length(x)))
      })
      
      idx_between <- which(x > 50 & x < 315 & !is.na(pred) & pred != 0)
      weights <- rep(1, length(y))
      weights[idx_between] <- (y[idx_between] / (pred[idx_between] + 1e-6))^4
      weights[weights > 1 | is.na(weights)] <- 1
    }
    
    # Compute metrics
    slope <- c(NA, diff(pred))
    idx <- which(x >= 50 & x <= 315)
    pos <- if (length(idx) > 0) x[idx][which.max(pred[idx])] else NA_real_
    
    sos_slope <- if (!is.na(pos)) {
      idx <- which(x < pos)
      if (length(idx) > 0) x[idx][which.max(slope[idx])] else NA_real_
    } else NA_real_
    
    eos_slope <- if (!is.na(pos)) {
      idx <- which(x > pos)
      if (length(idx) > 0) x[idx][which.min(slope[idx])] else NA_real_
    } else NA_real_
    
    integration_idx_slope <- which(x >= sos_slope & x <= 
                                     eos_slope & !is.na(pred))
    auc_slope <- if (length(integration_idx_slope) > 1) {
      sum(diff(x[integration_idx_slope]) * 
            zoo::rollmean(pred[integration_idx_slope], 2))
    } else NA_real_
    
    # Vmin before and after the peak
    Vmin_pre <- if (!is.na(pos)) min(pred[x <= pos], na.rm = TRUE)else NA_real_
    Vmin_post <- if (!is.na(pos)) min(pred[x >= pos], na.rm = TRUE) else NA_real_
    Vmax <- max(pred, na.rm = TRUE)
    
    # Relative thresholds
    p <- 0.5
    u_sos <- if (!is.na(Vmin_pre)) Vmin_pre + p * (Vmax - Vmin_pre) else NA_real_
    u_eos <- if (!is.na(Vmin_post)) Vmin_post + p * (Vmax - Vmin_post) else NA_real_
    
    # DOY where thresholds are crossed
    sos_threshold <- if (!is.na(u_sos) && !is.na(pos)) {
      candidates <- x[which(pred >= u_sos & x < pos)]
      if (length(candidates) > 0) candidates[1] else NA_real_
    } else NA_real_
    
    eos_threshold <- if (!is.na(u_eos) && !is.na(pos)) {
      candidates <- x[which(pred >= u_eos & x > pos)]
      if (length(candidates) > 0) rev(candidates)[1] else NA_real_
    } else NA_real_
    
    integration_idx_threshold <- which(x >= sos_threshold & 
                                         x <= eos_threshold & !is.na(pred))
    auc_threshold <- if (length(integration_idx_threshold) > 1) {
      sum(diff(x[integration_idx_threshold]) * 
            zoo::rollmean(pred[integration_idx_threshold], 2))
    } else NA_real_
    
    # 1. Predictions per DOY
    fits_df <- tibble(
      PlotObservationID = unique(df_index$PlotObservationID),
      DOY = x,
      value = pred,
      index = index_col
    )
    
    # 2. Summary metrics
    metrics_df <- tibble(
      PlotObservationID = unique(df_index$PlotObservationID),
      index = index_col,
      sos_slope = sos_slope,
      sos_threshold = sos_threshold,
      pos = pos,
      eos_slope = eos_slope,
      eos_threshold = eos_threshold,
      auc_slope = auc_slope,
      auc_threshold = auc_threshold,
      Vmin_pre = Vmin_pre,
      Vmin_post = Vmin_post,
      Vmax = Vmax,
      u_sos = u_sos,
      u_eos = u_eos
    )
    
    # 3. Join by PlotObservationID, index
    final_df <- left_join(fits_df, metrics_df, 
                          by = c("PlotObservationID", "index"))
  }
  
  # Run in parallel
  results <- future_map(index_dfs, process_index, .progress = TRUE)
  results <- purrr::compact(results)  # removes NULLs
  if (length(results) == 0) return(tibble())  # or return(NULL)
  bind_rows(results)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
# extract_monthly_avg_indices <- function(
#   GAM_data, 
#   monthly_doys = list("01" = 1:31, "02" = 32:59, "03" = 60:90, "04" = 91:120, 
#                       "05" = 121:151, "06" = 152:181, "07" = 182:212, 
#                       "08" = 213:243, "09" = 244:273, "10" = 274:304,
#                       "11" = 305:334, "12" = 335:365)) {
#   
#   monthly_df <- GAM_data %>%
#     mutate(month = purrr::map_chr(DOY, function(doy) {
#       month_name <- names(monthly_doys)[sapply(monthly_doys, 
#                                                function(r) doy %in% r)]
#       if (length(month_name) > 0) month_name else NA_character_
#     })) %>%
#     dplyr::filter(!is.na(month)) %>%
#     group_by(PlotObservationID, index, month) %>%
#     summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
#     mutate(avg_value = ifelse(is.infinite(avg_value), NA, avg_value)) %>%
#     arrange(PlotObservationID, match(month, names(monthly_doys))) %>%
#     pivot_wider(names_from = month, values_from = avg_value, 
#                 names_prefix = "avg_value_")
#   
#   # Calculate AUC between March and October using the trapezoidal rule
#   months_auc <- c("03", "04", "05", "06", "07", "08", "09", "10")
#   # Approximate DOY of the center of each month
#   doy_midpoints <- c(75, 105, 135, 165, 195, 225, 255, 285)  
#   
#   monthly_df <- monthly_df %>%
#     rowwise() %>%
#     mutate(
#       auc_mar_oct = {
#         values <- c_across(all_of(paste0("avg_value_", months_auc)))
#         if (any(is.na(values))) NA_real_ else sum(diff(doy_midpoints) *
#                                                     zoo::rollmean(values, 2))
#       }
#     ) %>%
#     ungroup()
#   
#   return(monthly_df)
# }


## ---------------------------------------------------------------------------------------------------------------------------------------
extract_monthly_avg_indices <- function(
  GAM_data, 
  monthly_doys = list("01" = 1:31, "02" = 32:59, "03" = 60:90, "04" = 91:120, 
                      "05" = 121:151, "06" = 152:181, "07" = 182:212, 
                      "08" = 213:243, "09" = 244:273, "10" = 274:304,
                      "11" = 305:334, "12" = 335:365)) {
  
  # Step 1: calculate monthly averages
  monthly_df <- GAM_data %>%
    mutate(month = purrr::map_chr(DOY, function(doy) {
      month_name <- names(monthly_doys)[sapply(monthly_doys, 
                                                function(r) doy %in% r)]
      if (length(month_name) > 0) month_name else NA_character_
    })) %>%
    dplyr::filter(!is.na(month)) %>%
    group_by(PlotObservationID, index, month) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(avg_value = ifelse(is.infinite(avg_value), NA, avg_value)) %>%
    arrange(PlotObservationID, factor(month, levels = names(monthly_doys))) %>%
    pivot_wider(names_from = month, values_from = avg_value, 
                names_prefix = "avg_value_")
  
  # Step 2: calculate AUC between DOY 75 and 285 usando valores originales
  auc_df <- GAM_data %>%
    filter(DOY >= 75, DOY <= 285) %>%
    group_by(PlotObservationID, index) %>%
    arrange(DOY) %>%
    summarise(
      auc_mar_oct = {
        valid <- !is.na(value)
        x <- DOY[valid]
        y <- value[valid]
        # If less than 5 points, AUC is NA
        if (length(x) < 5) NA_real_ else sum(diff(x) * zoo::rollmean(y, 2))
      },
      .groups = "drop"
    )
  
  # Paso 3: unir ambos resultados
  final_df <- left_join(monthly_df, auc_df, by = c("PlotObservationID", "index"))
  
  return(final_df)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_unweighted_fit <- function(
    # Data frame df with index values over time (DOY)
    df, 
    # Name of the vegetation indices columns (e.g., "NDVI", "EVI", "SAVI)
    index_cols = c("NDMI", "NDWI")
) {
  # Initialize list to store results
  fits_list <- list()
  
  # Loop over each index column
  for (index_col in index_cols) {
    df_index <- df %>%
      # Remove rows with missing index values and sort data by DOY
      filter(!is.na(.data[[index_col]])) %>% arrange(DOY)
    
    # Extract x (DOY) and y (index) vectors for modelling
    x <- df_index$DOY
    y <- df_index[[index_col]]
    
    # If there are fewer than 11 observations or all values are NA, skip
    if (length(x) < 11 || all(is.na(y))) {
      next
    }
    
    # Replace early/late DOY values
    base_value_early <- mean(df_index %>% filter(DOY <= 50) %>% 
                               pull(index_col), na.rm = TRUE)
    base_value_late  <- mean(df_index %>% filter(DOY >= 315) %>% 
                               pull(index_col), na.rm = TRUE)

    df_index <- df_index %>%
      mutate(!!index_col := case_when(
        DOY <= 50 ~ base_value_early,
        DOY >= 315 ~ base_value_late,
        TRUE ~ .data[[index_col]]
      ))

    x <- df_index$DOY
    y <- df_index[[index_col]]
    weights <- rep(1, length(y))
    
    # Fit GAM (unweighted) with a thin plate spline (bs = "tp")
    # to smooth the index curve
    gam_unweighted <- mgcv::bam(y ~ s(x, bs = "tp"))
    pred <- predict(gam_unweighted, newdata = tibble(x = x))
    
    # Create tibble to store original and predicted index values
    fits_df <- tibble(
      PlotObservationID = unique(df$PlotObservationID),
      DOY = x,
      index = index_col,
      value = pred
    )
    
    fits_list[[index_col]] <- fits_df
  }
  
  if (length(fits_list) == 0) {
    return(tibble())
  }
  
  bind_rows(fits_list)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
plot_histogram <- function(data, x_var, x_label) {
  ggplot(data %>%
           dplyr::filter(EUNISa_1 %in% c("T", "R", "S", "Q")),
         aes(x = !!sym(x_var))) +
    geom_histogram(color = "black", fill = "white") +
    labs(x = x_label, y = "Frequency") +
    theme_bw()
}


## ---------------------------------------------------------------------------------------------------------------------------------------
distr_plot <- function(data, y_vars, y_labels) {
  for (i in seq_along(y_vars)) {
    y_var <- y_vars[[i]]
    y_label <- y_labels[[i]]
    
    p <- ggplot(data = data %>%
                  dplyr::filter(EUNISa_1 %in% c("T", "R", "S", "Q")),
                aes(x = EUNISa_1_descr, y = !!sym(y_var), fill = EUNISa_1_descr)) +
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
      geom_point(aes(y = !!sym(y_var), color = EUNISa_1_descr),
                 position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
      stat_summary(fun.y = mean, geom = "point", shape = 20, size = 1) +
      stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1,
                                                     label = length(x)),
                   geom = "text", aes(label = ..label..), vjust = 0.5) +
      labs(y = y_label, x = "EUNIS level 1") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      guides(fill = FALSE, color = FALSE) +
      theme_bw() + coord_flip()
    
    print(p)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------
distr_plot_biogeo <- function(data, y_vars, y_labels) {
  plots <- list()
  
  for (i in seq_along(y_vars)) {
    y_var <- y_vars[[i]]
    y_label <- y_labels[[i]]
    
    p <- ggplot(data = data %>%
                  dplyr::filter(EUNISa_1 %in% c("T", "R", "S", "Q")),
                aes(x = EUNISa_1_descr, y = !!sym(y_var), fill = EUNISa_1_descr)) +
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
      geom_point(aes(y = !!sym(y_var), color = EUNISa_1_descr),
                 position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
      stat_summary(fun.y = mean, geom = "point", shape = 20, size = 1) +
      stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1,
                                                     label = length(x)),
                   geom = "text", aes(label = ..label..), vjust = 0.5) +
      labs(y = y_label, x = "EUNISa_1_descr") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      guides(fill = FALSE, color = FALSE) +
      theme_bw() + coord_flip() + facet_wrap(~ biogeo)
    
    plots[[y_var]] <- p
  }
  
  return(plots)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
split_train_test <- function(data, proportion = 0.7) {
  train_indices <- sample(1:nrow(data), size = floor(proportion * nrow(data)))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  return(list(train = train_data, test = test_data))
  }


## ---------------------------------------------------------------------------------------------------------------------------------------
run_rf <- function(vars_RF, train_data, response_var, ntree = 500) 
  {
  
  # Detect and register available cores (leave one free)
  n_cores <- parallel::detectCores() - 1
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  train_name <- deparse(substitute(train_data))
  
  # Export necessary variables to the cluster
  clusterExport(cl, varlist = c("vars_RF", "train_data", "response_var"),
                envir = environment())

  
  # Set seed for reproducibility
  set.seed(123)
  
  # Measure execution time
  execution_time <- system.time({
    rf_model <- fastcforest(
      formula = reformulate(vars_RF, response = response_var),
      data = train_data,
      controls = party::cforest_control(
        mtry = round(sqrt(length(vars_RF))),
        ntree = ntree,
        # Decision trees were developed using subsampling without replacement,
        # which is appropriate to use when predictors vary in their scale of
        # measurement (Strobl et al., 2007).
        replace = FALSE
      ),
      parallel = TRUE
    )
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  # Return both the model and execution time
  list(model = rf_model, time = execution_time)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_varimp <- function(model, nperm = 100) {

  # Measure execution time
  execution_time <- system.time({
    varimp_result <- permimp(model, conditional = FALSE, progressBar = TRUE)
  })

  return(list(varimp = varimp_result, time = execution_time))
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_varimp_cond <- function(model, nperm = 100) {

  # Measure execution time
  execution_time <- system.time({
    varimp_result <- permimp(model, conditional = TRUE, progressBar = TRUE)
  })

  return(list(varimp = varimp_result, time = execution_time))
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_roc_level1 <- function(model, test_data) {
  # Measure execution time
  execution_time <- system.time({
    # Step 1: Predict probabilities
    probabilities <- predict(model, newdata = test_data, type = "prob")
    
    # Step 2: Convert list of matrices to a proper data frame
    prob_matrix <- t(sapply(probabilities, as.vector))
    prob_df <- as.data.frame(prob_matrix)
    colnames(prob_df) <- c("Q", "R", "S", "T")
    
    # Step 3: Prepare actual class labels
    actual <- factor(test_data$EUNISa_1, levels = c("Q", "R", "S", "T"))
    
    # Step 4: Binarize actual labels
    actual_bin <- model.matrix(~ actual - 1)
    colnames(actual_bin) <- gsub("actual", "", colnames(actual_bin))
    
    # Step 5: Compute ROC data for each class
    roc_data <- lapply(levels(actual), function(class) {
      roc_obj <- roc(actual_bin[, class], prob_df[[class]])
      auc_val <- round(auc(roc_obj), 3)
      data.frame(
        FPR = rev(roc_obj$specificities),
        TPR = rev(roc_obj$sensitivities),
        Class = paste0(class, " (AUC = ", auc_val, ")")
      )
    }) %>% bind_rows()
  })
  
  # Return both ROC data and execution time
  return(list(roc = roc_data, time = execution_time))
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compute_roc_level2 <- function(model, test_data) {
  # Measure execution time
  execution_time <- system.time({
    # Step 1: Predict probabilities
    probabilities <- predict(model, newdata = test_data, type = "prob")
    
    # Step 2: Convert list of matrices to a proper data frame
    prob_matrix <- t(sapply(probabilities, as.vector))
    prob_df <- as.data.frame(prob_matrix)
    colnames(prob_df) <- c("Q1", "Q2", "Q4", "Q5", "R1", "R2", "R3", "R4", "R5",
                           "R6", "S3", "S4", "T1", "T3")
    
    # Step 3: Prepare actual class labels
    actual <- factor(test_data$EUNISa_2, 
                     levels = c("Q1", "Q2", "Q4", "Q5", "R1", "R2", "R3", "R4",
                                "R5", "R6", "S3", "S4", "T1", "T3"))
    
    # Step 4: Binarize actual labels
    actual_bin <- model.matrix(~ actual - 1)
    colnames(actual_bin) <- gsub("actual", "", colnames(actual_bin))
    
    # Step 5: Compute ROC data for each class
    roc_data <- lapply(levels(actual), function(class) {
      roc_obj <- roc(actual_bin[, class], prob_df[[class]])
      auc_val <- round(auc(roc_obj), 3)
      data.frame(
        FPR = rev(roc_obj$specificities),
        TPR = rev(roc_obj$sensitivities),
        Class = paste0(class, " (AUC = ", auc_val, ")")
      )
    }) %>% bind_rows()
  })
  
  # Return both ROC data and execution time
  return(list(roc = roc_data, time = execution_time))
}


## ---------------------------------------------------------------------------------------------------------------------------------------
rough_validation_S2 <- function(data) {
  data %>%
    mutate(
      valid_1_NDWI = case_when(
        NDWI_max > 0.3 ~ "wrong",
        TRUE ~ NA_character_
      ),
      valid_1_CH = case_when(
        EUNISa_1 %in% c("R", "Q") & canopy_height > 2 ~ "wrong",
        EUNISa_1 == "T" & canopy_height < 3 ~ "wrong",
        EUNISa_1 == "S" & canopy_height > 3 ~ "wrong",
        TRUE ~ NA_character_
      ),
      valid_1_count = rowSums(across(c(valid_1_NDWI, valid_1_CH), ~ . == "wrong"), na.rm = TRUE),
      valid_1 = if_else(valid_1_count > 0, "At least 1 rule broken", "No rules broken so far")
    ) %>%
    # Keep only rows with no rules broken
    dplyr::filter(valid_1 == "No rules broken so far")
}


## ---------------------------------------------------------------------------------------------------------------------------------------
rough_validation_SatEmb <- function(data) {
  data %>%
    mutate(
      valid_1_CH = case_when(
        EUNISa_1 %in% c("R", "Q") & canopy_height > 2 ~ "wrong",
        EUNISa_1 == "T" & canopy_height < 3 ~ "wrong",
        EUNISa_1 == "S" & canopy_height > 3 ~ "wrong",
        TRUE ~ NA_character_
      ),
      valid_1_count = rowSums(across(c(valid_1_CH), ~ . == "wrong"), na.rm = TRUE),
      valid_1 = if_else(valid_1_count > 0, "At least 1 rule broken", "No rules broken so far") 
    ) %>%
    # Keep only rows with no rules broken
    dplyr::filter(valid_1 == "No rules broken so far")
}


## ---------------------------------------------------------------------------------------------------------------------------------------
rough_validation_Landsat <- function(data) {
  data %>%
    mutate(
      valid_1_NDWI = case_when(
        NDWI_max > 0.3 ~ "wrong",
        TRUE ~ NA_character_
      ),
      valid_1_count = rowSums(across(c(valid_1_NDWI), ~ . == "wrong"), na.rm = TRUE),
      valid_1 = if_else(valid_1_count > 0, "At least 1 rule broken", "No rules broken so far")
    ) %>%
    # Keep only rows with no rules broken
    dplyr::filter(valid_1 == "No rules broken so far")
}


## ---------------------------------------------------------------------------------------------------------------------------------------
distr_plot_percentiles <- function(data, y_vars, y_labels) {
  for (i in seq_along(y_vars)) {
    y_var <- y_vars[[i]]
    y_label <- y_labels[[i]]
    
    # Calculate percentiles per EUNISa_1 group
    percentiles <- data %>%
      group_by(EUNISa_1) %>%
      summarise(
        p10 = quantile(.data[[y_var]], 0.1, na.rm = TRUE),
        p90 = quantile(.data[[y_var]], 0.9, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join percentiles back to data
    data_flagged <- data %>%
      left_join(percentiles, by = "EUNISa_1") %>%
      mutate(outlier_flag = case_when(
        .data[[y_var]] < p10 ~ "low",
        .data[[y_var]] > p90 ~ "high",
        TRUE ~ "mid"
      ))
    
    # Filter and plot
    p <- ggplot(data = data_flagged %>%
                  dplyr::filter(EUNISa_1 %in% c("T", "R", "S", "Q")),
                aes(x = EUNISa_1_descr, y = .data[[y_var]])) +
      geom_flat_violin(aes(fill = EUNISa_1_descr),
                       position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
      geom_point(aes(color = ifelse(outlier_flag == "mid",
                                    EUNISa_1_descr, "grey")),
                 position = position_jitter(width = 0.15), size = 1,
                 alpha = 0.6) +
      geom_boxplot(aes(fill = EUNISa_1_descr), width = 0.2, outlier.shape = NA,
                   alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 20, size = 1) +
      stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) +
                                                       0.1, label = length(x)),
                   geom = "text", aes(label = ..label..), vjust = 0.5) +
      labs(y = y_label, x = "EUNIS level 1") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      guides(fill = FALSE, color = FALSE) +
      theme_bw() + coord_flip() +
      scale_color_manual(values = c(
        "Forests and other wooded land" = "#F8766D",
        "Grasslands" = "#7CAE00",
        "Heathlands, scrub and tundra" = "#00BFC4",
        "Wetlands" = "#C77CFF",
        "grey" = "grey"))
    
    print(p)
  }
}


## ---------------------------------------------------------------------------------------------------------------------------------------
calculate_df_aux <- function(data, prob_cols) {
  data %>%
    rowwise() %>%
    mutate(
      # Probability of reference (original) class
      p_ref = get(paste0("prob_", original_EUNIS)),
      # Maximum probability between all classes
      max_prob = max(c_across(all_of(prob_cols)), na.rm = TRUE),
      # Second maximum probability
      second_max = sort(c_across(all_of(prob_cols)), decreasing = TRUE)[2],
      # Difference between original class and second best
      delta = p_ref - second_max,
      # TRUE if original class is not the most probable
      neg_flag = p_ref < max_prob,
      # TRUE if difference is small (uncertainty)
      small_flag = delta < 0.1,
      # How much worse is the original class with respect to the best class
      gap_neg = max_prob - p_ref
    ) %>%
    ungroup()
}


## ---------------------------------------------------------------------------------------------------------------------------------------
filter_probs_conserv <- function(gdf, df) {
  frac <- nrow(gdf) / nrow(df)
  
  # Removes negatives
  cap_total <- if (frac < 0.05) {
    # max 35% of the class for small classes (i.e. T)
    ceiling(0.35 * nrow(gdf))
  } else {
    # max 10% of the class for other classes
    ceiling(0.10 * nrow(gdf))
  }
  
  neg <- gdf %>% filter(neg_flag) %>% arrange(desc(gap_neg))
  drop_neg <- head(neg, cap_total)
  cap_left <- cap_total - nrow(drop_neg)

    drop_ids <- c(drop_neg$PlotObservationID)
  gdf %>% filter(!PlotObservationID %in% drop_ids) %>%
    select(-max_prob, -second_max, -delta, -neg_flag, -small_flag, -gap_neg)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
filter_probs_neg35_pos10_class_size <- function(gdf, df) {
  frac <- nrow(gdf) / nrow(df)
  
  # Removes negatives
  cap_total <- if (frac < 0.05) {
    # max 35% of the class for small classes (i.e. T)
    ceiling(0.35 * nrow(gdf))
  } else {
    # max 10% of the class for other classes
    ceiling(0.10 * nrow(gdf))
  }
  
  neg <- gdf %>% filter(neg_flag) %>% arrange(desc(gap_neg))
  drop_neg <- head(neg, cap_total)
  cap_left <- cap_total - nrow(drop_neg)

  drop_pos <- tibble()
  if (cap_left > 0) {
    remaining <- nrow(gdf) - nrow(drop_neg)
    # If there is room, removes positives with doubts (small_flag = TRUE)
    # until 10% of the rest
    pos_cap <- ceiling(0.10 * remaining)

    drop_pos <- gdf %>%
      filter(!neg_flag & small_flag) %>%
      arrange(delta) %>%
      slice_head(n = min(cap_left, pos_cap))
  }

  drop_ids <- c(drop_neg$PlotObservationID, drop_pos$PlotObservationID)
  gdf %>% filter(!PlotObservationID %in% drop_ids) %>%
    select(-max_prob, -second_max, -delta, -neg_flag, -small_flag, -gap_neg)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
filter_probs_neg35_pos10 <- function(gdf) {
  # Removes negatives until 35% of the class
  cap_total <- ceiling(0.35 * nrow(gdf))  # max 35% of the class

  neg <- gdf %>% filter(neg_flag) %>% arrange(desc(gap_neg))
  drop_neg <- head(neg, cap_total)
  cap_left <- cap_total - nrow(drop_neg)

  drop_pos <- tibble()
  if (cap_left > 0) {
    remaining <- nrow(gdf) - nrow(drop_neg)
    # If there is room, removes positives with doubts (small_flag = TRUE)
    # until 10% of the rest
    pos_cap <- ceiling(0.10 * remaining)

    drop_pos <- gdf %>%
      filter(!neg_flag & small_flag) %>%
      arrange(delta) %>%
      slice_head(n = min(cap_left, pos_cap))
  }

  drop_ids <- c(drop_neg$PlotObservationID, drop_pos$PlotObservationID)
  gdf %>% filter(!PlotObservationID %in% drop_ids) %>%
    select(-max_prob, -second_max, -delta, -neg_flag, -small_flag, -gap_neg)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
filter_probs_neg35 <- function(gdf) {
  # Removes negatives until 35% of the class
  cap_total <- ceiling(0.35 * nrow(gdf))  # max 35% of the class

  neg <- gdf %>% filter(neg_flag) %>% arrange(desc(gap_neg))
  drop_neg <- head(neg, cap_total)
  cap_left <- cap_total - nrow(drop_neg)

  drop_ids <- c(drop_neg$PlotObservationID)
  gdf %>% filter(!PlotObservationID %in% drop_ids) %>%
    select(-max_prob, -second_max, -delta, -neg_flag, -small_flag, -gap_neg)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
plot_confusion_matrix <- function(predictions, reference, 
                                  title = "Confusion Matrix") {
  cm_df <- as.data.frame(as.table(confusionMatrix(predictions, reference)))
  colnames(cm_df) <- c("Prediction", "Reference", "Freq")

  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(x = "Reference", y = "Prediction") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold")
    )
}


## ---------------------------------------------------------------------------------------------------------------------------------------
compare_classes <- function(data_before, data_after, class_col = "EUNISa_1") {
  
  # Count classes before and after
  counts <- full_join(
    data_before %>% count(.data[[class_col]]) %>% rename(before = n),
    data_after %>% count(.data[[class_col]]) %>% rename(after = n),
    by = class_col
  )

  # Convert to long format
  counts_long <- counts %>%
    pivot_longer(cols = c(before, after), names_to = "state", 
                 values_to = "n") %>%
    mutate(state = factor(state, levels = c("before", "after")))

  # Plot
  ggplot(counts_long, aes(x = .data[[class_col]], y = n, fill = state)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
      title = "Size of each class before and after filtering",
      x = "Class (original_EUNIS)",
      y = "Number of rows",
      fill = "State"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1))
}


## ---------------------------------------------------------------------------------------------------------------------------------------
run_rf_new <- function(vars_RF, train_data, response_var, ntree = 500) {
  set.seed(123)
  execution_time <- system.time({
    rf_model <- moreparty::fastcforest(
      formula  = reformulate(vars_RF, response = response_var),
      data     = train_data,
      controls = party::cforest_control(
        mtry    = max(1, round(sqrt(length(vars_RF)))),
        ntree   = ntree,
        replace = FALSE
      )
    )
  })
  list(model = rf_model, time = execution_time)
}


## ---------------------------------------------------------------------------------------------------------------------------------------
cv_fastcforest <- function(data, vars_RF, response_var, k = 5, ntree = 500) {

  # 1) Folds (estratificados si 'response_var' es factor)
  folds <- caret::createFolds(data[[response_var]], k = k, list = TRUE)

  # 2) Backend paralelo (PSOCK habitual en Windows)
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores, type = "PSOCK")
  doParallel::registerDoParallel(cl)

  # 3) Alinear rutas de librerías en los workers
  parallel::clusterCall(cl, function(p) .libPaths(p), .libPaths())

  # 4) Cargar paquetes necesarios en cada worker
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(moreparty)  # <- imprescindible
      library(party)      # <- si usas cforest_control/unbiased
      library(caret)
    })
    TRUE
  })

  # 5) Verificar que moreparty::fastcforest existe en cada worker
  ok <- parallel::clusterEvalQ(cl, exists("fastcforest", where = asNamespace("moreparty")))
  if (!all(unlist(ok))) {
    parallel::stopCluster(cl)
    stop("En al menos un worker no está disponible moreparty::fastcforest tras cargar paquetes.")
  }

  # 6) Exportar función y objetos necesarios
  parallel::clusterExport(
    cl,
    varlist = c("run_rf_new", "vars_RF", "response_var", "data", "ntree"),
    envir   = environment()
  )

  # 7) CV paralelo con foreach — nota: .packages incluye moreparty y party
  cv_results <- foreach::foreach(
    i = seq_len(k),
    .packages = c("moreparty", "party", "caret"),
    .errorhandling = "pass"
  ) %dopar% {

    test_idx <- folds[[i]]
    train_df <- data[-test_idx, ]
    test_df  <- data[test_idx, ]

    fit   <- run_rf_new(vars_RF, train_df, response_var, ntree = ntree)
    preds <- predict(fit$model, newdata = test_df)

    list(
      fold = i,
      obs  = test_df[[response_var]],
      pred = preds,
      time = fit$time
    )
  }

  # 8) Cerrar clúster
  parallel::stopCluster(cl)

  # 9) Reportar errores por fold si los hubo
  had_error <- vapply(cv_results, inherits, logical(1), "error")
  if (any(had_error)) {
    idx <- which(had_error)
    stop(sprintf("Errores en folds: %s. Primer error: %s",
                 paste(idx, collapse = ", "),
                 cv_results[[idx[1]]]$message))
  }

  # 10) Combinar predicciones
  cv_df <- do.call(rbind, lapply(cv_results, function(x)
    data.frame(fold = x$fold, obs = x$obs, pred = x$pred, stringsAsFactors = FALSE)))

  list(
    predictions = cv_df,
    timing      = lapply(cv_results, `[[`, "time")
  )
}


## ---------------------------------------------------------------------------------------------------------------------------------------
cv_fastcforest_future <- function(data, vars_RF, response_var, k = 5, ntree = 500) {
  
  # Crear folds (estratificados si la respuesta es factor)
  folds <- caret::createFolds(data[[response_var]], k = k, list = TRUE)
  
  # Ejecutar CV en paralelo usando future_lapply
  cv_results <- future_lapply(seq_len(k), function(i) {
    
    # Cargar paquetes dentro del worker
    library(moreparty)
    library(party)
    
    # Partición train/test para el fold i
    test_idx <- folds[[i]]
    train_df <- data[-test_idx, ]
    test_df  <- data[test_idx, ]
    
    # Entrenar modelo
    fit <- run_rf_new(vars_RF, train_df, response_var, ntree = ntree)
    
    # Predecir
    preds <- predict(fit$model, newdata = test_df)
    
    # Devolver resultados del fold
    list(
      fold = i,
      obs  = test_df[[response_var]],
      pred = preds,
      time = fit$time
    )
  })
  
  # Combinar en un único data.frame
  cv_df <- do.call(rbind, lapply(cv_results, function(x)
    data.frame(
      fold = x$fold,
      obs  = x$obs,
      pred = x$pred,
      stringsAsFactors = FALSE
    )
  ))
  
  list(
    predictions = cv_df,
    timing = lapply(cv_results, `[[`, "time")
  )
}


## ---------------------------------------------------------------------------------------------------------------------------------------
# Session info
sessionInfo()

