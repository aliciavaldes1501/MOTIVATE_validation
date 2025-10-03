## ---------------------------------------------------------------------------------------------------------
printall <- function(tibble) {
  print(tibble, width = Inf)
}


## ---------------------------------------------------------------------------------------------------------
extract_info <- function(filename) {
  first_word <- strsplit(filename, "_")[[1]][1]
  biogeo <- str_extract(first_word,
                        "^(ALP|ANA|ARC|ATL|BLACKSEA|BOR|CON|MACARONESIA|MED|PANONIA|STEPPIC)")
  unit <- str_remove(first_word, biogeo)
  if (is.na(unit) || unit == "") unit <- NA_character_
  list(biogeo = biogeo, unit = unit)
}


## ---------------------------------------------------------------------------------------------------------
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


## ---------------------------------------------------------------------------------------------------------
# Session info
sessionInfo()

