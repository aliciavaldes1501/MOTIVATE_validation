## -------------------------------------------------------------------------------------------------------------------------------------------------------------
extract_info <- function(filename) {
  first_word <- strsplit(filename, "_")[[1]][1]
  biogeo <- str_extract(first_word,
                        "^(ALP|ANA|ARC|ATL|BLACKSEA|BOR|CON|MACARONESIA|MED|PANONIA|STEPPIC)")
  unit <- str_remove(first_word, biogeo)
  if (is.na(unit) || unit == "") unit <- NA_character_
  list(biogeo = biogeo, unit = unit)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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
                    sos_slope = NA_real_, pos = NA_real_, eos_slope = NA_real_, 
                    auc_slope = NA_real_, DOY = df_index$DOY, value = NA_real_))
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
          pos = NA_real_,
          eos_slope = NA_real_, 
          auc_slope = NA_real_,
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
      pos = pos,
      eos_slope = eos_slope,
      auc_slope = auc_slope
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
extract_monthly_avg_indices <- function(GAM_data) {

  # 1. Build monthly DOY mapping per year (handles leap years automatically)
  build_monthly_doys <- function(y) {
    dates <- seq.Date(
      from = as.Date(paste0(y, "-01-01")),
      to   = as.Date(paste0(y, "-12-31")),
      by = "day"
    )
    data.frame(
      DOY = lubridate::yday(dates),
      month = format(dates, "%m"),
      year = y
    )
  }

  # Create lookup table for all years present
  years <- unique(GAM_data$year)
  doy_lookup <- dplyr::bind_rows(lapply(years, build_monthly_doys))

  # 2. Join GAM_data with the DOY lookup to assign month correctly
  GAM_with_month <- GAM_data %>%
    left_join(doy_lookup, by = c("DOY", "year"))

  # 3. Compute monthly averages
  monthly_df <- GAM_with_month %>%
    filter(!is.na(month)) %>%
    group_by(PlotObservationID, index, year, month) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(PlotObservationID, index, year, month) %>%
    pivot_wider(
      names_from = month, 
      values_from = avg_value, 
      names_prefix = "avg_value_"
    )

  # 4. Compute AUC for each year independently (same DOY window always)
  auc_df <- GAM_data %>%
    filter(DOY >= 75, DOY <= 285) %>%
    group_by(PlotObservationID, index, year) %>%
    arrange(DOY) %>%
    summarise(
      auc_mar_oct = {
        valid <- !is.na(value)
        x <- DOY[valid]
        y <- value[valid]
        if (length(x) < 5) NA_real_ else sum(diff(x) * zoo::rollmean(y, 2))
      },
      .groups = "drop"
    )

  # 5. Combine results
  final_df <- left_join(monthly_df, auc_df,
                        by = c("PlotObservationID", "index", "year"))

  return(final_df)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    
    # If there are fewer than 10 observations or all values are NA, skip
    if (length(x) < 10 || all(is.na(y))) {
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
split_train_test <- function(data, proportion = 0.7) {
  train_indices <- sample(1:nrow(data), size = floor(proportion * nrow(data)))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  return(list(train = train_data, test = test_data))
  }


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
run_rf_parallel <- function(vars_RF, train_data, response_var, ntree = 500) {
  # Build formula locally (no NSE)
  fmla <- reformulate(vars_RF, response = response_var)

  # Start cluster
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)

  # Make sure packages are available on workers
  parallel::clusterEvalQ(cl, {
    library(party)
  })

  # Export the *data* to workers under a simple name
  local_data <- train_data
  parallel::clusterExport(cl, varlist = c("local_data"), envir = environment())

  # Fit in parallel
  execution_time <- system.time({
    rf_model <- fastcforest(
      formula  = fmla,
      data     = local_data,   # use the exported name
      controls = party::cforest_control(
        mtry  = max(1, round(sqrt(length(vars_RF)))),
        ntree = ntree,
        # Decision trees were developed using subsampling without replacement,
        # which is appropriate to use when predictors vary in their scale of
        # measurement (Strobl et al., 2007).
        replace = FALSE
      ),
      parallel = TRUE
    )
  })

  parallel::stopCluster(cl)

  list(model = rf_model, time = execution_time)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
compute_varimp <- function(model, nperm = 100) {

  # Measure execution time
  execution_time <- system.time({
    varimp_result <- permimp(model, conditional = FALSE, progressBar = TRUE)
  })

  return(list(varimp = varimp_result, time = execution_time))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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
    actual <- factor(test_data$EUNIS1, levels = c("Q", "R", "S", "T"))
    
    # Step 4: Binarize actual labels
    actual_bin <- model.matrix(~ actual - 1)
    colnames(actual_bin) <- gsub("actual", "", colnames(actual_bin))
    
    # Step 5: Compute ROC data for each class
    roc_data <- lapply(levels(actual), function(class) {
      roc_obj <- roc(actual_bin[, class], prob_df[[class]])
      auc_val <- round(auc(roc_obj), 3)
      data.frame(
        FPR = 1 - roc_obj$specificities,
        TPR = roc_obj$sensitivities,
        Class = paste0(class, " (AUC = ", auc_val, ")")
      )
    }) %>% bind_rows()
  })
  
  # Return both ROC data and execution time
  return(list(roc = roc_data, time = execution_time))
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# data: your tibble/data.frame
# y_var: name of the numeric column as a string, e.g., "NDVI_pos_value"
filter_by_group_percentiles <- function(data, y_var) {
  cats <- c("T","Q","R","S")

  # Compute thresholds per group for the selected column
  qs <- data %>%
    dplyr::group_by(EUNIS1) %>%
    dplyr::summarise(
      p10 = stats::quantile(.data[[y_var]], probs = 0.10, na.rm = TRUE),
      p90 = stats::quantile(.data[[y_var]], probs = 0.90, na.rm = TRUE),
      .groups = "drop"
    )

  # Join thresholds and filter rows according to the rule
  d_filtered <- data %>%
    dplyr::left_join(qs, by = "EUNIS1") %>%
    dplyr::filter(
      dplyr::case_when(
        EUNIS1 == "T" ~ .data[[y_var]] >= p10,        # keep >= 10% for T
        EUNIS1 %in% c("Q","R","S") ~ .data[[y_var]] <= p90,  # keep <= 90% for Q/R/S
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::select(-p10, -p90)  # drop helper columns

  return(d_filtered)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
distr_plot <- function(data,
                       y_vars,
                       y_labels,
                       grey = TRUE,
                       p_low = 0.10,
                       p_high = 0.90,
                       low_groups = c("T"),
                       high_groups = c("Q", "R", "S"),
                       grey_color = "grey60",
                       print_plots = FALSE) {
  stopifnot(length(y_vars) == length(y_labels))

  # Ensure y_vars/y_labels can be length-1 strings
  if (!is.vector(y_vars))   y_vars   <- c(y_vars)
  if (!is.vector(y_labels)) y_labels <- c(y_labels)

  library(ggplot2)
  library(dplyr)
  library(rlang)
  library(stringr)

  base_data <- data %>%
    dplyr::filter(EUNIS1 %in% union(low_groups, high_groups))

  plots <- vector("list", length(y_vars))

  for (i in seq_along(y_vars)) {
    y_var   <- y_vars[[i]]
    y_label <- y_labels[[i]]

    if (grey) {
      df_flag <- base_data %>%
        group_by(EUNIS1) %>%
        mutate(
          q_low  = quantile(!!sym(y_var), p_low,  na.rm = TRUE),
          q_high = quantile(!!sym(y_var), p_high, na.rm = TRUE),
          grey_flag = case_when(
            EUNIS1 %in% low_groups  ~ (!!sym(y_var)) <= q_low,
            EUNIS1 %in% high_groups ~ (!!sym(y_var)) >= q_high,
            TRUE ~ FALSE
          ),
          grey_flag = coalesce(grey_flag, FALSE)
        ) %>%
        ungroup()
    }

    p <- ggplot(
      data = base_data,
      aes(x = EUNIS1, y = !!sym(y_var), fill = EUNIS1)
    ) +
      # Violin
      geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +

      # Points (with or without greying)
      {
        if (grey) {
          list(
            geom_point(
              data = df_flag %>% dplyr::filter(!grey_flag),
              aes(y = !!sym(y_var), color = EUNIS1),
              position = position_jitter(width = 0.15),
              size = 0.05, alpha = 0.25
            ),
            geom_point(
              data = df_flag %>% dplyr::filter(grey_flag),
              aes(y = !!sym(y_var)),
              color = grey_color,
              position = position_jitter(width = 0.15),
              size = 0.05, alpha = 0.6
            )
          )
        } else {
          geom_point(
            aes(y = !!sym(y_var), color = EUNIS1),
            position = position_jitter(width = 0.15),
            size = 0.05, alpha = 0.25
          )
        }
      } +

      # BOX PLOT LAST (on top) — only change for rule #1
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +

      labs(y = y_label, x = "EUNIS level 1") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      guides(fill = "none", color = "none") +
      theme_bw() + coord_flip()  +
      scale_color_manual(values = c(
        "#0072B2", # blue
        "#D55E00", # vermillion
        "#009E73", # bluish green
        "#CC79A7"  # reddish purple
        )) +
      scale_fill_manual(values = c(
        "#0072B2", # blue
        "#D55E00", # vermillion
        "#009E73", # bluish green
        "#CC79A7"  # reddish purple
        ))
    
    if (print_plots) print(p)
    plots[[i]] <- p
  }

  # Return a single ggplot if length == 1 (so you can use it with grid.arrange directly)
  if (length(plots) == 1L) {
    return(plots[[1L]])
  } else {
    return(plots)  # list of ggplots
  }
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
filter_probs_neg20_pos10 <- function(gdf) {
  # Removes negatives until 20% of the class
  cap_total <- ceiling(0.20 * nrow(gdf))  # max 20% of the class

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


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
filter_probs_neg20 <- function(gdf) {
  # Removes negatives until 20% of the class
  cap_total <- ceiling(0.20 * nrow(gdf))  # max 20% of the class

  neg <- gdf %>% filter(neg_flag) %>% arrange(desc(gap_neg))
  drop_neg <- head(neg, cap_total)
  cap_left <- cap_total - nrow(drop_neg)

  drop_ids <- c(drop_neg$PlotObservationID)
  gdf %>% filter(!PlotObservationID %in% drop_ids) %>%
    select(-max_prob, -second_max, -delta, -neg_flag, -small_flag, -gap_neg)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_confusion_matrix <- function(predictions, reference, 
                                  title = "Confusion Matrix") {
  cm_df <- as.data.frame(as.table(confusionMatrix(predictions, reference)))
  colnames(cm_df) <- c("Prediction", "Reference", "Freq")

  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Reference", y = "Prediction") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold")
    )
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
compare_classes <- function(
  data_before, data_step1, data_step2 = NULL,
  class_col = "EUNIS1",
  x_lab = NULL,
  title = NULL,
  return = c("plot", "both", "counts")
) {
  return <- match.arg(return)

  # Helper: safe counting that works with empty data frames
  count_safe <- function(df, out_name) {
    if (is.null(df) || nrow(df) == 0) {
      tibble(!!class_col := character(), !!out_name := integer())
    } else {
      if (!(class_col %in% names(df))) {
        abort(paste0("Column '", class_col, "' is missing in one of the inputs."))
      }
      df %>% count(.data[[class_col]], name = out_name)
    }
  }

  # Build counts table (before + step1; optionally step2)
  counts <- full_join(
    count_safe(data_before, "before"),
    count_safe(data_step1,  "step1"),
    by = class_col
  )

  has_step2 <- !is.null(data_step2)
  if (has_step2) {
    counts <- full_join(
      counts,
      count_safe(data_step2, "step2"),
      by = class_col
    )
  } else {
    # Ensure the column exists for consistent downstream handling (as NA)
    counts <- counts %>% mutate(step2 = NA_integer_)
  }

  # Replace missing counts with zero for plotting
  counts <- counts %>%
    mutate(
      before = replace_na(before, 0L),
      step1  = replace_na(step1,  0L),
      step2  = replace_na(step2,  0L)
    )

  # Prepare long format depending on availability of step2
  if (has_step2) {
    counts_long <- counts %>%
      pivot_longer(
        cols = c(before, step1, step2),
        names_to = "state",
        values_to = "n"
      ) %>%
      mutate(state = factor(state, levels = c("before", "step1", "step2")))
    legend_title <- "State"
  } else {
    counts_long <- counts %>%
      select(any_of(c(class_col, "before", "step1"))) %>%
      pivot_longer(
        cols = c(before, step1),
        names_to = "state",
        values_to = "n"
      ) %>%
      mutate(state = factor(state, levels = c("before", "step1")))
    legend_title <- "State"
  }

    # Plot
  p <- ggplot(counts_long, aes(x = .data[[class_col]], y = n, fill = state)) +
    geom_col(position = position_dodge(width = 0.85),
             width = 0.8, color = "black", linewidth = 0.25) +
    labs(
      title = title,
      x = x_lab,
      y = "Number of plot observations",
      fill = legend_title
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_x_discrete(
    breaks = c("Q", "R", "S", "T"),
    
    labels = function(x) {
      x <- ifelse(x == "Q", "Q - Wetlands", x)
      x <- ifelse(x == "R", "R - Grasslands", x)
      x <- ifelse(x == "S", "S - Shrublands", x)
      x <- ifelse(x == "T", "T - Forests", x)
      x
      }
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Optionally attach counts to the plot as an attribute for quick retrieval
  attr(p, "counts") <- counts

  # Return mode
  if (return == "plot") {
    return(p)
  } else if (return == "counts") {
    return(counts)
  } else {
    # both
    return(list(plot = p, counts = counts))
  }
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Session info
sessionInfo()

