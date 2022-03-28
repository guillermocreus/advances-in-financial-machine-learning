get_features_primary_model <- function(returns, window_length, threshold) {

  prices <- cumprod(1 + returns)
  MA <- SMA(prices, n = window_length)

  realized_vol <- get_daily_vol(returns, window_length)*sqrt(window_length)
  log_returns <- log(1 + returns)
  
  cusum_pos <- cusum_neg <- as.xts(rep(NaN, length(realized_vol)),
                                   order.by = index(realized_vol))
  
  reset_cusum_pos <- reset_cusum_neg <- as.xts(rep(0, length(realized_vol)),
                                               order.by = index(realized_vol))
  
  ind_0 <- index(realized_vol)[1]
  cusum_pos[ind_0] <- max(0, as.vector(log_returns[ind_0]))
  cusum_neg[ind_0] <- min(0, as.vector(log_returns[ind_0]))
  
  for (i in 2:length(realized_vol)) {
    ind <- index(realized_vol)[i]
    ind_before <- index(realized_vol)[i - 1]
    
    reset_pos <- (cusum_pos[ind_before] >= threshold*realized_vol[ind_before])
    reset_neg <- (cusum_neg[ind_before] <= -threshold*realized_vol[ind_before])

    if (reset_pos) {
      cusum_pos[ind_before] <- 0
      reset_cusum_pos[ind_before] <- 1
    }
    if (reset_neg) {
      cusum_neg[ind_before] <- 0
      reset_cusum_neg[ind_before] <- 1
    }
    
    cusum_pos[ind] <- 
      max(0, as.vector(cusum_pos[ind_before]) + as.vector(log_returns[ind]))
    
    cusum_neg[ind] <- 
      min(0, as.vector(cusum_neg[ind_before]) + as.vector(log_returns[ind]))
  }
  
  features_primary_model <- cbind(lag(log(MA/prices)),
                                  lag(cusum_pos/realized_vol),
                                  lag(cusum_neg/realized_vol),
                                  lag(reset_cusum_pos),
                                  lag(reset_cusum_neg),
                                  lag(get_rolling_volatility(returns)))
  
  colnames(features_primary_model) <- c("log_MA_p", "cusum_pos", "cusum_neg",
                                        "reset_cusum_pos", "reset_cusum_neg",
                                        "volatility")
  index(features_primary_model) <- as.Date(index(features_primary_model))
  return(na.omit(features_primary_model))
}

get_labels_primary_model <- function(returns, features, 
                                     window_length, max_time) {
  
  realized_vol <- get_daily_vol(returns, window_length)*sqrt(window_length)
  
  labels <- realized_returns <- features[, 1]
  labels[] <- realized_returns[] <- NaN
  colnames(labels) <- "label"
  colnames(realized_returns) <- "realized_returns"
  
  for (i in 1:nrow(features)) {
    if (!is.na(match(index(features)[i], index(lag(realized_vol))))) {
      realized_vol_now <- as.numeric(lag(realized_vol)[index(features)[i]])
      
      start_returns_window <- match(index(features)[i], index(returns))
      end_returns_window <- min(length(returns), start_returns_window + max_time - 1)
      
      returns_window <- cumprod(1 + returns[start_returns_window:end_returns_window]) - 1
      upper_barrier <- realized_vol_now
      lower_barrier <- -realized_vol_now
      
      cross_up <- which(returns_window > upper_barrier)
      min_cross_up <- Inf
      if (length(cross_up) > 0) min_cross_up <- min(cross_up)
      
      cross_down <- which(returns_window < lower_barrier)
      min_cross_down <- Inf
      if (length(cross_down) > 0) min_cross_down <- min(cross_down)
      
      first_cross <- min(min_cross_up, min_cross_down)
      if (first_cross == Inf) first_cross <- length(returns_window)
      
      labels[i] <- 1
      if (returns_window[first_cross] <= 0) labels[i] <- 0
      
      realized_returns[i] <- returns_window[first_cross]
    }
  }
  
  return(list("labels" = labels,
              "realized_returns" = realized_returns))
}