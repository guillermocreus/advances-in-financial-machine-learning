enter_decision_2_models <- function(pred_m1, pred_m2, thr_m1, thr_m2) {
  pred_m1 <- pred_m1[index(pred_m2)]
  pred_m2 <- pred_m2[index(pred_m1)]
  
  enter_decision <- (pred_m1 > thr_m1) & (pred_m2 > thr_m2)
  
  return(enter_decision)
}


Sharpe_Ratio_from_predictions <- function(returns, enter, window_length, 
                                          max_time, tc, threshold =  -1,
                                          ptsl = c(2, 2), verbose = TRUE) {
  
  portfolio_returns <- returns
  portfolio_returns[] <- NaN
  
  realized_vol <- get_daily_vol(returns, window_length)*sqrt(window_length)
  
  last_day <- as.Date(index(enter)[1]) - 1
  
  for (i in 1:length(enter)) {
    today <- as.Date(index(enter)[i])
    realized_vol_today <- as.numeric(lag(realized_vol)[today])
    
    if (enter[i] & today > last_day & length(realized_vol_today) != 0
        & !is.na(realized_vol_today)) {
      start_returns_window <- match(today, index(returns))
      end_returns_window <- min(length(returns), start_returns_window + max_time - 1)
      
      returns_window <- cumprod(1 + returns[start_returns_window:end_returns_window]) - 1
      upper_barrier <- realized_vol_today*ptsl[1]
      lower_barrier <- -realized_vol_today*ptsl[2]
      
      cross_up <- which(returns_window > upper_barrier)
      min_cross_up <- Inf
      if (!is.null(cross_up)) min_cross_up <- min(cross_up)
      
      cross_down <- which(returns_window < lower_barrier)
      min_cross_down <- Inf
      if (!is.null(cross_down)) min_cross_down <- min(cross_down)
      
      first_cross <- min(min_cross_up, min_cross_down)
      if (first_cross == Inf) first_cross <- length(returns_window)
      
      for (j in start_returns_window:(start_returns_window + first_cross - 1)) {
        # Whenever we open or close a position, we must pay for transaction costs
        if (j == start_returns_window | j == (start_returns_window + first_cross - 1)) 
          portfolio_returns[index(returns)[j]] <- returns[j] - tc
        else
          portfolio_returns[index(returns)[j]] <- returns[j]
      }
      
      last_day <- index(returns)[start_returns_window + first_cross - 1]
    }
  }
  portfolio_returns <- na.omit(portfolio_returns)
  number_opportunities <- length(portfolio_returns)
  number_years <- as.numeric(tail(index(portfolio_returns), 1) - index(portfolio_returns)[1])/365
  
  SR <- mean(portfolio_returns)/sd(portfolio_returns)
  SR <- SR*sqrt(number_opportunities/number_years)
  
  if (verbose) {
    print(paste("Threshold =", round(threshold, 3), 
                "SR =", round(SR, 3), 
                "Wealth =", round(tail(cumprod(1 + portfolio_returns), 1), 3)))
  }  
  return(SR)
}