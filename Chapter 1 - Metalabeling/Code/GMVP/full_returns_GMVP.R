get_full_returns <- function(returns, enter, window_length, 
                             max_time, tc, ptsl = c(2, 2), signal = NA,
                             stop_loss = 0.005) {
  
  keepOriginalBarriers <- is.na(signal[1])
  
  if (is.na(signal[1])) {
    signal <- enter
    signal[] <- 1
  }
  
  portfolio_returns <- returns
  portfolio_returns[] <- NaN
  
  realized_vol <- get_daily_vol(returns, window_length)*sqrt(window_length)
  
  last_day <- as.Date(index(enter)[1]) - 1
  
  for (i in 1:length(enter)) {
    today <- as.Date(index(enter)[i])
    signal_today <- as.numeric(signal[today])
    realized_vol_today <- as.numeric(lag(realized_vol)[today])
    
    if (enter[i] & today > last_day & length(realized_vol_today) != 0
        & !is.na(realized_vol_today)) {
      start_returns_window <- match(today, index(returns))
      end_returns_window <- min(length(returns), start_returns_window + max_time - 1)
      
      returns_window <- cumprod(1 + returns[start_returns_window:end_returns_window]) - 1
      upper_barrier <- realized_vol_today*ptsl[1]
      lower_barrier <- -realized_vol_today*ptsl[2]
      
      if (!keepOriginalBarriers) {
        if (signal_today == 1)
          lower_barrier <- -min(0.5*ptsl[2]*realized_vol_today, stop_loss)
        else if (signal_today == -1)
          upper_barrier <- min(0.5*ptsl[1]*realized_vol_today, stop_loss)
      }
      
      cross_up <- which(returns_window > upper_barrier)
      min_cross_up <- Inf
      if (length(cross_up) != 0) min_cross_up <- min(cross_up)
      
      cross_down <- which(returns_window < lower_barrier)
      min_cross_down <- Inf
      if (length(cross_down) != 0) min_cross_down <- min(cross_down)
      
      first_cross <- min(min_cross_up, min_cross_down)
      if (first_cross == Inf) first_cross <- length(returns_window)
      
      for (j in start_returns_window:(start_returns_window + first_cross - 1)) {
        # Whenever we open or close a position, we must pay for transaction costs
        if (j == start_returns_window | j == (start_returns_window + first_cross - 1)) 
          portfolio_returns[index(returns)[j]] <- signal_today*(returns[j]) - tc
        else
          portfolio_returns[index(returns)[j]] <- signal_today*(returns[j])
      }
      
      last_day <- index(returns)[start_returns_window + first_cross - 1]
    }
  }
  portfolio_returns <- na.omit(portfolio_returns)
  
  return(portfolio_returns)
}