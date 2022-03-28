get_discrete_returns <- function(returns, enter, window_length, 
                                 max_time, tc, ptsl = c(2, 2)) {
  
  discrete_returns <- returns
  discrete_returns[] <- NaN
  
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
      
      discrete_returns[index(returns_window)[first_cross]] <- 
        returns_window[first_cross] - 2*tc
      
      last_day <- index(returns)[start_returns_window + first_cross - 1]
    }
  }
  discrete_returns <- na.omit(discrete_returns)
  
  return(discrete_returns)
}