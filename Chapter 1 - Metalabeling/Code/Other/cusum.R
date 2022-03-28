# FALTA:
#   - Min. return
#   - Check prices
#   - EWM + Volatility (factor if it's not log returns!)

get_complete_returns <- function(returns, filtered_returns, rf, window_length, signal) {
  
  rf. <- rf[-c(1:window_length)]
  complete_returns <- returns[-c(1:window_length)]
  complete_returns[] <- -5
  
  complete_returns[index(filtered_returns)] <- filtered_returns#*signal[index(filtered_returns)]
  complete_returns[which(complete_returns == -5)] <- rf.[which(complete_returns == -5)]
  
  return(complete_returns)
}

get_touch_individual <- function(returns, 
                                 start_candidates,
                                 signal = NULL,
                                 holding_time, 
                                 realized_vol, 
                                 tc = (5*0.01)*0.01, 
                                 ptsl = c(2, 2),
                                 stop_loss = 0.005) {
  
  if (is.null(signal)) {
    signal <- returns
    signal[, ] <- 1
  }
  
  last <- index(start_candidates)[1] - 1
  filtered_returns <- returns
  real_returns <- start_candidates
  real_returns[] <- filtered_returns[] <- NaN
  
  for (k in 1:length(start_candidates)) {
    now <- index(start_candidates)[k]
    signal_now <- as.numeric(signal[now])
    if (start_candidates[k] == 1 & last < now) {
      realized_vol_ <- lag(realized_vol)[now]
      
      start_returns_window <- max(now, index(returns)[1])
      end_returns_window <- index(returns)[nrow(returns)]
      ind_now_ret <- match(now, index(returns))
      if ((ind_now_ret + holding_time - 1) <= nrow(returns))
        end_returns_window <- index(returns)[ind_now_ret + holding_time - 1]
      
      start_returns_window <- which(index(returns) == start_returns_window)
      end_returns_window <- which(index(returns) == end_returns_window)
      
      returns_window <- cumprod(1 + returns[start_returns_window:end_returns_window]) - 1
      
      upper_barrier <- as.numeric(ptsl[1]*realized_vol_)
      lower_barrier <- -as.numeric(ptsl[2]*realized_vol_)
      
      if (signal_now == 1)
        lower_barrier <- -min(0.5*ptsl[2]*realized_vol_, stop_loss)
      else if (signal_now == -1)
        upper_barrier <- min(0.5*ptsl[1]*realized_vol_, stop_loss)
      
      cross_up <- which(returns_window > upper_barrier)
      min_cross_up <- Inf
      if (!is.null(cross_up)) min_cross_up <- min(cross_up)
      
      cross_down <- which(returns_window < lower_barrier)
      min_cross_down <- Inf
      if (!is.null(cross_down)) min_cross_down <- min(cross_down)
      
      first_cross <- min(min_cross_up, min_cross_down)
      if (first_cross == Inf) first_cross <- length(returns_window)
    
      real_returns[now] <- as.numeric(returns_window[first_cross])*signal_now - 2*tc
      
      ret_2_add <- 
        returns[start_returns_window:(start_returns_window + first_cross - 1)]*signal_now
      ret_2_add[1] <- ret_2_add[1] - tc
      ret_2_add[length(ret_2_add)] <- ret_2_add[length(ret_2_add)] - tc
      
      filtered_returns[index(ret_2_add)] <- ret_2_add
      
      last <- index(returns)[start_returns_window + first_cross - 1]
      
    }
  }
  
  return(list("real_returns" = na.omit(real_returns),
              "filtered_returns" = na.omit(filtered_returns)))
}

get_daily_vol <- function(returns, window_length) {
  realized_vol <- as.xts(apply(returns, 2, runSD, n = window_length), 
                         order.by = index(returns))
  
  return(na.omit(realized_vol))
  
  # Another way:
  log_returns <- log(1 + returns)
  realized_vol <- as.xts(apply(log_returns, 2, runSD, n = as.integer(1.5*window_length)), 
                         order.by = index(log_returns))*factor
  
  return(na.omit(realized_vol))
}

get_touch <- function(returns, holding_time,
                      window_length, threshold, ptsl = c(1, 1), 
                      rf, signal) {
  
  realized_vol <- get_daily_vol(returns, window_length)
  
  start_candidates <- get_start_position(returns, window_length = window_length, 
                                         threshold = threshold, realized_vol)
  
  real_returns <- list()
  complete_returns <- returns[-c(1:window_length), ]
  complete_returns[, ] <- 0
   
  for (i in 1:ncol(returns)) {
    aux_call <- get_touch_individual(returns[, i], start_candidates[[i]], 
                                     holding_time = holding_time, 
                                     realized_vol[, i])
    
    complete_returns[, i] <- as.vector(get_complete_returns(returns = returns[, i], 
                                                            filtered_returns = aux_call$filtered_returns,
                                                            rf = rf, window_length = window_length,
                                                            signal = signal[, i]))
    
    real_returns[[i]] <- as.xts(aux_call$real_returns, order.by = aux_call$real_start)
    
  }
  return(list("real_returns" = real_returns,
              "complete_returns" = complete_returns,
              "prices" = cumprod(1 + complete_returns)))
}