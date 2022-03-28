get_bins <- function(returns, start_candidates, max_time, tc,
                     realized_vol, signal, ptsl = c(2, 2), stop_loss = 0.005) {
  
  start_position <- index(start_candidates)[which(start_candidates == 1)]
  start_position <- start_position[start_position >= index(signal)[1]]
  
  bins <- realized_returns <- returns
  bins[, ] <- realized_returns[, ] <- NaN
  
  exit <- c()
  
  for (i in 1:length(start_position)) {
    ind <- start_position[i]
    realized_vol_now <- lag(realized_vol)[ind]
    
    start_returns_window <- max(ind, index(returns)[1])
    start_returns_window <- match(start_returns_window, index(returns))
    
    end_returns_window <- nrow(returns)
    if (start_returns_window + max_time - 1 <= nrow(returns)) 
      end_returns_window <- start_returns_window + max_time - 1
    
    returns_window <- cumprod(1 + returns[start_returns_window:end_returns_window]) - 1
    
    upper_barrier <- ptsl[1]*realized_vol_now
    lower_barrier <- -ptsl[2]*realized_vol_now
    
    if (signal[ind] == 1) lower_barrier <- -min(0.5*ptsl[2]*realized_vol_now, stop_loss)
    else if (signal[ind] == -1) upper_barrier <- min(0.5*ptsl[1]*realized_vol_now, stop_loss)
    
    cross_up <- which(returns_window > upper_barrier)
    min_cross_up <- Inf
    if (length(cross_up) > 0) min_cross_up <- min(cross_up)
    
    cross_down <- which(returns_window < lower_barrier)
    min_cross_down <- Inf
    if (length(cross_down) > 0) min_cross_down <- min(cross_down)
    
    first_cross <- min(min_cross_up, min_cross_down)
    if (first_cross == Inf) first_cross <- length(returns_window)
    
    realized_returns[ind] <- as.numeric(returns_window[first_cross]) - 2*tc
    bins[ind] <- 0
    if (as.numeric(realized_returns[ind])*as.numeric(signal[ind]) > 0) 
      bins[ind] <- 1
    
    exit <- c(exit, as.numeric(index(returns_window)[first_cross]))
  }
  return(list("bins" = bins, 
              "realized_returns" = realized_returns,
              "exit" = exit))
}

ewmsd <- function(x, alpha, span) {
  n <- length(x)
  sapply(
    1:n,
    function(i, x, alpha) {
      if (i < span) return(NaN)
      y <- x[(i - span + 1):i]
      weights <- (1 - alpha)^((span - 1):0)
      ewma <- sum(weights * y) / sum(weights)
      bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
      ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
      return(ewmsd)
    },
    x = x,
    alpha = alpha
  )
}

get_rolling_volatility <- function(returns, lookback = NULL) {
  if (is.null(lookback)) {
    if (periodicity(returns)$scale == "daily") lookback <- 21 # 1 month
    else lookback <- 12 # 1 year
  }
  factor <- sqrt(lookback)

  # HARDCODE
  factor <- 1
  
  realized_vol <- as.xts(apply(log(1 + returns), 2, ewmsd, alpha = 0.96, span = 12), 
                         order.by = index(returns))*factor
  
  return(na.omit(realized_vol))
}

get_start_position <- function(returns, threshold = 1, realized_vol) {
  log_returns <- log(1 + returns)
  
  all_NaN <- as.xts(matrix(rep(NaN, nrow(realized_vol)*ncol(realized_vol)), 
                           ncol = ncol(realized_vol), 
                           nrow = nrow(realized_vol)), 
                    order.by = index(realized_vol))
  
  cusum_pos <- cusum_neg <- events <- all_NaN
  
  ind_0 <- index(realized_vol)[1]
  
  cusum_pos[ind_0, ] <- pmax(0, as.vector(log_returns[ind_0, ]))
  cusum_neg[ind_0, ] <- pmin(0, as.vector(log_returns[ind_0, ]))
  
  start_candidates <- returns
  start_candidates[, ] <- 0
  
  indices <- index(realized_vol)[-1]
  
  for (i in 1:length(indices)) {
    ind_before <- ind_0
    if (i > 1) ind_before <- indices[i - 1]
    ind <- indices[i]
    
    reset_pos <- which(cusum_pos[ind_before, ] >= threshold*realized_vol[ind_before, ])
    reset_neg <- which(cusum_neg[ind_before, ] <= -threshold*realized_vol[ind_before, ])
    
    cusum_pos[ind_before, reset_pos] <- 0
    cusum_neg[ind_before, reset_neg] <- 0
    
    events_to_add <- unique(c(reset_pos, reset_neg))
    
    # WARNING: Since you enter the following day ==> we append "ind" and NOT "ind_before"
    if (length(events_to_add) > 0) start_candidates[ind, events_to_add] <- 1
    
    cusum_pos[ind, ] <- pmax(0, as.vector(cusum_pos[ind_before, ]) + as.vector(log_returns[ind, ]))
    cusum_neg[ind, ] <- pmin(0, as.vector(cusum_neg[ind_before, ]) + as.vector(log_returns[ind, ]))
  }
  
  return(start_candidates)
}

get_labels <- function(returns, max_time, tc, threshold = 1, ptsl = c(1, 1), signal) {
  
  realized_vol <- get_daily_vol(returns, 20)
  
  start_candidates <- get_start_position(returns = returns, 
                                         threshold = threshold, 
                                         realized_vol = realized_vol)
  
  labels <- realized_returns <- exit <- list()
  
  for (i in 1:ncol(returns)) {
     call <- get_bins(returns = returns[, i], 
                      start_candidates = start_candidates[, i], 
                      max_time = max_time, 
                      tc = tc,
                      realized_vol = realized_vol[, i], 
                      signal = signal[, i],
                      ptsl = ptsl)
     
    labels[[i]] <- call$bins
    realized_returns[[i]] <- call$realized_returns
    realized_returns[[i]] <- realized_returns[[i]][index(labels[[i]])]
    exit[[i]] <- as.xts(call$exit, order.by = index(na.omit(realized_returns[[i]])))
  }
  
  return(list("labels" = labels, 
              "realized_returns" = realized_returns,
              "exit" = exit))
}