get_RS <- function(returns) {
  mult1 <- returns
  mult2 <- c(1, cumprod(1 + returns)[-length(returns)])
  v0 <- mult1*mult2
  
  which_up <- returns > 0
  
  num <- sum(v0*which_up)
  den <- sum(-v0*(1 - which_up))
  
  return(as.numeric(num/den))
}

get_RSI <- function(returns, days) {
  RSI <- returns
  RSI[] <- NaN
  
  for (i in (days - 1):length(returns)) {
    RSI[i] <- 100 - 100/(1 + get_RS(returns[(i - days + 2):i]))
  }
  
  return(RSI)
}

get_acf <- function(returns, lookback, lag = 5) {
  
  acf <- returns
  acf[] <- NaN
  
  for (i in lookback:length(acf)) {
    v0 <- returns[(i - lookback + 1):(i - lag)]
    v1 <- returns[(lag + i - lookback + 1):i]
    
    num <- var(v0, v1)
    den <- sqrt(var(v0)*var(v1))
    
    acf[i] <- num/den
  }
  
  return(acf)
}

get_period_returns <- function(returns) {
  lookback <- 12
  if (periodicity(returns)$scale == "daily") lookback <- 5
  
  period_returns <- returns
  period_returns[] <- NaN
  
  for (i in lookback:nrow(returns)) {
    period_returns[i] <- tail(cumprod(1 + returns[(i - lookback + 1):i]), 1) - 1
  }
  return(period_returns)
}

get_individual_features <- function(returns, signal, MA) {
  prices_individual <- cumprod(1 + returns)
  log_MA_p <- log(MA/prices_individual)
  
  acf_14_lag1 <- get_acf(returns, lookback = 14, lag = 1)
  acf_14_lag5 <- get_acf(returns, lookback = 14, lag = 5)
  
  RSI_9 <- get_RSI(returns, 9)
  RSI_14 <- get_RSI(returns, 14)
  RSI_25 <- get_RSI(returns, 25)
  
  volatility_all <- get_rolling_volatility(returns)
  volatility_9 <- get_daily_vol(returns, window_length = 9)
  volatility_14 <- get_daily_vol(returns, window_length = 14)
  volatility_25 <- get_daily_vol(returns, window_length = 25)
  
  features <- cbind(lag(log_MA_p), lag(returns), lag(get_period_returns(returns)), 
                    signal, lag(RSI_9), lag(RSI_14), lag(RSI_25), lag(volatility_all), 
                    lag(volatility_9), lag(volatility_14), lag(volatility_25),
                    lag(acf_14_lag1), lag(acf_14_lag5))
  
  colnames(features) <- c("log_MA_p", "returns", "period_returns", "signal", 
                          "RSI_9", "RSI_14", "RSI_25", "volatility_natural", 
                          "volatility_9", "volatility_14", "volatility_25",
                          "acf_lag1", "acf_lag5")
  
  return(na.omit(features))
}

get_features <- function(returns, signal, MA) {
  features <- list()
  for (i in 1:ncol(returns)) {
    features[[i]] <- get_individual_features(returns[, i], signal[, i], MA[, i])
  }
  
  return(features)
}