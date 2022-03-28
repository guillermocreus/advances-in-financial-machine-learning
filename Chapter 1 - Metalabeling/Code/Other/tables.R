Sharpe_ratio <- function(ex, daily = FALSE) {
  if (!daily) {
    return(mean(ex)/sd(ex)*sqrt(12))
  }
  return(mean(ex)/sd(ex)*sqrt(252))
}

glabadanidis_strategy <- function(signal, signal_before, returns, rf, tc) {
  returns_MA <- returns
  returns_MA[] <- 0
  
  ind_mkt <- which(signal == 1)
  ind_rf <- which(signal <= 0)
  ind_tc <- which((signal == 1 & signal_before == -1) | (signal == -1 & signal_before == 1))
  
  if (length(ind_mkt) > 0) 
    returns_MA[ind_mkt] <- returns[ind_mkt]
  
  if(length(ind_rf) > 0)
    returns_MA[ind_rf] <- rf[ind_rf]
  
  if (length(ind_tc) > 0) 
    returns_MA[ind_tc] <- returns_MA[ind_tc] - tc
  
  return(returns_MA)
}

get_MA <- function(returns, rf, tc, window_length = 24) {
  daily <- TRUE
  if (periodicity(returns)$scale == "monthly")
    daily <- FALSE
  
  prices <- cumprod(1 + returns)
  MA <- apply(prices, 2, function(z) {return(SMA(z, n = window_length))})
  MA <- as.xts(MA, order.by = index(prices))
  
  # Signal: 1 if prices > MA, -1 if MA > prices
  signal <- sign(lag(prices - MA))
  before <- lag(signal)
  before[1, ] <- 0
  
  returns_MA <- returns
  returns_MA[, ] <- 0

  for (i in 1:ncol(signal)) {
    returns_MA[, i] <- glabadanidis_strategy(signal[, i], before[, i], returns[, i], rf, tc)
  }
  
  returns_MA <- returns_MA[-(1:window_length)]
  rf <- rf[-(1:window_length)]
  returns <- returns[-(1:window_length)]
  signal <- signal[-(1:window_length)]
  
  table <- table.AnnualizedReturns(returns_MA, Rf = rf,
                                   geometric = FALSE, digits = 6)
  
  SR <- apply((returns_MA - as.vector(rf)), 2, function(z) return(Sharpe_ratio(z, daily)))
  
  MA_24_table <- rbind(table[1:2,]*100, 
                       round(skewness(returns_MA), digits = 4), 
                       "SR" = round(SR, digits = 4))
  
  return(list("table" = t(MA_24_table), 
              "MA" = MA, 
              "returns" = returns_MA, 
              "prices" = cumprod(1 + returns_MA),
              "signal" = signal))
}

get_BH <- function(returns, rf, window_length = 24, daily = FALSE) {
  table <- table.AnnualizedReturns(returns[-(1:window_length),], 
                                   Rf = rf[-(1:window_length)],
                                   geometric = FALSE, 
                                   digits = 6)
  
  prices <- cumprod(1 + returns)
  
  sum_prices_before <- lag(cumsum(prices), k = window_length + 1)
  sum_prices_before[1:(window_length + 1),] <- 0
  
  MA <- (lag(cumsum(prices)) - sum_prices_before)/window_length
  MA <- MA[-(1:window_length),]
  
  ex_ret <- returns - as.vector(rf)
  ex_ret <- ex_ret[-(1:window_length),]
  SR <- apply(ex_ret, 2, function(z) return(Sharpe_ratio(z, daily)))
  
  
  buy_hold_table <- rbind(table[1:2,]*100, 
                          round(skewness(returns[-(1:window_length)]), digits = 4), 
                          "SR" = round(SR, digits = 4))
  
  
  return(list("table" = t(buy_hold_table), 
              "MA" = MA, 
              "returns" = returns, 
              "prices" = prices))
}

clean_returns <- function(returns, start, end, daily = FALSE) {
  N <- ncol(returns)
  if (!daily) returns$Date <- as.yearmon(as.character(returns$Date), "%Y%m")
  else returns$Date <- as.Date(as.character(returns$Date), "%Y%m%d")
  
  dates <- returns$Date
  returns <- as.xts(returns[, (N-9):N], order.by = returns$Date)
  returns <- returns[dates >= start & dates <= end]*1e-2
  return(returns)
}