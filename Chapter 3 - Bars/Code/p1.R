library(xts)  # Time series framework
library(ggplot2)  # Fancy plots
library(quantmod)  # Candle charts

dd <- read.table("spy.txt", header = FALSE, sep = "", dec = ".", stringsAsFactors = FALSE)
dd <- data.frame(apply(dd[-1, ], 2, as.numeric))
colnames(dd) <- c("Date", "Bin", "Volume", "Price")

sum(dd$Volume[1:26])

# Day: 2012-12-28
# Volume: 123,486,362
# Price: 132.75
# Actual volume: 2,426,680,000
# Actual price: 1,402.43


sum(dd$Volume[27:52])

# Day: 2012-12-31
# Volume: 218,373,347
# Price: 132.2830
# Actual volume: 3,204,330,000
# Actual price: 1,426.19

expandDate <- function(s) {
  # yyyymmdd
  year <- substr(s, 1, 4)
  month <- substr(s, 5, 6)
  day <- substr(s, 7, 8)
  
  return(paste(year, month, day, sep = "-"))
}

dates <- unlist(lapply(dd$Date, expandDate))

nBins <- 26
startTrading <- 9.5
endTrading <- 16

dd <- apply(dd[, 3:4], 2, as.numeric)

intra <- NULL

for (k in 1:(nrow(dd)/nBins)) {
  if (k == 1) {
    intra <- startTrading*60*60 + seq(as.POSIXct(dates[(k - 1)*nBins + 1]), 
                                      by = 60*60*(endTrading - startTrading)/(nBins - 1), 
                                      length.out = nBins)
  } else if (k > 1) {
    intra <- c(intra, 
               startTrading*60*60 + seq(as.POSIXct(dates[(k - 1)*nBins + 1]), 
                                        by = 60*60*(endTrading - startTrading)/(nBins - 1), 
                                        length.out = nBins))
  }
}

dd <- xts(dd, order.by = intra)
head(dd)

meanVolume <- 0
sum <- 0
for (k in 1:(nrow(dd)/nBins)) {
  sum <- sum + as.numeric(sum(dd$Volume[(k - 1)*nBins + 1:nBins]))
}

meanVolume <- sum/(nrow(dd)/nBins)
# 97,586,165  (Average daily volume)

getDailyVolumeBars <- function(prices, volume, delta = 3.7e6) {
  # Delta = 3.7e6 - Standard value
  # Objective: generate volume-sampled candles
    # Time: time stamp of the last time bar
    # Open
    # High
    # Low
    # Close
  
  times <- index(prices)[1]
  times <- times[-1]
  bars <- c()
  
  volAcc <- 0
  residualVol <- 0
  lastReset <- 1
  
  for (k in 1:length(prices)) {
    volAcc <- volAcc + as.numeric(volume[k])
    while (volAcc > delta) {
      # Open
      open <- as.numeric(prices[lastReset])
      
      # High
      high <- max(prices[lastReset:k])
      
      # Low
      low <- min(prices[lastReset:k])
      
      # Close
      close <- as.numeric(prices[k])
      
      bars <- rbind(bars, c(open, high, low, close))
      times <- c(times, index(prices)[k])
      
      lastReset <- k
      volAcc <- volAcc - delta
    }
  }

  colnames(bars) <- c("Open", "High", "Low", "Close")
  bars <- xts(bars, order.by = times)
  return(bars)
}

MRes <- getDailyVolumeBars(prices = dd$Price, volume = dd$Volume, delta = 4e6)
head(MRes)

df <- cbind(dd, dd)
colnames(df) <- c("Open", "High", "Low", "Close")
df[, ] <- NA
df[index(MRes), ] <- MRes
chartSeries(df[1:50, ])
chartSeries(df)
candleChart(df, multi.col = TRUE, theme = 'white') 

library(plotly)

dfComplete <- df
for (k in 1:nrow(dfComplete)) {
  if (k == 1) {
    dfComplete[k, ] <- rep(132, ncol(dfComplete))   
  } else if (is.na(dfComplete[k, 1])) {
    dfComplete[k, ] <- rep(dfComplete$Close[k - 1], ncol(dfComplete))
  }
}

dfAux <- data.frame(Bars = 1:nrow(MRes), coredata(MRes))[1:30, ]
fig <- dfAux %>% plot_ly(x = ~Bars, type = "candlestick",
                         open = ~Open, close = ~Close,
                         high = ~High, low = ~Low) 
fig <- fig %>% layout(title = "Basic Candlestick Chart")
fig

volRet <- na.omit(diff(log(MRes$Close)))

h <- hist(volRet, breaks = 100, density = 40,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(volRet), max(volRet), length = 400) 
yfit <- dnorm(xfit, mean = mean(volRet), sd = sd(volRet)) 
yfit <- yfit*diff(h$mids[1:2])*length(volRet) 

lines(xfit, yfit, col = "black", lwd = 1)
# format(index(dfComplete)[1], format = "%d")

# _____ IGNORE _____

plotTimeSeries <- function(tSeries, title = "", choose = NULL) {
  if(is.null(choose)) {
    choose <- 1:ncol(tSeries)
  }
  moltenDD <- fortify(tSeries[, choose], melt = TRUE)
  
  ggplot(moltenDD, aes(x = Index, y = Value, col = Series)) + 
    geom_line(show.legend = FALSE) + xlab(element_blank()) + ylab(element_blank()) +
    ggtitle(title)
}

GSPC <- getSymbols("^GSPC", from = "2010-01-01", to = "2015-12-31", auto.assign = FALSE)
prices <- Cl(GSPC)
R_daily <- diff(log(prices))[-1]
plot(R_daily, col = 'blue', lwd = 1, ylab = "log-return", main = "S&P 500 index")

h <- hist(as.vector(R_daily), breaks = 100, prob = TRUE, col = "lightgray", 
          xlab = "return", main = "Histogram of log-returns")
xfit <- seq(min(R_daily), max(R_daily), length = 100) 
yfit <- dnorm(xfit, mean = mean(R_daily), sd = 0.85*sd(R_daily))
lines(xfit, yfit, col = "blue", lwd = 2)

qqnorm(R_daily, col = "blue", main = "QQ plot of log-returns")
qqline(R_daily, lwd = 2)

acf(R_daily, main="Autocorrelation of log-returns")

y_t <- log(GSPC$GSPC.Adjusted)
colnames(y_t) <- "LogPrices"

r_t <- diff(y_t)[-1]
r_t <- to.weekly(r_t)$r_t.Close
colnames(r_t) <- "LogReturns"

plotTimeSeries(y_t, title = "S&P500 Index")
plotTimeSeries(r_t, title = "S&P500 Index")

# h <- hist(r_t, breaks = 50, density = 50,
#           col = "lightgray", xlab = "Accuracy", main = "Overall")
# xfit <- seq(min(r_t), max(r_t), length = 4000) 
# yfit <- dnorm(xfit, mean = mean(r_t), sd = sd(r_t))
# yfit <- yfit*diff(h$mids[1:2])*length(r_t)
# 
# lines(xfit, yfit, col = "black", lwd = 1)
# format(index(dfComplete)[1], format = "%d")

# __________