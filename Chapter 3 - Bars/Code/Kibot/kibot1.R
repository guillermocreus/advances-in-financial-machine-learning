library(xts)  # Time series framework
library(quantmod)  # Candle charts
library(numbers)  # Modulus
library(ks)  # KDE plot
library(tseries)  # Jarque-Bera statistic
library(ggplot2)  # Fancy plots
theme_update(text = element_text(size=20))  # Globally change the font size
Sys.setenv("LANGUAGE"="En")  # Make English the language of the environment
Sys.setlocale("LC_ALL", "English")

df <- read.csv("~/Desktop/IVE_tickbidask.txt", header = FALSE, 
               stringsAsFactors = FALSE)

df <- xts(df[, 3:ncol(df)],
          order.by = as.POSIXct(paste(df[, 1], df[, 2]),
                                format = "%m/%d/%Y %H:%M:%S"))
colnames(df) <- c("price", "bid", "ask", "size")
df <- cbind(df, df$size, df$size*df$price)
colnames(df) <- c(colnames(df)[1:4], "volume", "dollarVolume")

df <- df["2013-01-01/2014-01-01"]

madOutlier <- function(x, thresh = 3) {
  return((abs(x - median(x)) / mad(x)) > thresh)
}

df <- df[!madOutlier(df$price), ]

tickBars <- function(df, m = 90) {
  idxBars <- df[, 1]
  idxBars[] <- 1:nrow(df)
  idxBars <- (mod(idxBars, m) == 0)
  return(idxBars)
}

idxTickBars <- tickBars(df, m = 60)
tickDf <- df[which(idxTickBars == 1), ]
head(tickDf)
plot(tickDf$price[1:100])

volumeBars <- function(df, m = 16e3) {
  sumVol <- 0
  idxBars <- df[, 1]
  idxBars[] <- 0
  for (k in 1:nrow(df)) {
    if (mod(k, 5000) == 0) print(paste(100*k/nrow(df), "%"))
    sumVol <- sumVol + as.numeric(df$volume[k])
    if (sumVol >= m) {
      sumVol <- 0
      idxBars[k] <- 1
    }
  }
  return(idxBars)
}

idxVolumeBars <- volumeBars(df, m = 8e3)
volumeDf <- df[which(idxVolumeBars == 1), ]
head(volumeDf)
plot(volumeDf$price[1:100])

dollarBars <- function(df, m = 1.7e6) {
  sumDollarVol <- 0
  idxBars <- df[, 1]
  idxBars[] <- 0
  for (k in 1:nrow(df)) {
    if (mod(k, 5000) == 0) print(paste(100*k/nrow(df), "%"))
    sumDollarVol <- sumDollarVol + as.numeric(df$dollarVolume[k])
    if (sumDollarVol >= m) {
      sumDollarVol <- 0
      idxBars[k] <- 1
    }
  }
  return(idxBars)
}

idxDollarBars <- dollarBars(df, m = 800e3)
dollarDf <- df[which(idxDollarBars == 1), ]
head(dollarDf)
plot(dollarDf$price[1:100])

timeBars <- function(df, m = 10) {
  ep <- endpoints(index(df), on = "minutes", k = m)
  
  idxBars <- df[, 1]
  idxBars[] <- 0
  idxBars[ep[-1]] <- 1
  return(idxBars)
}

idxTimeBars <- timeBars(df, m = 8)
timeDf <- df[which(idxTimeBars == 1), ]
head(timeDf)
plot(timeDf$price[1:100])

plot(tickDf$price)
plot(volumeDf$price)
plot(dollarDf$price)
plot(timeDf$price)

tickReturns <- diff(log(tickDf$price))[-1]
volumeReturns <- diff(log(volumeDf$price))[-1]
dollarReturns <- diff(log(dollarDf$price))[-1]
timeReturns <- diff(log(timeDf$price))[-1]

# _____ KDE Plots _____

h1 <- kde(as.vector((volumeReturns - mean(volumeReturns))/sd(volumeReturns)))
plot(h1, col = "#00FF00", lwd = 2, xlim = c(-5, 5), 
     main = "Pdf of log-returns", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, 
     cex.sub = 1.5)

h2 <- kde(as.vector((dollarReturns - mean(dollarReturns))/sd(dollarReturns)))
plot(h2, col = "#0000FF", lwd = 2, xlim = c(-5, 5), add = TRUE)

h3 <- kde(as.vector((tickReturns - mean(tickReturns))/sd(tickReturns)))
plot(h3, col = "#FF0000", lwd = 2, xlim = c(-5, 5), add = TRUE)

h4 <- kde(as.vector((timeReturns - mean(timeReturns))/sd(timeReturns)))
plot(h4, col = "#8B008B", lwd = 2, xlim = c(-5, 5), add = TRUE)

xfit <- seq(from = -5, to = 5, length.out = 200)
yfit <- dnorm(xfit, sd = 0.5)
lines(xfit, yfit, col = "black", lwd = 2)

legend("topleft", c("Volume", "Dollar", "Tick", "Time", "N(mu = 0, sd = 0.5)"),
       text.col = c("#00FF00", "#0000FF", "#FF0000", "#8B008B", "black"), 
       cex = 0.8, pt.cex = 2, pt.lwd = 1)

# _______________

# _____ Tick bars _____
h <- hist(tickReturns, breaks = 200, prob = TRUE, col = "lightgray", 
          xlab = "return", main = "Tick bars returns")
xfit <- seq(min(tickReturns), max(tickReturns), length = 100) 
yfit <- dnorm(xfit, mean = mean(tickReturns), sd = sd(tickReturns))

qqnorm(tickReturns, col = "#FF0000", main = "Tick bars returns")
qqline(tickReturns, lwd = 2)
# _______________

# _____ Volume bars _____
h <- hist(volumeReturns, breaks = 200, prob = TRUE, col = "lightgray", 
          xlab = "return", main = "Volume bars returns")
xfit <- seq(min(volumeReturns), max(volumeReturns), length = 100) 
yfit <- dnorm(xfit, mean = mean(volumeReturns), sd = sd(volumeReturns))
lines(xfit, yfit, col = "blue", lwd = 2)
lines(xfit, yfit, col = "blue", lwd = 2)

qqnorm(volumeReturns, col = "#00FF00", main = "Volume bars returns")
qqline(volumeReturns, lwd = 2)
# _______________

# _____ Dollar bars _____
h <- hist(dollarReturns, breaks = 200, prob = TRUE, col = "lightgray", 
          xlab = "return", main = "Dollar bars returns")
xfit <- seq(min(dollarReturns), max(dollarReturns), length = 100) 
yfit <- dnorm(xfit, mean = mean(dollarReturns), sd = sd(dollarReturns))
lines(xfit, yfit, col = "blue", lwd = 2)

qqnorm(dollarReturns, col = "#0000FF", main = "Dollar bars returns")
qqline(dollarReturns, lwd = 2)
# _______________

# _____ Time bars _____
h <- hist(timeReturns, breaks = 200, prob = TRUE, col = "lightgray", 
          xlab = "return", main = "Time bars returns")
xfit <- seq(min(timeReturns), max(timeReturns), length = 100) 
yfit <- dnorm(xfit, mean = mean(timeReturns), sd = sd(timeReturns))
lines(xfit, yfit, col = "blue", lwd = 2)

qqnorm(timeReturns, col = "#8B008B", main = "Time bars returns")
qqline(timeReturns, lwd = 2)
# _______________

nTst <- 150

nTick <- length(tickReturns) - nTst
nVolume <- length(volumeReturns) - nTst
nDollar <- length(dollarReturns) - nTst
nTime <- length(timeReturns) - nTst

# nTick <- as.integer(0.8*length(tickReturns))
# nVolume <- as.integer(0.8*length(volumeReturns))
# nDollar <- as.integer(0.8*length(dollarReturns))
# nTime <- as.integer(0.8*length(timeReturns))

predTick <- c()
for (k in nTick:(length(tickReturns) - 1)) {
  tickAR <- ar(coredata(tickReturns)[1:k])
  predTick <- c(predTick, as.numeric(predict(tickAR, n.ahead = 1)$pred))
}

predVolume <- c()
for (k in nVolume:(length(volumeReturns) - 1)) {
  volumeAR <- ar(coredata(volumeReturns)[1:k])
  predVolume <- c(predVolume, as.numeric(predict(volumeAR, n.ahead = 1)$pred))
}

predDollar <- c()
for (k in nDollar:(length(dollarReturns) - 1)) {
  dollarAR <- ar(coredata(dollarReturns)[1:k])
  print(dollarAR$order)
  predDollar <- c(predDollar, as.numeric(predict(dollarAR, n.ahead = 1)$pred))
}

predTime <- c()
for (k in nTime:(length(timeReturns) - 1)) {
  timeAR <- ar(coredata(timeReturns)[1:k])
  print(timeAR$order)
  predTime <- c(predTime, as.numeric(predict(timeAR, n.ahead = 1)$pred))
}

tickRMSE <- sqrt(
  sum((predTick - coredata(tickReturns)[-c(1:nTick)])^2)/length(predTick)
)

volumeRMSE <- sqrt(
  sum((predVolume - coredata(volumeReturns)[-c(1:nVolume)])^2)/length(predVolume)
)

dollarRMSE <- sqrt(
  sum((predDollar - coredata(dollarReturns)[-c(1:nDollar)])^2)/length(predDollar)
)

timeRMSE <- sqrt(
  sum((predTime - coredata(timeReturns)[-c(1:nTime)])^2)/length(predTime)
)

eTick <- predTick - coredata(tickReturns)[-c(1:nTick)]
eVolume <- predVolume - coredata(volumeReturns)[-c(1:nVolume)]
eDollar <- predDollar - coredata(dollarReturns)[-c(1:nDollar)]
eTime <- predTime - coredata(timeReturns)[-c(1:nTime)]

tickMAD <- median(abs(eTick - median(eTick)))
volumeMAD <- median(abs(eVolume - median(eVolume)))
dollarMAD <- median(abs(eDollar - median(eDollar)))
timeMAD <- median(abs(eTime - median(eTime)))

eTick <- predTick - coredata(tickReturns)[-c(1:nTick)]
eVolume <- predVolume - coredata(volumeReturns)[-c(1:nVolume)]
eDollar <- predDollar - coredata(dollarReturns)[-c(1:nDollar)]
eTime <- predTime - coredata(timeReturns)[-c(1:nTime)]

tickMAPE <- abs((eTick)/coredata(tickReturns)[-c(1:nTick)])/length(predTick)
tickMAPE[which(is.infinite(tickMAPE))] <- predTick[which(is.infinite(tickMAPE))]
tickMAPE <- sum(tickMAPE)

volumeMAPE <- abs((eVolume)/coredata(volumeReturns)[-c(1:nVolume)])/length(predVolume)
volumeMAPE[which(is.infinite(volumeMAPE))] <- predVolume[which(is.infinite(volumeMAPE))]
volumeMAPE <- sum(volumeMAPE)

dollarMAPE <- abs((eDollar)/coredata(dollarReturns)[-c(1:nDollar)])/length(predDollar)
dollarMAPE[which(is.infinite(dollarMAPE))] <- predDollar[which(is.infinite(dollarMAPE))]
dollarMAPE <- sum(dollarMAPE)

timeMAPE <- abs((eTime)/coredata(timeReturns)[-c(1:nTime)])/length(predTime)
timeMAPE[which(is.infinite(timeMAPE))] <- predTime[which(is.infinite(timeMAPE))]
timeMAPE <- sum(timeMAPE)

print(c("tickRMSE" = tickRMSE,
        "volumeRMSE" = volumeRMSE,
        "dollarRMSE" = dollarRMSE,
        "timeRMSE" = timeRMSE))

print(c("tickMAPE" = tickMAPE,
        "volumeMAPE" = volumeMAPE,
        "dollarMAPE" = dollarMAPE,
        "timeMAPE" = timeMAPE))

print(c("tickMAD" = tickMAD,
        "volumeMAD" = volumeMAD,
        "dollarMAD" = dollarMAD,
        "timeMAD" = timeMAD))

# save.image(file = "~/Dropbox/Otros/kibot.RData")

# _____ Sampling points _____

days <- c("2013-03-13", "2013-07-10", "2013-08-21")
for (k in 1:length(days)) {
  regularDay <- df[days[k]]
  tickDay <- tickDf[days[k]]
  volumeDay <- volumeDf[days[k]]
  dollarDay <- dollarDf[days[k]]
  timeDay <- timeDf[days[k]]
  
  print(nrow(tickDay))
  print(nrow(volumeDay))
  print(nrow(dollarDay))
  print(nrow(timeDay))
  cat("\n")
  
  allDay <- cbind(regularDay$price, tickDay$price, volumeDay$price, 
                  dollarDay$price, timeDay$price)
  colnames(allDay) <- c("Regular", "Tick", "Volume", "Dollar", "Time")
  
  moltenDDRegular <- fortify(allDay[, 1], melt = TRUE)
  moltenDD <- fortify(allDay[, 2:ncol(allDay)], melt = TRUE)
  colnames(moltenDD)[2] <- "Bars"
  
  graphStart <- as.POSIXct(paste(days[k], "8:00:00"),
                           format = "%m/%d/%Y %H:%M:%S")
  
  graphEnd <- as.POSIXct(paste(days[k], "16:00:00"),
                         format = "%m/%d/%Y %H:%M:%S")
  
  g <- ggplot(moltenDDRegular, aes(x = Index, y = Value)) + 
    geom_line(colour = "grey25", size = 1) + xlab(element_blank()) + ylab(element_blank()) +
    geom_point(data = moltenDD, aes(x = Index, y = Value, col = Bars, shape = Bars),
               size = 4.5) +
    scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#8B008B")) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    scale_x_datetime(date_breaks = "1 hour", date_minor_breaks = "1 hour", 
                     limits = c(graphStart, tail(index(allDay), 1)), 
                     labels = function(x) paste(format(x, "%H:00"))) +
    ggtitle(days[k])
  
  print(g)
}

# _____ Regular zoom _____

regularDay <- df[days[2]]
graphStart <- as.POSIXct(paste(days[2], "8:00:00"),
                         format = "%m/%d/%Y %H:%M:%S")
moltendd <- fortify(regularDay$price, melt = TRUE)
colnames(moltendd) <- c("Index", "Series", "Price")

ggplot(moltendd, aes(x = Index, y = Price)) + geom_line(colour = "black") +
  scale_x_datetime(date_breaks = "1 hour", date_minor_breaks = "1 hour", 
                   limits = c(graphStart, tail(index(regularDay), 1)), 
                   labels = function(x) paste(format(x, "%H:00"))) +
  ggtitle(days[2])

# _______________

# _____ Tick zoom _____

tickDay <- tickDf[days[2]]
dd <- data.frame(cbind(1:nrow(tickDay), tickDay$price))
rownames(dd) <- NULL
colnames(dd) <- c("Bars", "Price")

ggplot(dd, aes(x = Bars, y = Price)) + geom_line(colour = "#FF0000") +
  ggtitle(paste("Tick Bars of", days[2]))

# _______________

# _____ Volume zoom _____

volumeDay <- volumeDf[days[2]]
dd <- data.frame(cbind(1:nrow(volumeDay), volumeDay$price))
rownames(dd) <- NULL
colnames(dd) <- c("Bars", "Price")

ggplot(dd, aes(x = Bars, y = Price)) + geom_line(colour = "#00FF00") +
  ggtitle(paste("Volume Bars of", days[2]))

# _______________

# _____ Dollar zoom _____

dollarDay <- dollarDf[days[2]]
dd <- data.frame(cbind(1:nrow(dollarDay), dollarDay$price))
rownames(dd) <- NULL
colnames(dd) <- c("Bars", "Price")

ggplot(dd, aes(x = Bars, y = Price)) + geom_line(colour = "#0000FF") +
  ggtitle(paste("Dollar Bars of", days[2]))

# _______________

# _____ Time zoom _____

timeDay <- timeDf[days[2]]
dd <- data.frame(cbind(1:nrow(timeDay), timeDay$price))
rownames(dd) <- NULL
colnames(dd) <- c("Bars", "Price")

ggplot(dd, aes(x = Bars, y = Price)) + geom_line(colour = "#8B008B") +
  ggtitle(paste("Time Bars of", days[2]))

# _______________

# _____ Jarque-Bera Test _____

tickJB <- jarque.bera.test(tickReturns)$statistic
volumeJB <- jarque.bera.test(volumeReturns)$statistic
dollarJB <- jarque.bera.test(dollarReturns)$statistic
timeJB <- jarque.bera.test(timeReturns)$statistic

print(c("tickJB" = tickJB,
        "volumeJB" = volumeJB,
        "dollarJB" = dollarJB,
        "timeJB" = timeJB))

# Everything is 0
# print(c("tickJB" = tickJB$p.value,
#         "volumeJB" = volumeJB$p.value,
#         "dollarJB" = dollarJB$p.value))

set.seed(1998)
x <- rnorm(1000)
nullJB <- jarque.bera.test(x)
print(c(nullJB$statistic, nullJB$p.value))

# _______________

# _____ Autocorrelation _____

tickAutocorr <- acf(as.vector(tickReturns), lag.max = 1, plot = FALSE)
volumeAutocorr <- acf(as.vector(volumeReturns), lag.max = 1, plot = FALSE)
dollarAutocorr <- acf(as.vector(dollarReturns), lag.max = 1, plot = FALSE)
timeAutocorr <- acf(as.vector(timeReturns), lag.max = 1, plot = FALSE)

print(c("Tick autocorrelation" = tickAutocorr$acf[2],
        "Volume autocorrelation" = volumeAutocorr$acf[2],
        "Dollar autocorrelation" = dollarAutocorr$acf[2],
        "Time autocorrelation" = timeAutocorr$acf[2]))

acf(as.vector(tickReturns), lag.max = 5)
acf(as.vector(volumeReturns), lag.max = 5)
acf(as.vector(dollarReturns), lag.max = 5)
acf(as.vector(timeReturns), lag.max = 5)

# _______________

# _____ Variance of Variance of monthly returns _____

tickVars <- volumeVars <- dollarVars <- timeVars <- c()
for (k in 1:12) {
  thisMonth <- which(as.numeric(format(index(tickReturns), "%m")) == k)
  tickVars <- c(tickVars, as.numeric(var(tickReturns[thisMonth])))
  
  thisMonth <- which(as.numeric(format(index(volumeReturns), "%m")) == k)
  volumeVars <- c(volumeVars, as.numeric(var(volumeReturns[thisMonth])))

  thisMonth <- which(as.numeric(format(index(dollarReturns), "%m")) == k)
  dollarVars <- c(dollarVars, as.numeric(var(dollarReturns[thisMonth])))

  thisMonth <- which(as.numeric(format(index(timeReturns), "%m")) == k)
  timeVars <- c(timeVars, as.numeric(var(timeReturns[thisMonth])))
}

tickVar <- var(tickVars)
volumeVar <- var(volumeVars)
dollarVar <- var(dollarVars)
timeVar <- var(timeVars)

print(c("Tick Variance" = tickVar,
        "Volume Variance" = volumeVar,
        "Dollar Variance" = dollarVar,
        "Time Variance" = timeVar))

# _______________

# _____ Week _____

epTick <- endpoints(index(tickReturns), on = "weeks")
epVolume <- endpoints(index(volumeReturns), on = "weeks")
epDollar <- endpoints(index(dollarReturns), on = "weeks")
epTime <- endpoints(index(timeReturns), on = "weeks")

weeklyTickBars <- diff(epTick)
weeklyTickBarsMean <- mean(weeklyTickBars)
weeklyTickBarsSd <- sd(weeklyTickBars)

weeklyVolumeBars <- diff(epVolume)
weeklyVolumeBarsMean <- mean(weeklyVolumeBars)
weeklyVolumeBarsSd <- sd(weeklyVolumeBars)

weeklyDollarBars <- diff(epDollar)
weeklyDollarBarsMean <- mean(weeklyDollarBars)
weeklyDollarBarsSd <- sd(weeklyDollarBars)

weeklyTimeBars <- diff(epTime)
weeklyTimeBarsMean <- mean(weeklyTimeBars)
weeklyTimeBarsSd <- sd(weeklyTimeBars)

print(c("Weekly tick bars" = weeklyTickBarsMean,
        "Weekly volume bars" = weeklyVolumeBarsMean,
        "Weekly dollar bars" = weeklyDollarBarsMean,
        "Weekly time bars" = weeklyTimeBarsMean))

print(c("Weekly tick bars sd" = weeklyTickBarsSd,
        "Weekly volume bars sd" = weeklyVolumeBarsSd,
        "Weekly dollar dars sd" = weeklyDollarBarsSd,
        "Weekly time bars sd" = weeklyTimeBarsSd))

normalizeData <- function(v) {
  return((v - min(v))/(max(v) - min(v)))
}

weeklyCounts <- cbind(weeklyTickBars, weeklyVolumeBars, weeklyDollarBars, weeklyTimeBars)
weeklyCounts <- apply(weeklyCounts, 2, normalizeData)
weeklyCounts <- xts(weeklyCounts, order.by = index(tickReturns)[epTick[-1]])
colnames(weeklyCounts) <- c("Tick", "Volume", "Dollar", "Time")

moltenDD <- fortify(weeklyCounts, melt = TRUE)

ggplot(moltenDD, aes(x = Index, y = Value)) + geom_line(aes(col = Series)) + 
  geom_point(aes(col = Series)) + xlab(element_blank()) + ylab(element_blank()) +
  scale_color_manual(values = c("#FF0000", "#00FF00", "#0000FF", "#8B008B")) +
  ggtitle("Weekly bar counts")

# _______________