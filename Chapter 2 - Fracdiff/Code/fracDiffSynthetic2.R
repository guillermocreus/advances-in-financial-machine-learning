library(ggplot2)  # Fancy plots
library(reshape2)  # Melting data frame
library(xts)  # Time series
library(tseries)  # Augmented Dickey Fuller (ADF) Test
library(pracma)  # Dividing and multiplying polynomials
library(dplyr)  # %>%
library(randomForest)  # Train Random Forests

# _____ NN _____
library(keras)
library(tensorflow)
tensorflow::tf$random$set_seed(1998)
# _______________

load(file = "~/Dropbox/Otros/clean_returns_GMVP.RData") 
GMVPPrices <- cumprod(1 + GMVP_returns)
GMVPLogPrices <- log(GMVPPrices)

source("plots.R")
source("fracDiffUtils.R")

period <- 100
n <- 1000
x <- 1 + 0.02*sin(mod(0:(n - 1), period)/period*2*pi)
y <- c()
for (i in 1:length(x)) {
  y <- c(y, rnorm(1, mean = x[i], sd = 0.005))
}

y <- cumsum(y)

n <- 3000
nTrn <- 2950
nTst <- n - nTrn
mu <- 0.01
vol <- 0.03
period <- 365*7
muSin <- mu*sin(mod(1:n, period)/period*2*pi)

wInverse <- getInverseWeights_FFD(0.88, 1e-4, n)

allMSE <- c("FracDiff" = 0, "Returns" = 0)

set.seed(1995)
nIter <- 2500
for (k in 1:nIter) {
  x <- c()
  for (i in 1:length(muSin)) {
    x <- c(x, mu/3 + rnorm(n = 1, mean = muSin[i], sd = vol/2))
  }
  
  y <- cumsum(x)
  yTrn <- y[1:nTrn]
  yTst <- y[-c(1:nTrn)]
  
  yd <- na.omit(fracDiff_FFD(y, 0.88, 1e-4))
  ydTst <- tail(yd, nTst)
  ydTrn <- yd[1:(length(yd) - nTst)]
  
  # _____ Frac Diff _____
  w <- 2*pi/period
  x1 <- sin(w*(1:length(ydTrn)))
  x2 <- cos(w*(1:length(ydTrn)))
  x3 <- 1:length(ydTrn)
  ddTrn <- data.frame(cbind(x1, x2, x3, ydTrn))
  
  x1 <- sin(w*((length(ydTrn) + 1):length(yd)))
  x2 <- cos(w*((length(ydTrn) + 1):length(yd)))
  x3 <- (length(ydTrn) + 1):length(yd)
  ddTst <- data.frame(cbind(x1, x2, x3, ydTst))
  
  linearModD <- lm(ydTrn ~ ., data = ddTrn)
  # yHatDTrn <- predict(linearModD, newdata = ddTrn)
  yHatDTst <- predict(linearModD, newdata = ddTst)
  # plot(yHatDTrn)
  # plot(yHatDTst)
  # _______________
  
  # _____ Returns _____
  xTrn <- x[1:nTrn]
  xTst <- x[-c(1:nTrn)]
  
  x1 <- sin(w*(1:length(xTrn)))
  x2 <- cos(w*(1:length(xTrn)))
  x3 <- 1:length(xTrn)
  ddTrn <- data.frame(cbind(x1, x2, x3, xTrn))
  
  x1 <- sin(w*((length(xTrn) + 1):length(x)))
  x2 <- cos(w*((length(xTrn) + 1):length(x)))
  x3 <- (length(xTrn) + 1):length(x)
  ddTst <- data.frame(cbind(x1, x2, x3, xTst))
  
  linearMod <- lm(xTrn ~ ., data = ddTrn)
  # xHatTrn <- predict(linearMod, newdata = ddTrn)
  xHatTst <- predict(linearMod, newdata = ddTst)
  # plot(xHatTrn)
  # plot(xHatTst)
  # _______________
  
  aux <- completeFracDiff_FFD(yTrn, yDiffOriginal, 0.88, 1e-4)
  yHatD <- c(aux[1:(n - length(ydTrn) - length(yHatDTst))], ydTrn, yHatDTst)
  xRet <- c(xTrn, xHatTst)
  
  xRet <- as.xts(xRet, order.by = as.Date("2000-01-01") + 1:n)
  yHatD <- as.xts(yHatD, order.by = as.Date("2000-01-01") + 1:n)
  
  allTSeries <- cbind(xRet, yHatD)
  # print(plotTimeSeries(allTSeries))
  
  yIntRet <- cumsum(xRet)
  yIntD <- inverseFracDiff_FFD(yHatD, 0.88, 1e-4, w = wInverse)
  
  allMSE <- allMSE + c(sum((yIntD - y)^2), sum((yIntRet - y)^2))
  
  # print(paste("Returns MSE", sum((yIntRet - y)^2)))
  # print(paste("Frac Diff MSE:", sum((yIntD - y)^2)))
  # cat("\n")
  
  allIntTSeries <- cbind(yIntRet, yIntD, y)
  # print(plotTimeSeries(allIntTSeries[nTrn:n]))
}

allMSE <- allMSE/(nIter*nTst)
print(allMSE)
100*as.numeric((allMSE[2] - allMSE[1])/allMSE[1])
