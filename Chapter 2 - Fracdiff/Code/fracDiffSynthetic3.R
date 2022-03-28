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

value2Add <- function (ts, n) {
  beta <- 0.25
  val <- beta*tail(ts, 1)
  for (i in 1:(n - 1)) {
    val <- val + ts[length(ts) - i]*beta*(1 - beta)^i
  }
  return(val)
}

set.seed(1998)
nMA <- 5
n <- 3000
nTst <- as.integer(0.15*n)

mu <- 0.005
sigma <- 0.01

y <- seq(from = 0, to = 0.3, length.out = nMA)
yTheor <- y
# yRet <- y

for (i in 1:(n - nMA)) {
  y <- c(y, mean(y[i:(i + nMA - 1)]) + rnorm(1, mean = mu, sd = sigma))
  yTheor <- c(yTheor, mean(yTheor[i:(i + nMA - 1)]) + mu)
  # yRet <- c(yRet, tail(yRet, 1) + 0.0005728606)
}

y <- as.xts(y, order.by = as.Date("2000-01-01") + 1:length(y))
yTheor <- as.xts(yTheor, order.by = as.Date("2000-01-01") + 1:length(yTheor))
# yRet <- as.xts(yRet, order.by = as.Date("2000-01-01") + 1:length(yRet))

allD <- seq(from = 0, to = 1, length.out = 11)
alltSeries2Plot <- y

for (i in 1:length(allD)) {
  tSeriesFracDiff_FFD <- na.omit(fracDiff_FFD(y, allD[i], 1e-4))
  if (i > 1) alltSeries2Plot <- cbind(alltSeries2Plot, tSeriesFracDiff_FFD)
}

colnames(alltSeries2Plot) <- as.character(allD)
plotTimeSeries(alltSeries2Plot, title = "FFD - Synthetic Time Series")
plotTimeSeries(tail(alltSeries2Plot, 500), 
               title = "FFD - Synthetic Time Series (last 500 values)")

allD <- seq(from = 0, to = 1, length.out = 21)
allADFStatistics <- rep(NA, length(allD))
stationaryTSeries<- rep(FALSE, length(allD))
for (i in 1:length(allD)) {
  
  # TRAIN!!!
  tSeriesFracDiff_FFD <- na.omit(fracDiff_FFD(y[1:2550], allD[i], 1e-4))
  ADFTest <- adf.test(tSeriesFracDiff_FFD)
  
  print(i)
  print(ADFTest)
  cat("\n")
  
  allADFStatistics[i] <- ADFTest$statistic
  if (ADFTest$p.value <= 0.01) stationaryTSeries[i] <- TRUE
}

dataADF <- data.frame(cbind(allD, allADFStatistics, as.factor(stationaryTSeries)))
ggplot(dataADF, aes(x = allD, y = allADFStatistics)) + 
  geom_line(colour = "gold2", size = 1) + 
  geom_point(aes(fill = stationaryTSeries), size = 3, shape = 21, stroke = 0) + 
  scale_fill_manual(values = c("red2", "green3")) +
  xlab("d") + ylab("ADF Statistic") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, length.out = 11)) +
  ggtitle("ADF Test Statistic as a function of d")

adf.test(na.omit(fracDiff_FFD(y, 0.2, 1e-4)))
plot(y)
plot(yTheor)
plot(diff(y))

# d <- 0.25
d <- 0.2
th <- 1e-4
yD <- fracDiff_FFD(y, d, th)
dataD <- xts()

for (lags in 1:nMA) {
  dataD <- cbind(dataD, stats::lag(yD, k = lags))
}

index(dataD) <- as.Date(index(dataD))
dataD <- na.omit(cbind(dataD, yD))
colnames(dataD) <- c(paste("lag", 1:nMA, sep = ""), "Label")

dataDTrn <- dataD[1:(nrow(dataD) - nTst), ]
dataDTst <- tail(dataD, nTst)

x <- diff(y)
dataRet <- xts()

for (lags in 1:nMA) {
  dataRet <- cbind(dataRet, stats::lag(x, k = lags))
}

index(dataRet) <- as.Date(index(dataRet))
dataRet <- na.omit(cbind(dataRet, x))
colnames(dataRet) <- c(paste("lag", 1:nMA, sep = ""), "Label")

dataRetTrn <- dataD[1:(nrow(dataRet) - nTst), ]
dataRetTst <- tail(dataRet, nTst)


# _____ Frac Diff Model _____

set.seed(1998)
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = ncol(dataDTrn) - 1) %>%
  layer_dense(units = 4, activation = "elu",
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104)) %>%
  layer_dense(units = 1, activation = "elu", 
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104))

model %>% compile(
  # optimizer = keras::optimizer_sgd(0.01, momentum = 0.9),
  optimizer = "adam",
  loss = "mse"
)

history <- model %>% fit(
  x = dataDTrn[, -ncol(dataDTrn)], 
  y = dataDTrn$Label,
  epochs = 5000,
  batch_size = 256,
  validation_split = 0.15,
  verbose = 0
)

# _______________

# _____ Returns Model _____

set.seed(1998)
modelRet <- keras_model_sequential() %>%
  layer_flatten(input_shape = ncol(dataRetTrn) - 1) %>%
  layer_dense(units = 4, activation = "elu",
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104)) %>%
  layer_dense(units = 1, activation = "elu", 
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104))

modelRet %>% compile(
  # optimizer = keras::optimizer_sgd(0.01, momentum = 0.9),
  optimizer = "adam",
  loss = "mse"
)

historyRet <- modelRet %>% fit(
  x = dataRetTrn[, -ncol(dataRetTrn)], 
  y = dataRetTrn$Label,
  epochs = 5000,
  batch_size = 256,
  validation_split = 0.15,
  verbose = 0
)

# _______________


# _____ Predictions _____

forecastDTst <- model %>% predict(dataDTst[, -ncol(dataDTst)])
forecastDTst <- as.xts(forecastDTst, order.by = index(dataDTst))

forecastRetTst <- modelRet %>% predict(dataRetTst[, -ncol(dataRetTst)])
forecastRetTst <- as.xts(forecastRetTst, order.by = index(dataRetTst))

# _______________

# _____ Recovering the original time series _____

# _____ Frac Diff _____

yD <- completeFracDiff_FFD(y, y, d, th)

wInv <- getInverseWeights_FFD(d, th, length(yD))
yIntD <- sequentialForecast(forecastDTst, yD, d, th, wInv)

timeSeries2Plot <- cbind(y, stats::lag(y), yIntD)[index(dataDTst)]
colnames(timeSeries2Plot) <- c("y", "y_lagged", "y_FracDiff")
plotTimeSeries(timeSeries2Plot[1:20, ], title = "Out-of-sample performance Frac Diff")
plotTimeSeries(timeSeries2Plot, title = "Out-of-sample performance Frac Diff")

# _______________

# _____ Returns _____

wInvRet <- rep(1, length(yD))
yRet <- completeFracDiff_FFD(y, y, 1, th)

yIntRet <- sequentialForecast(forecastRetTst, yRet, 1, th, wInvRet)

timeSeries2Plot <- cbind(y, stats::lag(y), yIntRet)[index(dataRetTst)]
colnames(timeSeries2Plot) <- c("y", "y_lagged", "y_Returns")
plotTimeSeries(timeSeries2Plot[1:20, ], title = "Out-of-sample performance Returns")
plotTimeSeries(timeSeries2Plot, title = "Out-of-sample performance Returns")

# _______________

# _______________

# _____ Results _____

allTimeSeries <- cbind(y, stats::lag(y), yIntD, yIntRet)[index(dataDTst)]
colnames(allTimeSeries) <- c("y", "y_lagged", "y_FracDiff", "y_Returns")

plotTimeSeries(allTimeSeries, title = "Out-of-sample performance")

yAux <- y[index(dataDTst)]
yLagAux <- stats::lag(y)[index(dataDTst)]
yIntDAux <- yIntD[index(dataDTst)]
yIntRetAux <- yIntRet[index(dataDTst)]

# Root Mean Square Error (RMSE)
RMSENaive <- sqrt(sum((yAux - yLagAux)^2)/nrow(dataDTst)) 
RMSEFracDiff <- sqrt(sum((yAux- yIntDAux)^2)/nrow(dataDTst)) 
RMSERet <- sqrt(sum((yAux - yIntRetAux)^2)/nrow(dataDTst))
vRMSE <- c("RMSE Naive" = RMSENaive,
           "RMSE FD" = RMSEFracDiff, 
           "RMSE Returns" = RMSERet)

# Mean Absolute Percentage Error (MAPE)
MAPENaive <- sum(abs((yAux - yLagAux)/yAux))/nrow(dataDTst)
MAPEFracDiff <- sum(abs((yAux- yIntDAux)/yAux))/nrow(dataDTst)
MAPERet <- sum(abs((yAux - yIntRetAux)/yAux))/nrow(dataDTst)
vMAPE <- c("MAPE Naive" = MAPENaive,
           "MAPE FD" = MAPEFracDiff, 
           "MAPE Returns" = MAPERet)

# Median Absolute Deviation (MAD)
eNaive <- yLagAux - yAux
eFracDiff <- yIntDAux - yAux
eRet <- yIntRetAux - yAux

MADNaive <- median(abs(eNaive - median(eNaive)))
MADFracDiff <- median(abs(eFracDiff - median(eFracDiff)))
MADRet <- median(abs(eRet - median(eRet)))
vMAD <- c("MAD Naive" = MADNaive,
          "MAD FD" = MADFracDiff, 
          "MAD Returns" = MADRet)

print(vRMSE)
print(vMAPE)
print(vMAD)

dd <- data.frame(cbind(vRMSE, vMAD, vMAPE))
colnames(dd) <- c("RMSE", "MAD", "MAPE")
rownames(dd) <- c("Naive", "FFD", "Returns")
dd <- cbind(dd, "Names" = rownames(dd))

dd.m <- melt(dd, id.vars = "Names")
colnames(dd.m)[2] <- "Metric"

print(ggplot(dd.m, aes(Names, value)) +   
        geom_bar(aes(fill = Metric), position = "dodge", stat = "identity") +
        xlab(element_blank()) + ylab(element_blank()) +
        ggtitle("Error metrics (Test)"))
# _______________