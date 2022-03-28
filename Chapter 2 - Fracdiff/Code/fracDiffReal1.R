library(ggplot2)  # Fancy plots
library(reshape2)  # Melting data frame
library(xts)  # Time series
library(tseries)  # Augmented Dickey Fuller (ADF) Test
library(pracma)  # Dividing and multiplying polynomials
library(dplyr)  # %>%
library(randomForest)  # Train Random Forests
library(quantmod)  # Download Symbols
library(stats)  # Lag a Time Series

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

# _____ Auxiliary functions _____

normalizeData <- function(dd, mins = NULL, maxs = NULL) {
  if (is.null(dim(dd))) {
    if (is.null(mins[1]) | is.null(maxs[1])) {
      return((dd - min(dd))/(max(dd) - min(dd)))
    } 
    return((dd - mins)/(maxs - mins))
  }
  
  if (is.null(mins) | is.null(maxs)) {
    mins <- apply(dd, 2, min)
    maxs <- apply(dd, 2, max)
  } 
  
  for (k in 1:ncol(dd)) {
    dd[, k] <- (dd[, k] - mins[k])/(maxs[k] - mins[k])
  }
  return(dd)
}

unNormalizeData <- function(dd, mins, maxs) {
  if (is.null(dim(dd))) {
    return(dd*(maxs - mins) + mins)
  }
  
  for (k in 1:ncol(dd)) {
    dd[, k] <- dd[, k]*(maxs[k] - mins[k]) + mins[k]
  }
  return(dd)
}

# _______________

# _____ NN experiment _____

# _____ Data _____

getSymbols("^GSPC", from = as.Date("2005-01-01"), to = as.Date("2020-09-01"))
dd <- log(GSPC[, c("GSPC.Open", "GSPC.Close", "GSPC.High", "GSPC.Low")])
colnames(dd) <- c("Open", "Close", "High", "Low")

nTrn <- as.integer(0.8*nrow(dd))
n <- nrow(dd)

ddTrn <- dd[1:nTrn, ]
ddTst <- dd[-c(1:nTrn), ]
# _______________

d <- 0.25
th <- 1e-4

dataD <- na.omit(apply(dd, 2, fracDiff_FFD, d = d, th = th))
dataD <- as.xts(dataD, order.by = as.Date(rownames(dataD)))
apply(dataD, 2, adf.test)

dataDTrn <- dataD[index(ddTrn)]
dataDTst <- dataD[index(ddTst), ]

dataRet <- na.omit(apply(dd, 2, diff))
dataRet <- as.xts(dataRet, order.by = as.Date(rownames(dataRet)))
apply(dataRet, 2, adf.test)

dataRetTrn <- dataRet[index(ddTrn)]
dataRetTst <- dataRet[index(ddTst)]

# _____ Normalize data _____

minDTrn <- apply(dataDTrn, 2, min)
maxDTrn <- apply(dataDTrn, 2, max)
vDTrn <- stats::lag(dataDTrn$Close, k = -1)
vDTst <- stats::lag(dataDTst$Close, k = -1)

dataDTrn <- normalizeData(dataDTrn)
dataDTst <- normalizeData(dataDTst, mins = minDTrn, maxs = maxDTrn)

minRetTrn <- apply(dataRetTrn, 2, min)
maxRetTrn <- apply(dataRetTrn, 2, max)
vRetTrn <- stats::lag(dataRetTrn$Close, k = -1)
vRetTst <- stats::lag(dataRetTst$Close, k = -1)

dataRetTrn <- normalizeData(dataRetTrn)
dataRetTst <- normalizeData(dataRetTst, mins = minRetTrn, maxs = maxRetTrn)

# _______________

# _____ Labels _____

dataDTrn <- na.omit(cbind(dataDTrn, vDTrn))
dataDTst <- na.omit(cbind(dataDTst, vDTst))
colnames(dataDTrn)[ncol(dataDTrn)] <- colnames(dataDTst)[ncol(dataDTst)] <- "Label"

dataRetTrn <- na.omit(cbind(dataRetTrn, vRetTrn))
dataRetTst <- na.omit(cbind(dataRetTst, vRetTst))
colnames(dataRetTrn)[ncol(dataRetTrn)] <- colnames(dataRetTst)[ncol(dataRetTst)] <- "Label"

# _______________

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

# optimizer = keras::optimizer_sgd(0.01, momentum = 0.9)
# min(val_loss) = 0.0003422203
# min(loss) = 0.001072888

# optimizer = adam
# min(val_loss) = 0.0003385579
# min(loss) = 0.001078328

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

modelRet2 <- keras_model_sequential() %>%
  layer_flatten(input_shape = ncol(dataRetTrn) - 1) %>%
  layer_dense(units = 4, activation = "sigmoid",
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104)) %>%
  layer_dense(units = 1, activation = "sigmoid", 
              kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                              maxval = 0.05,
                                                              seed = 104))

modelRet2 %>% compile(
  # optimizer = keras::optimizer_sgd(0.01, momentum = 0.9),
  optimizer = "adam",
  loss = "mse"
)

historyRet2 <- modelRet2 %>% fit(
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
# forecastDTst <- as.xts(forecastDTst, 
#                        order.by = c(tail(index(dataDTst), length(forecastDTst) - 1),
#                                     tail(index(ddTst), 1)))
forecastDTst <- as.xts(forecastDTst[1:(length(forecastDTst) - 1)], 
                       order.by = index(dataDTst)[2:nrow(dataDTst)])

forecastRetTst <- modelRet %>% predict(dataRetTst[, -ncol(dataRetTst)])
# forecastRetTst <- as.xts(forecastRetTst, 
#                          order.by = c(tail(index(dataRetTst), length(forecastRetTst) - 1),
#                                       tail(index(ddTst), 1)))
forecastRetTst <- as.xts(forecastRetTst[1:(length(forecastRetTst) - 1)], 
                         order.by = index(dataRetTst)[2:nrow(dataRetTst)])

forecastRet2Tst <- modelRet2 %>% predict(dataRetTst[, -ncol(dataRetTst)])
# forecastRet2Tst <- as.xts(forecastRet2Tst, 
#                           order.by = c(tail(index(dataRetTst), length(forecastRet2Tst) - 1),
#                                        tail(index(ddTst), 1)))
forecastRet2Tst <- as.xts(forecastRet2Tst[1:(length(forecastRet2Tst) - 1)], 
                          order.by = index(dataRetTst)[2:nrow(dataRetTst)])

# RMSED <- sqrt(sum((dataDTst$Label - yHatDTst)^2)/length(yHatDTst)) 
# RMSERet <- sqrt(sum((dataRetTst$Label - yHatRetTst)^2)/length(yHatRetTst))
# RMSERet2 <- sqrt(sum((dataRetTst$Label - yHatRet2Tst)^2)/length(yHatRet2Tst))
# 
# print(c("RMSE FD" = RMSED, 
#         "RMSE Returns - V1" = RMSERet, 
#         "RMSE Returns - V2" = RMSERet2))


# yD <- dataDTst$Label
# allPredD <- cbind(yHatDTst, yD)
# colnames(allPredD) <- c("yHatDTst", "yD")
# 
# plotTimeSeries(allPredD, "Out-of-sample Predictions of FD Models")
# 
# yRet <- dataRetTst$Label
# allPredRet <- cbind(yHatRetTst, yHatRet2Tst, yRet)
# colnames(allPredRet) <- c("yHatRetTst", "yHatRet2Tst", "yRet")
# 
# plotTimeSeries(allPredRet, title = "Out-of-sample Predictions of Returns Models")

# _______________

# _____ Recovering the original time series _____

# _____ Frac Diff _____

# y <- log(GSPC$GSPC.Close)
y <- log(GSPC$GSPC.Close)[-nrow(GSPC)]
yD <- completeFracDiff_FFD(y, y, d, th)
# forecastDTst <- unNormalizeData(forecastDTst, 
                                # mins = minDTrn["Close"], maxs = maxDTrn["Close"])
                       
wInv <- getInverseWeights_FFD(d, th, length(yD))
yIntD <- sequentialForecast(forecastDTst, yD, d, th, wInv)

timeSeries2Plot <- cbind(y, stats::lag(y), yIntD)
colnames(timeSeries2Plot) <- c("y", "y_lagged", "y_FracDiff")
plotTimeSeries(timeSeries2Plot["2017-07/"], 
               title = "Out-of-sample performance Frac Diff Model")
plotTimeSeries(timeSeries2Plot["2017-09/2017-12"],
               title = "Out-of-sample performance Frac Diff Model")
plotTimeSeries(timeSeries2Plot["2018-05/2018-09"],
               title = "Out-of-sample performance Frac Diff Model")

# _______________

# _____ Returns _____

wInvRet <- rep(1, length(yD))
yRet <- completeFracDiff_FFD(y, y, 1, th)

# forecastRetTst <- unNormalizeData(forecastRetTst, 
                                  # mins = minRetTrn["Close"], maxs = maxRetTrn["Close"])
# forecastRet2Tst <- unNormalizeData(forecastRet2Tst, 
                                   # mins = minRetTrn["Close"], maxs = maxRetTrn["Close"])

yIntRet <- sequentialForecast(forecastRetTst, yRet, 1, th, wInvRet)
yIntRet2 <- sequentialForecast(forecastRet2Tst, yRet, 1, th, wInvRet)

timeSeries2Plot <- cbind(y, stats::lag(y), yIntRet, yIntRet2)
colnames(timeSeries2Plot) <- c("y", "y_lagged", "y_Returns1", "y_Returns2")
plotTimeSeries(timeSeries2Plot["2017-07/"], 
               title = "Out-of-sample performance Returns Models")
plotTimeSeries(timeSeries2Plot["2017-09/2017-12"],
               title = "Out-of-sample performance Returns Models")
plotTimeSeries(timeSeries2Plot["2018-05-01/2018-05-20"],
               title = "Out-of-sample performance Returns Models")

# _______________

# _______________

# _____ Results _____

allTimeSeries <- cbind(y, stats::lag(y), yIntD, yIntRet)
colnames(allTimeSeries) <- c("y", "y_lagged", "y_FracDiff", "y_Returns")

plotTimeSeries(allTimeSeries["2017-07/"], title = "Out-of-sample performance")
plotTimeSeries(allTimeSeries["2018-07/2018-10"], title = "Out-of-sample performance")
plotTimeSeries(allTimeSeries["2019-03/2019-06"], title = "Out-of-sample performance")

yAux <- y[index(forecastDTst)]
yLagAux <- stats::lag(y)[index(forecastDTst)]
yIntDAux <- yIntD[index(forecastDTst)]
yIntRetAux <- yIntRet[index(forecastDTst)]
yIntRet2Aux <- yIntRet2[index(forecastDTst)]

RMSENaive <- sqrt(sum((yAux - yLagAux)^2)/nTst) 
RMSEFracDiff <- sqrt(sum((yAux- yIntDAux)^2)/nTst) 
RMSERet <- sqrt(sum((yAux - yIntRetAux)^2)/nTst)
RMSERet2 <- sqrt(sum((yAux - yIntRet2Aux)^2)/nTst)

MAPENaive <- sum(abs((yAux - yLagAux)/yAux))/nTst
MAPEFracDiff <- sum(abs((yAux- yIntDAux)/yAux))/nTst
MAPERet <- sum(abs((yAux - yIntRetAux)/yAux))/nTst
MAPERet2 <- sum(abs((yAux - yIntRet2Aux)/yAux))/nTst

eNaive <- yAux - yLagAux
eFracDiff <- yAux - yIntDAux
eRet <- yAux - yIntRetAux
eRet2 <- yAux - yIntRet2Aux

MADNaive <- median(abs(eNaive -  median(eNaive)))
MADFracDiff <- median(abs(eFracDiff - median(eFracDiff)))
MADRet <- median(abs(eRet - median(eRet)))
MADRet2 <- median(abs(eRet2 - median(eRet2)))

vRMSE <- c("RMSE Naive" = RMSENaive,
           "RMSE FD" = RMSEFracDiff, 
           "RMSE Returns" = RMSERet)

vMAPE <- c("MAPE Naive" = MAPENaive,
           "MAPE FD" = MAPEFracDiff, 
           "MAPE Returns" = MAPERet)

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


v <- c(0.00536052043812079, 0.00809707045176565, 0.00230788157796315, 0.00372811639517124,
       0.00349862723263457, 0.00508556644149666, 0.00693446969438647, 0.00223643683868287,
       0.00139208991991586, 0.00324965314120327, 0.00318037339402239, 0.00439926394598605,
       0.00198529775515217, 0.00230571658586375, 0.00125136543345469, 0.00192034799217009,
       0.0027646949109371,  0.00205674249443245, 0.0110934195173388, 0.00448586362996215,
       0.00891543746533983, 0.0247869945460602)
