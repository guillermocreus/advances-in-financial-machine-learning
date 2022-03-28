library(rvest)
library(stringr)
library(data.table)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(portfolioBacktest)
library(TTR)
library(ggfortify)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(reshape2)
library(randomForest)
library(caret)
library(purrr)  # Random Bernoulli numbers
library(CVXR)

library(keras)
library(tensorflow)
tensorflow::tf$random$set_seed(1998)

setwd("~/Dropbox/HKUST/TFG/Chapter\ 1\ -\ Metalabeling/Code")

source("Other/tables.R")
source("Other/cusum.R")
source("Other/labels.R")
source("Other/features.R")
source("Other/ROC.R")
source("Other/misc.R")
source("Other/plots.R")

source("GMVP/primary_model_GMVP.R")
source("GMVP/secondary_model_GMVP.R")
source("GMVP/primary_model_ROC.R")
source("GMVP/auxiliary_functions_GMVP.R")
source("GMVP/full_returns_GMVP.R")
source("GMVP/discrete_returns_GMVP.R")

# __________ Data __________
load(file = "~/Dropbox/Otros/clean_returns_GMVP.RData") 
GMVP_prices <- cumprod(1 + GMVP_returns)
GMVP_MA <- SMA(GMVP_prices, n = 20)
GMVP_signal <- lag(GMVP_prices >= GMVP_MA)
GMVP_signal[GMVP_signal == 0] <- - 1
index(GMVP_signal) <- as.Date(index(GMVP_signal))
# __________________________

# _______ HYPERPARAMETERS _______
tc <- 5*0.01*1e-2  # 5 bps
window_length <- 20
holding_time <- 10
nPrecision <- 11
# _______________________________


# __________________ Primary Model __________________
xPrimaryModelBase <- get_features_primary_model(GMVP_returns, 
                                                window_length = 20,
                                                threshold = 1.5)

callLabels <- get_labels_primary_model(GMVP_returns, 
                                       xPrimaryModelBase,
                                       window_length = window_length,
                                       max_time = holding_time)

yPrimaryModel <- callLabels$labels
realizedReturnsPrimaryModel <- callLabels$realized_returns

probabilities <- seq(0, 0.5, length.out = nPrecision)

F1ScoreTestM1 <- recallTestM1 <- precisionTestM1 <- 1:length(probabilities)
primaryModels <- list()
allPredictedTrainM1 <- allPredictedValidationM1 <- allPredictedTestM1 <- list()

for (i in 1:length(probabilities)) {
  pBernoulli <- probabilities[i]
  set.seed(1998)
  swap <- rbernoulli(nrow(yPrimaryModel), p = pBernoulli)
  
  fakeFeature <- swap*(1 - yPrimaryModel) + (1 - swap)*yPrimaryModel
  xPrimaryModel <- cbind(xPrimaryModelBase, fakeFeature)
  colnames(xPrimaryModel)[ncol(xPrimaryModel)] <- "fakeFeature"
  
  dataPrimaryModel <- na.omit(cbind(xPrimaryModel, yPrimaryModel))
  
  nSplit <- as.integer(0.8*nrow(dataPrimaryModel))
  nSplitTrain <- as.integer(0.8*nSplit)
  
  trainDataPrimaryModel <- dataPrimaryModel[1:nSplitTrain, ]
  validationDataPrimaryModel <- dataPrimaryModel[(nSplitTrain + 1):nSplit, ]
  testDataPrimaryModel <- dataPrimaryModel[-c(1:nSplit), ]
  
  trainReturnsPrimaryModel <- realizedReturnsPrimaryModel[1:nSplitTrain]
  validationReturnsPrimaryModel <-
    realizedReturnsPrimaryModel[(nSplitTrain + 1):nSplit]
  testReturnsPrimaryModel <- realizedReturnsPrimaryModel[-c(1:nSplit)]
  
  xM1Trn <- as.matrix(trainDataPrimaryModel[, -ncol(trainDataPrimaryModel)])
  yM1Trn <- as.vector(trainDataPrimaryModel[, ncol(trainDataPrimaryModel)])
  
  xM1Val <- as.matrix(
    validationDataPrimaryModel[, -ncol(validationDataPrimaryModel)]
  )
  yM1Val <- as.vector(
    validationDataPrimaryModel[, ncol(validationDataPrimaryModel)]
  )
  
  xM1Tst <- as.matrix(testDataPrimaryModel[, -ncol(testDataPrimaryModel)])
  yM1Tst <- as.vector(testDataPrimaryModel[, ncol(testDataPrimaryModel)])
  
  set.seed(1998)
  primaryModel <- keras_model_sequential() %>%
    layer_flatten(input_shape = ncol(xM1Trn)) %>%
    layer_dense(units = 20,
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 1, activation = "sigmoid",
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104))
  
  primaryModel %>% compile(
    optimizer = "adam",  # "adam", "rmsprop"
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
  
  history <- primaryModel %>% fit(
    x = xM1Trn, 
    y = yM1Trn,
    epochs = 1000,
    batch_size = 5000, # Std: 256
    validation_data = list(xM1Val, yM1Val),
    verbose = 3
  )
  
  
  predictedTrainM1 <- primaryModel %>% predict(xM1Trn)
  predictedTrainM1 <- as.xts(predictedTrainM1,
                             order.by = index(trainDataPrimaryModel))
  
  predictedValidationM1 <- primaryModel %>% predict(xM1Val)
  predictedValidationM1 <- as.xts(predictedValidationM1,
                                  order.by = index(validationDataPrimaryModel))
  
  predictedTestM1 <- primaryModel %>% predict(xM1Tst)
  predictedTestM1 <- as.xts(predictedTestM1,
                            order.by = index(testDataPrimaryModel))  
  
  # _____ Metrics _____
  
  yHat <- round(predictedTestM1)
  TP <- sum(yHat == yM1Tst)
  FP <- sum(yHat != yM1Tst)
  TN <- 0
  FN <- 0
  
  recall <- TP/(TP + FN)
  precision <- TP/(TP + FP)
  f1Score <- 2/(1/recall + 1/precision)
  
  # _______________
  
  # _____ Saving data _____
  primaryModels[[i]] <- primaryModel
  
  F1ScoreTestM1[i] <- f1Score
  recallTestM1[i] <- recall
  precisionTestM1[i] <- precision
  
  allPredictedTrainM1[[i]] <- predictedTrainM1
  allPredictedValidationM1[[i]] <- predictedValidationM1
  allPredictedTestM1[[i]] <- predictedTestM1
  # _______________
}
save(primaryModels, 
     F1ScoreTestM1, recallTestM1, precisionTestM1,
     allPredictedTrainM1, allPredictedValidationM1, allPredictedTestM1,
     file = "~/Dropbox/Otros/coinFlipPrimary.RData")
