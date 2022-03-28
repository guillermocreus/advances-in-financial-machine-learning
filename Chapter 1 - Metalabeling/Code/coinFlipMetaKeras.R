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
library(gridExtra)  # Arranging several plots
library(reshape2)
library(randomForest)
library(caret)  # Random forest interface
library(purrr)  # Random Bernoulli numbers
library(lightgbm)  # Light GBM
library(CVXR)  # Convex optimization

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
load(file = "~/Dropbox/Otros/coinFlipPrimary.RData")
GMVP_prices <- cumprod(1 + GMVP_returns)
GMVP_MA <- SMA(GMVP_prices, n = 20)
GMVP_signal <- lag(GMVP_prices >= GMVP_MA)
GMVP_signal[GMVP_signal == 0] <- -1
index(GMVP_signal) <- as.Date(index(GMVP_signal))
# __________________________

# _______ HYPERPARAMETERS _______
tc <- 5*0.01*1e-2  # 5 bps
window_length <- 20
holding_time <- 10
# _______________________________

scaleData <- function(x) {
  return((x - mean(x))/sd(x))
}

xPrimaryModelBase <- get_features_primary_model(GMVP_returns, 
                                                window_length = 20,
                                                threshold = 1.5)

callLabels <- get_labels_primary_model(GMVP_returns, 
                                       xPrimaryModelBase,
                                       window_length = window_length,
                                       max_time = holding_time)

yPrimaryModel <- callLabels$labels
realizedReturnsPrimaryModel <- callLabels$realized_returns

probabilities <- seq(0, 0.5, length.out = length(primaryModels))
kerasRecallTestMM <- kerasPrecisionTestMM <- 1:length(primaryModels)
kerasF1ScoreTestMM <- 1:length(primaryModels)
kerasMetaModels <- kerasPredictionsTest <- list()

for (i in 1:length(primaryModels)) {
  
  # Data
  
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
  testDataPrimaryModel <- dataPrimaryModel[(nSplit + 1):nrow(dataPrimaryModel), ]
  
  trainReturnsPrimaryModel <- realizedReturnsPrimaryModel[1:nSplitTrain]
  validationReturnsPrimaryModel <-
    realizedReturnsPrimaryModel[(nSplitTrain + 1):nSplit]
  testReturnsPrimaryModel <- realizedReturnsPrimaryModel[-c(1:nSplit)]
  
  # primaryModel <- primaryModels[[i]]
  
  # Predictions
  
  # predictedTrainM1 <- primaryModel %>% predict(xM1Trn)
  # predictedTrainM1 <- as.xts(predictedTrainM1,
  #                            order.by = index(trainDataPrimaryModel))
  # 
  # predictedValidationM1 <- primaryModel %>% predict(xM1Val)
  # predictedValidationM1 <- as.xts(predictedValidationM1,
  #                                 order.by = index(validationDataPrimaryModel))
  # 
  # predictedTestM1 <- primaryModel %>% predict(xM1Tst)
  # predictedTestM1 <- as.xts(predictedTestM1,
  #                           order.by = index(testDataPrimaryModel)) 
  
  predictedTrainM1 <- allPredictedTrainM1[[i]]
  predictedValidationM1 <- allPredictedValidationM1[[i]]
  predictedTestM1 <- allPredictedTestM1[[i]]
  
  # Features
  
  # Predictions from primary model
  predictionsPrimaryModel <- c(predictedTrainM1,
                               predictedValidationM1,
                               predictedTestM1)
  
  # Features from returns
  xSecondaryModel <- get_features(GMVP_returns, 
                                  signal = GMVP_signal,
                                  MA = GMVP_MA)[[1]]
  # Solving UTC problems
  index(xSecondaryModel) <- as.Date(index(xSecondaryModel))
  
  xSecondaryModel <- na.omit(cbind(predictionsPrimaryModel,
                                   xSecondaryModel))
  colnames(xSecondaryModel)[1] <- "predictionsPrimaryModel"
  
  # Labels
  
  ySecondaryModel <- (
    (2*round(predictionsPrimaryModel) - 1)*realizedReturnsPrimaryModel > 0
  )
  
  
  # Merging Features w/ Labels
  dataSecondaryModel <- na.omit(cbind(xSecondaryModel, ySecondaryModel))
  colnames(dataSecondaryModel)[ncol(dataSecondaryModel)] <- "label"
  
  # Train, Validation and Test Data
  trainDataSecondaryModel <- 
    dataSecondaryModel[index(trainDataPrimaryModel)]
  
  validationDataSecondaryModel <- 
    dataSecondaryModel[index(validationDataPrimaryModel)]
  
  testDataSecondaryModel <- 
    dataSecondaryModel[index(testDataPrimaryModel)]
  
  # _____ Train _____
  xKerasTrn <- trainDataSecondaryModel
  yKerasTrn <- xKerasTrn$label
  xKerasTrn <- xKerasTrn[, -ncol(xKerasTrn)]
  
  xKerasTrn <- as.matrix(xKerasTrn)
  colnames(xKerasTrn) <- rownames(xKerasTrn) <- NULL
  yKerasTrn <- as.vector(yKerasTrn)
  
  xKerasTrn <- apply(xKerasTrn, 2, scaleData)
  
  # _______________
  
  # _____ Validation _____
  xKerasV <- validationDataSecondaryModel
  yKerasV <- xKerasV$label
  xKerasV <- xKerasV[, -ncol(xKerasV)]
  
  xKerasV <- as.matrix(xKerasV)
  colnames(xKerasV) <- rownames(xKerasV) <- NULL
  yKerasV <- as.vector(yKerasV)
  
  xKerasV <- apply(xKerasV, 2, scaleData)
  
  # _______________
  
  # _____ Test _____
  xKerasTst <- testDataSecondaryModel
  yKerasTst <- xKerasTst$label
  xKerasTst <- xKerasTst[, -ncol(xKerasTst)]
  
  xKerasTst <- as.matrix(xKerasTst)
  colnames(xKerasTst) <- rownames(xKerasTst) <- NULL
  yKerasTst <- as.vector(yKerasTst)
  
  xKerasTst <- apply(xKerasTst, 2, scaleData)
  
  # _______________
  
  model <- keras_model_sequential() %>%
    layer_flatten(input_shape = ncol(xKerasTrn)) %>%
    layer_dense(units = 100,
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104)) %>%
    layer_activation_leaky_relu() %>%
    layer_dense(units = 50,
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104)) %>%
    layer_activation_leaky_relu() %>%
    layer_dense(units = 25,
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104)) %>%
    layer_activation_leaky_relu() %>%
    layer_dense(units = 1, activation = "sigmoid",
                kernel_initializer = initializer_random_uniform(minval = -0.05,
                                                                maxval = 0.05,
                                                                seed = 104))
  
  model %>%
    compile(
      optimizer = "rmsprop",
      loss = "binary_crossentropy",
      metrics = c("Recall", "Precision")
    )
  
  history <- model %>% fit(
    x = xKerasTrn, 
    y = yKerasTrn,
    epochs = 200,
    batch_size = 256,
    validation_data = list(xKerasV, yKerasV),
    verbose = 0
    # validation_split = 0.2
  )
  
  yHatTst <- model %>% predict_classes(xKerasTst)
  yHatTst <- as.xts(yHatTst, order.by = index(testDataSecondaryModel))
  TP <- sum(yHatTst == 1 & yKerasTst == 1)
  FN <- sum(yHatTst == 0 & yKerasTst == 1)
  FP <- sum(yHatTst == 1 & yKerasTst == 0)
  
  recall <- TP/(TP + FN)
  precision <- TP/(TP + FP)
  f1Score <- 2/(1/recall + 1/precision)
  
  print(paste("Iteracion", i))
  print(recall)
  print(precision)
  print(f1Score)
  
  kerasMetaModels[[i]] <- model
  kerasF1ScoreTestMM[i] <- f1Score
  kerasRecallTestMM[i] <- recall
  kerasPrecisionTestMM[i] <- precision
  kerasPredictionsTest[[i]] <- yHatTst
}

save(kerasMetaModels, kerasPredictionsTest,
     kerasF1ScoreTestMM, kerasRecallTestMM, kerasPrecisionTestMM,
     file = "~/Dropbox/Otros/coinFlipMetaKeras.RData")
