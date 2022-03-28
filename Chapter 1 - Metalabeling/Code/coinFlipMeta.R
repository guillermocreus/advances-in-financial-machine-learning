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
GMVP_signal[GMVP_signal == 0] <- - 1
index(GMVP_signal) <- as.Date(index(GMVP_signal))
# __________________________

# _______ HYPERPARAMETERS _______
tc <- 5*0.01*1e-2  # 5 bps
window_length <- 20
holding_time <- 10
# _______________________________

callLabels <- get_labels_primary_model(GMVP_returns, 
                                       xPrimaryModelBase,
                                       window_length = window_length,
                                       max_time = holding_time)

yPrimaryModel <- callLabels$labels
realizedReturnsPrimaryModel <- callLabels$realized_returns

probabilities <- seq(0, 0.5, length.out = length(randomForestBernoulliList))
randomForestBernoulliMetaModels <- list()
bestThBernoulliMetaModels <- allSrTestM1 <- allSrTestMM <- 1:length(randomForestBernoulliList)

for (i in 1:length(randomForestBernoulliList)) {
  
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
  
  randomForestPrimaryModel <- randomForestBernoulliList[[i]]
  
  # Predictions
  
  predictedTrainM1 <- randomForestPrimaryModel$finalModel$predicted
  predictedTrainM1 <- as.xts(predictedTrainM1,
                             order.by = index(trainDataPrimaryModel))
  
  predictedValidationM1 <- predict(randomForestPrimaryModel, 
                                  validationDataPrimaryModel)
  predictedValidationM1 <- as.xts(predictedValidationM1,
                                 order.by = as.Date(names(predictedValidationM1)))
  
  predictedTestM1 <- predict(randomForestPrimaryModel, testDataPrimaryModel)
  predictedTestM1 <- as.xts(predictedTestM1,
                           order.by = as.Date(names(predictedTestM1)))
  
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
  
  xSecondaryModel <- na.omit(cbind(predictionsPrimaryModel, xSecondaryModel))
  
  
  # Labels
  
  ySecondaryModel <- get_labels_secondary_model(realizedReturnsPrimaryModel,
                                                predictionsPrimaryModel,
                                                bestThBernoulliList[i])
  
  # Merging Features w/ Labels
  dataSecondaryModel <- na.omit(cbind(xSecondaryModel, ySecondaryModel))
  
  # Train, Validation and Test Data
  trainDataSecondaryModel <- 
    dataSecondaryModel[index(trainDataPrimaryModel)]
  
  validationDataSecondaryModel <- 
    dataSecondaryModel[index(validationDataPrimaryModel)]
  
  testDataSecondaryModel <- 
    dataSecondaryModel[index(testDataPrimaryModel)]
  
  set.seed(1998)
  randomForestSecondaryModel <- train(label~. , trainDataSecondaryModel, 
                                      method = "rf", importance = TRUE)
  randomForestBernoulliMetaModels[[i]] <- randomForestSecondaryModel
  
  predictedValidationM2 <- 
    predict(randomForestSecondaryModel, validationDataSecondaryModel)
  predictedValidationM2 <- 
    as.xts(predictedValidationM2, 
           order.by = as.Date(names(predictedValidationM2)))
  
  predictedTestM2 <-
    predict(randomForestSecondaryModel, testDataSecondaryModel)
  predictedTestM2 <- 
    as.xts(predictedTestM2,
           order.by = as.Date(names(predictedTestM2)))
  
  # Hyper-parameter tuning via CV
  
  thresholds <- seq(from = 0, to = 1, length.out = 20)
  allSrSecondaryModel <- allNumDiscreteReturns <- c()
  for (th in thresholds) {
    enterDecision <- enter_decision_2_models(predictedValidationM1, 
                                             predictedValidationM2,
                                             bestThBernoulliList[i],
                                             th)
    
    SR <- Sharpe_Ratio_from_predictions(returns = GMVP_returns,
                                        enter = enterDecision,
                                        window_length = window_length,
                                        max_time = holding_time,
                                        tc = tc,
                                        threshold = th,
                                        verbose = FALSE)
    
    if (length(SR) == 0) SR <- -Inf
    allSrSecondaryModel <- c(allSrSecondaryModel, SR)
    
    discreteReturns <- get_discrete_returns(returns = GMVP_returns,
                                            enter = enterDecision,
                                            window_length = window_length,
                                            max_time = holding_time,
                                            tc = tc)
    
    nDisc <- length(discreteReturns)
    if (length(nDisc) == 0) nDisc <- 0
    if (is.na(nDisc)) nDisc <- 0
    allNumDiscreteReturns <- c(allNumDiscreteReturns, nDisc)
  }
  
  # ________ IGNORE ________
  dd <- round(data.frame(cbind(thresholds, 
                               allNumDiscreteReturns, 
                               allSrSecondaryModel)), 4)
  
  dd$`Enough observations` <- 
    dd$allNumDiscreteReturns > median(allNumDiscreteReturns)/2
  
  print(ggplot(dd, aes(x=thresholds, y=allSrSecondaryModel, 
                       color=`Enough observations`)) +
          geom_point() + xlab("Threshold") + ylab("SR") +
          ggtitle("SR Secondary model (CV)"))
  
  # _______________________
  
  print(round(data.frame(cbind(thresholds, 
                               allNumDiscreteReturns, 
                               allSrSecondaryModel)), 4))
  
  minNRet <- median(allNumDiscreteReturns)/2
  indReliable <- which(allNumDiscreteReturns > minNRet)
  allSrSecondaryModel <- allSrSecondaryModel[indReliable]
  
  pointsSr <- rank(allSrSecondaryModel)
  
  thresholdsReliable <- thresholds[indReliable]
  
  bestThM2 <- thresholdsReliable[max(which(pointsSr == max(pointsSr)))]
  bestThBernoulliMetaModels[i] <- bestThM2
  
  # Sharpe Ratios
  
  BhSrValidation <- SharpeRatio.annualized(GMVP_returns[index(validationDataPrimaryModel)])
  BhSrValidation <- as.numeric(BhSrValidation)
  
  BhSrTest <- SharpeRatio.annualized(GMVP_returns[index(testDataPrimaryModel)])
  BhSrTest <- as.numeric(BhSrTest)
  
  SrValidationM1 <- Sharpe_Ratio_from_predictions(returns = GMVP_returns,
                                                  enter = predictedValidationM1 > bestThBernoulliList[i],
                                                  window_length = window_length,
                                                  max_time = holding_time,
                                                  tc = tc,
                                                  verbose = FALSE)
  
  SrValidationM2 <- allSrSecondaryModel[match(bestThM2, thresholds)]
  
  enterDecision <- enter_decision_2_models(predictedTestM1, 
                                           predictedTestM2,
                                           bestThBernoulliList[i],
                                           bestThM2)
  
  SrTestM2 <- Sharpe_Ratio_from_predictions(returns = GMVP_returns,
                                            enter = enterDecision,
                                            window_length = window_length,
                                            max_time = holding_time,
                                            tc = tc,
                                            verbose = FALSE)

  SrTestM1 <- Sharpe_Ratio_from_predictions(returns = GMVP_returns,
                                            enter = predictedTestM1 > bestThBernoulliList[i],
                                            window_length = window_length,
                                            max_time = holding_time,
                                            tc = tc,
                                            verbose = FALSE)
  
  cat(paste(paste("Iteration", i),
            paste("Coin flip p = ", probabilities[i]),
            paste("SR (Validation) - B&H =", 
                  round(BhSrValidation, 4)),
            paste("SR (Validation) - ML Primary Model =", 
                  round(SrValidationM1, 4)),
            paste("SR (Validation) - ML Meta Model =", 
                  round(SrValidationM2, 4)),
            paste("SR (Test) - B&H =", 
                  round(BhSrTest, 4)),
            paste("SR (Test) - ML Primary Model =", 
                  round(SrTestM1, 4)),
            paste("SR (Test) - ML Meta Model =", 
                  round(SrTestM2, 4)),
            sep = "\n"))
  # _______________________
  
  allSrTestM1[i] <- SrTestM1
  allSrTestMM[i] <- SrTestM2
}

save(randomForestBernoulliMetaModels, bestThBernoulliMetaModels,
     allSrTestM1, allSrTestMM,
     file = "~/Dropbox/Otros/coinFlipMeta.RData")
