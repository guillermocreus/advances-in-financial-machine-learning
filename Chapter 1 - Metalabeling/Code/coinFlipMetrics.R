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
# load(file = "~/Dropbox/Otros/coinFlipMeta.RData")
# load(file = "~/Dropbox/Otros/coinFlipReturns.RData")
load(file = "~/Dropbox/Otros/coinFlipMetaKeras.RData")

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

xPrimaryModelBase <- get_features_primary_model(GMVP_returns, 
                                                window_length = 20,
                                                threshold = 1.5)

callLabels <- get_labels_primary_model(GMVP_returns, 
                                       xPrimaryModelBase,
                                       window_length = window_length,
                                       max_time = holding_time)

yPrimaryModel <- callLabels$labels
realizedReturnsPrimaryModel <- callLabels$realized_returns

aux <- na.omit(cbind(xPrimaryModelBase, yPrimaryModel))
nSplit <- as.integer(0.8*nrow(aux))
yM1Tst <- yPrimaryModel[-c(1:nSplit)]

probabilities <- seq(0, 0.5, length.out = length(primaryModels))

precisionM1 <- recallM1 <- f1ScoreM1 <- 1:length(probabilities)
precisionMMKeras <- recallMMKeras <- f1ScoreMMKeras <- 1:length(probabilities)
# precisionMMRF <- recallMMRF <- f1ScoreMMRF <- 1:length(probabilities)

for (i in 1:length(probabilities)) {
  # _____ Primary Model _____
  
  recallM1[i] <- recallTestM1[i]
  precisionM1[i] <- precisionTestM1[i]
  f1ScoreM1[i] <- F1ScoreTestM1[i]
  
  # _______________

  # # _____ Meta Model (RF) _____
  # 
  # yHatCoinFlipM2 <- coinFlipM2Predictions[[i]] > bestThBernoulliMetaModels[i]
  # yHatCoinFlipMM <- (yHatCoinFlipM1 == 1) & (yHatCoinFlipM2 == 1)
  # TP <- sum(yHatCoinFlipMM == yCoinFlipM1[[i]])
  # TN <- sum(yHatCoinFlipMM == 0 & yHatCoinFlipM1 != yCoinFlipM1[[i]])
  # FP <- sum(yHatCoinFlipMM != yCoinFlipM1[[i]])
  # FN <- sum(yHatCoinFlipMM == 0 & yHatCoinFlipM1 == yCoinFlipM1[[i]])
  # 
  # recallMMRF[i] <- TP/(TP + FN)
  # precisionMMRF[i] <- TP/(TP + FP)
  # f1ScoreMMRF[i] <- 2/(1/recallMMRF[i] + 1/precisionMMRF[i])
  # # _______________
    
  # _____ Meta Model _____

  yHatCoinFlipM1 <- round(allPredictedTestM1[[i]])
  yHatCoinFlipM2 <- round(kerasPredictionsTest[[i]])
  
  TP <- sum(yHatCoinFlipM1 == yM1Tst & yHatCoinFlipM2 == 1)
  FP <- sum(yHatCoinFlipM1 != yM1Tst & yHatCoinFlipM2 == 1)
  TN <- sum(yHatCoinFlipM1 != yM1Tst & yHatCoinFlipM2 == 0)
  FN <- sum(yHatCoinFlipM1 == yM1Tst & yHatCoinFlipM2 == 0)
  
  recallMMKeras[i] <- TP/(TP + FN)
  precisionMMKeras[i] <- TP/(TP + FP)
  f1ScoreMMKeras[i] <- 2/(1/recallMMKeras[i] + 1/precisionMMKeras[i])
  
  # _______________
  
  # _______________
}

recall <- data.frame(cbind(probabilities, recallM1, recallMMKeras))

precision <- data.frame(cbind(probabilities, precisionM1, precisionMMKeras))

f1Score <- data.frame(cbind(probabilities, f1ScoreM1, f1ScoreMMKeras))

colnames(recall) <- colnames(precision) <- colnames(f1Score) <- c("probabilities",
                                                                  "Primary Model",
                                                                  "Meta Model")

# _____ Recall _____
dd.m <- melt(recall, id.vars = "probabilities")
ggplot(dd.m, aes(x = probabilities, y = value, col = variable)) + 
  geom_line(aes(col = variable), size = 2) + labs(col = "Model") +
  xlab("Probability (Coin Flip)") + ylab("Recall") +
  geom_point(aes(shape = variable, colour = variable), size = 4, show.legend = FALSE) +
  ggtitle("Recall of Coin Flip Models")
# _______________

# _____ Precision _____
dd.m <- melt(precision, id.vars = "probabilities")
ggplot(dd.m, aes(x = probabilities, y = value, col = variable)) + 
  geom_line(aes(col = variable), size = 2) + labs(col = "Model") +
  xlab("Probability (Coin Flip)") + ylab("Precision") +
  geom_point(aes(shape = variable, colour = variable), size = 4, show.legend = FALSE) +
  ggtitle("Precision of Coin Flip Models")
# _______________

# _____ F1 Score _____
dd.m <- melt(f1Score, id.vars = "probabilities")
ggplot(dd.m, aes(x = probabilities, y = value, col = variable)) + 
  geom_line(aes(col = variable), size = 2) + labs(col = "Model") +
  xlab("Probability (Coin Flip)") + ylab("F1 Score") +
  geom_point(aes(shape = variable, colour = variable), size = 4, show.legend = FALSE) +
  ggtitle("F1 Score of Coin Flip Models")
# _______________
