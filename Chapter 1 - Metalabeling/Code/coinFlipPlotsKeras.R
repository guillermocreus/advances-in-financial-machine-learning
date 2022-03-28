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
load(file = "~/Dropbox/Otros/coinFlipPrimary.RData")
load(file = "~/Dropbox/Otros/coinFlipMetaKeras.RData")

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

probabilities <- seq(0, 0.5, length.out = length(primaryModels))

coinFlipPlots <- list()
coinFlipM1Predictions <- coinFlipM2Predictions <- list()
coinFlipM1Returns <- coinFlipMMReturns <- list()

for (i in 1:length(primaryModels)) {
  
  pBernoulli <- probabilities[i]
  
  # Predictions
  
  predictedTestM1 <- allPredictedTestM1[[i]]
  predictedTestM2 <- kerasPredictionsTest[[i]]
  
  # ________ Plots ________
  
  returnsM1 <- get_full_returns(returns = GMVP_returns,
                                enter = (predictedTestM1 > -Inf),
                                window_length = window_length,
                                max_time = holding_time,
                                tc = tc,
                                signal = 2*round(predictedTestM1) - 1)
  
  returnsMM <- get_full_returns(returns = GMVP_returns,
                                enter = (predictedTestM2 == 1),
                                window_length = window_length,
                                max_time = holding_time,
                                tc = tc,
                                signal = 2*round(predictedTestM1) - 1)
  

  coinFlipM1Returns[[i]] <- returnsM1
  coinFlipMMReturns[[i]] <- returnsMM
  
  startT1.1 <- index(returnsM1)[1]
  startT1.2 <- index(returnsMM)[1]
  startT1 <- min(startT1.1, startT1.2)
  startT1 <- match(startT1, index(GMVP_returns))
  
  endT1.1 <- tail(index(returnsM1), 1)
  endT1.2 <- tail(index(returnsMM), 1)
  endT1 <- max(endT1.1, endT1.2)
  endT1 <- match(endT1, index(GMVP_returns))
  
  retBH <- retBase <- GMVP_returns[startT1:endT1]
  retBase[] <- 0
  
  returnsM1Graph <- returnsMMGraph <- retBase
  returnsM1Graph[index(returnsM1)] <- returnsM1
  returnsMMGraph[index(returnsMM)] <- returnsMM
    
  prices2Graph <- cbind(cumprod(1 + retBH),
                        cumprod(1 + returnsM1Graph),
                        cumprod(1 + returnsMMGraph))
  
  colnames(prices2Graph) <- c("B&H", "M1", "MM")
  
  moltenPrices <- fortify(prices2Graph, melt = TRUE)
  coinFlipPlots[[i]] <- ggplot(moltenPrices, 
                               aes(x = Index, y = Value, col = Series)) + 
    geom_line() + xlab(element_blank()) + ylab(element_blank()) +
    ggtitle(paste("Coin flip model (p = ",
                  pBernoulli, ")", sep = "")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  print(coinFlipPlots[[i]])
  # _______________________
}

ggarrange(
  coinFlipPlots[[1]], coinFlipPlots[[2]],
  coinFlipPlots[[3]], coinFlipPlots[[4]],
  ncol = 2, nrow = 2, common.legend = TRUE,
  legend = "bottom"
)

ggarrange(
  coinFlipPlots[[5]], coinFlipPlots[[6]],
  coinFlipPlots[[7]], coinFlipPlots[[8]],
  ncol = 2, nrow = 2, common.legend = TRUE,
  legend = "bottom"
)

ggarrange(
  coinFlipPlots[[9]], coinFlipPlots[[10]],
  coinFlipPlots[[11]],
  ncol = 2, nrow = 2, common.legend = TRUE, 
  legend = "bottom"
)

save(retBH, coinFlipM1Returns, coinFlipMMReturns,
     coinFlipM1Predictions, coinFlipM2Predictions,
     file = "~/Dropbox/Otros/coinFlipReturns.RData")

# _____ SR Charts _____

allSrTestM1 <- unlist(lapply(coinFlipM1Returns, SR_strategy))
allSrTestMM <- unlist(lapply(coinFlipMMReturns, SR_strategy))

probabilitiesChar <- as.character(probabilities)
probabilitiesChar[1] <- "0.00"
ind2Correct <- seq(3, 11, length.out = 5)
probabilitiesChar[ind2Correct] <- 
  paste(probabilitiesChar[ind2Correct], "0", sep = "")

dd <- data.frame(cbind(probabilitiesChar, allSrTestM1, allSrTestMM))
colnames(dd) <- c("pCoin", "Primary Model", "Meta Model")
dd.m <- melt(dd, id.vars = "pCoin")
dd.m$value <- as.numeric(dd.m$value)

SrTest <- round(0.9513077, 2)

ggplot(dd.m, aes(pCoin, value)) +   
  geom_bar(aes(fill = variable), position = position_dodge(width = 0.8),
           stat = "identity", width = 0.8) +
  geom_hline(yintercept = c(SrTest), linetype = "dotted") +
  geom_text(aes(0, SrTest, label = "B&H", hjust = -0.2, vjust = -0.5)) +
  labs(title = "Sharpe Ratio of Coin Flip Models", 
       x = "Probability (Coin Flip)", y = "Sharpe Ratio") +
  labs(fill = "Model") +
  scale_y_continuous(breaks = c(seq(from = 0, to = 2.5, by = 0.5), SrTest))

dd.m2 <- dd.m
dd.m2$pCoin <- probabilities
dd.m2$value <- as.numeric(dd.m2$value)

ggplot(dd.m2, aes(x = pCoin, y = value, col = variable)) +   
  geom_line(aes(col = variable), size = 2) +
  geom_point(aes(shape = variable, col = variable), size = 4, show.legend = FALSE) +
  geom_hline(yintercept = SrTest, linetype = "dotted", size = 2) +
  geom_text(aes(x = 0, y = SrTest, label = "B&H", hjust = -0.2, vjust = -0.5), 
            colour = "black", size = 5) +
  labs(title = "Sharpe Ratio of Coin Flip Models", 
       x = "Probability (Coin Flip)", y = "Sharpe Ratio") +
  labs(col = "Model") +
  scale_y_continuous(breaks = c(seq(from = 0, to = 2.5, by = 0.5), SrTest),
                     minor_breaks = seq(from = 0, to = 2.5, by = 0.25))



