get_ROC_point <- function(actual, predicted, threshold) {
  
  predicted <- (predicted > threshold)
  
  TP <- sum(predicted == 1 & actual == 1)
  TN <- sum(predicted == 0 & actual == 0)
  FP <- sum(predicted == 1 & actual == 0)
  FN <- sum(predicted == 0 & actual == 1)
  
  TPR <- TP/(TP + FN)
  FPR <- FP/(FP + TN)
  
  return(c("FPR" = FPR, "TPR" = TPR))
}

get_ROC_curve <- function(train_label, train_predicted,
                          test_label, test_predicted,
                          type_portfolio = "GMVP") {
  
  ROC_train <- ROC_test <- data.frame("FPR" = c(), "TPR" = c())
  thresholds <- seq(from = 0, to = 1, length.out = 20)
  
  for (i in 1:length(thresholds)) {
    new_point <- get_ROC_point(actual = train_label, 
                               predicted = train_predicted,
                               threshold = thresholds[i])
    ROC_train <- rbind(ROC_train, new_point)
    
    new_point <- get_ROC_point(actual = test_label, 
                               predicted = test_predicted,
                               threshold = thresholds[i])
    ROC_test <- rbind(ROC_test, new_point)
  }
  
  colnames(ROC_train) <- colnames(ROC_test) <- c("FPR", "TPR")
  
  print(ggplot(ROC_train, mapping = aes(x = FPR, y = TPR, color = "Train")) +
          geom_line(size = 1.5, alpha = 0.7) +
          geom_line(ROC_test, mapping = aes(x = FPR, y = TPR, color = "Test"), 
                    size = 1.5, alpha = 0.7) +
          geom_line(mapping = aes(x = seq(0, 1, length.out = 20), 
                                  y = seq(0, 1, length.out = 20), 
                                  color = "Random"),
                    linetype = "dotted") +
          labs(title = paste("ROC curve", type_portfolio, "portfolio"), 
               x = "False Positive Rate", 
               y = "True Positive Rate") + 
          scale_color_manual(name = "", 
                             values = c("Train" = "skyblue2", 
                                        "Test" = "olivedrab2", 
                                        "Random" = "black")))
}