# Returns True Positive Rate and False Positive Rate
get_TPR_and_FPR <- function(signal, realized_returns, prob_model_2, threshold) {
  
  TP <- sum(signal*realized_returns > 0 & prob_model_2 >= threshold)
  TN <- sum(signal*realized_returns < 0 & prob_model_2 < threshold)
  FP <- sum(signal*realized_returns < 0 & prob_model_2 >= threshold)
  FN <- sum(signal*realized_returns > 0 & prob_model_2 < threshold)
  
  TPR <- TP/(TP + FN)
  FPR <- FP/(FP + TN)
  
  return(list("TPR" = TPR, "FPR" = FPR, "precision" = TP/(TP + FP)))  
}

get_ROC <- function(signal, realized_returns, prob_model_2) {
  thresholds <- seq(0, 1, length.out = 20)
  TPR <- c()
  FPR <- c()
  precision <- c()
  
  for (i in thresholds) {
    call_ROC <- get_TPR_and_FPR(signal, realized_returns, prob_model_2, i)
    TPR <- c(TPR, call_ROC$TPR)
    FPR <- c(FPR, call_ROC$FPR)
    precision <- c(precision, call_ROC$precision)
  }
  
  F1 <- 2/(1/TPR + 1/precision)
  
  return(as.data.frame(cbind(thresholds, FPR, TPR, precision, F1)))
}

ROC_wrapper <- function(signal_train, 
                        signal_test, 
                        realized_returns_train,
                        realized_returns_test, 
                        prob_model_2_train, 
                        prob_model_2_test,
                        decile) {
  
  params_train <- get_ROC(signal_train, 
                          realized_returns_train, 
                          prob_model_2_train)
  params_test <- get_ROC(signal_test, 
                         realized_returns_test, 
                         prob_model_2_test)
  
  train_results <- params_train[, c("FPR", "TPR")]
  test_results <- params_test[, c("FPR", "TPR")]
  
  title_decile <- paste("ROC Curve of Decile", decile)
  
  # ggplot() + 
  #   geom_line(data = ruz, aes(date, val1, color = "a"), size = 1.5) + 
  #   geom_line(data = dfr, aes(date, val2 * 100, color = "b"), size = 1.5) + 
  #   scale_color_manual(name = "Colors", 
  #                      values = c("a" = "blue", "b" = "red"))
  
  return(ggplot(train_results, mapping = aes(x = FPR, y = TPR, color = "Train")) +
           geom_line(size = 1.5, alpha = 0.7) +
           geom_line(test_results, mapping = aes(x = FPR, y = TPR, color = "Test"), 
                     size = 1.5, alpha = 0.7) +
           geom_line(mapping = aes(x = seq(0, 1, length.out = 20), 
                                   y = seq(0, 1, length.out = 20), 
                                   color = "Random"),
                     linetype = "dotted") +
           labs(title = title_decile, 
                x = "False Positive Rate", 
                y = "True Positive Rate") + 
           scale_color_manual(name = "", 
                              values = c("Train" = "skyblue2", 
                                         "Test" = "olivedrab2", 
                                         "Random" = "black")))
}


