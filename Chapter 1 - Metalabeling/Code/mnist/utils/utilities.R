getMetrics <- function(actual, predicted) {
  
  TP <- sum(predicted == 1 & actual == 1)
  FN <- sum(predicted == 0 & actual == 1)
  FP <- sum(predicted == 1 & actual == 0)
  TN <- sum(predicted == 0 & actual == 0)
  
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  f1Score <- 2/(1/recall + 1/precision)
  return(list("precision" = precision,
              "recall" = recall,
              "f1Score" = f1Score))
}