get_labels_secondary_model <- function(realized_returns, 
                                       predictions_primary_model, 
                                       thr) {
  
  enter <- (predictions_primary_model > thr)
  
  labels <- realized_returns
  labels[] <- 0
  labels[enter == 1 & realized_returns > 0] <- 1
  
  colnames(labels) <- "label"
  
  return(labels)
}