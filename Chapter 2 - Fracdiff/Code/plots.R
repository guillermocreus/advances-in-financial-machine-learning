plotTimeSeries <- function(tSeries, title = "", choose = NULL) {
  if(is.null(choose)) {
    choose <- 1:ncol(tSeries)
  }
  moltenDD <- fortify(tSeries[, choose], melt = TRUE)
  
  ggplot(moltenDD, aes(x = Index, y = Value, col = Series)) + 
    geom_line() + xlab(element_blank()) + ylab(element_blank()) +
    ggtitle(title)
}