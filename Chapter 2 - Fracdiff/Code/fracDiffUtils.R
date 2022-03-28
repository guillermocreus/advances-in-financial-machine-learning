getWeights <- function(d, size) {
  if (size <= 1) {
    stop("Size <= 1")
  }
  w <- 1
  for (k in 1:(size - 1)) {
    newWeight <- -(d-k+1)/k*as.numeric(tail(w, 1))
    w <- c(w, newWeight)
  }
  return(w)
}

fracDiff <- function(tSeries, d, th = 0.01) {
  fracDiffSeries <- tSeries
  fracDiffSeries[] <- NA
  w <- getWeights(d = d, size = length(tSeries))
  normCumSumW <- cumsum(abs(w))
  normCumSumW <- normCumSumW/tail(normCumSumW, 1)
  skip <- normCumSumW < 1 - th
  for (i in 1:length(tSeries)) {
    if (!skip[i]) {
      fracDiffSeries[i] <- as.numeric(sum(tSeries[1:i]*rev(w[1:i])))
    }
  }
  return(na.omit(fracDiffSeries))
}

getWeights_FFD <- function(d, th) {
  w <- 1
  k <- 1
  while (TRUE) {
    newWeight <- -(d-k+1)/k*as.numeric(tail(w, 1))
    if (abs(newWeight) < th) {
      break
    } else {
      w <- c(w, newWeight)
    }
    k <- k + 1
  }
  return(w)
}

vectorAdd <- function(v1, v2) {
  # v1 + v2
  if (length(v1) > length(v2)) {
    v2 <- c(v2, rep(0, length(v1) - length(v2)))
    return(v1 + v2)
  }
  
  v1 <- c(v1, rep(0, length(v2) - length(v1)))
  return(v1 + v2)
}

getInverseWeights_FFD <- function(d, th, size) {
  if (size <= 1) {
    stop("Size <= 1")
  }
  
  b <- c(1)
  a <- getWeights_FFD(d, th)
  q <- c(0)
  for (i in 1:size) {
    bInd0 <- min(which(b != 0))
    aInd0 <- min(which(a != 0))
    
    qNow <- rep(0, bInd0)
    qNow[bInd0] <- b[bInd0]/a[aInd0]
    
    q <- vectorAdd(q, qNow)
    
    b <- vectorAdd(b, -rev(polymul(rev(a), rev(qNow))))
  }
  return(q)
}

completeFracDiff_FFD <- function(y, yDiff, d, th) {
  w <- getWeights_FFD(d, th)
  yDiff <- na.omit(fracDiff_FFD(y, d, th))
  aux <- y[1:(length(y) - length(yDiff))]
  
  for (i in 1:(length(y) - length(yDiff))) {
    aux[i] <- as.numeric(sum(y[1:i]*rev(w[1:i])))
  }
  
  yDiff <- c(aux, yDiff)

  return(yDiff)
}

fracDiff_FFD <- function(tSeries, d, th) {
  fracDiffSeries <- tSeries
  fracDiffSeries[] <- NA
  w <- getWeights_FFD(d, th)
  
  if (length(w) > length(tSeries)) 
    stop("length(weights) > length(timeSeries) - try raising the threshold")
  
  for (i in length(w):length(tSeries)) {
    fracDiffSeries[i] <- as.numeric(sum(tSeries[(i - length(w) + 1):i]*rev(w)))
  }
  return(fracDiffSeries)
}

inverseFracDiff_FFD <- function(tSeries, d, th, w = NULL) {
  tSeries <- na.omit(tSeries)
  if (length(tSeries) == 0) stop("Time Series provided contained all NAs")
  
  inverseFracDiffSeries <- tSeries
  inverseFracDiffSeries[] <- NA
  
  if (is.null(w[1])) w <- getInverseWeights_FFD(d, th, length(tSeries))
  
  for (i in 1:length(tSeries)) {
    inverseFracDiffSeries[i] <- as.numeric(sum(tSeries[1:i]*rev(w[1:i])))
  }
  return(inverseFracDiffSeries)
}

sequentialForecast <- function(forecasts, yD, d, th, w = NULL) {
  if (is.null(w[1])) w <- getInverseWeights_FFD(d, th, length(yD))
  
  yForecast <- inverseFracDiff_FFD(yD[1:(length(yD) - length(forecasts))], d, th, 
                                   w = w[1:(length(yD) - length(forecasts))])
  contF <- 1
  for (k in (length(yD) - length(forecasts) + 1):length(yD)) {
    yDAux <- c(yD[1:(k - 1)], forecasts[contF])
    yIntAux <- inverseFracDiff_FFD(yDAux, d, th, w = w[1:length(yDAux)])
    yForecast <- c(yForecast, tail(yIntAux, 1))
    
    contF <- contF + 1
  }
  
  return(yForecast)
}