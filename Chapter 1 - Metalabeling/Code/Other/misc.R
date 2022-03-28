merge_data <- function(features_, labels_) {
  data <- list()
  for (i in 1:length(labels_)) {
    data[[i]] <- as.data.frame(na.omit(cbind(features_[[i]], labels_[[i]])))
    colnames(data[[i]])[ncol(data[[i]])] <- "label"
    # rownames(data[[i]]) <- NULL
  }
  return(data)
}

split_data <- function(data) {
  train_data <- list()
  test_data <- list()
  
  for (i in 1:length(data)) {
    N_total <- nrow(data[[i]])
    n_split <- as.integer(0.7*N_total)
    
    train_data[[i]] <- data[[i]][1:n_split, ]
    test_data[[i]] <- data[[i]][(n_split + 1):N_total, ]
  }
  return(list("train" = train_data, "test" = test_data))
}

split_returns <- function(returns) {
  train_returns <- list()
  test_returns <- list()
  
  returns <- lapply(returns, na.omit)
  for (i in 1:length(returns)) {
    N_total <- length(returns[[i]])
    n_split <- as.integer(0.7*N_total)
    
    train_returns[[i]] <- returns[[i]][1:n_split, ]
    test_returns[[i]] <- returns[[i]][(n_split + 1):N_total, ]
  }
  
  return(list("train" = train_returns,
              "test" = test_returns))
}

SR_meta_model <- function(returns, probabilities, signal, exit, threshold) {
  threshold <- max(min(1, threshold), 0)
  # print(assertthat::are_equal(length(returns), length(signal)))
  # print(assertthat::are_equal(length(returns), length(probabilities)))
  returns <- returns*signal
  
  enter <- non_overlap <- probabilities
  enter[] <- NaN
  non_overlap[] <- 0
  
  last <- as.numeric(as.Date("1900-01-01"))
  for (i in 1:length(non_overlap)) {
    if (as.numeric(index(returns)[i]) > last) {
      non_overlap[i] <- 1
      last <- exit[i]
    }
  }
  
  enter[probabilities >= threshold & non_overlap == 1] <- 1
  
  returns <- na.omit(enter*returns)
  number_opportunities <- length(returns)
  number_years <- as.numeric(index(returns)[length(returns)] - index(returns)[1])/365
  
  # print(mean(returns))
  # print(median(returns))
  # print(mean(returns)*number_opportunities/number_years)
  # print(sd(returns))
  return(mean(returns)/sd(returns)*sqrt(number_opportunities/number_years))
}

returns_meta_model <- function(returns, probabilities, signal, exit, threshold) {
  threshold <- max(min(1, threshold), 0)
  returns <- returns*signal
  
  enter <- non_overlap <- probabilities
  enter[] <- NaN
  non_overlap[] <- 0
  
  last <- as.numeric(as.Date("1900-01-01"))
  for (i in 1:length(non_overlap)) {
    if (as.numeric(index(non_overlap)[i]) > last) {
      non_overlap[i] <- 1
      last <- exit[i]
    }
  }
  
  enter[probabilities >= threshold & non_overlap == 1] <- 1
  
  returns <- na.omit(enter*returns)
  return(returns)
}

SR_strategy <- function(returns) {
  SR <- mean(returns)/sd(returns)

  number_opportunities <- length(returns)
  last_day <- tail(index(returns), 1)
  first_day <- index(returns)[1]
  number_years <- as.numeric(last_day - first_day)/365
  
  SR <- SR*sqrt(number_opportunities/number_years)
  
  return(SR)
}

get_RMSE <- function(random_forests, test_df) {

  RMSE <- data.frame("Decile" = 1:10,
                     "RMSE_train" = 1:10,
                     "RMSE_test" = 1:10)

  for (i in 1:length(random_forests)) {
    prob_meta_model_train <- random_forests[[i]]$finalModel$predicted
    prob_meta_model_test <- as.vector(predict(random_forests[[i]],
                                              newdata = test_df[[i]]))
    
    RMSE$RMSE_train[i] <- min(random_forests[[i]]$results$RMSE)
    RMSE$RMSE_test[i] <- sqrt(mean((prob_meta_model_test - test_df[[i]]$label)^2))
  }

  return(RMSE)

}

SR_wrapper <- function(random_forests, train_df, test_df,
                       train_returns, test_returns,
                       train_exit_time, test_exit_time) {
  
  thresholds <- seq(0, 1, length.out = 20)
  base_NaN <- rep(NaN, length(thresholds))
  SR_train <- data.frame("threshold" = thresholds,
                         "dec01" = base_NaN, "dec02" = base_NaN, "dec03" = base_NaN,
                         "dec04" = base_NaN, "dec05" = base_NaN, "dec06" = base_NaN,
                         "dec07" = base_NaN, "dec08" = base_NaN, "dec09" = base_NaN,
                         "dec10" = base_NaN)

  SR_results <- data.frame("dec01" = 1:5, "dec02" = 1:5, "dec03" = 1:5, "dec04" = 1:5,
                         "dec05" = 1:5, "dec06" = 1:5, "dec07" = 1:5, "dec08" = 1:5,
                         "dec09" = 1:5, "dec10" = 1:5)

  rownames(SR_results) <- c("SR_benchmark_train", "SR_train","SR_benchmark_test",
                            "SR_test","best_thr")

  for (i in 1:length(random_forests)) {
    max_SR_train <- -Inf
    best_thr <- NULL
    for (j in 5:(length(thresholds) - 5)) {
      SR <- SR_meta_model(returns = train_returns[[i]],
                          probabilities = random_forests[[i]]$finalModel$predicted,
                          signal = train_df[[i]]$signal,
                          exit = train_exit_time[[i]],
                          threshold = thresholds[j])
      
      if (length(SR) > 0) {
        if(!is.null(SR) & !is.na(SR) & (SR > max_SR_train)) {
          max_SR_train <- SR
          best_thr <- thresholds[j]
        }
      }
      
      SR_train[j, i + 1] <- SR
    }
    
    prob_meta_model_test <- as.vector(predict(random_forests[[i]],                      
                                              newdata = test_df[[i]]))
    
    SR_test <- SR_meta_model(returns = test_returns[[i]],
                             probabilities = prob_meta_model_test,
                             signal = test_df[[i]]$signal,
                             exit = test_exit_time[[i]],
                             threshold = best_thr)
    
    # Benchmark train
    SR_results[1, i] <- SR_meta_model(returns = train_returns[[i]],
                                      probabilities = rep(2, length(train_returns[[i]])),
                                      signal = train_df[[i]]$signal,
                                      exit = train_exit_time[[i]],
                                      threshold = 0.5)
    
    # Max. SR in train
    SR_results[2, i] <- max_SR_train
    
    # Benchmark test
    SR_results[3, i] <- SR_meta_model(returns = test_returns[[i]],
                                      probabilities = rep(2, length(test_returns[[i]])),
                                      signal = test_df[[i]]$signal,
                                      exit = test_exit_time[[i]],
                                      threshold = 0.5)
    # SR in test
    SR_results[4, i] <- SR_test
    # Threshold
    SR_results[5, i] <- best_thr
  }
  
  # print(SR_train, digits = 3)
  # print(SR_results, digits = 3)
  
  return(list("train" = SR_train, "results" = SR_results))
}

get_all_returns <- function(random_forests, train_df, test_df, 
                            train_returns, test_returns,
                            train_exit_time, test_exit_time, best_thr) {
  all_ret <- NULL
  cols <- 10
  if (!is.null(random_forests))
    cols <- length(random_forests)
  
  for (i in 1:cols) {
    prob_trn <- rep(2, nrow(train_df[[i]]))
    prob_tst <- rep(2, nrow(test_df[[i]]))
    
    if(!is.null(random_forests)) {
      prob_trn <- random_forests[[i]]$finalModel$predicted
      prob_tst <- as.vector(predict(random_forests[[i]], newdata = test_df[[i]]))
    }
    
    train_ret_meta <- returns_meta_model(returns = train_returns[[i]],
                                         probabilities = prob_trn,
                                         signal = train_df[[i]]$signal,
                                         exit = train_exit_time[[i]],
                                         threshold = best_thr[i])
    
    test_ret_meta <- returns_meta_model(returns = test_returns[[i]],
                                        probabilities = prob_tst,
                                        signal = test_df[[i]]$signal,
                                        exit = test_exit_time[[i]],
                                        threshold = best_thr[i])
    
    if (i > 1) 
      all_ret <- cbind(all_ret, rbind(train_ret_meta, test_ret_meta))
    else 
      all_ret <- rbind(train_ret_meta, test_ret_meta)
  }
  
  clean_NaN <- function(z) {
    z[is.na(z)] <- 0
    return(z)
  }
  
  all_ret <- apply(all_ret, 2, clean_NaN)
  all_ret <- as.xts(all_ret, order.by = as.Date(rownames(all_ret)))
  return(all_ret)
}

get_annual_returns <- function(ret_BH, ret_glab, ret_prim, ret_meta, start_test) {
  n_years <- as.numeric(tail(index(ret_BH), 1) - start_test)/365
  
  ret_BH <- 
    as.vector(tail(cumprod(1 + ret_BH[index(ret_BH) >= start_test]), 1))^(1/n_years)
  
  ret_glab <- 
    as.vector(tail(cumprod(1 + ret_glab[index(ret_glab) >= start_test]), 1))^(1/n_years)
  
  ret_prim <- 
    as.vector(tail(cumprod(1 + ret_prim[index(ret_prim) >= start_test]), 1))^(1/n_years)
  
  ret_meta <- 
    as.vector(tail(cumprod(1 + ret_meta[index(ret_meta) >= start_test]), 1))^(1/n_years)
  
  dd <- cbind(100*(ret_BH - 1), 100*(ret_glab - 1), 
              100*(ret_prim - 1), 100*(ret_meta - 1))
  colnames(dd) <- c("BH", "Glab", "Primary", "Meta")
  rownames(dd) <- 1:10
  print(dd)
}