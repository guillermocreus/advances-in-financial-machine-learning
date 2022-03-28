library(rvest)
library(CVXR)
library(xts)
library(portfolioBacktest)
library(PerformanceAnalytics)
library(TTR)
library(lubridate)
library(imputeFin)
library(ggplot2)

setwd("~/Dropbox/meta-labeling\ (w\ Guillermo)/Code/Current")

plotTimeSeries <- function(prices_portfolios, title = "", 
                           choose = NULL,
                           show_legend = TRUE) {
  if(is.null(choose)) {
    choose <- 1:ncol(prices_portfolios)
  }
  molten_prices <- fortify(prices_portfolios[, choose], melt = TRUE)
  if (show_legend) {
    ggplot(molten_prices, aes(x = Index, y = Value, col = Series)) + 
      geom_line() + xlab(element_blank()) + ylab(element_blank()) +
      ggtitle(title) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y", date_minor_breaks = "8 months")
  }  
  else {
    ggplot(molten_prices, aes(x = Index, y = Value)) + 
      geom_line(color = "#CC6666") + xlab(element_blank()) + ylab(element_blank()) +
      ggtitle(title)
  }
}

# _________________________________________________________________

lookback <- 400

# _________________________________________________________________

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500_changes <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE)

SP500 <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()

SP500 <- SP500[[1]]
SP500_changes <- SP500_changes[[1]]
dates_aux <- SP500_changes$Date[-1]
added <- SP500_changes$Added[-1]
removed <- SP500_changes$Removed[-1]

Tix <- SP500$Symbol
Tix[Tix == "BRK.B"] <- "BRK-B"
Tix[Tix == "BF.B"] <- "BF-B"

GMVP <- function(Sigma) {
  w <- Variable(ncol(Sigma))
  prob <- Problem(Minimize(quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  res <- solve(prob)
  return(as.vector(res$getValue(w)))
}

stock_data <- stockDataDownload(Tix, from = "2000-01-01", to = "2020-09-01",
                                local_file_path = "../../../Otros/")

at_least_1_NA <- function(z) {
  return(1 - prod(1 - is.na(z)))
}

ind_good <- as.vector(which(apply(stock_data$adjusted, 2, at_least_1_NA) == 0))

stock_data_filtered <- lapply(stock_data, function(z) return(z[, ind_good]))

stock_returns <- diff(log(stock_data_filtered$adjusted))[-1]

GMVP_returns <- stock_returns[, 1]
GMVP_returns[] <- NA
colnames(GMVP_returns) <- "GMVP_ret"

for (i in lookback:(nrow(stock_returns) - 1)) {
  # Update the portfolio
  if (year(index(stock_returns)[i - 1]) != year(index(stock_returns)[i]) 
      | i == lookback) {
    X_good <- stock_returns[(1 + i - lookback):i, ]
    Sigma <- cov(X_good)
    w_GMVP <- GMVP(Sigma)
  }
  
  # Calculate returns
  GMVP_returns[i + 1] <- stock_returns[i + 1, ] %*% w_GMVP
}

GMVP_returns <- na.omit(GMVP_returns)
GMVP_returns <- exp(GMVP_returns) - 1
GMVP_prices <- cumprod(1 + GMVP_returns)
y_outliers <- log(GMVP_prices)

plotTimeSeries(y_outliers)

y_imputed <- y_outliers
y_imputed[] <- NA

N_div <- 10

for (i in 1:N_div) {
  left_trn <- as.integer(nrow(y_outliers)*(i - 1)/N_div) + 1
  right_trn <- as.integer(nrow(y_outliers)*i/N_div)

  x1 <- impute_AR1_t(y_outliers[left_trn:right_trn],
                     outlier_prob_th = 0.005,
                     remove_outliers = TRUE)
  
  y_imputed[index(x1)] <- x1
}

dd <- cbind(y_outliers, y_imputed)
colnames(dd) <- c("GMVP Outliers", "GMVP Imputed")

plotTimeSeries(dd, title = "Removal of outliers in GMVP time series")
plotTimeSeries(y_outliers)
plotTimeSeries(y_imputed)

y_imputed <- c(as.xts(0, order.by = index(y_imputed)[1] - 1),
                   y_imputed)

X_log <- diff(y_imputed)[-1]
X <- exp(X_log) - 1

GMVP_returns <- X
colnames(GMVP_returns) <- "GMVP_ret"
save.image(file = "../../../Otros/clean_returns_GMVP.RData")
