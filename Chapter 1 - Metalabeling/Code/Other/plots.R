plot_portfolios <- function(prices_portfolios, title = "", 
                            choose = NULL,
                            show_legend = TRUE) {
  if(is.null(choose)) {
    choose <- 1:ncol(prices_portfolios)
  }
  molten_prices <- fortify(prices_portfolios[, choose], melt = TRUE)
  if (show_legend) {
    ggplot(molten_prices, aes(x = Index, y = Value, col = Series)) + 
      geom_line() + xlab(element_blank()) + ylab(element_blank()) +
      ggtitle(title)
  }  
  else {
    ggplot(molten_prices, aes(x = Index, y = Value)) + 
      geom_line(color = "#CC6666") + xlab(element_blank()) + ylab(element_blank()) +
      ggtitle(title)
  }
}

plot_MA <- function(prices, MA, title = "") {
  prices_to_plot <- cbind(prices,MA)
  colnames(prices_to_plot) <- c("prices", "MA")
  molten_prices <- fortify(prices_to_plot, melt = TRUE)
  ggplot(molten_prices, aes(x = Index, y = Value, col = Series)) + 
    geom_line() + xlab(element_blank()) + ylab(element_blank()) +
    ggtitle(title)
}

plot_MA_2 <- function(benchmark, prices, MA, title = "") {
  prices_to_plot <- cbind(MA, prices, benchmark)
  colnames(prices_to_plot) <- c("MA", "prices strategy", "benchmark")
  molten_prices <- fortify(prices_to_plot, melt = TRUE)
  ggplot(molten_prices, aes(x = Index, y = Value, col = Series)) + 
    geom_line() + xlab(element_blank()) + ylab(element_blank()) +
    ggtitle(title)
}

print_ROCs <- function(random_forests, train_df, test_df, 
                       train_returns, test_returns) {

	ROC_plots <- list()
	for (i in 1:length(random_forests)) {
	  prob_meta_model_train <- random_forests[[i]]$finalModel$predicted
	  prob_meta_model_test <- as.vector(predict(random_forests[[i]],
	                                            newdata = test_df[[i]]))
	  
	  ROC_plots[[i]] <- ROC_wrapper(signal_train = train_df[[i]]$signal,
	                                signal_test = test_df[[i]]$signal, 
	                                realized_returns_train = train_returns[[i]],
	                                realized_returns_test = test_returns[[i]],
	                                prob_model_2_train = prob_meta_model_train,
	                                prob_model_2_test = prob_meta_model_test,
	                                decile = i)
	}
	
	grid.arrange(ROC_plots[[1]], ROC_plots[[2]],
	             ROC_plots[[3]], ROC_plots[[4]],
	             ROC_plots[[5]], ROC_plots[[6]],
	             ROC_plots[[7]], ROC_plots[[8]],
	             ROC_plots[[9]], ROC_plots[[10]],
	             ncol = 2, nrow = 5)
}

get_Sharpe_Ratio_barchart <- function(SR_results, factor = "") {
	dd <- t(as.matrix(SR_results))
	dd <- data.frame(dd, "Names" = rownames(dd)) # Deciles
	dd <- dd[, c("SR_BH", "SR_Glabad.", "SR_benchmark_test", "SR_test", "Names")]
	colnames(dd) <- c("Buy & Hold",
	                  "Glabadanidis",
	                  "Primary Model",
	                  "Meta Model",
	                  "Names")
	dd.m <- melt(dd[1:5, ], id.vars = "Names")
	
	print(ggplot(dd.m, aes(Names, value)) +   
	  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
	  labs(title = paste("Sharpe Ratio (Test) -", factor, "- Dec. 1:5"), 
	       x = "", y = "Sharpe Ratio"))
	
	dd.m <- melt(dd[6:10, ], id.vars = "Names")
	
	print(ggplot(dd.m, aes(Names, value)) +   
	  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
	  labs(title = paste("Sharpe Ratio (Test) -", factor, "- Dec. 6:10"), 
	       x = "", y = "Sharpe Ratio"))
}

get_Drawdown_barchart <- function(ret_BH, ret_glab, ret_prim, ret_meta, 
                                  start_test, factor = "") {
  
  dd <- cbind(apply(ret_BH[index(ret_BH) >= start_test], 2, maxDrawdown),
              apply(ret_glab[index(ret_glab) >= start_test], 2, maxDrawdown),
              apply(ret_prim[index(ret_prim) >= start_test], 2, maxDrawdown),
              apply(ret_meta[index(ret_meta) >= start_test], 2, maxDrawdown))
        
  dd <- data.frame(-100*dd, "Names" = rownames(dd))
  colnames(dd) <- c("Buy & Hold",
                    "Glabadanidis",
                    "Primary Model",
                    "Meta Model",
                    "Names")
  
  dd.m <- melt(dd[1:5, ], id.vars = "Names")
  
  print(ggplot(dd.m, aes(Names, value)) +   
          geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
          labs(title = paste("Maximum Drawdown (Test) -", factor, "- Dec. 1:5"), 
               x = "", y = "Max. Drawdown (%)"))
  
  dd.m <- melt(dd[6:10, ], id.vars = "Names")
  
  print(ggplot(dd.m, aes(Names, value)) +   
          geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
          labs(title = paste("Maximum Drawdown (Test) -", factor, "- Dec. 6:10"), 
               x = "", y = "Max. Drawdown (%)"))
}