library(ggplot2)  # Plots
library(data.table)  # Melting df

F1ScoreN1 <- readRDS("F1ScoreN1.Rds")
F1ScoreN2 <- readRDS("F1ScoreN2.Rds")
F1ScoreN3 <- readRDS("F1ScoreN3.Rds")
F1ScoreN4 <- readRDS("F1ScoreN4.Rds")

recallN1 <- readRDS("recallN1.Rds")
recallN2 <- readRDS("recallN2.Rds")
recallN3 <- readRDS("recallN3.Rds")
recallN4 <- readRDS("recallN4.Rds")

precisionN1 <- readRDS("precisionN1.Rds")
precisionN2 <- readRDS("precisionN2.Rds")
precisionN3 <- readRDS("precisionN3.Rds")
precisionN4 <- readRDS("precisionN4.Rds")

allSigmaEps <- seq(from = 0, to = 3, length.out = 31)
# _____ F1-Score _____ 
# _____ N = 1 _____
dd <- data.frame(cbind(allSigmaEps, F1ScoreN1))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "F1-Score - #features(M1) = 1", 
       x = "Sigma", y = "F1-Score") +
  labs(col = "Model") 
# _______________
# _____ N = 2 _____
dd <- data.frame(cbind(allSigmaEps, F1ScoreN2))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "F1-Score - #features(M1) = 2", 
       x = "Sigma", y = "F1-Score") +
  labs(col = "Model") 
# _______________
# _____ N = 3 _____
dd <- data.frame(cbind(allSigmaEps, F1ScoreN3))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "F1-Score - #features(M1) = 3", 
       x = "Sigma", y = "F1-Score") +
  labs(col = "Model") 
# _______________
# _____ N = 4 _____
dd <- data.frame(cbind(allSigmaEps, F1ScoreN4))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "F1-Score - #features(M1) = 4", 
       x = "Sigma", y = "F1-Score") +
  labs(col = "Model") 
# _______________

# _____ Precision _____ 
# _____ N = 1 _____
dd <- data.frame(cbind(allSigmaEps, precisionN1))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Precision - #features(M1) = 1", 
       x = "Sigma", y = "Precision") +
  labs(col = "Model") 
# _______________
# _____ N = 2 _____
dd <- data.frame(cbind(allSigmaEps, precisionN2))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Precision - #features(M1) = 2", 
       x = "Sigma", y = "Precision") +
  labs(col = "Model") 
# _______________
# _____ N = 3 _____
dd <- data.frame(cbind(allSigmaEps, precisionN3))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Precision - #features(M1) = 3", 
       x = "Sigma", y = "Precision") +
  labs(col = "Model") 
# _______________
# _____ N = 4 _____
dd <- data.frame(cbind(allSigmaEps, PrecisionN4))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Precision - #features(M1) = 4", 
       x = "Sigma", y = "Precision") +
  labs(col = "Model") 
# _______________

# _____ Recall _____ 
# _____ N = 1 _____
dd <- data.frame(cbind(allSigmaEps, recallN1))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Recall - #features(M1) = 1", 
       x = "Sigma", y = "Recall") +
  labs(col = "Model") 
# _______________
# _____ N = 2 _____
dd <- data.frame(cbind(allSigmaEps, recallN2))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Recall - #features(M1) = 2", 
       x = "Sigma", y = "Recall") +
  labs(col = "Model") 
# _______________
# _____ N = 3 _____
dd <- data.frame(cbind(allSigmaEps, recallN3))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Recall - #features(M1) = 3", 
       x = "Sigma", y = "Recall") +
  labs(col = "Model") 
# _______________
# _____ N = 4 _____
dd <- data.frame(cbind(allSigmaEps, recallN4))
colnames(dd) <- c("Sigma", "M1", "MM")
dd.m <- melt(dd, id.vars = "Sigma")
dd.m$value <- as.numeric(dd.m$value)
colnames(dd.m)[2] <- "Model"

ggplot(dd.m, aes(x = Sigma, y = value, col = Model)) +   
  geom_point(aes(col = Model)) +
  geom_line(aes(col = Model)) +
  labs(title = "Recall - #features(M1) = 4", 
       x = "Sigma", y = "Recall") +
  labs(col = "Model") 
# _______________