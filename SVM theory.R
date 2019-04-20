# core principle of svm - marian maximization

#The best decision boundary is one that maximizes the margin: maximal margin separator
#Maximal margin separator lies halfway between the two clusters.


# create a unifomly distributed dataset n = 600, value between 0,1

#set seed
set.seed(42)
#set number of data points. 
n <- 600
#Generate data frame with two uniformly distributed predictors lying between 0 and 1.
df <- data.frame(x1 = runif(n, min = 0, max = 1), 
                 x2 = runif(n, min = 0, max = 1))

#classify data points depending on location
df$y <- factor(ifelse(df$x2 - 1.4*df$x1 < 0, -1, 1), 
               levels = c(-1, 1))

#set margin
delta <- 0.07

# retain only those points that lie outside the margin
df1 <- df[abs(1.4*df$x1 - df$x2) > delta, ]

#build plot
plot_margins <- ggplot(data = df1, aes(x = x1, y = x2, color = y)) + geom_point() + 
  scale_color_manual(values = c("red", "blue")) + 
  geom_abline(slope = 1.4, intercept = 0)+
  geom_abline(slope = 1.4, intercept = delta, linetype = "dashed") +
  geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed")

#display plot 
plot_margins


