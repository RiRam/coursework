library(ggplot2)
library(reshape)
library(dplyr)

theme_set(theme_bw())

set.seed(42)
#############################
# Use the estimate_coin_bias(N,p) function that simulates flipping a coin with probability p of landing heads N 
# times and returns an estimate of the bias using the sample mean p_hat
#############################

estimate_coin_bias <- function(n, p) {
  mean(rbinom(n,1,p))
}

n <- 100
p <- 0.3
p_hat <- replicate(1e5, estimate_coin_bias(n, p))

# plot the sampling distribution
qplot(x=p_hat, geom="histogram", binwidth=0.01) +
  geom_vline(xintercept=p) +
  geom_vline(xintercept=mean(p_hat), linetype=2, color="red")

#############################
# Run this simulation 1000 times, for all combinations of N = {10,100,100} and p = {0.1, 0.5, 0.9}
#############################

plot_data <- data.frame()
for (n in c(10, 100, 1000)) {
  for (p in c(0.1, 0.5, 0.9)) {
    tmp <- data.frame(n=n, p_hat=replicate(1e5, estimate_coin_bias(n, p)))
    plot_data <- rbind(plot_data, tmp)
  }
}

#############################
# Plot the distribution of p_hat values for each N, p setting
#############################

qplot(data=plot_data, x=p_hat, geom="histogram", binwidth=0.01, facets = . ~ n)

#############################
# Plot the standard deviation of the p_hat distribution as a function of the sample size N
#############################

plot_data <- mutate(plot_data, stdev = sd(p_hat))
ggplot(data=plot_data, aes(x = p_hat, y = n, color = stdev)) + geom_line()

#############################
# Create one plot of the p_hat distributions, faceted by different N values for p = 0.5 using ggplot
#############################

ggplot(data=plot_data, aes(x = p_hat)) + geom_histogram() + facet_wrap(n)
