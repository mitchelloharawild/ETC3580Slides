library(faraway)
library(tidyverse)
library(forecast)

y <- fpp::credit$income

median(y)
bmedians <- numeric(1000)
for(i in seq_along(bmedians))
  bmedians[i] <- median(sample(y, replace=TRUE))
quantile(bmedians, prob=c(0.025, 0.975))

gghistogram(y, add.normal=TRUE)

gghistogram(log(y), add.normal=TRUE)

logy <- log(y)
mu <- mean(logy)
sigma <- sd(logy)
n <- length(y)
for(i in seq_along(bmedians))
  bmedians[i] <- exp(median(rnorm(n, mu, sigma)))
quantile(bmedians, prob=c(0.025, 0.975))

