## A simple example where Stan is slow
## Simulated data adapted from classic Pearson-Lee data of heights of mothers and daughters

setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/PearsonLee")
library("MASS")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

N <- 1000
mu <- c(63, 63)
sigma <- c(2.5, 2.5)
rho <- 0.5
Sigma <- diag(sigma) %*% cbind(c(1, rho), c(rho, 1)) %*% diag(sigma)
z <- mvrnorm(N, mu, Sigma)
y <- round(z)
fit_2 <- stan("bivariate_normal_rounded_2.stan")

## Now try it without the prior and it's much faster

fit_trivial <- stan("trivial_rounded.stan")

