setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/DifferentSoftware")

## Linear regression using different software options

## Load everything in
library("arm")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library("rstanarm")

## Create fake data
N <- 100
b <- 1:3
x1 <- rnorm(N)
x2 <- rnorm(N)
X <- cbind(rep(1,N), x1, x2)
sigma <- 2
y <- X %*% b + rnorm(N, 0, sigma)
dat <- data.frame(y, x1, x2)

## Fit and display using lm, listing predictors one at a time
fit1 <- lm(y ~ x1 + x2, data = dat)
display(fit1)

## Extract estimates and uncertainties
b_hat <- coef(fit1)
b_se <- se.coef(fit1)
print(cbind(b_hat, b_se))

## Fit and display using lm, using matrix of predictors
fit2 <- lm(y ~ X)
display(fit2)

## FIt and display using stan_glm
fit3 <- stan_glm(y ~ x1 + x2, data = dat)
print(fit3)
print(fit3, digits=2)
## Run again just to see some simulation variability
fit3 <- stan_glm(y ~ x1 + x2, data = dat)
print(fit3, digits=2)

## Extract estimates and uncertainties
b_hat <- coef(fit3)
b_se <- se(fit3)
print(cbind(b_hat, b_se))

