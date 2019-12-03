#' ---
#' title: "Regression and Other Stories: Scalability"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Demonstrate how the computation time scales with bigger data. See
#' Chapter 22 in Regression and Other Stories.
#' 
#' -------------
#'

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' **Load packages**
library(arm)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(tictoc)

#' **Linear regression**
#' 
#' **Create fake data with n=100\,000 and p=100**
SEED <- 1656
set.seed(SEED)
n <- 1e5
k <- 100
x <- rnorm(n)
xn <- matrix(rnorm(n*(k-1)),nrow=n)
a <- 2
b <- 3
sigma <- 10
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, xn, y)

#' **Fit using lm**<br>
tic()
fit1 <- lm(y ~ ., data=fake)
toc()
#display(fit1)

#' **Fit using stan_glm and MCMC**<br>
#' stan_glm is fast for linear regression with n>k and small or
#' moderate k (using OLS trick)
tic()
fit2 <- stan_glm(y ~ ., data=fake, mean_PPD=FALSE,
                 refresh=500, seed=SEED, cores=1)
toc()
#print(fit2, digits=2)

#' **Fit using stan_glm and optimization**<br>
#' Using optimization with normal approximation and Pareto smoothed
#' importance resampling provides coefficient standard errors, but
#' also diagnostic whether normal approximation at the mode is
#' appropriate.
tic()
fit3 <- stan_glm(y ~ ., data=fake, algorithm='optimizing', init=0) 
toc()
#print(fit3, digits=2)

#' **Logistic regression**
#' 
#' **Create fake data with 10\,000 observations and p=100**
SEED <- 1655
set.seed(SEED)
n <- 1e4
k <- 100
x <- rnorm(n)
xn <- matrix(rnorm(n*(k-1)),nrow=n)
a <- 2
b <- 3
sigma <- 1
y <- as.numeric(a + b*x + sigma*rnorm(n) > 0)
fake <- data.frame(x, xn, y)

#' **Fit using glm**<br>
tic()
fit1 <- glm(y ~ ., data=fake, family=binomial())
toc()
#display(fit1)

#' **Fit using stan_glm and MCMC**<br>
tic()
fit2 <- stan_glm(y ~ ., data=fake, family=binomial(), mean_PPD=FALSE,
                 init_r=0.1, seed=SEED)
toc()
#print(fit2, digits=2)

#' **Fit using stan_glm and optimization**<br>
#' Using optimization with normal approximation and Pareto smoothed
#' importance resampling provides coefficient standard errors, but
#' also diagnostic whether normal approximation at the mode is
#' appropriate.
tic()
fit3 <- stan_glm(y ~ ., data=fake, family=binomial(),
                algorithm='optimizing', init=0, draws = 16000, keep_every = 4) 
toc()
#print(fit3, digits=2)

#' **Logistic regression**
#' 
#' **Create fake data with 100\,000 observations and p=100**
SEED <- 1655
set.seed(SEED)
n <- 1e5
k <- 100
x <- rnorm(n)
xn <- matrix(rnorm(n*(k-1)),nrow=n)
a <- 2
b <- 3
sigma <- 1
y <- as.numeric(a + b*x + sigma*rnorm(n) > 0)
fake <- data.frame(x, xn, y)

#' **Fit using glm**<br>
tic()
fit1 <- glm(y ~ ., data=fake, family=binomial())
toc()
#display(fit1)

#' **Fit using stan_glm and optimization**<br>
#' Using optimization with normal approximation and Pareto smoothed
#' importance resampling provides coefficient standard errors, but
#' also diagnostic whether normal approximation at the mode is
#' appropriate.
tic()
fit3 <- stan_glm(y ~ ., data=fake, family=binomial(),
                 algorithm='optimizing', init=0) 
toc()
summary(fit3, digits=2)
