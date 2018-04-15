#' ---
#' title: "Regression and Other Stories: FakeKCV"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Demonstration of $K$-fold cross-validation using simulated data
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("MASS")      # needed for mvrnorm()
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Generate fake data**
#'
#' $60\times 30$ matrix representing 30 predictors that are random but
#' not independent; rather, we draw them from a multivariate normal
#' distribution with correlations 0.8:
set.seed(1754)
n <- 60
k <- 30
rho <- 0.8
Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)
X <- mvrnorm(n, rep(0,k), Sigma)
b <- c(c(-1, 1, 2), rep(0,k-3))
y <- X %*% b + rnorm(n)*2
fake <- data.frame(X, y) 
fit_1 <- stan_glm(y ~ .,
                  prior=normal(0, 10, autoscale=FALSE), data=fake)
loo_1 <- loo(fit_1)
kfold_1 <- kfold(fit_1)

#' The regularized horseshoe prior `hs()` is weakly informative,
#' stating that it is likely that only small number of predictors are
#' relevant, but we don't know which ones.
fit_2 <- update(fit_1, prior=hs())
kfold_2 <- kfold(fit_2)
compare_models(kfold_1,kfold_2)
