#' ---
#' title: "Regression and Other Stories: FakeKCV"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     code_download: true
#' ---

#' Demonstration of $K$-fold cross-validation using simulated
#' data. See Chapter 11 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("MASS")      # needed for mvrnorm()
library("rstanarm")
library("loo")
#' Set random seed for reproducability
SEED <- 1754

#' #### Generate fake data
#'
#' $60\times 30$ matrix representing 30 predictors that are random but
#' not independent; rather, we draw them from a multivariate normal
#' distribution with correlations 0.8:
set.seed(SEED)
n <- 60
k <- 30
rho <- 0.8
Sigma <- rho*array(1, c(k,k)) + (1-rho)*diag(k)
X <- mvrnorm(n, rep(0,k), Sigma)
b <- c(c(-1, 1, 2), rep(0,k-3))
y <- X %*% b + rnorm(n)*2
fake <- data.frame(X, y)

#' #### Weakly informative prior
fit_1 <- stan_glm(y ~ ., prior=normal(0, 10, autoscale=FALSE),
                  data=fake, seed=SEED, refresh=0)
(loo_1 <- loo(fit_1))
#' In this case, Pareto smoothed importance sampling LOO fails, but
#' the diagnostic recognizes this with many high Pareto k values. We
#' can run slower, but more robust K-fold-CV
kfold_1 <- kfold(fit_1)

#' #### An alternative weakly informative prior<br>
#' The regularized horseshoe prior `hs()` is weakly informative,
#' stating that it is likely that only small number of predictors are
#' relevant, but we don't know which ones.
k0 <- 2 # prior guess for the number of relevant variables
tau0 <- k0/(k-k0) * 1/sqrt(n)
hs_prior <- hs(df=1, global_df=1, global_scale=tau0, slab_scale=3, slab_df=7)
fit_2 <- stan_glm(y ~ ., prior=hs_prior, data=fake, seed=SEED,
                  control=list(adapt_delta=0.995), refresh=200)
(loo_2 <- loo(fit_2))
#' PSIS-LOO performs better now, but there is still one bad diagnostic
#' value, and thus we run again slower, but more robust K-fold-CV
kfold_2 <- kfold(fit_2)

#' #### Comparison of models
loo_compare(loo_1,loo_2)
loo_compare(kfold_1,kfold_2)

#' As PSIS-LOO fails, PSIS-LOO comparison underestimates the
#' difference between the models. The Pareto k diagnostic correctly
#' identified the problem, and more robust K-fold-CV shows that by
#' using a better prior we can get better predictions.
