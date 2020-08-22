#' ---
#' title: "Regression and Other Stories: KidIQ cross-validation"
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

#' Linear regression and leave-one-out cross-validation. See Chapter 11 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")
library("loo")
library("foreign")
# for reproducability
SEED <- 1507

#' #### Load children's test scores data
kidiq <- read.csv(root("KidIQ/data","kidiq.csv"))
head(kidiq)

#' ## Linear regression
#' 
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq,
                  seed=SEED, refresh = 0)
print(fit_3)

#' #### Estimate the predictive performance of a model
#' using within-sample plug-in (ie with mean parameters) log-score
pluginlogscore_3 <- sum(dnorm(kidiq$kid_score, fitted(fit_3), sigma(fit_3), log = TRUE))
round(pluginlogscore_3, 1)

#' #### Estimate the predictive performance of a model
#' using within-sample posterior predictive (ie integrating over parameters) log-score
sigmas <- as.matrix(fit_3)[,'sigma']
preds <- posterior_linpred(fit_3)
nsims <- nrow(preds)
logscore_3 <- sum(log(rowMeans(sapply(1:nsims, FUN = function(i) dnorm(kidiq$kid_score, preds[i,], sigmas[i], log=FALSE)))))
round(logscore_3, 1)

#' ## Add five pure noise predictors to the data
set.seed(SEED)
n <- nrow(kidiq)
kidiqr <- kidiq
kidiqr$noise <- array(rnorm(5*n), c(n,5))

#' #### Linear regression with additional noise predictors
fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr,
                   seed=SEED, refresh = 0)
print(fit_3n)

#' #### Estimate the predictive performance of a model
#' using within-sample plug-in (ie with mean parameters) log-score
pluginlogscore_3n <- sum(dnorm(kidiq$kid_score, fitted(fit_3n), sigma(fit_3n), log = TRUE))
round(pluginlogscore_3n, 1)

#' #### Estimate the predictive performance of a model
#' using within-sample posterior predictive (ie integrating over parameters) log-score
sigmas <- as.matrix(fit_3n)[,'sigma']
preds <- posterior_linpred(fit_3n)
logscore_3n <- sum(log(rowMeans(sapply(1:nsims, FUN = function(i) dnorm(kidiq$kid_score, preds[i,], sigmas[i], log=FALSE)))))
round(logscore_3n, 1)

#' #### Compare models with within-sample plug-in log scores
round(pluginlogscore_3n - pluginlogscore_3, 1)

#' #### Compare models with within-sample posterior predictive log scores
round(logscore_3n - logscore_3, 1)

#' ## Compare models with LOO-CV
#' 

#' #### Estimate the predictive performance of models using LOO-CV
loo_3 <- loo(fit_3)
print(loo_3)
loo_3n <- loo(fit_3n)
print(loo_3n)
loo_compare(loo_3, loo_3n)

#' #### Linear regression with different predictors
fit_1 <- stan_glm(kid_score ~ mom_hs, data=kidiq,
                  seed = SEED, refresh = 0)
loo_1 <- loo(fit_1)
print(loo_1)
loo_compare(loo_3, loo_1)

#' #### Linear regression with interaction
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_iq:mom_hs,
                  data=kidiq, seed=SEED, refresh = 0)
print(fit_4)
loo_4 <- loo(fit_4)
print(loo_4)
loo_compare(loo_3, loo_4)

#' #### Linear regression with log-transformation and interaction
fit_5 <- stan_glm(kid_score ~ mom_hs + log(mom_iq) + log(mom_iq):mom_hs,
                  data=kidiq, seed=SEED, refresh = 0)
print(fit_5)
loo_5 <- loo(fit_5)
print(loo_5)
loo_compare(loo_3, loo_5)

#' #### Compare several models
loo_compare(loo_1, loo_3, loo_4, loo_5)

