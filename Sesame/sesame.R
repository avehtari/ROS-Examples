#' ---
#' title: "Regression and Other Stories: Sesame street"
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

#' Causal analysis of Sesame Street experiment. See Chapters 18 and 21
#' in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")
library("brms")

#' Set random seed for reproducability
SEED <- 1234

#' #### Load data
sesame <- read.csv(root("Sesame/data","sesame.csv"))
head(sesame)

#' ## Compliance
(sesame_tab <- table(sesame[,c('watched','encouraged')]))
round(prop.table(sesame_tab, margin=2), digits=2)

#' ## Wald estimator
#' 
#' Estimate the intent-to-treat (ITT) effect of the instrument
#' (encouragement) on the treatment (regular watching), that is,
#' percentage of children actually induced to watch Sesame Street by
#' the intervention
itt_zt <- stan_glm(watched ~ encouraged, data=sesame, seed=SEED, refresh=0)
print(itt_zt, digits=2)

#' Estimate the intent-to-treat (ITT) estimate on the outcome
#' (post-treatment letter identification)
itt_zy <- stan_glm(postlet ~ encouraged, data=sesame, refresh=0)
print(itt_zy, digits=1)

#' Calculate Wald estimate, ie the ratio of the above two estimates
wald_est <- coef(itt_zy)["encouraged"] / coef(itt_zt)["encouraged"]
round(wald_est, digits=1)

#' ## Two stage approach
#'
#' #### Predict the "treatment" variable on the randomized instrument
#' 
#' The first step is to regress the "treatment" variable---an
#' indicator for regular watching (watched)---on the randomized
#' instrument, encouragement to watch (encouraged).
fit_2a <- stan_glm(watched ~ encouraged, data=sesame, seed=SEED, refresh=0)
print(fit_2a, digits=2)
summary(fit_2a$fitted, digits=2)
sesame$watched_hat <- fit_2a$fitted
#' Then we plug predicted values of watched into the equation
#' predicting the letter recognition outcome.
fit_2b <- stan_glm(postlet ~ watched_hat, data=sesame, seed=SEED, refresh=0)
print(fit_2b, digits = 1)

#' ## Two stage approach with instrumental variables
#'
#' Two stage approach with adjusting for covariates in an instrumental variables framework.
#'
#' #### Predict the "treatment" variable on the randomized instrument and pre-treatment variables.
#' 
#' The first step is to regress the "treatment" variable on the
#' randomized instrument, encouragement to watch (encouraged) and
#' pre-treatment variables.
fit_3a <- stan_glm(watched ~ encouraged + prelet + as.factor(site) + setting,
                   data=sesame, seed=SEED, refresh=0)
print(fit_3a, digits=2)
summary(fit_3a$fitted, digits=2)
#' Then we plug predicted values of watched into the equation
#' predicting the letter recognition outcome.
watched_hat_3 <- fit_3a$fitted
fit_3b <- stan_glm(postlet ~ watched_hat_3 + prelet + as.factor(site) + setting,
                   data=sesame, seed=SEED, refresh=0)
print(fit_3b, digits = 1)

#' #### Estimate the standard errors
#'
#' Use the predictor matrix from this second-stage regression.
X_adj <- X <- model.matrix(fit_3b)
X_adj[,"watched_hat_3"] <- sesame$watched
n <- nrow(X)
p <- ncol(X)
#' #### Compute the standard deviation of the adjusted residuals
RMSE1 <- sqrt(sum((sesame$postlet - X %*% coef(fit_3b))^2)/(n-p))
RMSE2 <- sqrt(sum((sesame$postlet - X_adj %*% coef(fit_3b))^2)/(n-p))
se_adj <- se(fit_3b)["watched_hat_3"] * sqrt(RMSE1 / RMSE2)
print(se_adj, digits=2)

#' ## Two-stage approach with brms
#'
#' #### Predict the "treatment" variable on the randomized instrument
f1 <- bf(watched ~ encour)
f2 <- bf(postlet ~ watched)
#+ results='hide'
IV_brm_a <- brm(f1 + f2, data=sesame, seed=SEED)
#+
print(IV_brm_a, digits=1)

#' #### Incorporate other pre-treatment variables as controls
f1 <- bf(watched ~ encour + prelet + setting + factor(site))
f2 <- bf(postlet ~ watched + prelet + setting + factor(site))
#+ results='hide'
IV_brm_b <- brm(f1 + f2, data=sesame, seed=SEED)
#+
print(IV_brm_b, digits=1)
