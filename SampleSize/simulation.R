#' ---
#' title: "Regression and Other Stories: Sample size simulation"
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

#' Sample size simulation. See Chapter 16 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")

#' #### Simulated data 1: predictor range (-0.5, 0.5)
N <- 1000
sigma <- 10
y <- rnorm(N, 0, sigma)
x1 <- sample(c(-0.5,0.5), N, replace=TRUE)
x2 <- sample(c(-0.5,0.5), N, replace=TRUE)
fake <- data.frame(c(y,x1,x2))

#' #### Fit models
fit_1a <- stan_glm(y ~ x1, data = fake, refresh = 0)
fit_1b <- stan_glm(y ~ x1 + x2 + x1:x2, data = fake, refresh = 0)
print(fit_1a)
print(fit_1b)

#' #### Simulated data 2: predictor range (0, 1)
x1 <- sample(c(0,1), N, replace=TRUE)
x2 <- sample(c(0,1), N, replace=TRUE)

#' #### Fit models
fit_2a <- stan_glm(y ~ x1, data = fake, refresh = 0)
fit_2b <- stan_glm(y ~ x1 + x2 + x1:x2, data = fake, refresh = 0)
print(fit_2a)
print(fit_2b)


#' #### Simulated data 2: predictor range (-1, 1)
x1 <- sample(c(-1,1), N, replace=TRUE)
x2 <- sample(c(-1,1), N, replace=TRUE)

#' #### Fit models
fit_3a <- stan_glm(y ~ x1, data = fake, refresh = 0)
fit_3b <- stan_glm(y ~ x1 + x2 + x1:x2, data = fake, refresh = 0)
print(fit_3a)
print(fit_3b)
