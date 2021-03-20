#' ---
#' title: "Regression and Other Stories: Robit"
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

#' Comparison of robit and logit models for binary data. See Chapter
#' 15 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("cmdstanr")
options(mc.cores = 1)
library("ggdist")
logit <- qlogis
invlogit <- plogis

#' ## Generate data from logit model

# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(1234)
N <- 50
x <- runif(N, -9, 9)
a <- 0
b <- 0.8
p <- invlogit(a + b*x)
y <- rbinom(N, 1, p)
df <- 4
data_1 <- list(N=N, x=x, y=y, df=df)

#' ## Fit logit and probit models using the simulated data
#'
#' #### Show Stan code for the models
writeLines(readLines(root("Robit","logit.stan")))
writeLines(readLines(root("Robit","robit.stan")))

#' #### Compile models
logit_model <- cmdstan_model("logit.stan")
robit_model <- cmdstan_model("robit.stan")

#' #### Sample and compute posterior medians
fit_logit_1 <- logit_model$sample(data=data_1, refresh=0)
print(fit_logit_1)
a_hat_logit_1 <- median(fit_logit_1$draws("a"))
b_hat_logit_1 <- median(fit_logit_1$draws("b"))

#' #### Sample and compute posterior medians
fit_robit_1 <- robit_model$sample(data=data_1, refresh=0)
print(fit_robit_1)
a_hat_robit_1 <- median(fit_robit_1$draws("a"))
b_hat_robit_1 <- median(fit_robit_1$draws("b"))

#' #### Plot
if (savefigs) pdf("logistic2b.pdf", height=4, width=6)
#+
par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(data_1$x, data_1$y, yaxt="n", main="Data from a logistic regression", xlab="x", ylab="y")
axis(2, c(0,1))
curve(invlogit(a_hat_logit_1 + b_hat_logit_1*x), add=TRUE, lty=2)
curve(pstudent_t(a_hat_robit_1 + b_hat_robit_1*x, data_1$df, 0, sqrt((data_1$df-2)/data_1$df)), add=TRUE, lty=1)
legend (1, .3, c("fitted logistic regression", "fitted robit regression"), lty=c(2,1), cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## Add an outlier by flipping the class of one observation
low_value <- (1:N)[x==sort(x)[4]]
data_2 <- data_1
data_2$y[low_value] <- 1

#' #### Sample and compute posterior medians
fit_logit_2 <- logit_model$sample(data=data_2, refresh=0)
print(fit_logit_2)
a_hat_logit_2 <- median(fit_logit_2$draws("a"))
b_hat_logit_2 <- median(fit_logit_2$draws("b"))

#' #### Sample and compute posterior medians
fit_robit_2 <- robit_model$sample(data=data_2, refresh=0)
print(fit_robit_2)
a_hat_robit_2 <- median(fit_robit_2$draws("a"))
b_hat_robit_2 <- median(fit_robit_2$draws("b"))

#' Plot
if (savefigs) pdf("logistic2a.pdf", height=4, width=6)
#+
par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(data_2$x, data_2$y, yaxt="n", main="Contaminated data", xlab="x", ylab="y")
axis(2, c(0,1))
curve(invlogit(a_hat_logit_2 + b_hat_logit_2*x), add=TRUE, lty=2)
curve(pstudent_t(a_hat_robit_2 + b_hat_robit_2*x, data_2$df, 0, sqrt((data_2$df-2)/data_2$df)), add=TRUE, lty=1)
legend (1, .3, c("fitted logistic regression", "fitted robit regression"), lty=c(2,1), cex=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
