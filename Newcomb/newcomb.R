#' ---
#' title: "Regression and Other Stories: Newcomb"
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

#' Posterior predictive checking of Normal model for Newcomb's speed
#' of light data. See Chapter 11 in Regression and Other Stories.
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
library("rstanarm")
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
#+ eval=FALSE, include=FALSE
# grayscale figures for the book
color_scheme_set(scheme = "gray")

#' #### Load Data
#' 
#' Simon Newcomb's measurements of the speed of light, from Stigler
#' (1977).  The data are recorded as deviations from $24,\!800$
#' nanoseconds.
newcomb <- read.table(root("Newcomb/data","newcomb.txt"), header = TRUE)
head(newcomb)

#' #### Histogram of the data
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_hist.pdf"), height=4, width=5)
#+
hist(newcomb$y, main=NULL, ylab="", xlab="", yaxt="n", breaks=30)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Histogram of the data with bayesplot
mcmc_hist(newcomb, pars="y") + xlab("")

#' #### Fit a regression model with just the intercept term
#' 
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
fit <- stan_glm(y ~ 1, data=newcomb, refresh=0)

#' #### Simulate from the predictive distribution
sims <- as.matrix(fit)
n_sims <- nrow(sims)
n <- length(newcomb$y)
y_rep <- array(NA, c(n_sims, n))
for (s in 1:n_sims)
    y_rep[s,] <- rnorm(n, sims[s,1], sims[s,2])

#' #### Plot histogram of 20 replicates
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_rep_hist.pdf"), height=4, width=8)
#+
par(mfrow=c(4,5), mar=rep(2,4))
for (s in sample(n_sims, 20)) {
  hist(y_rep[s,], main=NULL, ylab="", xlab="", yaxt="n") }
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Simulate using built-in function
y_rep <- posterior_predict(fit)
#' #### Plot data and 19 replications using built-in function
ppc_hist(newcomb$y, y_rep[1:19, ], binwidth = 8)
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Newcomb/figs","newcomb_ppc_hist.pdf"), width = 9, height = 4)

#' #### Plot kernel density estimate of data and 100 replications using built-in function
ppc_dens_overlay(newcomb$y, y_rep[1:100, ]) + scale_y_continuous(breaks=NULL)
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Newcomb/figs","newcomb_ppc_dens_overlay.pdf"), width = 6, height = 2.5)

#' #### Plot test statistic for data and replicates
test <- function (y) {
  min(y) }
test_rep <- apply(y_rep, 1, test)
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_test_hist.pdf"), height=4, width=5)
#+
hist(test_rep, xlim=c(-50,20), breaks=20, yaxt="n",
     xlab="", ylab="", main=NULL)
lines(rep(test(newcomb$y),2), c(0,n_sims), lwd=3)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot test statistic for data and replicates using built-in function
ppc_stat(newcomb$y, y_rep, stat = "min", binwidth = 2)
#+ eval=FALSE, include=FALSE
if (savefigs) ggsave(root("Newcomb/figs","newcomb_ppc_stat.pdf"), width = 6, height = 4)
