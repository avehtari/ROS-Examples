#' ---
#' title: "Regression and Other Stories: Newcomb"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Posterior predictive checking of Normal model for Newcomb's speed of light data
#' 
#' -------------
#' 

#+ include=FALSE
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))
color_scheme_set(scheme = "gray")

#' **Data**<br>
#' Simon Newcomb's measurements of the speed of light, from Stigler
#' (1977).  The data are recorded as deviations from $24,\!800$
#' nanoseconds.
newcomb <- data.frame(y=c(28,26,33,24,34,-44,27,16,40,-2,29,22,24,21,25,30,
                          23,29,31,19,24,20,36,32,36,28,25,21,28,29,37,25,28,
                          26,30,32,36,26,30,22, 36,23,27,27,28,27,31,27,26,
                          33,26,32,32,24,39,28,24,25,32,25,29,27,28,29,16,23))

#' Plot histogram of the data
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_hist.pdf"), height=4, width=5)
#+
hist(newcomb$y, main=NULL, ylab="", xlab="", yaxt="n", breaks=30)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' Plot histogram of the data with bayesplot
mcmc_hist(newcomb, pars="y") + xlab("")

#' **Fit a regression model with just the intercept term**
fit <- stan_glm(y ~ 1, data=newcomb)

#' **Simulate from the predictive distribution**
sims <- as.matrix(fit)
n_sims <- nrow(sims)
n <- length(newcomb$y)
y_rep <- array(NA, c(n_sims, n))
for (s in 1:n_sims)
    y_rep[s,] <- rnorm(n, sims[s,1], sims[s,2])

#' **Plot histogram of 20 replicates**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_rep_hist.pdf"), height=4, width=8)
#+
par(mfrow=c(4,5), mar=rep(2,4))
for (s in sample(n_sims, 20))
  hist(y_rep[s,], main=NULL, ylab="", xlab="", yaxt="n")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Simulate using built-in function**
y_rep <- posterior_predict(fit)
#' **Plot data and 19 replications using built-in function**
ppc_hist(newcomb$y, y_rep[1:19, ], binwidth = 8)
#+ eval=FALSE, include=FALSE
ggsave(root("Newcomb/figs","newcomb_ppc_hist.pdf"), width = 9, height = 4)

#' **Plot kernel density estimate of data and 100 replications using built-in function**
ppc_dens_overlay(newcomb$y, y_rep[1:100, ]) + scale_y_continuous(breaks=NULL)
#+ eval=FALSE, include=FALSE
ggsave(root("Newcomb/figs","newcomb_ppc_dens_overlay.pdf"), width = 6, height = 2.5)

#' **Plot test statistic for data and replicates**
Test <- function (y)
  min(y)
test_rep <- apply(y_rep, 1, Test)
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Newcomb/figs","newcomb_test_hist.pdf"), height=4, width=5)
#+
hist(test_rep, xlim=range(Test(newcomb$y), test_rep), breaks=20, yaxt="n",
     xlab="", ylab="", main=NULL)
lines(rep(Test(newcomb$y),2), c(0,n_sims), lwd=3)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Plot test statistic for data and replicates using built-in function**
ppc_stat(newcomb$y, y_rep, stat = "min", binwidth = 2)
#+ eval=FALSE, include=FALSE
ggsave(root("Newcomb/figs","newcomb_ppc_stat.pdf"), width = 6, height = 4)
