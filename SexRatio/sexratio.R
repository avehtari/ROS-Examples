#' ---
#' title: "Regression and Other Stories: Beauty and sex ratio"
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

#' Example where an informative prior makes a difference. See Chapter 9
#' in Regression and Other Stories.
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
library("arm")
library("rstanarm")

#' ## Data
x <- seq(-2,2,1)
y <- c(50, 44, 50, 47, 56)
sexratio <- data.frame(x, y)

#' ## Informative priors
theta_hat_prior <- 0
se_prior <- 0.25
theta_hat_data <- 8
se_data <- 3
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2)/(1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))

#' ## Least-squares regression
fit <- lm(y ~ x, data = sexratio)
display(fit)

#' #### Plot data and least-squares regression line
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("SexRatio/figs","sexratio_bayes_1.pdf"), height=4, width=10)
#+
par(mfrow=c(1,2), mar=c(3,3,3,2), mgp=c(1.7,.5,0), tck=-.01)
plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main="Data on beauty and sex ratio",  pch=19, cex=1)
axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main="Data and least-squares regression line",  pch=19, cex=1)
axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
abline(coef(fit)[1], coef(fit)[2])
text(1, 52.2, paste("y = ", fround(coef(fit)[1], 1), " + ", fround(coef(fit)[2], 1), " x\n(Std err of slope is ", fround(se.coef(fit)[2], 1), ")", sep=""))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## Bayesian regression with weakly informative prior
fit_default <- stan_glm(y ~ x, data = sexratio, refresh = 0)
prior_summary(fit_default)
print(fit_default)

#' ## Bayesian regression with informative prior
fit_post <- stan_glm(y ~ x, data = sexratio,
                     prior = normal(0, 0.2),
                     prior_intercept = normal(48.8, 0.5),
                     refresh = 0)
prior_summary(fit_post)
print(fit_post)

#' #### Plot Posterior simulations under weakly informative and informative prior
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("SexRatio/figs","sexratio_bayes_2.pdf"), height=8, width=10)
#+
par(mfrow=c(2,2), mar=c(5,3,3,2), mgp=c(1.7,.5,0), tck=-.01)
fit_bayes <- list(as.data.frame(fit_default), as.data.frame(fit_post))
for (k in 1:2){
  sims <- fit_bayes[[k]]
  coef_est <- apply(sims, 2, median)
  b_se <- 1.483*median(abs(sims[,2] - median(sims[,2])))
  plot(sims[,1], sims[,2], xlim=range(fit_bayes[[1]][,1], fit_bayes[[2]][,1]), ylim=range(fit_bayes[[1]][,2], fit_bayes[[2]][,2]), xlab="Intercept, a", ylab="Slope, b", main=paste("Posterior simulations under", if (k==1) "default prior" else "informative prior"), pch=19, cex=.2, bty="l")
  plot(x, y, ylim=c(43, 57), xlab="Attractiveness of parent", ylab="Percentage of girl babies", bty="l", yaxt="n", main=if (k==1) "Least-squares regression line and\nposterior uncertainty given default prior" else "Bayes estimated regression line and\nposterior uncertainty given informative prior", pch=19, cex=1)
  axis(2, c(45,50,55), paste(c(45,50,55), "%", sep=""))
  for (i in 1:100){
    abline(sims[i,1], sims[i,2], lwd=.5, col="gray50")
  }
  abline(coef_est[1], coef_est[2], lwd=2)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
