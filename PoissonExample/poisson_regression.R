#' ---
#' title: "Regression and Other Stories: Poisson Example"
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

#' Demonstrate Poisson regression. See Chapter 15 in
#' Regression and Other Stories.
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
library("MASS")

#' Set random seed for reproducability
SEED <- 3579

#' ## Simulate fake data
n <- 100
x <- runif(n, -2, 2)
a <- 1
b <- 2
linpred <- a + b*x
y <- rpois(n, exp(linpred))
fake <- data.frame(x=x, y=y)
head(fake)

#' ## Poisson regression
#'
#' #### Fit Poisson regression model
fit_fake <- stan_glm(y ~ x, family=poisson(link="log"), data=fake, refresh=0)
print(fit_fake)

#+ eval=FALSE, include=FALSE
if (savefigs) pdf("poisson1.pdf", height=4, width=5)
#'
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
plot(x, y, ylim=c(-1, 200), yaxs="i", bty="l", main="Simulated data from Poisson regression", cex.main=0.9, type="n")
## curve(exp(a + b*x), from=-2.5, to=2.5, add=TRUE)
## Don't bother plotting true curve because it is so close to the fitted curve
curve(exp(coef(fit_fake)[1] + coef(fit_fake)[2]*x), from=-2.5, to=2.5, add=TRUE)
points(x, y, pch=20, cex=.6)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## Overdispersion
#' 
phi_grid <- c(0.1, 1, 10)
K <- length(phi_grid)
y_nb <- as.list(rep(NA, K))
fake_nb <- as.list(rep(NA, K))
fit_nb <- as.list(rep(NA, K))
for (k in 1:K){
  y_nb[[k]] <- rnegbin(n, exp(linpred), phi_grid[k])
  fake_nb[[k]] <- data.frame(x=x, y=y_nb[[k]])
  fit_nb[[k]] <- stan_glm(y ~ x, family=neg_binomial_2(link="log"), data=fake, refresh=0)
  print(fit_nb[[k]])
}

#+ eval=FALSE, include=FALSE
if (savefigs) pdf("poisson2.pdf", height=3, width=9)
#'
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
par(mfrow=c(1,3), oma=c(0,0,2,0))
for (k in 1:K) {
  phi <- phi_grid[k]
  if (phi==0.1)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 0.1)), type="n")
  else if (phi==1)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 1)), type="n")
  else if (phi==10)
    plot(x, y_nb[[k]], ylim=c(-1, 200), yaxs="i", bty="l", ylab="y", main=expression(paste(phi, " = ", 10)), type="n")
  ## curve(exp(a + b*x), from=-2.5, to=2.5, add=TRUE)
  ## Don't bother plotting true curve because it is so close to the fitted curve
  curve(exp(coef(fit_nb[[k]])[1] + coef(fit_nb[[k]])[2]*x), from=-2.5, to=2.5, add=TRUE)
  points(x, y_nb[[k]], pch=20, cex=.7)
}
mtext("Simulated data from overdispersed Poisson (negative binomial) regression", outer=TRUE, side=3, line=1, cex=0.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
