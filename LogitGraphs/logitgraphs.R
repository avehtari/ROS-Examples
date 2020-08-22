#' ---
#' title: "Regression and Other Stories: Logistic regression graphs"
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

#' Different ways of displaying logistic regression. See Chapter 14 in
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
options(mc.cores = parallel::detectCores())
logit <- qlogis
invlogit <- plogis

#' #### Simulate fake data from logit model
n <- 50
a <- 2
b <- 3
x_mean <- -a/b
x_sd <- 4/b
x <- rnorm(n, x_mean, x_sd)
y <- rbinom(n, 1, invlogit(a + b*x))
fake_1 <- data.frame(x, y)
head(fake_1)

#' #### Fit the model and save the coefficient estimates
fit_1 <- stan_glm(y ~ x, family=binomial(link="logit"), data=fake_1, refresh=0)
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]

#' #### Graph data and underying and fitted logistic curves
shifted <- function(a, delta=0.008) return(ifelse(a==0, delta, ifelse(a==1, 1 - delta, a)))
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("LogitGraphs/figs","logitgraph1a.pdf"), height=4.5, width=6)
#+
par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x, shifted(y), ylim=c(0, 1), xlab="x", ylab="y", yaxs="i", pch=20)
curve(invlogit(a + b*x), add=TRUE, col="gray30")
curve(invlogit(a_hat + b_hat*x), add=TRUE, lty=2, col="gray30")
x0 <- (1.5 - a) / b
text(x0, invlogit(1.5), paste("   True curve,   \n   y = invlogit(", round(a, 1), " + ", round(b, 1), "x)   ", sep=""), cex=.9, col="gray30",
  adj=if (a_hat + b_hat*x0 > 1.5) 0 else 1)
x0 <- (-1.5 - a_hat) / b_hat
text(x0, invlogit(-1.5), paste("   Fitted curve,   \n   y = invlogit(", round(a_hat, 1), " + ", round(b_hat, 1), "x)   ", sep=""), cex=.9, col="gray30",
  adj=if (a + b*x0 > -1.5) 0 else 1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Binned plot
K <- 5
bins <- as.numeric(cut(x, K))
x_bar <- rep(NA, K)
y_bar <- rep(NA, K)
for (k in 1:K){
  x_bar[k] <- mean(x[bins==k])
  y_bar[k] <- mean(y[bins==k])
}
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("LogitGraphs/figs","logitgraph1b.pdf"), height=4.5, width=6)
#+
par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x, shifted(y), ylim=c(0, 1), xlab="x", ylab="y", yaxs="i", pch=20, cex=.8, main="Data and binned averages", cex.main=.9, col="gray50")
points(x_bar, shifted(y_bar, 0.02), pch=21, cex=1.5)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Logistic regression as function of two predictors
n <- 100
beta <- c(2, 3, 4) # arbitrary choice of true coefficients in the model
x1 <- rnorm(n, 0, 0.4) # somewhat arbitary choice of scale of data, set so there will be a good mix of 0's and 1's
x2 <- rnorm(n, -0.5, 0.4)
y <- rbinom(n, 1, invlogit(cbind(rep(1, n), x1, x2) %*% beta))
fake_2 <- data.frame(x1, x2, y)

#' #### Fit the model and save the coefficient estimates
fit_2 <- stan_glm(y ~ x1 + x2, family=binomial(link="logit"), data=fake_2, refresh=0)
beta_hat <- coef(fit_2)

#' #### Graph data and discrimination lines
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("LogitGraphs/figs","logitgraph2.pdf"), height=5, width=6)
#+
par(mar=c(3,3,2,1), mgp=c(1.5,.5,0), tck=-.01)
plot(x1, x2, bty="l", type="n", main="Data and 10%, 50%, 90% discrimination lines\nfrom fitted logistic regression", cex.main=.9) 
points(x1[y==0], x2[y==0], pch=20)  # dots
points(x1[y==1], x2[y==1], pch=21) # circles
abline(-beta[1]/beta[3], -beta[2]/beta[3])
abline((logit(0.9) - beta[1])/beta[3], -beta[2]/beta[3], lty=2)
abline((logit(0.1) - beta[1])/beta[3], -beta[2]/beta[3],  lty=2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
