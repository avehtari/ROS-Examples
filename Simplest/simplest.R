#' ---
#' title: "Regression and Other Stories: Simple regression"
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

#' Linear regression with a single predictor. See Chapters 6 and 7 in
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

#' ## Fitting a regression using a data frame in R
#' 
#' #### Simulate fake data
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141) 
y <- a + b*x + sigma*rnorm(n)
fake <- data.frame(x, y)

#' #### Linear regression model
#' 
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
fit_1 <- stan_glm(y ~ x, data = fake, seed=2141, refresh = 0)
print(fit_1, digits=2)

#' #### Plot for the book
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Simplest/figs","simple.pdf"), height=4, width=5.5)
#+
par(mar=c(3,3,1,1), mgp=c(1.7,.5,0), tck=-.01)
plot(fake$x, fake$y, main="Data and fitted regression line", bty="l", pch=20,
     xlab = "x", ylab = "y")
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]
abline(a_hat, b_hat)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat*x_bar, paste("   y =", round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' ## Formulating comparisons as regression models
#' 
#' #### Simulate fake data
n_0 <- 200
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141)
y_0 <- rnorm(n_0, 2.0, 5.0)
fake_0 <- data.frame(y_0)
round(y_0, 1)
round(mean(y_0), 2)
round(sd(y_0)/sqrt(n_0), 2)

#' #### Estimating the mean is the same as regressing on a constant term
#+ results='hide'
fit_2 <- stan_glm(y_0 ~ 1, data = fake_0, seed=2141, refresh = 0,
                  prior_intercept = NULL, prior = NULL, prior_aux = NULL)
#+
print(fit_2)

#' #### Simulate fake data
n_1 <- 300
# set the random seed to get reproducible results
# change the seed to experiment with variation due to random noise
set.seed(2141)
y_1 <- rnorm(n_1, 8, 5)
diff <- mean(y_1) - mean(y_0)
se_0 <- sd(y_0)/sqrt(n_0)
se_1 <- sd(y_1)/sqrt(n_1)
se <- sqrt(se_0^2 + se_1^2)
print(diff)

print(se)

#' #### Estimating a difference is the same as regressing on an indicator variable
n <- n_0 + n_1
y <- c(y_0, y_1)
x <- c(rep(0, n_0), rep(1, n_1))
fake <- data.frame(y, x)
fit_3 <- stan_glm(y ~ x, data = fake, seed=2141, refresh = 0, 
                  prior_intercept = NULL, prior = NULL, prior_aux = NULL)
print(fit_3)

#' #### Plot for the book
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Simplest/figs","simplest_1.pdf"), height=4, width=5)
#+
par(mar=c(3,3,3,2), mgp=c(1.7,.5,0), tck=-.01)
plot(x, y, xlab="Indicator, x", ylab="y", bty="l", xaxt="n", main="Regression on an indicator is the same\nas computing a difference in means",  pch=19, cex=.5)
axis(1, c(0, 1))
abline(h=mean(y[x==0]), lty=2, col="gray50")
abline(h=mean(y[x==1]), lty=2, col="gray50")
abline(coef(fit_3)[1], coef(fit_3)[2])
text(.5, -1 + coef(fit_3)[1] + .5*coef(fit_3)[2], paste("y =", round(coef(fit_3)[1], 2), "+", round(coef(fit_3)[2], 2), "x"), cex=.9, adj=0)
text(.05, -1 + mean(y[x==0]), bquote(paste(bar(y)[0], " = ", .(round(mean(y[x==0]), 2)))), col="gray30", cex=.9, adj=0)
text(.95, 1 + mean(y[x==1]), bquote(paste(bar(y)[1], " = ", .(round(mean(y[x==1]), 2)))), col="gray30", cex=.9, adj=1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Repeat with flat priors
fit_3b <- stan_glm(y ~ x, data = fake, seed=2141, refresh = 0,
                  prior=NULL, prior_intercept=NULL, prior_aux=NULL)
print(fit_3b)
