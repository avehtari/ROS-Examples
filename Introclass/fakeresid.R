#' ---
#' title: "Regression and Other Stories: Introclass"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Plot residuals vs.\ predicted values, or residuals vs.\ observed values?
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Load data**
grades <- read.table(root("Introclass/data","gradesW4315.dat"), header=TRUE)
introclass  <- data.frame(midterm = grades[,"Midterm"],
                          final = grades[,"Final"])

#' **Fit linear regression model**
fit_1 <- stan_glm(final ~ midterm, data = introclass)
print(fit_1)

#' **Compute residuals**<br>
#' compute predictions from simulations
sims <- as.matrix(fit_1)
predicted <- colMeans(sims[,1] + sims[,2] %*% t(introclass$midterm))
#' or with built-in function
predicted <- colMeans(posterior_linpred(fit_1))
resid <- introclass$final - predicted

#' **Plot residuals vs predicted**
#+ eval=FALSE, include=FALSE
postscript(root("Introclass/figs","fakeresid1a.ps"), height=3.8, width=4.5)
#+
plot(predicted, resid, xlab="predicted value", ylab="residual",
     main="Residuals vs.\ predicted values", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis(2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline(0, 0, col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Plot residuals vs observed**
#+ eval=FALSE, include=FALSE
postscript(root("Introclass/figs","fakeresid1b.ps"), height=3.8, width=4.5)
#+
plot(introclass$final, resid, xlab="observed value", ylab="residual", main="Residuals vs.\ observed values", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis(2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline(0, 0, col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Simulate fake data**
a <- 65
b <- 0.7
sigma <- 15
n <- nrow(introclass)
introclass$y_fake <- a + b*introclass$midterm + rnorm(n, 0, 15)
fit_fake <- stan_glm(y_fake ~ midterm, data = introclass)

#' **Compute residuals**
#' compute predictions from simulations
sims <- as.matrix(fit_fake)
predicted_fake <- colMeans(sims[,1] + sims[,2] %*% t(introclass$midterm))
#' or with built-in function
predicted_fake <- colMeans(posterior_linpred(fit_fake))
resid_fake <- introclass$y_fake - predicted_fake

#' **Plot residuals vs predicted**
#+ eval=FALSE, include=FALSE
postscript(root("Introclass/figs","fakeresid2a.ps"), height=3.8, width=4.5)
#+
plot(predicted_fake, resid_fake, xlab="predicted value", ylab="residual", main="Fake data:  resids vs.\ predicted", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis(2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline(0, 0, col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Plot residuals vs observed**
#+ eval=FALSE, include=FALSE
postscript(root("Introclass/figs","fakeresid2b.ps"), height=3.8, width=4.5)
#+
plot(introclass$y_fake, resid_fake, xlab="observed value", ylab="residual", main="Fake data:  resids vs.\ observed", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis(2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline(0, 0, col="gray", lwd=.5)
#+ eval=FALSE, include=FALSE
dev.off()
