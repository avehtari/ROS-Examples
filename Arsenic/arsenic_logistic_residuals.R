#' ---
#' title: "Regression and Other Stories: Arsenic model residuals"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Residual plots for a logistic regression model: wells in Bangladesh
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("loo")
library("arm")

#' **Load data**
wells <- read.csv(root("Arsenic/data","wells.csv"))
wells$y <- wells$switch
n <- nrow(wells)

#' **Predict switching with distance, arsenic, education and intercations**
#+ results='hide'
wells$c_dist100 <- wells$dist100 - mean(wells$dist100)
wells$c_arsenic <- wells$arsenic - mean(wells$arsenic)
wells$c_educ4 <- wells$educ4 - mean(wells$educ4)
fit_8 <- stan_glm(y ~ c_dist100 + c_arsenic + c_educ4 +
                      c_dist100:c_educ4 + c_arsenic:c_educ4,
                  family = binomial(link="logit"), data = wells)
pred8 <- fitted(fit_8)

#' **Error rates**
error_rate_null <- mean(round(abs(wells$y-mean(pred8))))
round(error_rate_null, 2)
error_rate <- mean(round(abs(wells$y-pred8)))
round(error_rate, 2)

#' ### Residual plot
#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","arsenic.logitresidsa.ps"),
           height=3.5, width=4, horizontal=TRUE)
#+
plot(c(0,1), c(-1,1), xlab="Estimated  Pr (switching)", ylab="Observed - estimated",
     type="n", main="Residual plot", mgp=c(2,.5,0))
abline(0,0, col="gray", lwd=.5)
points(pred8, wells$y-pred8, pch=20, cex=.2)
#+ eval=FALSE, include=FALSE
dev.off()

#' ### Binned residual plots
#'

#' Function for binning residuals
binned_resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}

#' **Binned residual plot with respect to predicted probability**
#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","arsenic.logitresidsb.ps"),
           height=3.5, width=4, horizontal=T)
#+
br8 <- binned_resids(pred8, wells$y-pred8, nclass=40)$binned
plot(range(br8[,1]), range(br8[,2],br8[,6],-br8[,6]),
     xlab="Estimated  Pr (switching)", ylab="Average residual",
     type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline(0,0, col="gray", lwd=.5)
lines(br8[,1], br8[,6], col="gray", lwd=.5)
lines(br8[,1], -br8[,6], col="gray", lwd=.5)
points(br8[,1], br8[,2], pch=20, cex=.5)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Binned residual plots with respect to predictors**
#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","logitresids.2a.ps"),
           height=3.5, width=4, horizontal=T)
#+
br <- binned_resids(wells$dist, wells$y-pred8, nclass=40)$binned
plot(range(br[,1]), range(br[,2],br[,6],-br[,6]),
     xlab="Distance to nearest safe well", ylab="Average residual",
     type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline(0,0, col="gray", lwd=.5)
n_within_bin <- length(wells$y)/nrow(br)
lines(br[,1], br[,6], col="gray", lwd=.5)
lines(br[,1], -br[,6], col="gray", lwd=.5)
points(br[,1], br[,2], pch=20, cex=.5)
#+ eval=FALSE, include=FALSE
dev.off()
#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","arsenic.logitresids.2b.ps"),
           height=3.5, width=4, horizontal=T)
#+
br <- binned_resids(wells$arsenic, wells$y-pred8, nclass=40)$binned
plot(range(0,br[,1]), range(br[,2],br[,6],-br[,6]),
     xlab="Arsenic level", ylab="Average residual",
     type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br[,1], br[,6], col="gray", lwd=.5)
lines (br[,1], -br[,6], col="gray", lwd=.5)
points (br[,1], br[,2], pch=20, cex=.5)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Predict switching with distance, log(arsenic), education and intercations**
#' Use non-centered predictors for easier plotting
#+ results='hide'
wells$log_arsenic <- log(wells$arsenic)
fit_8b <- stan_glm(y ~ dist100 + log_arsenic + educ4 +
                      dist100:educ4 + log_arsenic:educ4,
                   family = binomial(link="logit"), data = wells)

#' **Predict switching with distance, log(arsenic), education and intercations**
#' Use non-centered predictors for easier plotting
#+ results='hide'
wells$log_arsenic <- log(wells$arsenic)
fit_8b <- stan_glm(y ~ dist100 + log_arsenic + educ4 +
                      dist100:educ4 + log_arsenic:educ4,
                   family = binomial(link="logit"), data = wells)
pred8b <- fitted(fit_8b)

#' **Error rate**
error_rate <- mean(round(abs(wells$y-pred8b)))
round(error_rate, 2)

#' **Plots for log model**
#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","arsenic.logmodel.ps"),
           height=3.5, width=4, horizontal=TRUE)
#+
jitter_binary <- function(a, jitt=.05){
  a + (1-2*a)*runif(length(a),0,jitt)
}
plot(c(0,max(wells$arsenic,na.rm=T)*1.02), c(0,1),
     xlab="Arsenic concentration in well water", ylab="Pr (switching)",
     type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
points(wells$arsenic, jitter_binary(wells$y), pch=20, cex=.1)
curve(invlogit(coef(fit_8b)[1]+coef(fit_8b)[2]*0+coef(fit_8b)[3]*log(x)+coef(fit_8b)[4]*mean(wells$educ4)+coef(fit_8b)[5]*0*mean(wells$educ4)+coef(fit_8b)[6]*log(x)*mean(wells$educ4)), from=.5, lwd=.5, add=T)
curve(invlogit(coef(fit_8b)[1]+coef(fit_8b)[2]*.5+coef(fit_8b)[3]*log(x)+coef(fit_8b)[4]*mean(wells$educ4)+coef(fit_8b)[5]*.5*mean(wells$educ4)+coef(fit_8b)[6]*log(x)*mean(wells$educ4)), from=.5, lwd=.5, add=T)
text(.25, .80, "if dist = 0", adj=0, cex=.8)
text(2, .63, "if dist = 50", adj=0, cex=.8)
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
postscript(root("Arsenic/figs","arsenic.logitresids.3b.ps"),
           height=3.5, width=4, horizontal=TRUE)
#+
br <- binned.resids(wells$arsenic, wells$y-pred8b, nclass=40)$binned
plot(range(0,br[,1]), range(br[,2],br[,6],-br[,6]),
     xlab="Arsenic level", ylab="Average residual", type="n",
     main="Binned residual plot\nfor model with log (arsenic)", mgp=c(2,.5,0))
abline(0,0, col="gray", lwd=.5)
n.within.bin <- length(wells$y)/nrow(br)
lines(br[,1], br[,6], col="gray", lwd=.5)
lines(br[,1], -br[,6], col="gray", lwd=.5)
points(br[,1], br[,2], pch=20, cex=.5)
#+ eval=FALSE, include=FALSE
dev.off()
