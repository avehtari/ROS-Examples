#' ---
#' title: "Regression and Other Stories: ChileSchools"
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

#' Code and figures for ChileSchools example. See Chapter 21 in
#' Regression and Other Stories.
#'
#' Data are from
#' 
#' - Chay, K. Y., McEwan, P. J., and Urquiola, M. (2005). The central
#'   role of noise in evaluating interventions that use test scores to
#'   rank schools. American Economic Review 95, 1237–1258.
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
library("foreign")
library("arm")
library("rstanarm")
library("brms")
library("ivpack")

#' #### Load data
#'
#' The outcomes in these analyses are the *gain scores* between 88 and 92. 
chile <- read.csv(root("ChileSchools/data","chile.csv"))
print(head(chile), digits=3)

#' ## Fit models
fit_1a <- stan_glm(read92 ~ eligible + rule2, data=chile, refresh=0)
fit_1b <- stan_glm(read92 ~ eligible + rule2, data=chile, subset = abs(rule2)<5, refresh=0)

fit_2b <- stan_glm(read92 ~ eligible + rule2 + eligible:rule2, data=chile, subset = abs(rule2)<5, refresh=0)

fit_3a <- stan_glm(read92 ~ eligible + rule2 + read88 + math88, data=chile, refresh=0)
fit_3b <- stan_glm(read92 ~ eligible + rule2 + read88 + math88, data=chile, subset = abs(rule2)<5, refresh=0)

chile$z_read88 <- (chile$read88 - mean(chile$read88))/sd(chile$read88)
chile$z_math88 <- (chile$math88 - mean(chile$math88))/sd(chile$math88)
fit_5a <- stan_glm(read92 ~ eligible + rule2 + z_read88 + z_math88 + eligible:z_read88, data=chile, refresh=0)
fit_5b <- stan_glm(read92 ~ eligible + rule2 + z_read88 + z_math88 + eligible:z_read88, data=chile, subset = abs(rule2)<5, refresh=0)

#' ## Plots
whiteline <- function(a, b, from=-1000, to=1000){
  curve(a + b*x, from=from, to=to, col="white", lwd=4, add=TRUE)
  curve(a + b*x, from=from, to=to, lwd=2, add=TRUE)
}

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ChileSchools/figs","rd_chile_1a.pdf"), height=3.6, width=5, colormodel="gray")
#+
par(mar=c(3,3,2,1), mgp=c(1.7, .5, 0), tck=-.01)
yy <- chile$read92
xx <- chile$rule2
plot(y=yy, x=xx, type="n", 
     xlab="Assignment variable",
     ylab="Post-test", bty="l", yaxt="n")
axis(2, seq(0,100,20))
points(xx[chile$eligible==0],yy[chile$eligible==0],pch=20,
       cex=.7,col="gray60")
points(xx[chile$eligible==1],yy[chile$eligible==1],pch=20,
       cex=.7)
whiteline(coef(fit_1a)[1], coef(fit_1a)[3], from=0)
whiteline(coef(fit_1a)[1] + coef(fit_1a)[2], coef(fit_1a)[3], to=0)
abline(v=0, lwd=4, col="gray")
mtext("All the data", 3, 1)
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ChileSchools/figs","rd_chile_1b.pdf"), height=3.6, width=5, colormodel="gray")
#+
par(mar=c(3,3,2,1), mgp=c(1.7, .5, 0), tck=-.01)
yy <- chile$read92
xx <- chile$rule2
plot(y=yy, x=xx, type="n", 
     xlab="Assignment variable",
     ylab="Post-test", bty="l", yaxt="n", xlim=c(-5,5), xaxs="i")
axis(2, seq(0,100,20))
points(xx[chile$eligible==0],yy[chile$eligible==0],pch=20,
       cex=.7,col="gray60")
points(xx[chile$eligible==1],yy[chile$eligible==1],pch=20,
       cex=.7)
whiteline(coef(fit_1b)[1], coef(fit_1b)[3], from=0)
whiteline(coef(fit_1b)[1] + coef(fit_1b)[2], coef(fit_1b)[3], to=0)
abline(v=0, lwd=4, col="gray")
mtext("Restricting to data near the cutoff", 3, 1)
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ChileSchools/figs","rd_chile_2b.pdf"), height=3.6, width=5, colormodel="gray")
#+
par(mar=c(3,3,2,1), mgp=c(1.7, .5, 0), tck=-.01)
yy <- chile$read92
xx <- chile$rule2
plot(y=yy, x=xx, type="n", 
     xlab="Assignment variable",
     ylab="Ppst-test", bty="l", yaxt="n", xlim=c(-5,5), xaxs="i")
axis(2, seq(0,100,20))
points(xx[chile$eligible==0],yy[chile$eligible==0],pch=20,
       cex=.7,col="gray60")
points(xx[chile$eligible==1],yy[chile$eligible==1],pch=20,
       cex=.7)
whiteline(coef(fit_2b)[1], coef(fit_2b)[3], from=0)
whiteline(coef(fit_2b)[1] + coef(fit_2b)[2], coef(fit_2b)[3] + coef(fit_2b)[4], to=0)
abline(v=0, lwd=4, col="gray")
mtext("Model with interaction", 3, 1)
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ChileSchools/figs","rd_chile_3b.pdf"), height=3.6, width=5, colormodel="gray")
#+
par(mar=c(3,3,2,1), mgp=c(1.7, .5, 0), tck=-.01)
yy <- chile$read92 - coef(fit_3b)[4] * (chile$read88 - mean(chile$read88)) - coef(fit_3b)[5] * (chile$math88 - mean(chile$math88))
xx <- chile$rule2
plot(y=yy, x=xx, type="n", 
     xlab="Assignment variable",
     ylab="Adjusted outcome", bty="l", yaxt="n", xlim=c(-5,5), xaxs="i")
axis(2, seq(-100,100,20))
points(xx[chile$eligible==0],yy[chile$eligible==0],pch=20,
       cex=.7,col="gray60")
points(xx[chile$eligible==1],yy[chile$eligible==1],pch=20,
       cex=.7)
whiteline(coef(fit_3b)[1] + coef(fit_3b)[4] *mean(chile$read88) + coef(fit_3b)[5] *mean(chile$math88), coef(fit_3b)[3], from=0)
whiteline(coef(fit_3b)[1] + coef(fit_3b)[4] *mean(chile$read88) + coef(fit_3b)[5] *mean(chile$math88) + coef(fit_3b)[2], coef(fit_3b)[3], to=0)
abline(v=0, lwd=4, col="gray")
mtext("Adjusting for pre-test scores", 3, 1)
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("ChileSchools/figs","rd_chile_4b.pdf"), height=3.6, width=5, colormodel="gray")
#+
par(mar=c(3,3,2,1), mgp=c(1.7, .5, 0), tck=-.01)
yy <- chile$read92 - coef(fit_3b)[4] * (chile$read88 - mean(chile$read88)) - coef(fit_3b)[5] * (chile$math88 - mean(chile$math88))
xx <- chile$rule2
n_bins <- 20
n <- length(xx)
halfwidth <- 5
cutpoints <- c(seq(-halfwidth, halfwidth, length=n_bins+1))
xx_bin <- rep(NA, n_bins)
yy_bin <- rep(NA, n_bins)
for (i in 1:n_bins){
  keep <- xx > cutpoints[i] & xx <= cutpoints[i+1]
  xx_bin[i] <- mean(xx[keep])
  yy_bin[i] <- mean(yy[keep])
}
plot(y=yy_bin, x=xx_bin, type="n", 
     xlab="Assignment variable",
     ylab="Adjusted outcome", bty="l", yaxt="n", xlim=c(-halfwidth, halfwidth), xaxs="i")
axis(2, seq(-100,100,1))
points(xx_bin[xx_bin>0], yy_bin[xx_bin>0],pch=20,
       cex=2, col="gray60")
points(xx_bin[xx_bin<0], yy_bin[xx_bin<0],pch=20,
       cex=2)
whiteline(coef(fit_3b)[1] + coef(fit_3b)[4] *mean(chile$read88) + coef(fit_3b)[5] *mean(chile$math88), coef(fit_3b)[3], from=0)
whiteline(coef(fit_3b)[1] + coef(fit_3b)[4] *mean(chile$read88) + coef(fit_3b)[5] *mean(chile$math88) + coef(fit_3b)[2], coef(fit_3b)[3], to=0)
abline(v=0, lwd=4, col="gray")
mtext("Binned averages with same regression lines", 3, 1)
#+ eval=FALSE, include=FALSE
dev.off()

#' ## Additional models

#' #### Noncompliance rates
mean(chile$p90[chile$eligible==0 & abs(chile$rule2) < 5])  # .053
mean(chile$p90[chile$eligible==1 & abs(chile$rule2) < 5]) # .606

#' #### IV model on restricted dataset
# with brms as in IV section earlier in the chapter
set.seed(1234)
chile$diff_read92 <- chile$read92 - chile$read88
rd_f1 <- bf(p90 ~ eligible)
rd_f2 <- bf(diff_read92 ~ p90)
#+ results='hide'
IV_brm_rd <- brm(formula=rd_f1 + rd_f2, data = chile[abs(chile$rule2) < 5,])
#+
print(IV_brm_rd, digits=3)
# t.e. is 5.08 with s.e. of .90

#' #### Compare to IV regression by two-stage least squares
summary(ivreg(formula = diff_read92 ~ p90 | eligible, data=chile, subset = abs(rule2) < 5))
# est is 5.09 with s.e. of .88
