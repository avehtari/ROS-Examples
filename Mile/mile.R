#' ---
#' title: "Regression and Other Stories: Mile"
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

#' Trend of record times in the mile run. See Chapter 3 in Regression
#' and Other Stories.
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
theme_set(bayesplot::theme_default(base_family = "sans"))

#' #### Load data
mile <- read.csv(root("Mile/data","mile.csv"), header=TRUE)
head(mile)

#' #### Linear model
#' 
#' The option `refresh = 0` supresses the default Stan sampling
#' progress output. This is useful for small data with fast
#' computation. For more complex models and bigger data, it can be
#' useful to see the progress.
fit <- stan_glm(seconds ~ year, data = mile, refresh = 0)
print(fit, digits=2)

#' #### Predictions for 1900 and 2000
print(1007 -.393*c(1900,2000))  # Approx
print(coef(fit)[1] + coef(fit)[2]*c(1900,2000), digits=4) # Exact

#' #### Example of increasing trend
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Mile/figs","aplusbx1a.pdf"), height=3.5, width=5)
#+
a <- 0.15
b <- 0.4
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot(c(0,2.2), c(0,a+2.2*b), pch=20, cex=.5, main="y = a + bx (with b > 0)",
  bty="l", type="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis(1, c(0,1,2))
axis(2, c(a,a+b,a+2*b), c("a","a+b","a+2b"))
abline(a, b)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Example of decreasing trend
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Mile/figs","aplusbx1b.pdf"), height=3.5, width=5)
#+
a <- 0.95
b <- -0.4
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot(c(0,2.2), c(0,a+.2), pch=20, cex=.5, main="y = a + bx (with b < 0)",
  bty="l", type="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis(1, c(0,1,2))
axis(2, c(a,a+b,a+2*b), c("a","a+b","a+2b"))
abline(a, b)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Approximate trend from the fit in range [0,2.1]
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Mile/figs","aplusbx2a.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve(1007 - 0.393*x, from=0, to=2.1, xlab="x", ylab="y", bty="l", xaxs="i",
  main="y = 1007 - 0.393x")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Approximate trend from the fit in range [0,100]
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Mile/figs","aplusbx2b.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve(1007 - 0.393*x, from=0, to=100, xlab="x", ylab="y", bty="l", xaxs="i",
  main="y = 1007 - 0.393x")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()     

#' #### Approximate trend of record times in the mile run from 1900 to 2000
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Mile/figs","aplusbx3.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve(1007 - 0.393*x, from=1900, to=2000,
      xlab="Year", ylab="Time (seconds)", bty="l", xaxs="i",
      main="Approx. trend of record times in the mile run",
      ylim=c(215, 265))
text(1960, 246, "y = 1007 - 0.393x")
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### ggplot version
ggplot(aes(x=year, y=seconds), data=mile) + geom_point(shape=1, size=2) +
    geom_abline(intercept=fit$coefficients[1], slope=fit$coefficients[2]) +
    labs(x="Year", y="Time (seconds)",
         title = "Approx. trend of record times in the mile run")

#+ eval=FALSE, include=FALSE
# A simple graph
# World record times in the mile run from 1900 to 2000
if (savefigs) pdf(root("Mile/figs","mile1a.pdf"), height=3.5, width=5)
plot(mile$year, mile$seconds,
     main="World record times in the mile run")
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
# World record times in the mile run from 1900 to 2000
# Improved graph
if (savefigs) pdf(root("Mile/figs","mile1b.pdf"), height=3.5, width=5)
par(mar=c(3,3,3,1), mgp=c(2,.5,0), tck=-.01)
plot(mile$year, mile$seconds, bty="l",
     main="World record times in the mile run", xlab="Year", ylab="Seconds")
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
# World record times in the mile run from 1900 to 2000
# Fitted line
if (savefigs) pdf(root("Mile/figs","mile2.pdf"), height=3.5, width=5)
par(mar=c(3,3,3,1), mgp=c(2,.5,0), tck=-.01)
plot(mile$year, mile$seconds, bty="l",
     main="World record times in the mile run", xlab="Year", ylab="Seconds")
curve(coef(fit)[1] + coef(fit)[2]*x, add=TRUE)
if (savefigs) dev.off()
