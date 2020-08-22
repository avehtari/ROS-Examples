#' ---
#' title: "Regression and Other Stories: Coverage"
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

#' Coverage - Illustration of coverage of intervals. See Chapter 4 in
#' Regression and Other Stories.
#' 
#' -------------
#'

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#+ eval=FALSE, include=FALSE
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' #### Simulate
n_rep <- 100
est <- rep(NA, n_rep)
conf <- array(NA, c(n_rep, 4))
mu <- 6
sigma <- 4
for (i in 1:n_rep){
  y <- rnorm(1, mu, sigma)
  est[i] <- y
  conf[i,] <- y + c(-2, -.67, .67, 2) * sigma
}

#' #### Plot
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Coverage/figs","coverage.pdf"), height=4, width=8)
#+
par(mar=c(3,3,0,0), mgp=c(1.5,.5,0), tck=-.01)
plot(c(-2, n_rep+2), range(conf), bty="l", xlab="Simulation", ylab="Estimate, 50%, and 95% confidence interval", xaxs="i", yaxt="n", type="n")
axis(2, seq(-10,20,10))
points(1:n_rep, est, pch=20)
abline(mu, 0, col="gray")
for (i in 1:n_rep){
  lines(c(i,i), conf[i,c(1,4)], lwd=.8)
  lines(c(i,i), conf[i,c(2,3)], lwd=2)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
