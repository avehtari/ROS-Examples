#' ---
#' title: "Regression and Other Stories: Pearson and Lee Heights"
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
#'

#' The heredity of height. Published in 1903 by Karl Pearson and Alice
#' Lee. See Chapter 6 in Regression and Other Stories.
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
library("HistData")

#' #### Load data
heights <- read.table(root("PearsonLee/data","Heights.txt"), header=TRUE)
n <- nrow(heights)
head(heights)

#' #### Linear regression
fit_1 <- stan_glm(daughter_height ~ mother_height, data = heights, refresh = 0)
#+
print(fit_1, digits=2)
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]

#' #### Plot mothers' and daughters' heights
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee1.pdf"), height=4.5, width=4.5)
#+
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01)
par(pty="s")
rng <- range(heights$mother_height, heights$daughter_height)
plot(heights$mother_height, heights$daughter_height, xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=rng, ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.5)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot mothers' and daughters' heights with jitter
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee2.pdf"), height=4.5, width=4.5)
#+
par(mar=c(3, 3, 2, 1), mgp=c(1.7, .5, 0), tck=-.01, pty="s")
plot(jitter(heights$mother_height, 0.5), jitter(heights$daughter_height), xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=rng, ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot mothers' and daughters' heights and fitted regression line
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee3a.pdf"), height=4.5, width=4.5)
#+
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01, pty="s")
plot(jitter(heights$mother_height, 0.5), jitter(heights$daughter_height), xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=c(rng[1], rng[2]), ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2)
x <- seq(48, 84, 6)
axis(1, x)
axis(2, x)
for (i in x){
  abline(h=i, col="gray70", lty=2)
  abline(v=i, col="gray70", lty=2)
}
abline(a_hat, b_hat, lwd=3, col="white")
abline(a_hat, b_hat, lwd=1.5)
points(mean(heights$mother_height), mean(heights$daughter_height), pch=20, cex=2, col="white")
mtext("Mothers' and daughters' heights,\naverage of data, and fitted regression line", side=3, line=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot fitted regression line and the average of the data
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee3b.pdf"), height=4.5, width=4.5)
#+
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01, pty="s")
plot(jitter(heights$mother_height, 0.5), jitter(heights$daughter_height), xlab="Mother's height (inches)", ylab="Adult daughter's height (inches)", bty="l", xlim=c(rng[1], rng[2]), ylim=rng, xaxt="n", yaxt="n", pch=20, cex=.2, type="n")
x <- seq(54, 72, 6)
axis(1, x)
axis(2, x)
abline(a_hat, b_hat, lwd=3, col="white")
abline(a_hat, b_hat, lwd=1.5)
lines(rep(mean(heights$mother_height), 2), c(0, mean(heights$daughter_height)), lwd=.5)
lines(c(0, mean(heights$mother_height)), rep(mean(heights$daughter_height), 2), lwd=.5)
axis(1, mean(heights$mother_height), round(mean(heights$mother_height), 1))
axis(2, mean(heights$daughter_height), round(mean(heights$daughter_height), 1))
text(68, 64, paste("y =", round(a_hat), "+", round(b_hat, 2), "x"))
text(63, 62, paste("Equivalently,  y = ", round(mean(heights$daughter_height), 1), " + ", round(b_hat, 2), " * (x - ", round(mean(heights$mother_height), 1), ")", sep=""))
points(mean(heights$mother_height), mean(heights$daughter_height), pch=20, cex=2)
mtext("The fitted regression line and the average of the data      ", side=3, line=1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot fitted regression line
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee4a.pdf"), height=4, width=4.5)
#+
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
plot(c(0, 100), c(0, 100), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
abline(h=0)
abline(v=0)
axis(2, round(a_hat), tck=0, las=1)
axis(1, 0, tck=0, las=1, line=-.4)
axis(2, 0, tck=0, las=1)
abline(a_hat, b_hat, lwd=2)
text(40, 40, paste("slope", round(b_hat, 2)))
mtext(paste("The line, y =", round(a_hat), "+", round(b_hat, 2), "x"), side=3, line=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Plot data and fitted regression line in the context of the data
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("PearsonLee/figs","PearsonLee4b.pdf"), height=4, width=4.5)
#+
par(mar=c(3, 3, 2, .1), mgp=c(2, .5, 0), tck=-.01)
plot(c(0, 100), c(0, 100), xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
abline(h=0)
abline(v=0)
axis(2, round(a_hat), tck=0, las=1)
points(jitter(heights$mother_height, 0.5), jitter(heights$daughter_height), pch=20, cex=.2)
abline(a_hat, b_hat, lwd=3, col="white")
abline(a_hat, b_hat, lwd=1.5)
axis(1, 0, tck=0, las=1, line=-.4)
axis(2, 0, tck=0, las=1)
axis(1, mean(heights$mother_height), round(mean(heights$mother_height), 1), tck=0, las=1, line=-.4)
axis(2, mean(heights$daughter_height), round(mean(heights$daughter_height), 1), tck=0, las=1, line=-.7)
lines(rep(mean(heights$mother_height), 2), c(0, mean(heights$daughter_height)), lwd=.5)
lines(c(0, mean(heights$mother_height)), rep(mean(heights$daughter_height), 2), lwd=.5)
text(40, 43, paste("slope", round(b_hat, 2)), cex=.9)
mtext(paste("The line, y =", round(a_hat), "+", round(b_hat, 2), "x, in the context of the data"), side=3, line=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
