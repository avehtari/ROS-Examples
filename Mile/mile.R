#' ---
#' title: "Regression and Other Stories: Mile"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Trend of record times in the mile run
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
mile <- read.table(root("Mile/data","mile2.txt"), header=TRUE)
year <- mile$yr + mile$month/12
seconds <- mile$min*60 + mile$sec
data <- data.frame(mile, year, seconds)

#' **Linear model*
fit <- stan_glm(seconds ~ year, data = data)
print(fit, digits=3)

#' **Predictions for 1900 and 2000**
print(1007 -.393*c(1900,2000))  # Approx
print(coef(fit)[1] + coef(fit)[2]*c(1900,2000), digits=4) # Exact

#' **Example of increasing trend**
#+ eval=FALSE, include=FALSE
pdf(root("Mile/figs","aplusbx1a.pdf"), height=3.5, width=5)
#+
a <- 0.15
b <- 0.4
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot(c(0,2.2), c(0,a+2.2*b), pch=20, cex=.5, main="y = a + bx",
  bty="l", type="n", xlab="x", ylab="y", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis(1, c(0,1,2))
axis(2, c(a,a+b,a+2*b), c("a","a+b","a+2b"))
abline(a, b)
#+ eval=FALSE, include=FALSE
dev.off()

#' **Example of decreasing trend**
#+ eval=FALSE, include=FALSE
pdf(root("Mile/figs","aplusbx1b.pdf"), height=3.5, width=5)
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
dev.off()

#' **Approximate trend from the fit in range [0,2.1]**
#+ eval=FALSE, include=FALSE
pdf(root("Mile/figs","aplusbx2a.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve(1007 - 0.393*x, from=0, to=2.1, xlab="x", ylab="y", bty="l",
  main="y = 1007 - 0.393x")
#+ eval=FALSE, include=FALSE
dev.off()

#' **Approximate trend from the fit in range [0,100]**
#+ eval=FALSE, include=FALSE
pdf(root("Mile/figs","aplusbx2b.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
curve(1007 - 0.393*x, from=0, to=100, xlab="x", ylab="y", bty="l",
  main="y = 1007 - 0.393x")
#+ eval=FALSE, include=FALSE
dev.off()     

#' **Approximate trend of record times in the mile run from 1900 to 2000**
#+ eval=FALSE, include=FALSE
pdf(root("Mile/figs","aplusbx3.pdf"), height=3.5, width=5)
#+
par(mar=c(3,3,1,1), mgp=c(2,.5,0), tck=-.01)
plot(year, seconds)
curve(1007 - 0.393*x, from=1900, to=2000, xlab="Year", ylab="Time (seconds)", bty="l",
  main="Approx. trend of record times in the mile run", ylim=c(210, 270), add=TRUE)
#+ eval=FALSE, include=FALSE
dev.off()
