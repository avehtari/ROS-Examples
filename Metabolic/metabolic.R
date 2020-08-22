#' ---
#' title: "Regression and Other Stories: Metabolic"
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

#' An example of how to interpret a power law or log-log
#' regression. See Chapter 3 in Regression and Other Stories.
#' 
#' Animals' body mass and metabolism comes from section 3.8.2 of
#' Gelman and Nolan: Teaching Statistics: A Bag of Tricks, second
#' edition. Oxford University Press, 2017.
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


#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Metabolic/figs","metabolic2a.pdf"), height=3.8, width=5)
#+
par (mar=c(3,3,1,1), mgp=c(1.8,.5,0), tck=-.01)
plot (c(log(.01), log(3000)), c(log(.06),log(1700)), xaxt="n", yaxt="n",
  xlab="log (body mass in kilograms)",
  ylab="log (metabolic rate in watts)",
  bty="l", type="n")
axis(1, seq(-4,8,2))
axis(2, seq(-2,6,2))
grid(col="gray")
abline(1.4, 0.74)
points(log(0.02), log(0.17), pch=20)
text(log(.02) + 0.2, log(0.17),"Mouse", cex=.9, adj=0)
points(log(65), log(90), pch=20)
text(log(65) - 0.2,log(90),"Man", cex=.9, adj=1)
points(log(3000), log(2000),pch=20)
text(log(3000) - 0.2, log(2000),"Elephant", cex=.9, adj=1)
text(log(2), log(3.5), "log y = 1.4 + 0.74 log x", cex=.9, adj=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Metabolic/figs","metabolic2b.pdf"), height=3.8, width=5)
#+
par (mar=c(3,3,1,1), mgp=c(1.8,.5,0), tck=-.01)
plot (c(0, 3000), c(0, 2000), 
  xlab="body mass in kilograms",
  ylab="metabolic rate in watts",
  bty="l", type="n")
curve(exp(1.4 + 0.74*log(x)), add=TRUE)
points(0.02, 0.17, pch=20)
text(0.02 + 50, 0.17, "Mouse", cex=.9, adj=0)
points(65, 90, pch=20)
text(65 + 50, 90 + 80, "Man", cex=.9, adj=1)
points(3000, 2000, pch=20)
text(3000 - 50, 2000, "Elephant", cex=.9, adj=1)
text(1250, 700, expression(y ~ "=" ~  e^{1.4 ~ + ~ 0.74 ~ l*o*g ~ x} ~ "=" ~ 4.1 ~ x ^ 0.74), adj=0)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
