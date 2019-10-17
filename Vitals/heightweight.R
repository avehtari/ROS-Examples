#' ---
#' title: "Regression and Other Stories: Heights and weights"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Height and weight distributions of women and men illustrating
#' central limit theorem and normal distribution. See Chapter 3 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()

#' **Summary data of height and weight distributions of women and men**
height.counts.women <- c(80,107,296,695,1612,2680,4645,8201,9948,11733,10270,9942,6181,3990,2131,1154,245,257,0,0,0,0)*10339/74167
weight.counts.women <- c(362,1677,4572,9363,11420,12328,9435,7023,5047,3621,2753,2081,1232,887,2366)*10339/74167
height.counts.men <- c(0,0,0,0,0,0,0,542,668,1221,2175,4213,5535,7980,9566,9578,8867,6716,5019,2745,1464,1263)*9983/67552

#' **Height distribution for all adults**
height.counts <- height.counts.men + height.counts.women

#' **Tick labels for heights in inches**
height.hist.names <- c("","55","","","","","60","","","","","65","","","","","70","","","","","75")

#' **Bar plots**
barplot (height.counts.women, names.arg=height.hist.names, xlab="height", ylab="Count", main="heights of women\n(histogram)")
barplot (height.counts, names.arg=height.hist.names, xlab="height", ylab="Count", main="heights of all adults\n(histogram)")
barplot (weight.counts.women, xlab="weight", ylab="Count", main="weights of women\n(histogram)")

#' **Normal distribution for heights of women**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("AgePeriodCohort/heights1a_women.pdf"), height=2, width=2.5)
#+
par(mar=c(3,1,3,1), mgp=c(0,0,0), tck=-.01)
curve(dnorm(x,63.7,2.7), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of women\n(normal distribution)", yaxt="n",mgp=c(2,.5,0),cex.main=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Normal distribution for heights of men**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("AgePeriodCohort/heights1a_men.pdf"), height=2, width=2.5)
#+
par(mar=c(3,1,3,1), mgp=c(0,0,0), tck=-.01)
curve(dnorm(x,69.1,2.9), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of men\n(normal distribution)", yaxt="n",mgp=c(2,.5,0),cex.main=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Mixture of normals distribution for heights of all adults**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("AgePeriodCohort/heights1b.pdf"), height=2, width=2.5)
#+
par(mar=c(3,1,3,1), mgp=c(0,0,0), tck=-.01)
curve(.52*dnorm(x,63.7,2.7)+.48*dnorm(x,69.1,2.9), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of all adults\n(not a normal distribution)", yaxt="n",mgp=c(2,.5,0),cex.main=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Normal distribution for log weights of men**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("AgePeriodCohort/weights1a.pdf"), height=2, width=2.5)
#+
par(mar=c(3,1,3,1), mgp=c(0,0,0), tck=-.01)
curve (dnorm(x,5.13,.17), 4, 6, xlab="logarithm of weight in pounds", ylab="", bty="n", yaxs="i", main="log weights of men\n(normal distribution)", yaxt="n",mgp=c(2,.5,0),cex.main=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **Log-normal distribution for weights of men**
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("AgePeriodCohort/weights1b.pdf"), height=2, width=2.5)
#+
par(mar=c(3,1,3,1), mgp=c(0,0,0), tck=-.01)
curve (dlnorm(x,5.13,.17), 50,350,xlab="weight in pounds", ylab="", bty="n", yaxs="i", main="weights of men\n(lognormal distribution)", yaxt="n",mgp=c(2,.5,0),cex.main=.8)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
