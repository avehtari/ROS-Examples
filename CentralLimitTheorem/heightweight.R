#' ---
#' title: "Regression and Other Stories: Heights and weights"
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

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' #### Summary data of height and weight distributions of women and men
height_counts_women <- c(80,107,296,695,1612,2680,4645,8201,9948,11733,10270,9942,6181,3990,2131,1154,245,257,0,0,0,0)*10339/74167
weight_counts_women <- c(362,1677,4572,9363,11420,12328,9435,7023,5047,3621,2753,2081,1232,887,2366)*10339/74167
height_counts_men <- c(0,0,0,0,0,0,0,542,668,1221,2175,4213,5535,7980,9566,9578,8867,6716,5019,2745,1464,1263)*9983/67552

#' #### Height distribution for all adults
height_counts <- height_counts_men + height_counts_women

#' #### Tick labels for heights in inches
height_hist_names <- c("","55","","","","","60","","","","","65","","","","","70","","","","","75")

#' #### Bar plots
barplot (height_counts_women, names.arg=height_hist_names, xlab="height", ylab="Count", main="heights of women\n(histogram)")
barplot (height_counts, names.arg=height_hist_names, xlab="height", ylab="Count", main="heights of all adults\n(histogram)")
barplot (weight_counts_women, xlab="weight", ylab="Count", main="weights of women\n(histogram)")

#' #### Normal distribution for heights of women
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","heights1a_women.pdf"), height=2.5, width=3.5)
#+
par(mar=c(3,1,3,1), tck=-.02)
curve(dnorm(x,63.7,2.7), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of women\n(normal distribution)", yaxt="n",mgp=c(1.5,.5,0),cex.main=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Normal distribution for heights of men
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","heights1a_men.pdf"), height=2.5, width=3.5)
#+
par(mar=c(3,1,3,1), tck=-.02)
curve(dnorm(x,69.1,2.9), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of men\n(normal distribution)", yaxt="n", mgp=c(1.5,.5,0), cex.main=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Mixture of normals distribution for heights of all adults
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","heights1b.pdf"), height=2.5, width=3.5)
#+
par(mar=c(3,1,3,1), tck=-.02)
curve(.52*dnorm(x,63.7,2.7)+.48*dnorm(x,69.1,2.9), 52, 81, xlab="height (inches)", ylab="", bty="n", yaxs="i", main="heights of all adults\n(not a normal distribution)", yaxt="n",mgp=c(1.5,.5,0), cex.main=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Normal distribution for log weights of men
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","weights1a.pdf"), height=2.5, width=3.5)
#+
par(mar=c(3,1,3,1), tck=-.02)
curve(dnorm(x,5.13,.17), 4, 6, xlab="logarithm of weight in pounds", ylab="", bty="n", yaxs="i", main="log weights of men\n(normal distribution)", yaxt="n", mgp=c(1.5,.5,0), cex.main=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' #### Log-normal distribution for weights of men
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","weights1b.pdf"), height=2.5, width=3.5)
#+
par(mar=c(3,1,3,1), tck=-.02)
curve(dlnorm(x,5.13,.17), 50, 350,xlab="weight in pounds", ylab="", bty="n", yaxs="i", main="weights of men\n(lognormal distribution)", yaxt="n", mgp=c(1.5,.5,0), cex.main=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Vitals/figs","normal_picture.pdf"), height=3.5, width=7, colormodel="gray")
#+
par(mar=c(2,0,2,0), tck=-.01)
curve(dnorm(x), -4, 4, ylim=c(0, 0.4), xlab="", ylab="", bty="n", yaxs="i", main="normal distribution", xaxt="n", yaxt="n")
axis(1, c(-4, -3, -2, -1,  0,  1, 2, 3, 4), c("", "-3", "-2", "-1",  "0",  "1", "2", "3", ""), mgp=c(1.5, .5, 0), cex.axis=1.2)
colors <- c("gray70", "gray50", "gray30")
for (i in 3:1){
  grid <- seq(-i, i, .01)
  polygon(c(grid, i, -i), c(dnorm(grid), 0, 0), col=colors[i])
}
text(0, .35*dnorm(0), "68%", cex=1.3)
text(-1.5, .3*dnorm(1.5), "13.5%", cex=1.3)
text(1.5, .3*dnorm(1.5), "13.5%", cex=1.3)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
