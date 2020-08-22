#' ---
#' title: "Regression and Other Stories: Stents"
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

#' Stents - comparing distributions. See Chapter 3 in
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

#' #### Exercise time
n <- c(104,90)
y_bar_pre <- c(528.0, 490.0)
y_bar_post <- c(556.3, 501.8)
s_pre <- c(178.7, 195.0)
s_post <- c(178.7, 190.9)
interval_width_diff <- c(45.1 - 11.6, 31.3 - (-7.8))
se_diff <- interval_width_diff/(2*qt(.975, n-1))
s_diff <- se_diff*sqrt(n)
rho <- (s_pre^2 + s_post^2 - s_diff^2)/(2*s_pre*s_post)
b <- rho*s_post/s_pre
b <- mean(b)
diff <- function (a) return(a[1] - a[2])
diff_simple <- diff(y_bar_post)
diff_gain <- diff(y_bar_post - y_bar_pre)
diff_regression <- diff(y_bar_post - b*y_bar_pre)
se_simple <- sqrt(s_post^2/n)
se_diff_simple <- sqrt(sum(se_simple^2))
se_gain <- sqrt(s_pre^2/n + s_post^2/n - 2*rho*s_pre*s_post/n)
se_diff_gain <- sqrt(sum(se_gain^2))
se_regression <- sqrt(b^2*s_pre^2/n + s_post^2/n - 2*b*rho*s_pre*s_post/n)
se_diff_regression <- sqrt(sum(se_regression^2))
diffs <- c(diff_simple, diff_gain, diff_regression)
ses <- c(se_diff_simple, se_diff_gain, se_diff_regression)
round(diffs, 1)
round(ses, 1)
round(diffs/ses, 1)
round(2*pnorm(-diffs/ses), 2)

#' #### Bootstrap
z <- (diffs/ses)[3]
print(1-pnorm(1.96 - z))
print(pnorm(-1.96 - z))

#' #### Treadmill score
n <- c(104,90)
y_bar_pre <- c(4.24, 4.18)
y_bar_post <- c(5.46, 4.28)
s_pre <- c(4.82, 4.65)
s_post <- c(4.79, 4.98)
interval_width_diff <- c(2.07 - 0.37, 1.19 - (-0.99))
se_diff <- interval_width_diff/(2*qt(.975, n-1))
s_diff <- se_diff*sqrt(n)
rho <- (s_pre^2 + s_post^2 - s_diff^2)/(2*s_pre*s_post)
b <- rho*s_post/s_pre
b <- mean(b)
diff <- function (a) return(a[1] - a[2])
diff_simple <- diff(y_bar_post)
diff_gain <- diff(y_bar_post - y_bar_pre)
diff_regression <- diff(y_bar_post - b*y_bar_pre)
se_simple <- sqrt(s_post^2/n)
se_diff_simple <- sqrt(sum(se_simple^2))
se_gain <- sqrt(s_pre^2/n + s_post^2/n - 2*rho*s_pre*s_post/n)
se_diff_gain <- sqrt(sum(se_gain^2))
se_regression <- sqrt(b^2*s_pre^2/n + s_post^2/n - 2*b*rho*s_pre*s_post/n)
se_diff_regression <- sqrt(sum(se_regression^2))
diffs <- c(diff_simple, diff_gain, diff_regression)
ses <- c(se_diff_simple, se_diff_gain, se_diff_regression)
round(diffs, 1)
round(ses, 1)
round(diffs/ses, 1)
round(2*pnorm(-diffs/ses), 2)

#' #### Graph showing distribution shift
#+ eval=FALSE, include=FALSE
if (savefigs) pdf("stents_shift_1.pdf", height=3, width=6)
#+ 
par(mar=c(3,3,0,1), mgp=c(1.5,.5,0), tck=-.01)
plot(c(0,1000), c(0, 1.1), yaxs="i", yaxt="n", xlab="Exercise time (seconds)", ylab="", bty="n", type="n")
curve(dnorm(x, 510, 190)/dnorm(0, 0, 190), add=TRUE)
curve(dnorm(x, 530, 190)/dnorm(0, 0, 190), add=TRUE)
text(270, .7, "Controls")
text(755, .7, "Treated")
abline(c(510, 510), c(0, 1))
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
