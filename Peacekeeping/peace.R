#' ---
#' title: "Regression and Other Stories: Peacekeeping"
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

#' Outcomes after civil war in countries with and without United
#' Nations peacekeeping. See Chapter 1 in Regression and Other
#' Stories.
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

#' #### Load data
peace <- read.dta(root("Peacekeeping/data","pk&pkept_old.dta"))

#' #### What percentage of countries had returned to civil wars:
censored <- peace$morewar==0
badness <- log(peace$hazard1)
peacekeepers <- peace$pk_dum==1
cfdate <- peace$cfdate
faildate <- peace$faildate
faildate[is.na(faildate)&!is.na(cfdate)] <- "2004-12-31"
delay <- (as.numeric(faildate) - as.numeric(cfdate))/365.24
ok <- peace$pcw==1 & !is.na(delay)
##ok <- cease_fire & !is.na(badness) & peace$t0 > 1979-12-31 & peace$pcw==1

#' 
#' -------------
#'

print(sum(ok))
print(mean(censored[ok]))
print(unique(peace[ok,3]))

print(mean(censored[ok & peacekeepers]))
print(mean(censored[ok & !peacekeepers]))

print(mean(delay[ok & !censored & peacekeepers]))
print(mean(delay[ok & !censored & !peacekeepers]))
print(median(delay[ok & !censored & peacekeepers]))
print(median(delay[ok & !censored & !peacekeepers]))

#' 
#' -------------
#'

#' #### Plot
#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Peacekeeping/figs","peacekeeping_1.pdf"), height=4, width=10)
#+
par(mfrow=c(1,2), mar=c(3,5,3,0), mgp=c(1.5,.5,0), tck=-.01)
subset <- ok & peacekeepers
hist(delay[subset & !censored], xlim=c(0,8), breaks=seq(0,8,.5), xlab="Years until return of war", ylab="", main=paste("With peacekeeping:  ", round(100*mean(censored[subset])), "% of countries stayed at peace.\nFor others, histogram of time until civil war returned:", sep=""), cex.main=.9, cex.axis=.9, cex.lab=.9)
subset <- ok & !peacekeepers
hist(delay[subset & !censored], xlim=c(0,8), breaks=seq(0,8,.5), xlab="Years until return of war", ylab="", main=paste("Without peacekeeping:  ", round(100*mean(censored[subset])), "% stayed at peace.\nFor others, histogram of time until civil war returned:", sep=""), cex.main=.9, cex.axis=.9, cex.lab=.9)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Peacekeeping/figs","peacekeeping_2.pdf"), height=5, width=7)
#+
ok2 <- ok & !is.na(badness)
badness2 <- badness/2 + 8
par(mar=c(3,4,2,0), tck=-.01, mgp=c(1.7,.5,0))
plot(badness2[ok2], delay[ok2], type="n", xlab="Pre-treatment measure of problems with the country", ylab="Delay (in years) before return of conflict\n(open circles indicate conflict has not yet returned)", bty="n", xaxt="n")
axis(1, quantile(badness2[ok2], c(.05, .95)), c("not so bad", "really bad"))
axis(1, range(badness2[ok2]), c("",""), tck=0)
points(badness2[ok2&peacekeepers&!censored], delay[ok2&peacekeepers&!censored], col="red", pch=20, cex=.9) # dot
points(badness2[ok2&!peacekeepers&!censored], delay[ok2&!peacekeepers&!censored], col="black", pch=20, cex=.9) # dot
points(badness2[ok2&peacekeepers&censored], delay[ok2&peacekeepers&censored], col="red", pch=21) # circle
points(badness2[ok2&!peacekeepers&censored], delay[ok2&!peacekeepers&censored], col="black", pch=21) # circle
mtext("Peacekeeping (red) is associated with slightly longer periods without war", line=1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf(root("Peacekeeping/figs","peacekeeping_3.pdf"), height=4, width=10)
#+
par(mfrow=c(1,2), mar=c(3,4,2,0), tck=-.01, mgp=c(1.7,.5,0))
ok2a <- ok&peacekeepers
plot(badness2[ok2], delay[ok2], type="n", xlab="Pre-treatment measure of problems with the country", ylab="Delay (in years) before return of conflict\n(open circles where conflict did not return)", bty="n", xaxt="n")
axis(1, quantile(badness2[ok2], c(.05, .95)), c("not so bad", "really bad"))
axis(1, range(badness2[ok2]), c("",""), tck=0)
points(badness2[ok2a&peacekeepers&!censored], delay[ok2a&peacekeepers&!censored], col="black", pch=20, cex=.6) # dot
points(badness2[ok2a&peacekeepers&censored], delay[ok2a&peacekeepers&censored], col="black", pch=1, cex=1.1) # circle
mtext("With U.N. peacekeeping", line=1, cex=1.1)
ok2b <- ok&!peacekeepers
plot(badness2[ok2], delay[ok2], type="n", xlab="Pre-treatment measure of problems with the country", ylab="", bty="n", xaxt="n")
axis(1, quantile(badness2[ok2], c(.05, .95)), c("not so bad", "really bad"))
axis(1, range(badness2[ok2]), c("",""), tck=0)
points(badness2[ok2b&!peacekeepers&!censored], delay[ok2b&!peacekeepers&!censored], col="black", pch=20, cex=.6) # dot
points(badness2[ok2b&!peacekeepers&censored], delay[ok2b&!peacekeepers&censored], col="black", pch=1, cex=1.1) # circle
mtext("Without U.N. peacekeeping", line=1, cex=1.1)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' 
#' -------------
#' 

log.delay <- log(delay)
plot (badness[ok], log.delay[ok], type="n", xlab="badness", ylab="delay")
points (badness[ok&peacekeepers], log.delay[ok&peacekeepers], pch=21) # circle
points (badness[ok&!peacekeepers], log.delay[ok&!peacekeepers], pch=20) # bullet

sqrt.delay <- sqrt(delay)
plot (badness[ok], sqrt.delay[ok], type="n", xlab="badness", ylab="delay")
points (badness[ok&peacekeepers], sqrt.delay[ok&peacekeepers], pch=21) # circle
points (badness[ok&!peacekeepers], sqrt.delay[ok&!peacekeepers], pch=20) # bullet

table(peacekeepers[ok], censored[ok])

par(mfrow=c(2,2))
time <- as.numeric (cfdate)
hist(time[peacekeepers&ok])
hist(time[!peacekeepers&ok])

## minidata <- cbind (cfdate, faildate, peacekeepers, round(badness,2), round(delay,2), censored)[ok,]
## rownames (minidata) <- peace$cname[ok]
## colnames (minidata) <- c ("cfdate", "faildate", "peacekeepers?", "badness", "delay", "censored?")
## write.csv(minidata, "minidata.csv")
