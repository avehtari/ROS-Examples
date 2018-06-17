#' ---
#' title: "Regression and Other Stories: Death"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Death - proportion of American adults supporting the death penalty
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()

#+ eval=FALSE, include=FALSE
postscript(root("Death/figs","polls.ps"), horizontal=TRUE)
#+
par(mar=c(5,5,4,2)+.1)
polls <- matrix(scan(root("Death/data","polls.dat")), ncol=5, byrow=TRUE)
support <- polls[,3]/(polls[,3]+polls[,4])
year <-  polls[,1] + (polls[,2]-6)/12
plot(year, support*100, xlab="Year",
      ylab="Percentage support for the death penalty", cex=2, cex.main=2,
      cex.axis=2, cex.lab=2, type="l")
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
postscript(root("Death/figs","states.ps"), horizontal=TRUE)
#+
death <- read.table(root("Death/data","dataforandy.txt"), header=TRUE)
ex.rate <- death[,7]/100
err.rate <- death[,6]/100
hom.rate <- death[,4]/100000
ds.per.homicide <- death[,2]/1000
ds <- death[,1]
ex <- ex.rate*ds
err <- err.rate*ds
hom <- ds/ds.per.homicide
pop <- hom/hom.rate
state.abbrs <- row.names(death)
std.err.rate <- sqrt((err+1)*(ds+1-err)/((ds+2)^2*(ds+3)))
par(mar=c(5,5,4,2)+.1)
plot(ds/hom, err.rate, xlab="Death sentences per homicide",
      ylab="Rate of reversal of death sentences", cex=2, cex.main=2,
      cex.axis=2, cex.lab=2, type="n")
text(ds/hom, err.rate, state.abbrs, cex=1.5)
for (i in 1:length(ds)){
  lines(rep(ds[i]/hom[i],2), err.rate[i] + c(-1,1)*std.err.rate[i], lwd=.5)
}
#+ eval=FALSE, include=FALSE
dev.off()

#+ eval=FALSE, include=FALSE
postscript(root("Death/figs","deathpolls.ps"), horizontal=TRUE)
#+
par(mar=c(5,5,4,2)+.1)
polls <- matrix(scan(root("Death/data","polls.dat")), ncol=5, byrow=TRUE)
support <- polls[,3]/(polls[,3]+polls[,4])
year <-  polls[,1] + (polls[,2]-6)/12
plot(year, support*100, xlab="Year", ylim=c(min(100*support)-1, max(100*support)+1),
      ylab="Percentage support for the death penalty", cex=2, cex.main=2,
      cex.axis=2, cex.lab=2, pch=20)
for (i in 1:nrow(polls))
  lines(rep(year[i],2), 100*(support[i]+c(-1,1)*sqrt(support[i]*(1-support[i])/1000)))
#+ eval=FALSE, include=FALSE
dev.off()
