#' ---
#' title: "Regression and Other Stories: Coop"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Coop - Example of hypothesis testing. See Chapter 4 in Regression
#' and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)
# switch this to TRUE to save figures in separate files
savefigs <- FALSE

#' **Load packages**
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()

#' **Load data**
data <- read.table(root("Coop/data","Riverbay.csv"), header=FALSE, sep=",")
votes <- data[,2:7]
candidate.totals <- votes[,6]
time.totals <- apply(votes, 2, sum)
voters <- c(600,1200,2444,3444,4444,5553)
extras <- votes
extras.voters <- voters
for (j in 2:6){
  extras[,j] <- votes[,j]-votes[,j-1]
  extras.voters[j] <- voters[j]-voters[j-1]
}
extras.totals <- apply(extras,2,sum)
names.old <- as.vector(data[,1])
names <- as.vector(data[,8])
winners <- rev(order(candidate.totals))
n.candidates <- length(candidate.totals)
actual <- rep(NA, n.candidates)
expected <- rep(NA, n.candidates)

#' Plot
#+ fig.width=8, fig.height=8
par(mfrow=c(6,5), mar=c(3,4,2,0), pty="m")
for (i in winners) {
  y <- extras[i,]/extras.voters
  plot(voters, y, ylim=range(0,y), type="l", xlab="", ylab="",
        main=names[i])
  p.hat <- candidate.totals[i]/5553
  actual[i] <- sd(as.numeric(y))
  expected[i] <- sqrt(mean(p.hat*(1-p.hat)/extras.voters))
}

#' Plot
par(mfrow=c(1,2))
par(pty="s")
plot(expected, actual, xlim=range(expected,actual),
      ylim=range(expected,actual))
abline(0,1)
#
plot(candidate.totals, actual)
points(candidate.totals, expected, col="red")

#' Plot
#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Coop/figs","coop1.ps"), height=3.5, horizontal=TRUE)
#+ eval=FALSE
par(mfrow=c(2,4), mar=c(3,4,2,0), pty="m")
for (i in winners[1:8]){
  y <- votes[i,]/voters
  plot(voters, y, ylim=c(0,.59), xlim=c(0,max(voters)*1.05),
        yaxs="i", xaxs="i", type="l", xlab="", ylab="",
        main=names[i], cex.main=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Coop/figs","coop2.ps"), , height=3.5, horizontal=TRUE)
#+
par(mfrow=c(2,4), mar=c(3,4,2,0), pty="m")
for (i in winners[1:8]){
  y <- extras[i,]/extras.voters
  plot(voters, y, ylim=c(0,.59), xlim=c(0,max(voters)*1.05),
        yaxs="i", xaxs="i", type="l", xlab="", ylab="",
        main=names[i], cex.main=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
}
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) postscript(root("Coop/figs","coop3.ps"), horizontal=TRUE)
#+
par(mar=c(5,5,4,2)+.1)
plot(candidate.totals, actual, xlim=c(0,max(candidate.totals)*1.05),
      ylim=c(0,max(actual)*1.08), xlab="total # of votes for the candidate",
      ylab="sd of separate vote proportions", pch=21, cex.lab=2, cex.axis=2, cex=2,
      xaxs="i", yaxs="i")
points(candidate.totals, expected, pch=20, cex=2)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#' **chi^2 tests**
chisq <- rep(NA, nrow(extras))
for (i in 1:nrow(extras)){
  observed <- rbind(extras[i,], extras.voters-extras[i,])
  expected <- rbind(extras.voters*sum(extras[i,])/sum(extras.voters),
                     extras.voters*(1-sum(extras[i,])/sum(extras.voters)))
  chisq[i] <- sum((observed-expected)^2/expected)
}
pvalue <- pchisq(chisq, 5)
chisq.total <- sum(chisq)
df.total <- 5*nrow(extras)
#
expected.extras <- outer(apply(extras,1,sum), apply(extras,2,sum))/sum(extras)
chisq.extras <- sum((extras-expected.extras)^2/expected.extras)
df.extras <- (27-1)*(6-1)
