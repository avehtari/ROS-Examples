#' ---
#' title: "Regression and Other Stories: Congress"
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

#' Predictive uncertainty for congressional elections. See Chapters 10
#' and 15 in Regression and Other Stories.
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

#' #### Load data
congress <- read.csv(root("Congress/data","congress.csv"))
inconsistent <- (congress$inc88==(-1) & congress$v86 > 0.5) |  (congress$inc88==1 & congress$v86 < 0.5)
head(congress)

#' ## Regression predicting 1988 from 1986
data88 <- data.frame(vote=congress$v88_adj, past_vote=congress$v86_adj, inc=congress$inc88)
fit88 <- stan_glm(vote ~ past_vote + inc, data=data88, refresh=0)
print(fit88, digits=2)

#' ## Simulation for inferences and predictions of new data points
#' #### Predict from 1988 to 1990
data90 <- data.frame(past_vote=congress$v88_adj, inc=congress$inc90)
#' #### Simulate predictive simulations of the vector of new outcomes
pred90 <- posterior_predict(fit88, newdata=data90)

#' #### Simulate the number of elections predicted to be won by the Democrats in 1990
dems_pred <- rowSums(pred90 > 0.5)
#' Alternately calculate that sum in a loop
n_sims <- 4000
dems_pred <- rep(NA, n_sims)
for (s in 1:n_sims) {
  dems_pred[s] <- sum(pred90[s,] > 0.5)
}

#' #### Our posterior mean and sd of how many districts the Dems will win
print(c(mean(dems_pred), sd(dems_pred)), digits=2)
#' #### Histogram of how many districts the Dems will win
hist(dems_pred)


#' ## Graphs

#+ eval=FALSE, include=FALSE
if (savefigs) pdf("hist88.pdf", height=3.2, width=4.1, colormodel="gray")
#+
par(mar=c(3,0,1,0), mgp=c(1.5, .5, 0), tck=-.01)
v88_hist <- ifelse(congress$v88<.1, .0001, ifelse(congress$v88>.9, .9999, congress$v88))
hist(v88_hist, breaks=seq(0,1,.05),
     xlab="Democratic share of the two-party vote", ylab="", yaxt="n", main="")
mtext("Congressional elections in 1988", 3, 0)
if (savefigs) dev.off()

if (savefigs) pdf("cong.pdf", height=4, width=4, colormodel="gray")
par(pty="s")
par(mar=c(3,0,3,0), mgp=c(1.7, .5, 0), tck=-.01)
plot(0, 0, xlim=c(0,1), ylim=c(0,1), type="n", xaxs="i", yaxs="i",
  xlab="Democratic vote share in 1986", ylab="Democratic vote share in 1988")
abline(0,1, lwd=.5)
jitt <- function(vote){
  n <- length(vote)
  ifelse(vote<0.1, runif(n, 0.01, 0.04), ifelse(vote>0.9, runif(n, 0.96, 0.99), vote))
}
j_v86 <- jitt(congress$v86)
j_v88 <- jitt(congress$v88)
points(j_v86[congress$inc88==0], j_v88[congress$inc88==0], pch=1, cex=1)
points(j_v86[congress$inc88==1], j_v88[congress$inc88==1], pch=16, cex=.8)
points(j_v86[congress$inc88==-1], j_v88[congress$inc88==-1], pch=4, cex=.8)
mtext("Raw data", 3, .7)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()

#+ eval=FALSE, include=FALSE
if (savefigs) pdf("congclean.pdf", height=4, width=4, colormodel="gray")
#+
par(pty="s")
par(mar=c(3,0,3,0), mgp=c(1.7, .5, 0), tck=-.01)
plot(0, 0, xlim=c(0,1), ylim=c(0,1), type="n", xaxs="i", yaxs="i",
  xlab="Adjusted Dem. vote share in 1986", ylab="Adjusted Dem. vote share in 1988")
abline(0,1, lwd=.5)
points(congress$v86_adj[congress$inc88==0], congress$v88_adj[congress$inc88==0], pch=1, cex=1)
points(congress$v86_adj[congress$inc88==1], congress$v88_adj[congress$inc88==1], pch=16, cex=.8)
points(congress$v86_adj[congress$inc88==-1], congress$v88_adj[congress$inc88==-1], pch=4, cex=.8)
mtext("Adjusted data", 3, .7)
#+ eval=FALSE, include=FALSE
if (savefigs) dev.off()
