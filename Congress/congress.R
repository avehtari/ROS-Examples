#' ---
#' title: "Regression and Other Stories: Congress"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predictive uncertainty for congressional elections. See Chapters 10
#' and 15 in Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' **Load packages**
library("rprojroot")
root<-has_dirname("ROS-Examples")$make_fix_file()
library("rstanarm")

#' **Load and pre-process data**
congress_list <- vector("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- root("Congress/data",paste(year, ".asc", sep=""))
  data_year <- matrix(scan(file), byrow=TRUE, ncol=5)
  data_year <- cbind(rep(year, nrow(data_year)), data_year)
  dvote <- data_year[,5]
  rvote <- data_year[,6]
  vote <- dvote/(dvote + rvote)
  inc <- data_year[,4]
  inc[inc == -9] <- 0
  missing <- dvote== -9 | rvote == -9
  vote[missing & inc== -1] <- 0
  vote[missing & inc== 1] <- 1
  vote[dvote == 0] <- 0
  vote[rvote == 0]  <- 1
  congress_list[[i]] <- data.frame(data_year, vote, inc)
}

i86 <- (1986-1896)/2 + 1
cong84 <- congress_list[[i86-1]]
cong86 <- congress_list[[i86]]
cong88 <- congress_list[[i86+1]]
cong90 <- congress_list[[i86+2]]

# Data have some errors so we'll do a crude search-and-destroy of apparent inconsistencies
inc_fix <- function(data1, data2){
  inc2_clean <- data2$inc
  inc2_clean[data2$inc == -1 & data1$vote > 0.5] <- 1
  inc2_clean[data2$inc == 1 & data1$vote < 0.5] <- -1
  inc2_clean
}

#' Impute uncontested elections
uncontested_adj <- function(vote){
  vote[vote < 0.1] <- 0.25
  vote[vote > 0.9] <- 0.75
  return(vote)
}
congress <- data.frame(inc86=inc_fix(cong84, cong86), inc88=inc_fix(cong86, cong88), inc90=inc_fix(cong88, cong90),
                       v86=cong86$vote, v88=cong88$vote, v90=cong90$vote,
                       v86_adj=uncontested_adj(cong86$vote), v88_adj=uncontested_adj(cong88$vote), v90_adj=uncontested_adj(cong90$vote))
congress$inc88[263] <- -1   # result of special election in Lousisiana 4th district
write.csv(congress, "congress.csv")

inconsistent <- (congress$inc88==(-1) & congress$v86 > 0.5) |  (congress$inc88==1 & congress$v86 < 0.5)

#' **Regression predicting 1988 from 1986**
data88 <- data.frame(vote=congress$v88_adj, past_vote=congress$v86_adj, inc=congress$inc88)
fit88 <- stan_glm(vote ~ past_vote + inc, data=data88)
print(fit88, digits=2)

#' **Simulation for inferences and predictions of new data points**</br>
#' **Predict from 1988 to 1990**
data90 <- data.frame(past_vote=congress$v88_adj, incumbency=congress$inc90)
#' **Simulate predictive simulations of the vector of new outcomes**
pred90 <- posterior_predict(fit88, newdata=data90)

#' **Simulate the number of elections predicted to be won by the Democrats in 1990**
dems_pred <- rowSums(pred90 > 0.5)
#' Alternately calculate that sum in a loop
n_sims <- 4000
demspred <- rep(NA, n_sims)
for (s in 1:n_sims) {
  demspred[s] <- sum(pred90[s,] > 0.5)
}

#' **Our posterior mean and sd of how many districts the Dems will win**
print(c(mean(demspred), sqrt(var(demspred))),digits=2)
#' **Histogram of how many districts the Dems will win**
hist(demspred)


#' **Graphs**

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
