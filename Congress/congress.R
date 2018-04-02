#' ---
#' title: "Regression and Other Stories: Congress"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predictive uncertainty for congressional elections
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("arm")
library("rstanarm")
options(mc.cores = parallel::detectCores())

#' **Load and pre-process data**
congress <- vector("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- root("Congress/data",paste(year, ".asc", sep=""))
  data_year <- matrix(scan(file), byrow=TRUE, ncol=5)
  data_year <- cbind(rep(year, nrow(data_year)), data_year)
  congress[[i]] <- data_year
}

i86 <- (1986-1896)/2 + 1
cong86 <- congress[[i86]]
cong88 <- congress[[i86+1]]
cong90 <- congress[[i86+2]]

v86 <- cong86[,5]/(cong86[,5]+cong86[,6])
bad86 <- cong86[,5]==-9 | cong86[,6]==-9
v86[bad86] <- NA
contested86 <- v86>.1 & v86<.9
inc86 <- cong86[,4]
inc86[inc86 == -9] <- 0

v88 <- cong88[,5]/(cong88[,5]+cong88[,6])
bad88 <- cong88[,5]==-9 | cong88[,6]==-9
v88[bad88] <- NA
contested88 <- v88>.1 & v88<.9
inc88 <- cong88[,4]
inc88[inc88 == -9] <- 0

v90 <- cong90[,5]/(cong90[,5]+cong90[,6])
bad90 <- cong90[,5]==-9 | cong90[,6]==-9
v90[bad90] <- NA
contested90 <- v90>.1 & v90<.9
inc90 <- cong90[,4]
inc90[inc90 == -9] <- 0

#' Impute uncontested elections
uncontested_adj <- function(vote){
  vote[vote < 0.1] <- 0.25
  vote[vote > 0.9] <- 0.75
  return(vote)
}
v86_adj <- uncontested_adj(v86)
v88_adj <- uncontested_adj(v88)
v90_adj <- uncontested_adj(v90)

#' **Regression predicting 1988 from 1986**
data_88 <- data.frame(vote=v88_adj, past_vote=v86_adj, inc=inc88)
fit_88 <- stan_glm(vote ~ past_vote + inc, data=data_88)
print(fit_88, digits=2)

#' **Simulation for inferences and predictions of new data points**</br>
#' **Predict from 1988 to 1990**
data_90 <- data.frame(past_vote=v88_adj, incumbency=inc90)
#' **Simulate predictive simulations of the vector of new outcomes**
pred_90 <- posterior_predict(fit_88, data=data_90)

#' **Simulate the number of elections predicted to be won by the Democrats in 1990**
dems_pred <- rowSums(pred_90 > 0.5)
#' Alternately calculate that sum in a loop
n_sims <- 4000
dems_pred <- rep(NA, n_sims)
for (s in 1:n_sims) {
  dems_pred[s] <- sum(pred_90[s,] > 0.5)
}

#' **Our posterior mean and sd of how many districts the Dems will win**
print(c(mean(dems_pred), sqrt(var(dems_pred))),digits=2)
#' **Histogram of how many districts the Dems will win**
hist(dems_pred)
