setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Storable")

library("arm")
library("rstanarm")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## R code for analysis of storable votes for the book

data_2player <- read.csv("2playergames.csv")
data_3player <- read.csv("3playergames.csv")
data_6player <- read.csv("6playergames.csv")
data_all <- rbind(data_2player, data_3player, data_6player)
data_all$factor_vote <- factor(data_all$vote, levels = c(1, 2, 3), labels = c("1", "2", "3"))
## Simple analysis using data from just one person

subset <- data_2player[,"person"]==401
y <- data_2player[subset, "vote"]
x <- data_2player[subset, "value"]

## From Jonah
data_2player <- read.csv("2playergames.csv")
data_401 <- subset(data_2player, person == 401, select = c("vote", "value"))
fit_1 <- stan_polr(factor(vote) ~ value, data = data_401, prior = R2(0.5, "mean"))
print(fit_1)

## 6 people

plotted <- c(101, 303, 409, 405, 504, 112)
story <- c("Perfectly monotonic",
           "One fuzzy and one sharp cutpoint",
           "Monotonic with one outlier",
           "Only 1's and 3's",
           "Almost only 3's",
           "Erratic")
n_plotted <- length(plotted)
data <- as.list(rep(NA, n_plotted))
fit <- as.list(rep(NA, n_plotted))
for (i in 1:n_plotted){
  ok <- data_all[,"person"]==plotted[i]
  data[[i]] <- data_all[ok,]
  fit[[i]] <- stan_polr(factor_vote ~ value, data=data[[i]], prior=R2(0.5, "mean"))
}

## Graph

pdf("sampledata4.pdf", height=5, width=8)
par(mfrow=c(2,3), mgp=c(1.5,.5,0), tck=-.01)
for (i in 1:n_plotted){
  sims <- as.matrix(fit[[i]])
  n_cutpoints <- 2
  cutpoints <- rep(NA, n_cutpoints)
  for (i_cut in 1:n_cutpoints){
    cutpoints[i_cut] <- median(sims[,i_cut+1]/sims[,1])
  }
  s <- median(1/sims[,1])
  plot(data[[i]][,"value"], data[[i]][,"vote"], xlim=c(0,100), ylim=c(1,3),
        xlab="Value", ylab="Vote", main=story[i], yaxt="n")
  axis (2, 1:(n_cutpoints+1))
  temp <- seq(0, 100, 0.1)
  prob <- array(NA, c(length(temp), n_cutpoints+1))
  expected <- rep(NA, length(temp))
  prob[,1] <- 1 - invlogit((temp-cutpoints[1]/s))
  expected <- 1*prob[,1]
  for (i_cut in 2:n_cutpoints){
    prob[,i_cut] <- invlogit((temp-cutpoints[i_cut-1])/s) -
      invlogit((temp-cutpoints[i_cut])/s)
    expected <- expected + i_cut*prob[,i_cut]
  }
  prob[,n_cutpoints+1] <- invlogit((temp-cutpoints[n_cutpoints])/s)
  expected <- expected + (n_cutpoints+1)*prob[,n_cutpoints+1]
  lines (temp, expected, lwd=.5)
  for (i_cut in 1:n_cutpoints){
    lines(rep(cutpoints[i_cut],2), i_cut+c(0,1), lwd=.5)
  }
}
dev.off()

