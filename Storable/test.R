setwd("~/AndrewFiles/books/regression.and.other.stories/Examples/Storable")

library("rstanarm")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## R code for analysis of storable votes for the book

data_2player <- read.csv("2playergames.csv")
data_3player <- read.csv("3playergames.csv")
data_6player <- read.csv("6playergames.csv")
data_all <- rbind(data_2player, data_3player, data_6player)

## Simple analysis using data from just one person (person #101)

ok <- data_all[,"person"]==101
data <- data_all[ok,]
fit_1 <- polr(factor(vote) ~ value, data=data)
fit_2 <- stan_polr(factor(vote) ~ value, data=data, prior=R2(0.5, "mean"))
