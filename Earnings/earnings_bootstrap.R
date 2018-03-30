#' ---
#' title: "Regression and Other Stories: Earnings"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Bootstrapping to simulate the sampling distribution
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("here")
library("arm")

#' **Load data**
earnings <- read.csv(here("Earnings/data","earnings.csv"))
earnings_all <- read.csv(here("Earnings/data","earnings.csv"))
earnings_all$positive <- earnings_all$earn > 0
#' only non-zero earnings
earnings <- earnings_all[earnings_all$positive, ]
n <- nrow(earnings)
earn <- earnings$earn
male <- earnings$male
print(earnings[1:10,])

#' **Median of women's earnings, divided by the median of men's earnings**
print(median(earn[male==0]) / median(earn[male==1]))

#' **A single bootstrap sample**
n <- length(earn)
boot <- sample(n, replace=TRUE)
earn_boot <- earn[boot]
male_boot <- male[boot]
ratio_boot <- median(earn_boot[male_boot==0]) / median(earn_boot[male_boot==1])

#' **A set of bootstrap simulations**
Boot_ratio <- function(data){
  n <- nrow(data)
  boot <- sample(n, replace=TRUE)
  earn_boot <- data$earn[boot]
  male_boot <- data$male[boot]
  ratio_boot <- median(earn_boot[male_boot==0]) / median(earn_boot[male_boot==1])
  return(ratio_boot)
}
n_sims <- 10000
output <- replicate(n_sims, Boot_ratio(data=earnings))

#' **Summarize the results graphically and numerically**
hist(output)
print(sd(output))
