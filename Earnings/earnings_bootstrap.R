#' ---
#' title: "Regression and Other Stories: Earnings"
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

#' Bootstrapping to simulate the sampling distribution. See Chapter 5 in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()

#' #### Load data
earnings <- read.csv(root("Earnings/data","earnings.csv"))
head(earnings)

#' #### Median of women's earnings, divided by the median of men's earnings
earn <- earnings$earn
male <- earnings$male
print(median(earn[male==0]) / median(earn[male==1]))

#' #### A single bootstrap sample
n <- nrow(earnings)
boot <- sample(n, replace=TRUE)
earn_boot <- earn[boot]
male_boot <- male[boot]
ratio_boot <- median(earn_boot[male_boot==0]) / median(earn_boot[male_boot==1])

#' #### A set of bootstrap simulations
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

#' #### Summarize the results graphically and numerically
hist(output)
round(sd(output), 2)
