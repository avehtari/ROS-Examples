#' ---
#' title: "Regression and Other Stories: Imputation"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Regression-based imputation for the Social Indicators Survey
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' **Show Stan code**
writeLines(readLines(root("Restaurant","restaurant.stan")))

#' **Compile Stan code**
#+ results='hide'
resto <- stan_model("restaurant.stan")

#' **Optimize**
fit <- optimizing(resto)
print(fit)
