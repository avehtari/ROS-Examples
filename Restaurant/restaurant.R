#' ---
#' title: "Regression and Other Stories: Restaurant"
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

#' Demonstration of using Stan for optimization. See Appendix B in
#' Regression and Other Stories.
#' 
#' -------------
#' 

#+ setup, include=FALSE
knitr::opts_chunk$set(message=FALSE, error=FALSE, warning=FALSE, comment=NA)

#' #### Load packages
library("rprojroot")
root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstan")
rstan_options(auto_write = TRUE)

#' #### Show Stan code
writeLines(readLines(root("Restaurant","restaurant.stan")))

#' #### Compile Stan code
#+ results='hide'
resto <- stan_model(root("Restaurant","restaurant.stan"))

#' #### Optimize
fit <- optimizing(resto)
print(fit)
