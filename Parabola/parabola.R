#' ---
#' title: "Regression and Other Stories: Parabola"
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

#' #### Plot a parabola $y = 15 + 10 x - 2 x^2$ 
curve(15+10*x-2*x^2, from=-2, to=5)

#' #### Show Stan code
writeLines(readLines(root("Parabola","parabola.stan")))

#' #### Compile Stan code
#+ results='hide'
model <- stan_model(root("Parabola","parabola.stan"))
# alternative way
# model <- stan_model(model_code="parameters {real x;} model {target += 15 + 10*x - 2*x^2;}")

#' #### Optimize
fit <- optimizing(model)
print(fit)
