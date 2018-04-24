#' ---
#' title: "Regression and Other Stories: KidIQ cross-validation"
#' author: "Andrew Gelman, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Linear regression and K-fold cross-validation.
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("foreign")

#' **Load children's test scores data**
kidiq <- read.dta(file=root("KidIQ/data","kidiq.dta"))

#' **Bayesian regression with the original predictors**
stan_fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
print(stan_fit_3)

#' **Leave-one-out cross-validation**
loo3 <- loo(stan_fit_3)
loo3

#' **K-fold cross-validation**
kcv3 <- kfold(stan_fit_3)
kcv3
