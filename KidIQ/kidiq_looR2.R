#' ---
#' title: "Regression and Other Stories: KidIQ LOO-R2"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Linear regression and leave-one-out cross-validation R-squared.
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("loo")
library("foreign")

#' **Load children's test scores data**
kidiq <- read.dta(file=root("KidIQ/data","kidiq.dta"))

#' ### Estimate the predictive performance of a model using LOO-CV
#+ results='hide'
fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq, seed=1507)
#+
loo_3 <- loo(fit_3)
print(loo_3)

#' **Add five pure noise predictors to the data
set.seed(1507)
n=nrow(kidiq)
kidiqr <- kidiq
kidiqr$noise <- array(rnorm(5*n), c(n,5))

#' ### Compare different models with LOO-CV
#' 
#' **Linear regression with additional noise predictors**
#+ results='hide'
fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr, seed=1507)

#' **Linear regression with interaction**
#+ results='hide'
fit_4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_iq:mom_hs, data=kidiq, seed=1507)

#' ### R2 without and with LOO-CV
R2 <- function(fit) {
    y <- get_y(fit)
    ypred <- posterior_linpred(fit)
    e <- colMeans(ypred)-y
    return(1-var(e)/var(y))
}
looR2 <- function(fit) {
    y <- get_y(fit)
    ypred <- posterior_linpred(fit)
    ll <- log_lik(fit)
    r_eff <- relative_eff(exp(ll), chain_id = rep(1:4, each = 1000))
    psis_object <- psis(log_ratios = -ll, r_eff = r_eff)
    ypredloo <- E_loo(ypred, psis_object, log_ratios = -ll)$value
    eloo <- ypredloo-y
    return(1-var(eloo)/var(y))
}
#' **R2 without LOO-CV**<br>
#' R2 increases when five noise predictors are addded
round(R2(fit_3),2)
round(R2(fit_3n),2)
#' **R2 with LOO-CV**<br>
#' LOO-R2 decreases when five noise predictors are addded
round(looR2(fit_3),2)
round(looR2(fit_3n),2)
#' LOO-R2 increases when interaction is addded
round(looR2(fit_4),2)
