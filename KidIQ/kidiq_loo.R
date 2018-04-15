#' ---
#' title: "Regression and Other Stories: KidIQ cross-validation"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Linear regression and leave-one-out cross-validation.
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
library("loo")
library("foreign")

#' **Load children's test scores data**
kidiq <- read.dta(file=root("KidIQ/data","kidiq.dta"))

#' ### Multiple predictors
#' 

#' **Maximum likelihood regression with the original predictors**
fit_3 <- glm(kid_score ~ mom_hs + mom_iq, data=kidiq)

#' **Alternative displays in R**
display(fit_3)
print(fit_3)
summary(fit_3)

#' **Add five pure noise predictors to the data
set.seed(1507)
n=nrow(kidiq)
kidiqr <- cbind(kidiq, noise = array(rnorm(5*n), c(n,5)))

#' **Maximum likelihood regression with additional noise predictors**
fit_3n <- glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr)
display(fit_3n)
print(fit_3n)
summary(fit_3n)

#' **Bayesian regression with the original predictors**
stan_fit_3 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=kidiq)
print(stan_fit_3)

#' **Bayesian regression with additional noise predictors**
stan_fit_3n <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=kidiqr)
print(stan_fit_3n)

#' **Leave-one-out cross-validation**
loo_3 <- loo(stan_fit_3)
print(loo_3)
loo_3n <- loo(stan_fit_3n)
print(loo_3n)
compare_models(loo3, loo3n)

#' **Bayesian regression with transformation and interaction**
stan_fit_5 <- stan_glm(kid_score ~ mom_hs + log(mom_iq) + log(mom_iq)*mom_hs, data=kidiq)
print(stan_fit_5)

#' **Leave-one-out cross-validation**
(loo5 <- loo(stan_fit_5))
compare_models(loo3, loo5)

#' **R2 without and with cross-validation**
R2 <- function(fit) {
    y <- get_y(fit)
    ypred <- posterior_linpred(fit)
    e <- colMeans(ypred)-y
    return(1-var(e)/var(y))
}
R2loo <- function(fit) {
    y <- get_y(fit)
    ypred <- posterior_linpred(fit)
    ll <- log_lik(fit)
    r_eff <- relative_eff(exp(ll), chain_id = rep(1:4, each = 1000))
    psis_object <- psis(log_ratios = -ll, r_eff = r_eff)
    ypredloo <- E_loo(ypred, psis_object, log_ratios = -ll)$value
    eloo <- ypredloo-y
    return(1-var(eloo)/var(y))
}
round(R2(stan_fit_3),2)
round(R2(stan_fit_3n),2)
round(R2loo(stan_fit_3),2)
round(R2loo(stan_fit_3n),2)

