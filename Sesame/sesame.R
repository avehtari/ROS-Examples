#' ---
#' title: "Regression and Other Stories: Electric Company"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Analysis of "Electric company" data
#' 
#' -------------
#' 

#' **Load packages**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("foreign")

#' **Load data**
sesame <- read.dta(file=root("Sesame/data","sesame.dta"))
sesame$encouraged <- sesame$encour
sesame$watched <- sesame$regular
sesame$y <- sesame$postlet
sesame$pretest <- sesame$prelet

#' **Estimate the percentage of children actually induced to watch
#' Sesame Street by the intervention**
#+ results='hide'
fit_1a <- stan_glm(watched ~ encouraged, data=sesame)
#+
print(fit_1a, digits=2)

#' **Estimate the percentage of children actually induced to watch
#' Sesame Street by the intervention**
#+ results='hide'
fit_1a <- stan_glm(watched ~ encouraged, data=sesame)
#+
print(fit_1a, digits=2)

#' **Compute the intent-to-treat estimate**
#+ results='hide'
fit_1b <- stan_glm(y ~ encouraged, data=sesame)
#+
print(fit_1b, digits=2)

#' **`Inflate'' by dividing by the percentage of children affected by the intervention**
iv_est <- coef(fit_1b)["encouraged"] / coef(fit_1a)["encouraged"]
print(iv_est, digits=2)

#' **Regress the ``treatment'' variable on the randomized instrument**
#+ results='hide'
fit_2a <- stan_glm(watched ~ encouraged, data=sesame)
#* **Plug predicted values into the equation predicting the outcome**
sesame$watched_hat_2 <- fit_2a$fitted
fit_2b <- stan_glm(y ~ watched_hat_2, data=sesame)
#+
print(fit_2b, digits=1)

#' **Same with adjusting for covariates**
#+ results='hide'
fit_3a <- stan_glm(watched ~ encouraged + pretest + as.factor(site) + setting,
  data=sesame)
watched_hat_3 <- fit_3a$fitted
fit_3b <- stan_glm(y ~ watched_hat_3 + pretest + as.factor(site) + setting,
  data=sesame)
#+
print(fit_3b, digits=1)

#' **Same with saving the predictor matrix**
#+ results='hide'
fit_3c <- stan_glm(y ~ watched_hat_3 + pretest + as.factor(site) + setting,
                   x = TRUE, data=sesame)

#' **Compute the standard deviation of the adjusted residuals**
X_adj <- fit_3c$x
X_adj[,"watched_hat_3"] <- sesame$watched
residual_sd_adj <- sd(sesame$y - X_adj %*% coef(fit_3c))
se_adj <- se(fit_3c)["watched_hat_3"]*residual_sd_adj / sigma(fit_3c)
print(se_adj, digits=2)
