#' ---
#' title: "Regression and Other Stories: Earnings"
#' author: "Andrew Gelman, Jennifer Hill, Aki Vehtari"
#' date: "`r format(Sys.Date())`"
#' ---

#' Predict respondents' yearly earnings using survey data from 1990.
#' 
#' -------------
#' 

#' **Load libraries**
#+ setup, message=FALSE, error=FALSE, warning=FALSE
library("rprojroot")
root<-has_dirname("RAOS-Examples")$make_fix_file()
library("rstanarm")
options(mc.cores = parallel::detectCores())
library("ggplot2")
library("bayesplot")
theme_set(bayesplot::theme_default(base_family = "sans"))

#' **Load data**
earnings_all <- read.csv(root("Earnings/data","earnings.csv"))
earnings_all$positive <- earnings_all$earn > 0
# scale earnings to thousands of dollars
earnings_all$earnk <- earnings_all$earn/1000 
# only non-zero earnings
earnings <- earnings_all[earnings_all$positive, ]
n <- nrow(earnings)
height_jitter_add <- runif(n, -.2, .2)

#' ### Compound discrete-continuos model

#' **Logistic regression on non-zero earnings**
#+ results='hide'
fit_2a <- stan_glm(positive ~ height + male,
                   family = binomial(link = "logit"),
                   data = earnings_all)
sims_2a <- as.matrix(fit_2a)
#+
print(fit_2a, digits=2)

#' **Linear regression on log scale**
earnings$log_earn <- log(earnings$earn)
#+ results='hide'
fit_2b <- stan_glm(log_earn ~ height + male, data = earnings)
sims_2b <- as.matrix(fit_2b)
#+
print(fit_2b, digits=2)

#' **Predictions for a new person**
new <- data.frame(height = 68, male = 0, positive=1)
pred_2a <- posterior_predict(fit_2a, newdata=new)
pred_2b <- posterior_predict(fit_2b, newdata=new)
pred <- ifelse(pred_2a == 1, exp(pred_2b), 0)
